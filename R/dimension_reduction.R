

#' Wrapper function for gWQS. All parameters are passed directly to the gwqs function of
#' the gWQS package. The parameters listed below are the ones commonly used when we 
#' do WQS. However, you can supply any parameters you wish from the gWQS documentation. 
#' See https://cran.r-project.org/web/packages/gWQS/gWQS.pdf
#' @export
#'
#' @param formula (formula) y-col ~ wqs
#' @param data (data.frame) data
#' @param mix_name (vector<character>) column names for the mixture components
#' @param valid_var (character) column name for the validation variable
#' @param b (number) number of bootstrap samples to use in parameter estimation
#' @param b1_pos (bool) whether weights are derived from models where the beta 
#' values were positive or negative
#' @param b1_constr (bool) whether to apply positive (if b1_pos = TRUE) or negative 
#' (if b1_pos = FALSE) constraints in the optimization function for the weight estimation
#' @param q (number) An integer to specify how mixture variables will be ranked, 
#' e.g. in quartiles (q = 4), deciles (q = 10), or percentiles (q = 100). 
#' If q = NULL then the values of the mixture variables are taken (these must 
#' be standardized)
#' @param family (character) family of the model, e.g. "gaussian", "binomial"
#' @param seed (number) seed for the random number generator
#' @return (results) Results object from gWQS. See gWQS documentation for details
#' and ways to interact with these results.
#' @examples
#' wqs(CASE_CNTL ~ wqs, mix_name = pollutants, data = df, valid_var='validation', 
#'      q = 10, validation = 0.3, b = 2, b1_pos = TRUE, b1_constr = FALSE, 
#'      family = "binomial", seed = 42)
wqs = function(...) {
    results = gwqs(...)
    return(results)
}


#' Perform multifactor dimension reduction (MDR) on the data. Uses cross-validation
#' and an optional grid search on smoothers, as well as preprocessing for best results.
#' Follows this tutorial: https://uc-r.github.io/naive_bayes
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param X_cols (array<character>) column names for the explanatory variables
#' @param y_col (character) column name for the response variable
#' @param cv (integer) (default=5) number of cross-validation folds
#' @param grid (bool) (default=FALSE) whether to perform a grid search on smoothers
#' @param nrows (number) a float between 0 and 1 indicating the percentage of rows to
#' randomly sample, or a number indicating the number of rows to sample
#' @return (matrix) Pearson correlation matrix
#' @examples
#' cor_mat(df)
mdr = function(df, X_cols, y_col, cv=5, grid=FALSE, nrows=1) {
    if (count_na(df[, c(X_cols, y_col)]) > 0) {
        stop("Data contains missing values")
    }
    
    if (nrows <= 1) {
        nrows = round(nrows * nrow(df))
    } else {
        nrows = round(nrows)
    }

    idx = sample(1:nrow(df), nrows, replace=FALSE)
    X = df[idx, X_cols]
    y = df[idx, y_col]

    train_control = caret::trainControl(
                method = "cv", 
                number = cv
            )

    if (grid) {
        search_grid = expand.grid(
                usekernel = c(TRUE, FALSE),
                fL = 0:5,
                adjust = seq(0, 5, by = 1)
            )

        nb = caret::train(
            x = X,
            y = as.factor(y),
            method = 'nb',
            trControl = train_control,
            tuneGrid = search_grid,
            preProc = c("BoxCox", "center", "scale", "pca")
        )
    } else {
        nb = caret::train(
            x = X,
            y = as.factor(y),
            method = 'nb',
            trControl = train_control,
            preProc = c("BoxCox", "center", "scale", "pca")
        )
    }

   return(nb) 
}


#' Perform supervised principal component analysis (SPCA) on the data. The optimal threshold
#' is estimated using cross validation. This produces a threshold which yields a subset of the
#' columns. 
#' See original paper (https://tibshirani.su.domains/ftp/spca.pdf) for details.
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param X_cols (array<character>) column names for the explanatory variables
#' @param y_col (character) (default='CASE_CNTL') column name for the response variable
#' @param min.threshold (number) (default=0.1) minimum threshold for the dimension reduction process
#' @param max.threshold (number) (default=5) maximum threshold for the dimension reduction process
#' @param num.thresholds (integer) (default=100) number of thresholds to use
#' @param test.size (number) (default=0.3) percentage of the data to use for testing during cross-validation
#' @param verbose (bool) (default=TRUE) whether to print progress to the console
#' @return (list) Named list with the following components:
#'  - cols (number) optimal columns (lowest mse)
#'  - model (number) optimal model (lowest mse)
#'  - mse (number) optimal mean squared error
#'  - threshold (number) optimal threshold
#' 
#' @examples
#' res = spca(df, X_cols=c("ARSENIC", ...))
#' res$model
#' res$cols
spca = function(df, X_cols, y_col='CASE_CNTL', min.threshold=0.1, max.threshold=5, num.thresholds=100, test.size=0.3, verbose=TRUE) {
  # Train-test split the data
  tts_df = train_test_split(df, test_size=test.size)

  train_df = tts_df$train
  test_df = tts_df$test

  X_train = train_df[, X_cols]
  y_train = train_df[, y_col]

  X_test = test_df[, X_cols]
  y_test = test_df[, y_col]

  # Center the X variables, using means derived from the training data (per the paper)
  X_train_col_means = colMeans(X_train)
  for (i in 1:ncol(X_train)) {
    X_train[, i] = X_train[, i] - X_train_col_means[i]
  }

  for (i in 1:ncol(X_test)) {
    X_test[, i] = X_test[, i] - X_train_col_means[i]
  }

  # Standardize the coefficients
  X_train_std = sapply(X_train, function(x) (t(x) %*% y_train) / sqrt((t(x) %*% x)))

  # Perform cross-validation to find the optimal threshold
  best_mse = Inf
  best_threshold = NA
  best_cols = NA
  best_model = NA

  thresholds = seq(min.threshold, max.threshold, length.out = num.thresholds)
  for (threshold in thresholds) {
    # Form a reduced data matrix consisting of only those features whose univariate
    # coefficient exceeds a threshold in absolute value
    C_theta = names(X_train_std[abs(X_train_std) > threshold])
    
    # If there are no features above the threshold, that means there will also be none
    # for higher thresholds, so return the best result so far 
    if (length(C_theta) == 0) {
      return(list(cols=best_cols, model=best_model, mse=best_mse, threshold=best_threshold))
    }
    # Otherwise, form a new data matrix with only the selected features
    X_theta = X_train[, C_theta]

    # Compute the SVD of the reduced data matrix
    s = svd(X_theta)
    U = s$u # nrow(X_theta) x num features

    # Get the first principal component
    pc1 = U[, 1]

    # Use this principal component in a logistic regression model to predict the outcome.
    logr_data = data.frame(pc1=pc1, CASE_CNTL=y_train)
    logr = glm('CASE_CNTL ~ 1 + pc1', data=logr_data, family='binomial')

    # Make a prediction on the test data using this model
    X_theta.test = X_test[, C_theta]
    s.test = svd(X_theta.test)
    pc1.test = s.test$u[, 1] # nrow(X_theta) x num features
    newdata = data.frame(pc1=pc1.test, CASE_CNTL=y_test)

    # Calculate metrics and compare to the best model
    preds = predict(logr, newdata=newdata)
    mse = mean((preds - y_test)^2)

    if (mse < best_mse) {
      best_mse = mse
      best_threshold = threshold
      best_cols = C_theta
      best_model = logr
    }
    if (verbose) {
      print(paste("Threshold:", threshold, "MSE:", mse, "Cols:", length(C_theta)))
    }
  }

  return(list(cols=best_cols, model=best_model, mse=best_mse, threshold=best_threshold))
}


#' Reduce dimensions of new data using the model from spca().
#' See original paper (https://tibshirani.su.domains/ftp/spca.pdf) for details.
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param X_cols (array<character>) reduced columns found from spca(). Should use spca()$cols.
#' @param y_col (character) (default='CASE_CNTL') column name for the response variable
#' @param model (Object) best model found from spca(). Should use spca()$model.
#' @return (array<numeric>) Pollutants reduced to one dimension using results from spca().
#' 
#' @examples
#' res = spca(df, X_cols=c("ARSENIC", ...))
#' res$model
#' res$cols
predict_spca = function(df, X_cols, y_col='CASE_CNTL', model) {
  X = df[, X_cols]
  y = df[, y_col]

  s = svd(X)
  U = s$u # nrow(X) x num features

  # Get the first principal component
  pc1 = U[, 1]

  # Form the new data frame
  newdata = data.frame(pc1=pc1, CASE_CNTL=y)

  # Make new predictions using the model
  preds = predict(model, newdata=newdata)

  return(preds)
}
