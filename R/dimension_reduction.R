

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
    if (has_na(df[, c(X_cols, y_col)]) > 0) {
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