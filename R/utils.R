

#' Returns number of rows with NA values in any of the cols supplied
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param cols (list) list of column names to look for NA values. Defaults to all columns.
#' @return (number) Number of rows with NA values in any of the cols supplied
#' @examples
#' count_na(df, c('YEAR', 'CIGDUR'))
count_na = function(df, cols=NULL) {
    if (is.null(cols)) {
        cols = names(df)
    }
    if (length(cols) > 1) {
        return(sum(rowSums(is.na(df[, cols])) > 0))
    } else {
        return(sum(is.na(df[, cols])))
    }
}


#' Remove rows with NA values in any of the requested columns
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param cols (list) list of column names to look for NA values
#' @return (data.frame) Same data as df, but with rows with NA values in any of the requested columns removed
#' @examples
#' remove_na(df, c('YEAR', 'CIGDUR'))
remove_na = function(df, cols=NULL) {
    if (is.null(cols)) {
        cols = names(df)
    }
    if (length(cols) > 1) {
        return(df[rowSums(is.na(df[, cols])) == 0, ])
    } else {
        return(df[is.na(df[, cols]) == FALSE, ])
    }
}

#' Fill rows with NA with the mean or median of the column
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param cols (list) list of column names to look for NA values
#' @param method (str) (default "median") "mean" or "median"
#' @return (data.frame) Same data as df, but with rows with NA values in any of the requested columns filled
#' @examples
#' fill_na(df, c('YEAR', 'CIGDUR'), method="mean")
fill_na = function(df, cols=NULL, method="median") {
    # Default to filling all columns
    if (is.null(cols)) {
        cols = names(df)
    }

    # Fill NA values with the mean or median of the column
    if (method == "mean") {
        for (col in cols) {
            df[is.na(df[, col]) == TRUE, col] = mean(df[, col], na.rm=TRUE)
        }
    } else if (method == "median") {
        for (col in cols) {
            df[is.na(df[, col]) == TRUE, col] = median(df[, col], na.rm=TRUE)
        }
    } else {
        stop("method must be 'mean' or 'median'")
    }
    return(df)
}


#' Make a correlation matrix with lower diagonal masked (for ease of readability)
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @return (matrix) Pearson correlation matrix
#' @examples
#' cor_mat(df)
cor_mat = function(df) {
  cor_mat = cor(df)
  lower_msk = lower.tri(cor_mat, diag=TRUE)
  cor_mat[lower_msk] = NA
  return (cor_mat)
}


#' Calculate the powerset of a list or similar object.
#' Source: https://rdrr.io/cran/rje/src/R/powerSet.R
#' @export
#'
#' @param x (set, list, array) 1-dimensional object
#' @return (set, list, array) Object of the same type as parameter s
#' @examples
#' powerset(c(1, 2, 3))
powerset = function (x) {
    m = length(x)
    if (m == 0) return(list(x[c()]))
  
    out = list(x[c()])
    if (length(x) == 1) 
        return(c(out, list(x)))
    for (i in seq_along(x)) {
        out = c(out, lapply(out[lengths(out) < m], function(y) c(y, x[i])))
    }
    out
}


#' One-hot categorical columns
#' @export
#'
#' @param df (data.frame) Data
#' @param cols (array) Array of names of columns to one-hot
#' @param drop_orig (bool) (default=TRUE), Whether or not drop the column and
#' only retain the newly created one-hotted columns. If FALSE, the original
#' columns will be included as well as the one-hotted columns.
#' @return (data.frame) Same data as df, additional columns for all columns
#' to one-hot. New column names are of the form `old-col-name_col-value`
#' @examples
#' onehot(df, cols=c('col1', 'col2'))
onehot = function(df, cols, drop_orig=TRUE) {
  for (c in cols) {
    df[, c] = as.factor(df[, c])
  }
  X_oh = mltools::one_hot(data.table::as.data.table(df), cols=cols, dropCols=drop_orig)
  return(as.data.frame(X_oh))
}