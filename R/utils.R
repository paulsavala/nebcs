

#' Returns number of rows with NA values in any of the cols supplied
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param cols (list) list of column names to look for NA values
#' @return (number) Number of rows with NA values in any of the cols supplied
#' @examples
#' has_na(df, c('YEAR', 'CIGDUR'))
has_na = function(df, cols) {
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
#' filter_na(df, c('YEAR', 'CIGDUR'))
filter_na = function(df, cols) {
    if (length(cols) > 1) {
        return(df[rowSums(is.na(df[, cols])) == 0, cols])
    } else {
        return(df[is.na(df[, cols]) == FALSE, cols])
    }
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


#' Calculate the powerset of a list or similar object
#' @export
#'
#' @param s (set, list, array) 1-dimensional object
#' @return (set, list, array) Object of the same type as parameter s
#' @examples
#' powerset(c(1, 2, 3))
powerset = function(s){
    len = length(s)
    l = vector(mode="list",length=2^len) ; l[[1]]=numeric()
    counter = 1L
    for(x in 1L:length(s)){
        for(subset in 1L:counter){
            counter=counter+1L
            l[[counter]] = c(l[[subset]],s[x])
        }
    }
    return(l)
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
  return(X_oh)
}


#' Euclidean distance function
#' @export
#'
#' @param x1 (tuple<real>) A tuple of real numbers
#' @param x2 (tuple<real>) A tuple of real numbers
#' @return (real) Euclidean distance between x1 and x2
#' @examples
#' dist(c(1, 2), c(3, 4))
euc_dist = function(x1, x2) {
  d = sqrt(sum((x1 - x2) ^ 2))
  return(d)
}