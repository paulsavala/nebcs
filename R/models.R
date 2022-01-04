#' Fit a logistic regression model to the data. This is just a wrapper for glm()
#' to simplify the syntax.
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param X_cols (array<character>) names of columns to use for input variable(s)
#' @param y_col (character) (default='CASE_CNTL') column name for the response variable
#' @return (object) Model object
#' 
#' @examples
#' logr(df, X_cols=c('CIGDUR', 'GENDER'))
logr = function(df, X_cols, y_col='CASE_CNTL') {
    X = df[, X_cols]
    y = df[, y_col]
    logr_data = data.frame(X=X, CASE_CNTL=y)
    logr = glm('CASE_CNTL ~ 1 + X', data=logr_data, family='binomial')
    return(logr)
}