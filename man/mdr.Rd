% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dimension_reduction.R
\name{mdr}
\alias{mdr}
\title{Perform multifactor dimension reduction (MDR) on the data. Uses cross-validation
and an optional grid search on smoothers, as well as preprocessing for best results.
Follows this tutorial: https://uc-r.github.io/naive_bayes}
\usage{
mdr(df, X_cols, y_col, cv = 5, grid = FALSE, nrows = 1)
}
\arguments{
\item{df}{(data.frame) NEBCS data}

\item{X_cols}{(array<character>) column names for the explanatory variables}

\item{y_col}{(character) column name for the response variable}

\item{cv}{(integer) (default=5) number of cross-validation folds}

\item{grid}{(bool) (default=FALSE) whether to perform a grid search on smoothers}

\item{nrows}{(number) a float between 0 and 1 indicating the percentage of rows to
randomly sample, or a number indicating the number of rows to sample}
}
\value{
(matrix) Pearson correlation matrix
}
\description{
Perform multifactor dimension reduction (MDR) on the data. Uses cross-validation
and an optional grid search on smoothers, as well as preprocessing for best results.
Follows this tutorial: https://uc-r.github.io/naive_bayes
}
\examples{
cor_mat(df)
}
