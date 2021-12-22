# NEBCS data loading, processing and analysis
Author: Paul Savala, St. Edward's University

## Installation
From an R terminal:
```
# Install devtools if you haven't already
install.packages('devtools')
library(devtools)
devtools::install_github('paulsavala/nebcs')
library(nebcs)
```

You may be prompted with "These packages have more recent versions available." Choose to update `CRAN packages only` or `None`, either is fine.

## Data loading
You must supply your own copy of the NEBCS data, it _is not_ included in this 
repo. However, once you have your own copy you can process it using the functions
below.

`load_data(google_id, dedup=FALSE, drop_missing_coords=FALSE, to_numeric=FALSE, N=0)`

Params:
- `google_id` (str) Google Drive ID of the file. Must be set to shareable to anyone with the link
- `dedup` (bool) (Optional) Whether or not to remove duplicate rows (occurs when someone moves)
- `drop_missing_coords` (bool) (Optional) Whether or not to drop rows with missing X/Y coordinates
- `to_numeric` (bool) (Optional) Whether or not to convert character columns (gender) to numeric
- `N` (int) (Optional) A positive integer indicating the years to keep for each person
- return (data.frame) A data frame of people


`fill_missing_cigdur(df)` - Fill missing smoking values for each CIG_CAT

Params:
- `df` (data.frame) NEBCS data
- return (data.frame) Same data as `df`, but with missing `CIGDUR` values filled


`get_case_cntl(df)` - A data frame with just the `PID` and `CASE_CNTL` values

Params:
- `df` (data.frame) NEBCS data
- return (data.frame) Data frame with just the PID and CASE_CNTL values


`train_test_split(df, test_size, combine=FALSE)` - Train-test split by person, so that no person shows up in both the training and test set. Note that test_size refers to the number/percentage of _people_, not of _rows_.

Params:
- `df` (data.frame) NEBCS data
- `test_size` (number) Float between 0 and 1 indicating a percentage of the data to use as the test set, or integer indicating the number of rows to use as the test set.
- `combine` (bool) (default=FALSE) Whether or not to combine the training and test sets with test set marked as `validation == 1`.
- return (list<data.frame>) Training set, test set, optionally combined set.


## Health variables
`get_health_cols()` - Returns the column names for the health variables.

Params:
- return (array) Array of health column names


## Pollutants
`get_pollutant_cols()` - Returns pollutant columns

Params:
- return (array) Array of pollutant column names


`group_pollutant_cols()` - Returns named list of pollutant column names, grouped by pollutant type

Params:
- return (list<array>) Named list of pollutant column names, grouped by pollutant type


## Low-rank kriging
`make_knots(df, N_k, ...)` - Choose knots according to a swapping algorithm. Uses `cover.design` from the `fields` package.
  
Params:
- `df` (data.frame) Data frame of NEBCS data
- `N_k` (int) Desired number of knots
- `...` Additional named arguments to pass to `cover.design(...)`
- return (data.frame) Data frame with columns `"x"` and `"y"`

  
`matern(d)` - Matern function
  
Params:
- `d` (number) Number to evaluate the matern function at
- return (real) Matern function evaluated at `d`

  
`euc_dist(x1, x2)` - Euclidean distance function
  
Params:
- `x1` (tuple<real>) A tuple of real numbers
- `x2` (tuple<real>) A tuple of real numbers
- return (real) Euclidean distance between `x1` and `x2`

  
`make_dist_mat(K)` - Given a data frame or matrix where the first column corresponds to the x-value and the second column corresponds to the y-value (or vice-versa), returns a matrix where the i-j'th coordinate is the Euclidean distance between the i'th and j'th rows.
Params:
- `K` (data.frame or matrix) A data frame or matrix of coordinates
- return (matrix) A matrix of Euclidean distances

`calculate_rho(K, scale=1)` - Calculate scaling parameter so that the max value is no more than 10. This allows for the Matern function to give a nice range of values and not just squishing everything down to 1.0.
  
Params:
- `K` (data.frame or matrix) A data frame or matrix of coordinates
- `scale` (real) The scale value
- return (real) `scale * rho`

  
## Dimension reduction
`wqs(...)` - Wrapper function for gWQS. All parameters are passed directly to the gwqs function of the gWQS package. The parameters listed below are the ones commonly used when we do WQS. However, you can supply any parameters you wish from the gWQS documentation.  See https://cran.r-project.org/web/packages/gWQS/gWQS.pdf

Params:
- `formula` (formula) y-col ~ wqs
- `data` (data.frame) data
- `mix_name` (vector<character>) column names for the mixture components
- `valid_var` (character) column name for the validation variable
- `b` (number) number of bootstrap samples to use in parameter estimation
- `b1_pos` (bool) whether weights are derived from models where the beta values were positive or negative
- `b1_constr` (bool) whether to apply positive (if `b1_pos = TRUE`) or negative (if `b1_pos = FALSE`) constraints in the optimization function for the weight estimation
- `q` (number) An integer to specify how mixture variables will be ranked, e.g. in quartiles (`q = 4`), deciles (`q = 10`), or percentiles (`q = 100`). If `q = NULL` then the values of the mixture variables are taken (these must be standardized)
- `family` (character) family of the model, e.g. "gaussian", "binomial"
- `seed` (number) seed for the random number generator
- return (results) Results object from gWQS. See gWQS documentation for details and ways to interact with these results.

  
`mdr(df, X_cols, y_col, cv=5, grid=FALSE, nrows=1)` - Perform multifactor dimension reduction (MDR) on the data. Uses cross-validation and an optional grid search on smoothers, as well as preprocessing for best results. Follows this tutorial: https://uc-r.github.io/naive_bayes

Params:
- `df` (data.frame) NEBCS data
- `X_cols` (array<character>) column names for the explanatory variables
- `y_col` (character) column name for the response variable
- `cv` (integer) (default=5) number of cross-validation folds
- `grid` (bool) (default=`FALSE`) whether to perform a grid search on smoothers
- `nrows` (number) a float between 0 and 1 indicating the percentage of rows to randomly sample, or a number indicating the number of rows to sample
- return (matrix) Pearson correlation matrix  

## Graphing
To-do

## Model fitting
To-do

## Fit analysis
To-do

## Utils
`count_na(df, cols=NULL)` - Returns number of rows with `NA` values in any of the cols supplied
  
Params:
- `df` (data.frame) NEBCS data
- `cols` (list) list of column names to look for `NA` values. Defaults to all columns.
- return (number) Number of rows with`NA` values in any of the cols supplied

  
`remove_na(df, cols=NULL)` - Remove rows with `NA` values in any of the requested columns
  
Params:
- `df` (data.frame) NEBCS data
- `cols` (list) list of column names to look for `NA` values
- return (data.frame) Same data as `df`, but with rows with `NA` values in any of the requested columns removed

  
`cor_mat(df)` - Make a correlation matrix with lower diagonal masked (for ease of readability)
  
Params:
- `df` (data.frame) NEBCS data
- return (matrix) Pearson correlation matrix

  
`onehot(df, cols, drop_orig=TRUE)` - One-hot categorical columns
  
Params:
- `df` (data.frame) Data
- `cols` (array) Array of names of columns to one-hot
- `drop_orig` (bool) (default=TRUE), Whether or not drop the column and only retain the newly created one-hotted columns. If FALSE, the original columns will be included as well as the one-hotted columns.
- return (data.frame) Same data as `df`, additional columns for all columns to one-hot. New column names are of the form `old-col-name_col-value`
