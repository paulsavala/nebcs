# NEBCS data loading, processing and analysis
Author: Paul Savala, St. Edward's University

## Installation
From an R terminal:
```
# Install devtools if you haven't already
install.packages('devtools')
library(devtools)
devtool::install_github('paulsavala/nebcs')
library(nebcs)
```

You may be prompted with "These packages have more recent versions available." Choose to update `CRAN packages only` or `None`, either is fine.

## Data loading
You must supply your own copy of the NEBCS data, it _is not_ included in this 
repo. However, once you have your own copy you can process it using the functions
below.

`df = load_data(path, dedup=FALSE, drop_missing_coords=FALSE, to_numeric=FALSE, N=0)`

Params:
- path (str) path to data file
- dedup (bool) (Optional) Whether or not to remove duplicate rows (occurs when someone moves)
- drop_missing_coords (bool) (Optional) Whether or not to drop rows with missing X/Y coordinates
- to_numeric (bool) (Optional) Whether or not to convert character columns (gender) to numeric
- N (int) (Optional) A positive integer indicating the years to keep for each person

## Health variables
To-do

## Pollutants
To-do

## Low-rank kriging
To-do

## Dimension reduction
To-do

## Graphing
To-do

## Model fitting
To-do

## Fit analysis
To-do
