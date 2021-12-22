# Test data with twelve rows and all PII removed
google_id = '18hwHXVxLKBoDCcQGIHFD8tqjVTjzqhRR'

# test_that("data loads from Google Drive", {
#   df = load_data(google_id)
#   expect_equal(nrow(df), 13)
# })

test_that("load_data parameters work correctly", {
  orig_df = load_data(local_path='test_data.csv')

  # dedup=TRUE removes duplicate rows
  df = load_data(local_path='test_data.csv', dedup=TRUE)
  expect_equal(sum(duplicated(df)), 0)

  # dedup=FALSE leaves duplicate rows
  df = load_data(local_path='test_data.csv', dedup=FALSE)
  expect_equal(nrow(df), nrow(orig_df))

  # drop_missing_coords=TRUE removes rows with missing coordinates
  df = load_data(local_path='test_data.csv', drop_missing_coords=TRUE)
  expect_equal(sum(is.na(df$X_COORD)), 0)
  expect_equal(sum(is.na(df$Y_COORD)), 0)

  # drop_missing_coords=FALSE leaves rows with missing coordinates
  df = load_data(local_path='test_data.csv', drop_missing_coords=FALSE)
  expect_equal(nrow(df), nrow(orig_df))

  # to_numeric=TRUE converts GENDER column to 1's and 2's
  df = load_data(local_path='test_data.csv', to_numeric=TRUE)
  expect_setequal(unique(df$GENDER), c(1, 2))

  # to_numeric=FALSE leaves all columns unchanged
  df = load_data(local_path='test_data.csv', to_numeric=FALSE)
  expect_equal(df, orig_df)

  # N=<int> keeps only the last N years. Note that if someone has less than N years
  # in the data, then they may end up having less than N years of data. So we go to 
  # N + 1 and test that what they have is greater than that.
  df = load_data(local_path='test_data.csv', N=2)
  last_year = max(orig_df$YEAR)
  expect_gt(min(df$YEAR), last_year - 3)

  # N=0 keeps all years
  df = load_data(local_path='test_data.csv', N=0)
  expect_equal(df, orig_df)
})

test_that("fill_missing_cigdur fills missing values", {
  df = load_data(local_path='test_data.csv')
  df = fill_missing_cigdur(df)
  expect_equal(sum(is.na(df$CIGDUR)), 0)
})

test_that("get_health_cols returns all health variable columns", {
  df = load_data(local_path='test_data.csv')
  health_cols = get_health_cols()
  expect_equal(length(health_cols), 10)
}) 

test_that("get_case_cntl returns one CASE_CNTL value per PID", {
  df = load_data(local_path='test_data.csv')
  case_cntl = get_case_cntl(df)
  expect_equal(length(case_cntl), length(unique(df$PID)))
  expect_equal(length(case_cntl$PID), length(unique(case_cntl$PID)))
})

test_that("train_test_split properly splits", {
  df = load_data(local_path='test_data.csv')

  # Split using test size as number of people
  tts_df = train_test_split(df, test_size=1)
  expect_equal(length(unique(tts_df$test$PID)), 1)

  # Split using test size as percentage of people
  tts_df = train_test_split(df, test_size=0.5)
  expect_equal(length(unique(tts_df$test$PID)), 1)

  # Make sure train and test sets are disjoint
  expect_setequal(intersect(tts_df$train$PID, tts_df$test$PID), vector())

  # Check that the combine flag combines train and test sets and sets a new validation column
  tts_df = train_test_split(df, test_size=1, combine=TRUE)
  expect_equal(nrow(tts_df$combined), nrow(df))
  expect_gt(sum(tts_df$combined$validation), 0)
})
