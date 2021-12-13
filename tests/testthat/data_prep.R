context("Data loading and prep")

test_that("onehot one-hots columns", {
  df = data.frame(a=c(1,2,3,4), b=c(1,2,3,4), c=c(1,2,3,4))
  oh_df = onehot(df, cols=c('a'), drop_orig=TRUE)
  oh_df_keep_orig = onehot(df, cols=c('a'), drop_orig=FALSE)
  
  # Doesn't affect number of rows
  expect_equal(nrow(df), nrow(oh_df))
  expect_equal(nrow(df), nrow(oh_df_keep_orig))
})

test_that("str_length of factor is length of level", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})

test_that("str_length of missing is missing", {
  expect_equal(str_length(NA), NA_integer_)
  expect_equal(str_length(c(NA, 1)), c(NA, 1))
  expect_equal(str_length("NA"), 2)
})
