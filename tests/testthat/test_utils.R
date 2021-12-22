test_that("count_na identifies NA values", {
  df = data.frame(a=c(NA, 1, 2), b=c(1, NA, 3), c=c(1, 2, 3))

  # Finds NA in a single column
  expect_gt(count_na(df, 'a'), 0)

  # Finds NA in two columns
  expect_gt(count_na(df, c('a', 'b')), 0)

  # Finds NA in all columns
  expect_gt(count_na(df), 0)

  # Fails to find NA when no NA are present
  expect_equal(count_na(df, 'c'), 0)
})


test_that("remove_na removes NA values", {
  orig_df = data.frame(a=c(NA, 1, 2), b=c(1, NA, 3), c=c(1, 2, 3))

  # Filters NA in a single column
  df = remove_na(orig_df, 'a')
  expect_equal(sum(is.na(df$a)), 0)

  # Filters NA in two columns
  df = remove_na(orig_df, c('a', 'b'))
  expect_equal(sum(is.na(df)), 0)

  # Filters NA in all columns
  df = remove_na(orig_df, c('a', 'b', 'c'))
  expect_equal(sum(is.na(df)), 0)

  # Leaves columns with no NA unchanged
  df = remove_na(orig_df, 'c')
  expect_equal(df, orig_df)
})

test_that("cor_mat returns a correlation matrix", {
  df = data.frame(a=c(1, 2, 3), b=c(1, 2, 3), c=c(1, 2, 3))
  cor_mat = cor_mat(df)
  expect_equal(nrow(cor_mat), 3)
  expect_equal(ncol(cor_mat), 3)
  expect_equal(sum(cor_mat, na.rm=TRUE), 3)
})

test_that("powerset returns the powerset of a set", {
  expect_setequal(powerset(c(1, 2, 3)), list(c(1, 2, 3), c(1, 2), c(1, 3), c(2, 3), c(1), c(2), c(3), numeric()))
})

test_that("onehot returns data frame with requested columns onehot-encoded", {
  orig_df = data.frame(a=c(1, 2, 3), b=c('a', 'a', 'b'))

  # Onehot a single column (drop originals)
  df = onehot(orig_df, 'a', drop_orig=TRUE)
  expected = data.frame(a_1=c(1, 0, 0), a_2=c(0, 1, 0), a_3=c(0, 0, 1), b=c('a', 'a', 'b'))
  expect_equal(df, expected)
})

test_that("euc_dist returns the Euclidean distance between two vectors", {
  expect_equal(euc_dist(c(1, 2, 3), c(1, 2, 3)), 0)
  expect_equal(euc_dist(c(1, 2, 3), c(1, 2, 4)), 1)
})
