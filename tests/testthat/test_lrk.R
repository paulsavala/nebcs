test_that("make_knots returns a data frame of knots", {
    df = data.frame(X_COORD=1:100, Y_COORD=1:100)
    N_k = 3
    knots_df = make_knots(df, N_k)
    expect_equal(nrow(knots_df), N_k)
    expect_equal(ncol(knots_df), 2)
    expect_setequal(names(knots_df), c("x", "y"))
})


test_that("matern returns real numbers", {
    # Test that the matern function returns real numbers
    expect_type(matern(-1), "double")
    expect_equal(matern(0), 1)
    expect_type(matern(1), "double")
})


test_that("make_dist_mat retruns a matrix of pairwise distances", {
    orig_df = data.frame(x=c(1, 2, 3), y=c(1, 2, 3))
    dist_mat = make_dist_mat(orig_df)
    expect_equal(nrow(dist_mat), 3)
    expect_equal(ncol(dist_mat), 3)
    expect_equal(dist_mat[1, 1], 0)
    expect_equal(dist_mat[1, 2], sqrt(2))
})


test_that("calculate_rho returns a positive value", {
    orig_df = data.frame(x=c(1, 2, 3), y=c(1, 2, 3))
    expect_type(calculate_rho(orig_df), "double")
    expect_type(calculate_rho(orig_df, scale=2), "double")
})
