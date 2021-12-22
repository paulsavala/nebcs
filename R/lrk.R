#' Choose knots according to a swapping algorithm. Uses cover.design from the
#' fields package.
#' @export
#'
#' @param df (data.frame) NEBCS data
#' @param N_k (int) Number of knots to choose
#' @param ... Additional named arguments to pass to cover.design
#' @return (data.frame) Data frame with columns x and y
#' @examples
#' get_case_cntl(df)
make_knots = function(df, N_k, ...) {
  # remove duplicate locations
  loc_df = df[, c('X_COORD', 'Y_COORD')]
  names(loc_df) = c('x', 'y')
  loc_df = loc_df[!duplicated(loc_df), ]
  design = fields::cover.design(R=loc_df, nd=N_k, ...)
  knots_df = as.data.frame(design$design)
  return(knots_df)
}


#' Matern function
#' @export
#'
#' @param d (real) A real number
#' @return (real) Matern function evaluated at d
#' @examples
#' matern(-1.23)
matern = function(d) {
  return(1 + abs(d) * exp(-abs(d)))
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


#' Given a data frame or matrix where the first column corresponds to the x-value
#' and the second column corresponds to the y-value (or vice-versa), returns a
#' matrix where the i-j'th coordinate is the Euclidean distance between the
#' i'th and j'th rows.
#' @export
#'
#' @param K (data.frame or matrix) A data frame or matrix of coordinates
#' @return (matrix) A matrix of Euclidean distances
#' @examples
#' K = data.frame(x=c(1, 2, 3), y=c(4, 5, 6))
#' make_dist_mat(K)
make_dist_mat = function(K) {
  D = array(numeric(), c(nrow(K), nrow(K)))
  for(n1 in 1:nrow(K)) {
    for(n2 in 1:nrow(K)) {
      D[n1, n2] = euc_dist(K[n1,], K[n2,])
    }
  }
  return(D)
}


#' Calculate scaling parameter so that the max value is no more than 10. This allows
#' for the Matern function to give a nice range of values and not just squishing 
#' everything down to 1.0.
#' @export
#'
#' @param K (data.frame or matrix) A data frame or matrix of coordinates
#' @param scale (real) The scale value
#' @return (real) scale * rho
#' @examples
#' calculate_rho(data.frame(x=c(1, 2, 3), y=c(4, 5, 6)))
calculate_rho = function(K, scale=1) {
  # Super-inefficient to do it this way, but also simple
  max_d = 0
  for(n1 in 1:nrow(K)) {
    for(n2 in 1:nrow(K)) {
      d = euc_dist(K[n1,], K[n2,])
      if (d > max_d) {
        max_d = d
      }
    }
  }
  # Scale the data so the max value is 10
  rho = max_d / 10
  return(scale * rho)
}
