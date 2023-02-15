#' The Generalised Pareto Distribution
#'
#' These functions provide information about the generalised Pareto distribution:
#'
#' `dgpd` gives the density,
#' `pgpd` gives the distribution function,
#' `qgpd` gives the quantile function,
#' `rgpd` generates random variates.
#'
#'
#' @author Zak Varty
#'
#' @param n 	Number of random variates to generate.
#' @param scale Vector of scale parameters, sigma > 0.
#' @param shape Vector of shape parameters, xi in R.
#' @param shift  Vector of threshold parameters, mu in R.
#' @return Vector of sampled values from generalised Pareto distribution.
#'
#' @examples
#' rgpd(n = 5, scale = 1, shape = 0, shift = 0)
#'
#' rgpd(n = 5, scale = 1:5, shape = 0.1, shift = 0)
#' rgpd(n = 5, scale = 1, shape = 0.1 * 1:5, shift = 0)
#' rgpd(n = 5, scale = 1, shape = 0, shift = 1:5)
#'
#' @export
rgpd <- function(n, scale = 1, shape = 0, shift = 0){

  # Tolerance on |xi| for when to use exponential limit
  TOLERANCE = 1e-10

  # Check that scale value(s) are positive
  stopifnot(exprs = {
    all(scale >= 0)
    length(scale) %in% c(1,n)
    length(shape) %in% c(1,n)
    length(shift) %in% c(1,n)
  })

  # Ensure scale, shape and shift are of same length.
  if ((length(scale) == 1) & (n > 1)) { scale <- rep(scale, n) }
  if ((length(shape) == 1) & (n > 1)) { shape <- rep(shape, n) }
  if ((length(shift) == 1) & (n > 1)) { shift <- rep(shift, n) }

  # Simulate sample
  sample <- shift + (scale / shape) * ((1 - stats::runif(n))^(-shape) - 1)

  # Check for and correct any values from exponential distribution (xi ≈ 0)
  which_shape_near_zero <- which(abs(shape) < TOLERANCE)
  n_shape_near_zero <- length(which_shape_near_zero)

  if (n_shape_near_zero > 0) {
    # correct the sampled values where xi ≈ 0
    exp_rates <- 1 / scale[which_shape_near_zero]
    exp_values <- stats::rexp(n_shape_near_zero, rate = exp_rates)
    shift_values <- shift[which_shape_near_zero]
    sample[which_shape_near_zero] <- shift_values + exp_values
  }

  return(sample)
}
