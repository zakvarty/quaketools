#' The Generalised Pareto Distribution
#'
#' These functions provide information about the generalised Pareto distribution
#' using the nu parametrisation. For shape parameters greater than xi > -0.5, nu and xi are
#' approximately orthogonal.
#'
#' * `dgpd_nu()` gives the probability density function,
#' * `pgpd_nu()` gives the cumulative distribution function,
#' * `qgpd_nu()` gives the quantile function,
#' * `rgpd_nu()` generates random variates.
#'
#' @details
#' Setting xi < 1e-10 draws from an exponential distribution. This is equivalent
#'  to `rexp()` but the seed is not handled in the same way.
#'
#' @author Zak Varty
#'
#' @param n Number of random variates to generate.
#' @param scale_alt Vector of alternative scale parameters, nu = sigma * (1 + xi).
#' @param shape Vector of shape parameters, xi in R.
#' @param shift  Vector of threshold parameters, mu in R.
#' @param shape_tolerance Not intended for standard use. Scalar value, such that when `abs(shape) < shape_tolerance`, values are simulated from an exponential distribution.
#' @return Vector of sampled values from generalised Pareto distribution.
#'
#' @examples
#' rgpd_nu(n = 5, scale_alt = 1, shape = 0, shift = 0, shape_tolerance = 1e-10)
#'
#' rgpd_nu(n = 5, scale_alt = 1:5, shape = 0.1, shift = 0)
#' rgpd_nu(n = 5, scale_alt = 1, shape = 0.1 * 1:5, shift = 0)
#' rgpd_nu(n = 5, scale_alt = 1, shape = 0, shift = 1:5)
#'
#' @export
rgpd_nu <- function(n, scale_alt = 1, shape = 0, shift = 0, shape_tolerance = 1e-10){

  # Check that the implied scale value(s) are positive
  scale <- scale_alt / (1 + shape)
  scale_error_message <-
    'Implied scale parameter(s), scale_alt / 1 + shape, must be positive.'
  if (any(scale <= 0)) { stop(scale_error_message) }

  # generate random variates using the implied scale parameter(s)
  rgpd(n, scale, shape, shift, shape_tolerance)
}
