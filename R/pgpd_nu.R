#' @inherit pgpd title description details
#'
#' @author Zak Varty
#'
#' @inheritParams pgpd
#' @param scale_alt Vector of alternative scale parameters, \eqn{\nu = \sigma * (1 + \xi)}.
#'
#' @examples
#' pgpd_nu(q = seq(-1, 2), shape = 0, scale_alt = 1)
#' pgpd_nu(q = 1, shape = c(0, -0.5), scale_alt = c(0.1,0.5))
#'
#' @export
pgpd_nu <- function(q, scale_alt = 1, shape = 0, shift = 0, shape_tolerance = 1e-10){

  # Check that the implied scale value(s) are positive
  scale <- scale_alt / (1 + shape)
  scale_error_message <-
    'Problem with implied scale parameters. scale_alt / (1 + shape) must be positive.'
  if (any(scale <= 0)) { stop(scale_error_message) }

  # calculate density values using the implied scale parameter(s)
  pgpd(q, scale, shape, shift, shape_tolerance)
}
