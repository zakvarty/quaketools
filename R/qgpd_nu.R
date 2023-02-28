#' @inherit qgpd title description details
#'
#' @author Zak Varty
#'
#' @inheritParams qgpd
#' @param scale_alt Vector of alternative scale parameters, \eqn{\nu = \sigma * (1 + \xi)}.
#'
#' @examples
#' qgpd_nu(p = 0.9, scale_alt = 1.1, shape = 0.1, shift = 0)
#' qgpd_nu(p = 0.9, scale_alt = 1.1, shape = 0.1, shift = 1)
#'
#' qgpd_nu(p = c(0.8, 0.9, 1), scale_alt = 1, shape = 0, shift = 0)
#' qgpd_nu(p = c(0.8, 0.9, 1), scale_alt = 1.1, shape = 0.1, shift = 0)
#' qgpd_nu(p = c(0.8, 0.9, 1), scale_alt = 0.9, shape = -0.1, shift = 0)
#'
#' qgpd_nu(p = 0.1, scale_alt = c(1,2,3), shape = 0, shift = c(1,2,3))
#'
#' @export
qgpd_nu <- function(p, scale_alt = 1, shape = 0, shift = 0, shape_tolerance = 1e-10){

  # Check that the implied scale value(s) are positive
  scale <- scale_alt / (1 + shape)
  scale_error_message <-
    'Problem with implied scale parameters. scale_alt / (1 + shape) must be positive.'
  if (any(scale <= 0)) { stop(scale_error_message) }

  # Calculate density values using the implied scale parameter(s)
  qgpd(p, scale, shape, shift, shape_tolerance)
}
