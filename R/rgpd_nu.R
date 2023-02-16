#' @inherit rgpd title description details
#'
#' @author Zak Varty
#'
#' @inheritParams rgpd
#' @param scale_alt Vector of alternative scale parameters, \eqn{\nu = \sigma * (1 + \xi)}.
#'
#' @examples
#' rgpd_nu(n = 5, scale_alt = 1, shape = 0, shift = 0)
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
