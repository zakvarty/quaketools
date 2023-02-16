#' @inherit dgpd title description details
#'
#' @author Zak Varty
#'
#' @inheritParams dgpd
#' @param scale_alt Vector of alternative scale parameters, \eqn{\nu = \sigma * (1 + \xi)}.
#'
#' @examples
#' evaluation_points <- c(-1, 0, 0.5, 1, 1.9, 2.1, 5)
#'
#' # equivalent to dgpd when shape = 0
#' dgpd(x = evaluation_points, scale = 1, shape = 0)
#' dgpd_nu(x = evaluation_points, scale_alt = 1, shape = 0)
#'
#' # differs when shape != 0
#' dgpd(x = evaluation_points, scale = 1, shape = -0.5)
#' dgpd_nu(x = evaluation_points, scale_alt = 1, shape = -0.5)
#'
#' @export
dgpd_nu <- function(x, scale = 1, shape = 0, shift = 0, shape_tolerance = 1e-10, log = FALSE){

  # Check that the implied scale value(s) are positive
  scale <- scale_alt / (1 + shape)
  scale_error_message <-
    'Problem with implied scale parameters. scale_alt / (1 + shape) must be positive.'
  if (any(scale <= 0)) { stop(scale_error_message) }

  # calculate density values using the implied scale parameter(s)
  dgpd(x, scale, shape, shift, shape_tolerance)
}
