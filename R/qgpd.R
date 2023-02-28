#' @inherit rgpd title description
#'
#' @details
#' Any shape values less than `shape_tolerance` are calculated for an exponential
#' distribution using `dexp()`.
#'
#' @author Zak Varty
#'
#' @inheritParams rgpd
#' @param p Vector of probabilities relating to the desired GPD quantiles.
#'
#' @return Quantile function value: \eqn{x_p} such that \eqn{\Pr(X \leq x_p) = p}.
#'
#' @examples
#' qgpd(p = 0.9, scale = 1, shape = 0.1, shift = 0)
#' qgpd(p = 0.9, scale = 1, shape = 0.1, shift = 1)
#'
#' qgpd(p = c(0.8, 0.9, 1), scale = 1, shape = 0, shift = 0)
#' qgpd(p = c(0.8, 0.9, 1), scale = 1, shape = 0.1, shift = 0)
#' qgpd(p = c(0.8, 0.9, 1), scale = 1, shape = -0.1, shift = 0)
#'
#' qgpd(p = 0.1, scale = c(1,2,3), shape = 0, shift = c(1,2,3))
#'
#' @importFrom stats qexp
#' @export
qgpd <- function(p, scale = 1, shape = 0, shift = 0, shape_tolerance = 1e-10){

  # Check inputs
  input_lengths <- c(length(p), length(scale), length(shape), length(shift))
  n <- max(input_lengths)
  stopifnot(exprs = {
    all(scale > 0)
    all(p >= 0)
    all(p <= 1)
    length(p) %in% c(1,n)
    length(scale) %in% c(1,n)
    length(shape) %in% c(1,n)
    length(shift) %in% c(1,n)
    length(shape_tolerance) == 1
    shape_tolerance >= 0
  })

  # Ensure q, scale, shape and mu are of same length.
  if ((length(scale) == 1) & (n > 1)) { scale <- rep(scale, n) }
  if ((length(shape) == 1) & (n > 1)) { shape <- rep(shape, n) }
  if ((length(shift) == 1) & (n > 1)) { shift <- rep(shift, n) }
  if ((length(p) == 1) & (n > 1)) {p <- rep(p, n)}


  # Calculate quantiles
  q <- shift + (scale / shape) * ((1 - p)^(-shape) - 1)

  # Check for and correct any values from exponential distribution (xi ≈ 0)
  which_shape_near_zero <- which(abs(shape) <= shape_tolerance)
  n_shape_near_zero <- length(which_shape_near_zero)

  if (n_shape_near_zero > 0) {
    exp_ps <- p[which_shape_near_zero]
    exp_rates <- 1 / scale[which_shape_near_zero]

    exp_qs <- shift + stats::qexp(p = exp_ps, rate = exp_rates)
    q[which_shape_near_zero] <- exp_qs
  }

  return(q)
}
