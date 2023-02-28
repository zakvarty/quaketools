#' @inherit rgpd title description
#'
#' @details
#' Any shape values less than `shape_tolerance` are calculated for an exponential
#' distribution using `dexp()`.
#'
#' @author Zak Varty
#'
#' @inheritParams rgpd
#' @param x   Vector of values at which to evaluate the probability density function.
#' @param log Logical; if TRUE then log-density is returned.
#'
#' @return Vector of probability density function values at `x`.
#'
#' @examples
#' evaluation_points <- c(-1, 0, 0.5, 1, 1.9, 2.1, 5)
#' dgpd(x = evaluation_points, scale = 1, shape = 0)
#' dgpd(x = evaluation_points, scale = 1, shape = -0.5)
#'
#' @export
dgpd <- function(x, scale = 1, shape = 0, shift = 0, shape_tolerance = 1e-10, log = FALSE){

  # Check inputs
  input_lengths <- c(length(x), length(scale), length(shape), length(shift))
  n <- max(input_lengths)
  stopifnot(exprs = {
    all(scale > 0)
    length(x) %in% c(1,n)
    length(scale) %in% c(1,n)
    length(shape) %in% c(1,n)
    length(shift) %in% c(1,n)
    length(shape_tolerance) == 1
    shape_tolerance >= 0
    length(log) == 1
    log %in% c(TRUE, FALSE)
  })

  # Ensure x, scale, shape and mu are of same length.
  if ((length(scale) == 1) & (n > 1)) { scale <- rep(scale, n) }
  if ((length(shape) == 1) & (n > 1)) { shape <- rep(shape, n) }
  if ((length(shift) == 1) & (n > 1)) { shift <- rep(shift, n) }
  if ((length(x) == 1) & (n > 1)) { x <- rep(x, n) }

  # Calculate the GPD (log-)density at each point in x
  if (log) {
    out <- -log(scale) + ((-1 / shape) - 1) * log(pmax((1 + shape * (x - shift) / scale), 0))
  } else {
    out <- (scale ^ (-1)) * pmax((1 + shape * (x - shift) / scale), 0) ^ ((-1 / shape) - 1)
  }

  # Amend values below threshold
  out_of_support_value <- ifelse(log, yes = -Inf, no = 0)
  out[which(x < shift)] <- out_of_support_value

  # Amend values above upper endpoint (if it exists)
  out[which((shape < 0) & (x >= (shift - scale / shape)))] <- out_of_support_value

  # Check for and correct any values from exponential distribution (xi ≈ 0)
  which_shape_near_zero <- which(abs(shape) <= shape_tolerance)
  n_shape_near_zero <- length(which_shape_near_zero)

  if (n_shape_near_zero > 0) {
    exp_xs <- x[which_shape_near_zero] - shift[which_shape_near_zero]
    exp_rates <- 1 / scale[which_shape_near_zero]
    exp_densities <- stats::dexp(x = exp_xs, rate = exp_rates, log = log)
    out[which_shape_near_zero] <- exp_densities
  }

  return(out)
}

