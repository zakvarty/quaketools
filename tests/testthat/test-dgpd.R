test_that("examples work", {
  evaluation_points <- c(-1, 0, 0.5, 1, 1.9, 2.1, 5)
  expect_no_condition(dgpd(x = evaluation_points, scale = 1, shape = 0))
  expect_no_condition(dgpd(x = evaluation_points, scale = 1, shape = 0.2))
  expect_no_condition(dgpd(x = evaluation_points, scale = 1, shape = -0.2))
})

test_that("exponential case works",{
  evaluation_points <- seq(-1, 4)
  dgpd_values <- dgpd(x = evaluation_points, scale = 2, shape = 0)
  empirical_values <- 0.5 * exp(-0.5 * evaluation_points) * (evaluation_points >= 0)
  expect_equal(dgpd_values, empirical_values)
})

test_that("linear case works", {
  evaluation_points <- seq(-1, 4)
  dgpd_values <- dgpd(x = evaluation_points, scale = 2, shape = -0.5)
  empirical_values <- pmax(0.5 - 0.125 * evaluation_points, 0) * (evaluation_points >= 0)
  expect_equal(dgpd_values, empirical_values)
})

test_that("uniform case works",{
  evaluation_points <- seq(-1, 4)
  dgpd_values <- dgpd(x = evaluation_points, scale = 4, shape = -1)
  empirical_values <-  (evaluation_points >= 0 & evaluation_points < 4) / 4
  expect_equal(dgpd_values, empirical_values)
})
