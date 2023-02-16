test_that("examples work", {
  evaluation_points <- c(-1, 0, 0.5, 1, 1.9, 2.1, 5)
  expect_no_condition(dgpd_nu(x = evaluation_points, scale_alt = 1, shape = 0))
  expect_no_condition(dgpd_nu(x = evaluation_points, scale_alt = 1, shape = -0.5))
})

test_that("output matches dgpd when shape = 0", {
  evaluation_points <- c(-1, 0, 0.5, 1, 1.9, 2.1, 5)
  sigma_values <- dgpd(x = evaluation_points, scale = 2, shape = 0)
  nu_values <- dgpd_nu(x = evaluation_points, scale_alt = 2, shape = 0)
  expect_equal(sigma_values, nu_values)
})
