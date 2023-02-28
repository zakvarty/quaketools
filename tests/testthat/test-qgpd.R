test_that("error when p not a valid probability", {
  expect_error(qgpd(p = -0.1))
  expect_error(qgpd(p = 1.1))
})

test_that("edge cases work",{
  # Exponential case
  expect_equal(qgpd(p = 1, scale = 1, shape = 0), Inf)
  expect_equal(qgpd(p = 0, scale = 1, shape = 0), 0)

  # Power law case
  expect_equal(qgpd(p = 1, scale = 1, shape = 0.1), Inf)
  expect_equal(qgpd(p = 0, scale = 1, shape = 0.1), 0)

  # Finite upper bound
  expect_equal(qgpd(p = 1, scale = 2, shape = -0.1), 20)
  expect_equal(qgpd(p = 0, scale = 2, shape = -0.1), 0)
})
