test_that("examples match with sigma parameterisation", {
  expect_equal(
    qgpd_nu(p = 0.9, scale_alt = 1.1, shape = 0.1, shift = 0),
    qgpd(p = 0.9, scale = 1, shape = 0.1, shift = 0)
  )

  expect_equal(
  qgpd_nu(p = 0.9, scale_alt = 1.1, shape = 0.1, shift = 1),
  qgpd(p = 0.9, scale = 1, shape = 0.1, shift = 1)
  )

  expect_equal(
  qgpd_nu(p = c(0.8, 0.9, 1), scale_alt = 1, shape = 0, shift = 0),
  qgpd(p = c(0.8, 0.9, 1), scale = 1, shape = 0, shift = 0)
  )

  expect_equal(
  qgpd_nu(p = c(0.8, 0.9, 1), scale_alt = 1.1, shape = 0.1, shift = 0),
  qgpd(p = c(0.8, 0.9, 1), scale = 1, shape = 0.1, shift = 0)
  )

  expect_equal(
  qgpd_nu(p = c(0.8, 0.9, 1), scale_alt = 0.9, shape = -0.1, shift = 0),
  qgpd(p = c(0.8, 0.9, 1), scale = 1, shape = -0.1, shift = 0)
  )

  expect_equal(
  qgpd_nu(p = 0.1, scale_alt = c(1,2,3), shape = 0, shift = c(1,2,3)),
  qgpd(p = 0.1, scale = c(1,2,3), shape = 0, shift = c(1,2,3))
  )
})
