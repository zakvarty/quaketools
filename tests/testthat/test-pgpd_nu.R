test_that("matches pgpd", {
  # When shape = 0
  expect_equal(
    pgpd(q = 1.5, scale = 1, shape = 0),
    pgpd_nu(q = 1.5, scale_alt = 1, shape = 0)
  )

  # When shape > 0
  expect_equal(
    pgpd(q = 1.5, scale = 1, shape = 0.5),
    pgpd_nu(q = 1.5, scale_alt = 1.5, shape = 0.5)
  )

  # When shape < 0
  expect_equal(
    pgpd(q = 1.5, scale = 1, shape = -0.5),
    pgpd_nu(q = 1.5, scale_alt = 0.5, shape = -0.5)
  )
})
