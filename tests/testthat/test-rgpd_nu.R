test_that("examples work", {
  expect_no_condition(rgpd_nu(n = 5, scale_alt = 1, shape = 0, shift = 0))
  expect_no_condition(rgpd_nu(n = 5, scale_alt = 1:5, shape = 0.1, shift = 0))
  expect_no_condition(rgpd_nu(n = 5, scale_alt = 1, shape = 0.1 * 1:5, shift = 0))
  expect_no_condition(rgpd_nu(n = 5, scale_alt = 1, shape = 0, shift = 1:5))
})

test_that("Output matches rgpd when shape = 0",{
set.seed(1234)
sample_1 <- rgpd(n = 5, scale = 2, shape = 0)
set.seed(1234)
sample_2 <- rgpd_nu(n = 5, scale_alt = 2, shape = 0)
expect_equal(sample_1, sample_2)
})

test_that("Output matches rgpd when shape != 0",{
set.seed(1234)
sample_1 <- rgpd(n = 5, scale = 2, shape = 0.1)
set.seed(1234)
sample_2 <- rgpd_nu(n = 5, scale_alt = 2.2, shape = 0.1)
expect_equal(sample_1, sample_2)
})
