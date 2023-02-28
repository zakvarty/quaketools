
test_that("examples work", {
  expect_no_condition(pgpd(q = seq(-1, 2), shape = 0, scale = 1))
  expect_no_condition(pgpd(q = seq(-1, 2), shape = 1e-11, scale = 1))
  expect_no_condition(pgpd(q = 1, shape = c(0,-1), scale = c(0.1,1)))
})


test_that("exponential example matches theory", {
  intended_output <- c(0, 0, 1 - exp(-1), 1 - exp(-2))
  pgpd_output_0 <- pgpd(q = seq(-1, 2), scale = 1, shape = 0)
  pgpd_output_1 <- pgpd(q = seq(-1, 2), scale = 1, shape = 1e-11)

  expect_equal(intended_output, pgpd_output_0)
  expect_equal(intended_output, pgpd_output_1)
})

test_that("UEP correction works",{
  expect_equal(pgpd(q = 5,scale = 2,shape = -0.5), 1)
})
