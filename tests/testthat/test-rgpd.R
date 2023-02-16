test_that("examples work", {
  expect_no_condition(rgpd(n = 5, scale = 1, shape = 0, shift = 0))
  expect_no_condition(rgpd(n = 5, scale = 1:5, shape = 0.1, shift = 0))
  expect_no_condition(rgpd(n = 5, scale = 1, shape = 0.1 * 1:5, shift = 0))
  expect_no_condition(rgpd(n = 5, scale = 1, shape = 0, shift = 1:5))
})

test_that("non-positive scale parameters are flagged", {
  expect_error(rgpd(n = 1, scale = -1, shape = 0, shift = 0))
  expect_error(rgpd(n = 1, scale = 0, shape = 0, shift = 0))
  expect_error(rgpd(n = 1, scale = c(1,-1), shape = 0, shift = 0))
  expect_error(rgpd(n = 1, scale = c(1,0), shape = 0, shift = 0))
})

test_that("negative shape_tolerance is flagged",{
  expect_error(rgpd(n = 5, scale = 1,shape = 0,shift = 0, shape_tolerance = -1))
})

test_that("shape_tolerance of length > 1 is flagged", {
  expect_error(rgpd(n = 5, scale = ,shape = 0,shift = 0, shape_tolerance = 1:5 * 1e-5))
})

test_that("setting the seed works", {
  set.seed(1234)
  gpd_sample_2 <- rgpd(n = 5, scale = 1, shape = 0, shift = 0)
  set.seed(1234)
  gpd_sample_1 <- rgpd(n = 5, scale = 1, shape = 0, shift = 0)
  expect_equal(gpd_sample_1, gpd_sample_2)
})
