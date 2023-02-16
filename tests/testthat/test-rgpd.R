test_that("setting the seed works", {
  set.seed(1234)
  gpd_sample_2 <- rgpd(n = 5, scale = 1, shape = 0, shift = 0)
  set.seed(1234)
  gpd_sample_1 <- rgpd(n = 5, scale = 1, shape = 0, shift = 0)
  expect_equal(gpd_sample_1, gpd_sample_2)
})

