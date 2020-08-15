
test_that("globalfit_stats", {
  set.seed(0)
  r <- globalfit(rnorm(n = 1000, mean = 10, sd = 1),
    packages = "stats", verbose = TRUE
  )
  expect_equal(r@continuity, TRUE)
  expect_equal(r@method, "MLE")
  expect_equal(2 * 2, 4)
})
