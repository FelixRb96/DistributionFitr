context("Global fit results")

test_that("globalfit_stats", {
  set.seed(0)
  expected_result_first <- new("optimParams",
    family = "norm", package = "stats", "estimatedValues" = c(mean = 10, sd = 1), log_lik = -14150.98, AIC = 28305.95, BIC = 28320.37, AICc = 28305.95,
    sanity = list(hist_check = 1, int_check = 1, L1_check = 0.05, good = TRUE)
  )
  r <- globalfit(rnorm(n = 10000, mean = 10, sd = 1), ,
    packages = "stats", verbose = TRUE
  )
  expect_s4_class(r, "globalfit")
  expect_type(r@fits, "list")
  realized_result_first <- r@fits[[1]]
  expect_s4_class(realized_result_first, "optimParams")
  expect_match(realized_result_first@family, expected_result_first@family)
  expect_match(realized_result_first@package, expected_result_first@package)
  expect_equal(realized_result_first@estimatedValues, expected_result_first@estimatedValues, tolerance = 1)
  expect_equal(realized_result_first@log_lik, expected_result_first@log_lik, tolerance = 1)
  expect_equal(realized_result_first@AIC, expected_result_first@AIC, tolerance = 1)
  expect_equal(realized_result_first@BIC, expected_result_first@BIC, tolerance = 1)
  expect_equal(realized_result_first@AICc, expected_result_first@AICc, tolerance = 1)
  expect_equal(realized_result_first@sanity, expected_result_first@sanity, tolerance = 0.1)
})
