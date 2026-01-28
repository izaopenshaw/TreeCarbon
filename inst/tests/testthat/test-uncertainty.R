# Tests for uncertainty propagation functions
# mc_uncertainty(), comprehensive_uncertainty(), helper functions

# ==============================================================================
# mc_uncertainty() tests
# ==============================================================================

test_that("mc_uncertainty returns correct class", {
  fn <- function(x) x^2
  inputs <- list(x = list(mean = 10, sd = 1))

  result <- mc_uncertainty(fn, inputs, N = 100)

  expect_s3_class(result, "mc_uncertainty")
})

test_that("mc_uncertainty returns all required fields", {
  fn <- function(x) x^2
  inputs <- list(x = list(mean = 10, sd = 1))

  result <- mc_uncertainty(fn, inputs, N = 100)

  expect_true("estimate" %in% names(result))
  expect_true("median" %in% names(result))
  expect_true("sd" %in% names(result))
  expect_true("cv" %in% names(result))
  expect_true("ci_low" %in% names(result))
  expect_true("ci_high" %in% names(result))
  expect_true("skewness" %in% names(result))
  expect_true("kurtosis" %in% names(result))
  expect_true("samples" %in% names(result))
})

test_that("mc_uncertainty estimate is close to expected value", {
  # For y = x^2 with x ~ N(10, 1): E[Y] ≈ E[X]^2 + Var[X] = 100 + 1 = 101
  fn <- function(x) x^2
  inputs <- list(x = list(mean = 10, sd = 1))

  result <- mc_uncertainty(fn, inputs, N = 5000, seed = 123)

  expect_equal(result$estimate, 101, tolerance = 3)
})

test_that("mc_uncertainty handles multiple inputs", {
  fn <- function(a, b) a * b
  inputs <- list(
    a = list(mean = 10, sd = 1),
    b = list(mean = 5, sd = 0.5)
  )

  result <- mc_uncertainty(fn, inputs, N = 1000)

  expect_equal(result$estimate, 50, tolerance = 5)
})

test_that("mc_uncertainty respects seed for reproducibility", {
  fn <- function(x) x^2
  inputs <- list(x = list(mean = 10, sd = 1))

  result1 <- mc_uncertainty(fn, inputs, N = 100, seed = 42)
  result2 <- mc_uncertainty(fn, inputs, N = 100, seed = 42)

  expect_equal(result1$estimate, result2$estimate)
  expect_equal(result1$sd, result2$sd)
})

test_that("mc_uncertainty decompose=TRUE returns decomposition", {
  fn <- function(a, b) a * b
  inputs <- list(
    a = list(mean = 10, sd = 1, source = "measurement"),
    b = list(mean = 5, sd = 0.5, source = "parameter")
  )

  result <- mc_uncertainty(fn, inputs, N = 500, decompose = TRUE)

  expect_true(!is.null(result$decomposition))
  expect_true("by_input" %in% names(result$decomposition))
  expect_true("by_source" %in% names(result$decomposition))
})

test_that("mc_uncertainty handles lognormal distribution", {
  fn <- function(x) x
  inputs <- list(x = list(mean = 10, sd = 2, dist = "lognormal"))

  result <- mc_uncertainty(fn, inputs, N = 1000)

  expect_true(all(result$samples > 0))  # Lognormal is positive
  expect_equal(result$estimate, 10, tolerance = 2)
})

test_that("mc_uncertainty handles uniform distribution", {
  fn <- function(x) x
  inputs <- list(x = list(mean = 10, sd = 1, dist = "uniform"))

  result <- mc_uncertainty(fn, inputs, N = 1000)

  expect_equal(result$estimate, 10, tolerance = 1)
})

test_that("mc_uncertainty CI contains median", {
  fn <- function(x) x^2
  inputs <- list(x = list(mean = 10, sd = 1))

  result <- mc_uncertainty(fn, inputs, N = 1000, conf = 0.90)

  expect_true(result$median >= result$ci_low)
  expect_true(result$median <= result$ci_high)
})

# ==== Error handling ====

test_that("mc_uncertainty errors on non-function", {
  expect_error(mc_uncertainty("not_a_function", list(x = list(mean = 10, sd = 1))),
               "must be a function")
})

test_that("mc_uncertainty errors on empty inputs", {
  fn <- function(x) x
  expect_error(mc_uncertainty(fn, list()),
               "non-empty")
})

test_that("mc_uncertainty errors on unnamed inputs", {
  fn <- function(x) x
  expect_error(mc_uncertainty(fn, list(list(mean = 10, sd = 1))),
               "must be named")
})

test_that("mc_uncertainty errors on missing mean/sd", {
  fn <- function(x) x
  expect_error(mc_uncertainty(fn, list(x = list(mean = 10))),
               "mean.*sd")
})

test_that("mc_uncertainty errors on negative sd", {
  fn <- function(x) x
  expect_error(mc_uncertainty(fn, list(x = list(mean = 10, sd = -1))),
               "non-negative")
})

# ==============================================================================
# Helper function tests
# ==============================================================================

test_that("measurement_uncertainty creates correct structure", {
  result <- measurement_uncertainty(30, absolute_error = 0.5)

  expect_equal(result$mean, 30)
  expect_equal(result$sd, 0.5)
  expect_equal(result$source, "measurement")
})

test_that("measurement_uncertainty uses relative error when no absolute", {
  result <- measurement_uncertainty(100, relative_error = 0.05)

  expect_equal(result$sd, 5)  # 100 * 0.05 = 5
})

test_that("parameter_uncertainty creates correct structure", {
  result <- parameter_uncertainty(1.868, se = 0.047)

  expect_equal(result$mean, 1.868)
  expect_equal(result$sd, 0.047)
  expect_equal(result$source, "parameter")
})

test_that("residual_uncertainty creates correct structure", {
  result <- residual_uncertainty(rse = 50)

  expect_equal(result$mean, 0)
  expect_equal(result$sd, 50)
  expect_equal(result$source, "residual")
})

# ==============================================================================
# method_choice_uncertainty tests
# ==============================================================================

test_that("method_choice_uncertainty returns correct structure", {
  estimates <- c(WCC = 1.25, BIOMASS = 1.18, allodb = 1.32, Bunce = 1.15)

  result <- method_choice_uncertainty(estimates)

  expect_true("consensus" %in% names(result))
  expect_true("sd" %in% names(result))
  expect_true("cv" %in% names(result))
  expect_true("range" %in% names(result))
  expect_true("spread" %in% names(result))
  expect_true("individual" %in% names(result))
})

test_that("method_choice_uncertainty consensus equals mean for equal weights", {
  estimates <- c(A = 10, B = 20, C = 30)

  result <- method_choice_uncertainty(estimates)

  expect_equal(result$consensus, 20)  # mean of 10, 20, 30
})

test_that("method_choice_uncertainty spread is correct", {
  estimates <- c(A = 10, B = 20, C = 30)

  result <- method_choice_uncertainty(estimates)

  expect_equal(result$spread, 20)  # 30 - 10
  expect_equal(result$range, c(10, 30))
})

test_that("method_choice_uncertainty respects weights", {
  estimates <- c(A = 10, B = 30)
  weights <- c(A = 3, B = 1)  # 3:1 weighting

  result <- method_choice_uncertainty(estimates, weights)

  # Weighted mean: (3*10 + 1*30)/4 = 60/4 = 15
  expect_equal(result$consensus, 15)
})

test_that("method_choice_uncertainty errors on single estimate", {
  expect_error(method_choice_uncertainty(c(A = 10)),
               "at least 2")
})

# ==============================================================================
# comprehensive_uncertainty tests
# ==============================================================================

test_that("comprehensive_uncertainty returns correct structure", {
  fn <- function(dbh) dbh^2
  inputs <- list(dbh = measurement_uncertainty(30, absolute_error = 0.5))

  result <- comprehensive_uncertainty(fn, inputs, N = 100)

  expect_s3_class(result, "comprehensive_uncertainty")
  expect_true("estimate" %in% names(result))
  expect_true("mc_result" %in% names(result))
  expect_true("total_sd" %in% names(result))
  expect_true("total_cv" %in% names(result))
  expect_true("summary_table" %in% names(result))
})

test_that("comprehensive_uncertainty includes residual when requested", {
  fn <- function(dbh) dbh^2
  inputs <- list(dbh = measurement_uncertainty(30, absolute_error = 0.5))

  result <- comprehensive_uncertainty(fn, inputs, N = 100,
                                      include_residual = TRUE, residual_cv = 0.2)

  # Summary table should include residual
  expect_true(any(grepl("residual", result$summary_table$source)))
})

test_that("comprehensive_uncertainty includes method uncertainty when provided", {
  fn <- function(dbh) dbh^2
  inputs <- list(dbh = measurement_uncertainty(30, absolute_error = 0.5))
  method_estimates <- c(other1 = 850, other2 = 950)

  result <- comprehensive_uncertainty(fn, inputs, N = 100,
                                      method_estimates = method_estimates)

  expect_true(!is.null(result$method_uncertainty))
  expect_true(any(grepl("method", result$summary_table$source)))
})

# ==============================================================================
# quick_uncertainty tests
# ==============================================================================

test_that("quick_uncertainty works for Bunce method", {
  result <- quick_uncertainty("Bunce", dbh = 30, N = 100)

  expect_s3_class(result, "mc_uncertainty")
  expect_true(result$estimate > 0)
})

test_that("quick_uncertainty works for Chave/WCC with height", {
  result <- quick_uncertainty("Chave", dbh = 30, height = 15, N = 100)

  expect_s3_class(result, "mc_uncertainty")
  expect_true(result$estimate > 0)
})

test_that("quick_uncertainty errors when height missing for WCC", {
  expect_error(quick_uncertainty("WCC", dbh = 30),
               "Height required")
})

test_that("quick_uncertainty respects dbh_error parameter", {
  result_low <- quick_uncertainty("Bunce", dbh = 30, dbh_error = 0.1, N = 500)
  result_high <- quick_uncertainty("Bunce", dbh = 30, dbh_error = 2.0, N = 500)

  # Higher measurement error should give higher CV
  expect_true(result_high$cv > result_low$cv)
})

# ==============================================================================
# Integration tests with allometric functions
# ==============================================================================

test_that("mc_uncertainty works with realistic Bunce function", {
  bunce_fn <- function(dbh, a, b) {
    exp(a + b * log(pi * dbh))
  }

  inputs <- list(
    dbh = list(mean = 30, sd = 0.75, source = "measurement"),
    a = list(mean = 1.868, sd = 0.047, source = "parameter"),
    b = list(mean = 2.268, sd = 0.057, source = "parameter")
  )

  result <- mc_uncertainty(bunce_fn, inputs, N = 1000, decompose = TRUE)

  # Should be positive biomass
  expect_true(result$estimate > 0)
  expect_true(result$median > 0)

  # Should have reasonable CV (typically 5-30% for biomass)
  expect_true(result$cv > 0)
  expect_true(result$cv < 100)

  # Decomposition should show parameter uncertainty dominates for this equation
  expect_true(!is.null(result$decomposition))
})

test_that("mc_uncertainty detects right-skewed distributions", {
  # Log-transformed equations typically produce right-skewed outputs
  bunce_fn <- function(dbh, a, b) {
    exp(a + b * log(pi * dbh))
  }

  inputs <- list(
    dbh = list(mean = 30, sd = 3, source = "measurement"),  # 10% error
    a = list(mean = 1.868, sd = 0.1, source = "parameter"),
    b = list(mean = 2.268, sd = 0.1, source = "parameter")
  )

  result <- mc_uncertainty(bunce_fn, inputs, N = 2000)

  # Biomass estimates are typically right-skewed
  expect_true(result$skewness > 0)
})



