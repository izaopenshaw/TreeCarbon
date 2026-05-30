# Tests for error propagation functions
# error_product(), MC_propagate()

# ==== error_product tests ====

test_that("error_product returns numeric result", {
  result <- error_product(10, 1, 5, 0.5)

  expect_type(result, "double")
})

test_that("error_product calculates product variance correctly", {
  # For product Z = A * B:
  # Var(Z) ≈ B²Var(A) + A²Var(B) (first-order approximation)
  a <- 10
  sig_a <- 1
  b <- 5
  sig_b <- 0.5

  result <- error_product(a, sig_a, b, sig_b, returnv = "sigma")

  # Expected sigma: |f| * sqrt((sig_a/a)^2 + (sig_b/b)^2) with f = a*b
  expected <- abs(a * b) * sqrt((sig_a / a)^2 + (sig_b / b)^2)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("error_product returnv='sigmasquared' returns variance (sigma^2 of product)", {
  a <- 10
  sig_a <- 1
  b <- 5
  sig_b <- 0.5

  result <- error_product(a, sig_a, b, sig_b, returnv = "sigmasquared")

  expected <- (a * b)^2 * ((sig_a / a)^2 + (sig_b / b)^2)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("error_product handles vectorized input", {
  a <- c(10, 20, 30)
  sig_a <- c(1, 2, 3)
  b <- c(5, 10, 15)
  sig_b <- c(0.5, 1, 1.5)

  result <- error_product(a, sig_a, b, sig_b)

  expect_length(result, 3)
})

test_that("error_product errors on invalid returnv", {
  expect_error(error_product(10, 1, 5, 0.5, returnv = "invalid"),
               "returnv must be")
})

# ==== MC_propagate tests ====
# API: MC_propagate(fn, inputs, N = ..., ...) with inputs = list(param = list(mean=, sd=), ...)

test_that("MC_propagate returns list with mean and sd", {
  f <- function(x) x^2
  inputs <- list(x = list(mean = 10, sd = 1))
  result <- MC_propagate(f, inputs, N = 1000)

  expect_type(result, "list")
  expect_true("mean" %in% names(result))
  expect_true("sd" %in% names(result))
})

test_that("MC_propagate mean is close to expected value", {
  # For y = x^2 with x ~ N(10, 1):
  # E[Y] ≈ E[X]^2 + Var[X] = 100 + 1 = 101
  f <- function(x) x^2
  inputs <- list(x = list(mean = 10, sd = 1))
  result <- MC_propagate(f, inputs, N = 10000)

  expect_equal(result$mean, 101, tolerance = 2)
})

test_that("MC_propagate uncertainty is positive", {
  f <- function(x) x^2
  inputs <- list(x = list(mean = 10, sd = 1))
  result <- MC_propagate(f, inputs, N = 1000)

  expect_true(result$sd > 0)
})

test_that("MC_propagate handles multi-parameter functions", {
  f <- function(a, b) a * b
  inputs <- list(
    a = list(mean = 10, sd = 1),
    b = list(mean = 5, sd = 0.5)
  )
  result <- MC_propagate(f, inputs, N = 1000)

  expect_true("mean" %in% names(result))
  expect_equal(result$mean, 50, tolerance = 5)
})

test_that("MC_propagate returns quantile CI in ci element", {
  f <- function(x) x^2
  inputs <- list(x = list(mean = 10, sd = 1))
  result <- MC_propagate(f, inputs, N = 1000)

  expect_true("ci" %in% names(result))
  expect_length(result$ci, 2)
  expect_true(result$ci[1] < result$ci[2])
})

test_that("MC_propagate errors on non-function", {
  inputs <- list(x = list(mean = 10, sd = 1))
  expect_error(MC_propagate(1, inputs))
})

test_that("MC_propagate requires matching fn argument names and inputs", {
  f <- function(a, b) a * b
  inputs <- list(a = list(mean = 10, sd = 1)) # missing b
  expect_error(MC_propagate(f, inputs, N = 100),
               "argument \"b\" is missing")
})
