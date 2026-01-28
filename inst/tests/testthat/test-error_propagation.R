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

  # Expected sigma: sqrt(b^2 * sig_a^2 + a^2 * sig_b^2)
  expected <- sqrt(b^2 * sig_a^2 + a^2 * sig_b^2)
  expect_equal(result, expected, tolerance = 0.01)
})

test_that("error_product returnv='var' returns variance", {
  a <- 10
  sig_a <- 1
  b <- 5
  sig_b <- 0.5

  result <- error_product(a, sig_a, b, sig_b, returnv = "var")

  # Variance = sigma^2
  expected <- b^2 * sig_a^2 + a^2 * sig_b^2
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

test_that("error_product errors on negative sigma", {
  expect_error(error_product(10, -1, 5, 0.5),
               "positive")
})

# ==== MC_propagate tests ====

test_that("MC_propagate returns list with estimate and uncertainty", {
  # Simple function: y = x^2
  f <- function(x) x^2
  result <- MC_propagate(f, mean = 10, sd = 1, n = 1000)

  expect_type(result, "list")
  expect_true("mean" %in% names(result))
  expect_true("sd" %in% names(result))
})

test_that("MC_propagate mean is close to expected value", {
  # For y = x^2 with x ~ N(10, 1):
  # E[Y] ≈ E[X]^2 + Var[X] = 100 + 1 = 101
  f <- function(x) x^2
  result <- MC_propagate(f, mean = 10, sd = 1, n = 10000)

  expect_equal(result$mean, 101, tolerance = 2)
})

test_that("MC_propagate uncertainty is positive", {
  f <- function(x) x^2
  result <- MC_propagate(f, mean = 10, sd = 1, n = 1000)

  expect_true(result$sd > 0)
})

test_that("MC_propagate handles multi-parameter functions", {
  # Function: y = a * b
  f <- function(a, b) a * b

  result <- MC_propagate(f,
                         mean = c(10, 5),
                         sd = c(1, 0.5),
                         n = 1000)

  expect_true("mean" %in% names(result))
  expect_equal(result$mean, 50, tolerance = 5)  # 10 * 5 = 50
})

test_that("MC_propagate returns CI when requested", {
  f <- function(x) x^2
  result <- MC_propagate(f, mean = 10, sd = 1, n = 1000, conf = 0.95)

  expect_true("ci_lower" %in% names(result) || "lower" %in% names(result))
  expect_true("ci_upper" %in% names(result) || "upper" %in% names(result))
})

test_that("MC_propagate errors on non-function", {
  expect_error(MC_propagate("not a function", mean = 10, sd = 1),
               "function")
})

test_that("MC_propagate errors on mismatched mean/sd lengths", {
  f <- function(a, b) a * b
  expect_error(MC_propagate(f, mean = c(10, 5, 3), sd = c(1, 0.5)),
               "length")
})



