# Tests for component functions
# woodbiomass(), crownbiomass(), rootbiomass(), ctoco2e()

# ==== woodbiomass tests ====

test_that("woodbiomass returns numeric for simple input", {
  result <- woodbiomass(10, 0.5)

  expect_type(result, "double")
  expect_equal(result, 5)  # 10 * 0.5 = 5
})

test_that("woodbiomass calculates correctly: vol * nsg", {
  result <- woodbiomass(2, 0.6)

  expect_equal(result, 1.2)  # 2 * 0.6 = 1.2
})

test_that("woodbiomass returns list when sig_treevol provided", {
  result <- woodbiomass(10, 0.5, sig_treevol = 1)

  expect_type(result, "list")
  expect_true("woodbiomass" %in% names(result))
  expect_true("sigma" %in% names(result))
})

test_that("woodbiomass uncertainty is positive", {
  result <- woodbiomass(10, 0.5, sig_treevol = 1)

  expect_true(result$sigma > 0)
})

test_that("woodbiomass errors on non-numeric treevol", {
  expect_error(woodbiomass("ten", 0.5), "treevol must be numeric")
})

test_that("woodbiomass warns on negative nsg", {
  expect_warning(woodbiomass(10, -0.5), "nsg must be numeric and positive")
})

test_that("woodbiomass handles vectorized input", {
  result <- woodbiomass(c(10, 20, 30), c(0.5, 0.5, 0.5))

  expect_length(result, 3)
  expect_equal(result, c(5, 10, 15))
})

# ==== ctoco2e tests ====

test_that("ctoco2e calculates correctly: C * (44/12)", {
  result <- ctoco2e(12)

  expect_equal(result, 44)  # 12 * (44/12) = 44
})

test_that("ctoco2e handles vectorized input", {
  result <- ctoco2e(c(12, 24, 36))

  expect_equal(result, c(44, 88, 132))
})

test_that("ctoco2e errors on non-numeric input", {
  expect_error(ctoco2e("twelve"), "numeric and positive")
})

test_that("ctoco2e errors on negative input", {
  expect_error(ctoco2e(-12), "numeric and positive")
})

# ==== crownbiomass tests ====

test_that("crownbiomass returns list with biomass", {
  # Using a known species code
  result <- crownbiomass("OK", dbh = 30)

  expect_type(result, "list")
  expect_true("biomass" %in% names(result))
})

test_that("crownbiomass biomass is positive", {
  result <- crownbiomass("OK", dbh = 30)

  expect_true(all(result$biomass > 0, na.rm = TRUE))
})

test_that("crownbiomass returns sigma when re_d provided", {
  result <- crownbiomass("OK", dbh = 30, re_d = 0.05)

  expect_true("sigma" %in% names(result))
})

test_that("crownbiomass warns for dbh < 7", {
  expect_warning(crownbiomass("OK", dbh = 5),
                 "dbh >= 7")
})

test_that("crownbiomass errors on mismatched lengths", {
  expect_error(crownbiomass(c("OK", "BE"), dbh = 30),
               "same")
})

# ==== rootbiomass tests ====

test_that("rootbiomass returns list with rootbiomass", {
  result <- rootbiomass("OK", dbh = 30)

  expect_type(result, "list")
  expect_true("rootbiomass" %in% names(result))
})

test_that("rootbiomass is positive", {
  result <- rootbiomass("OK", dbh = 30)

  expect_true(all(result$rootbiomass > 0, na.rm = TRUE))
})

test_that("rootbiomass returns sigma when re_dbh provided", {
  result <- rootbiomass("OK", dbh = 30, re_dbh = 0.05)

  expect_true("sigma" %in% names(result))
})

test_that("rootbiomass errors on mismatched lengths", {
  expect_error(rootbiomass(c("OK", "BE"), dbh = 30),
               "same")
})


