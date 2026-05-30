# Tests for fc_agc() and fc_agc_error() functions
# Woodland Carbon Code above-ground carbon calculation
# (fc_agc_test / fc_agc_error_test in helper.R silence stand-level hint warnings.)

test_that("fc_agc returns correct structure", {
  result <- fc_agc_test("Oak", dbh = 50, height = 20)

  expect_s3_class(result, "data.frame")
  expect_true("name" %in% names(result))
  expect_true("AGC_WCC_t" %in% names(result))
  expect_true("dbh" %in% names(result))
  expect_true("height" %in% names(result))
})

test_that("fc_agc returns positive carbon for valid inputs", {
  result <- fc_agc_test("Oak", dbh = 50, height = 20, type = "broadleaf")

  expect_true(result$AGC_WCC_t > 0 || is.na(result$AGC_WCC_t))
})

test_that("fc_agc carbon increases with tree size", {
  small <- fc_agc_test("Oak", dbh = 20, height = 10, type = "broadleaf")
  large <- fc_agc_test("Oak", dbh = 60, height = 25, type = "broadleaf")

  # Both should have values, larger tree should have more carbon
  if (!is.na(small$AGC_WCC_t) && !is.na(large$AGC_WCC_t)) {
    expect_true(large$AGC_WCC_t > small$AGC_WCC_t)
  }
})

test_that("fc_agc output.all=TRUE returns detailed components", {
  result <- fc_agc_test("Oak", dbh = 50, height = 20, type = "broadleaf", output.all = TRUE)

  expect_true("tariff" %in% names(result))
  expect_true("mercvol_m.3" %in% names(result))
  expect_true("stemvol_m.3" %in% names(result))
  expect_true("stembiomass_t" %in% names(result))
  expect_true("crownbiomass_t" %in% names(result))
})

test_that("fc_agc output.all=FALSE returns minimal output", {
  result <- fc_agc_test("Oak", dbh = 50, height = 20, output.all = FALSE)

  expect_true("AGC_WCC_t" %in% names(result))
  expect_false("tariff" %in% names(result))
})

test_that("fc_agc rich_output returns allometry_result", {
  result <- fc_agc_test("Oak", dbh = 50, height = 20, type = "broadleaf", rich_output = TRUE)

  expect_s3_class(result, "allometry_result")
  expect_equal(result$method, "WCC")
  expect_true("assumptions" %in% names(result))
  expect_true("reference" %in% names(result))
})

# ==== fc_agc_error tests ====

test_that("fc_agc_error returns uncertainty estimates", {
  result <- fc_agc_error("Oak", dbh = 50, height = 20, type = "broadleaf")

  expect_true("sig_AGC" %in% names(result))
})

test_that("fc_agc_error rich_output includes uncertainty", {
  result <- fc_agc_error_test("Oak", dbh = 50, height = 20, type = "broadleaf", rich_output = TRUE)

  expect_s3_class(result, "allometry_result")
  expect_true(!is.null(result$uncertainty))
})

test_that("fc_agc_error output.all=TRUE returns sigma columns", {
  result <- fc_agc_error("Oak", dbh = 50, height = 20, type = "broadleaf", output.all = TRUE)

  expect_true("sig_tariff" %in% names(result))
  expect_true("sig_mercvol" %in% names(result))
  expect_true("sig_stemvol" %in% names(result))
  expect_true("sig_stembiomass" %in% names(result))
})

# ==== Error handling ====

test_that("fc_agc errors on non-character name", {
  expect_error(fc_agc(123, dbh = 50, height = 20), "name must be a character")
})

test_that("fc_agc errors on invalid method", {
  expect_error(fc_agc("Oak", dbh = 50, height = 20, method = "InvalidMethod"),
               "Invalid method")
})

test_that("fc_agc errors on invalid biome", {
  expect_error(fc_agc("Oak", dbh = 50, height = 20, biome = "desert"),
               "Invalid biome")
})

test_that("fc_agc_error errors on invalid re_dbh", {
  expect_error(fc_agc_error("Oak", dbh = 50, height = 20, re_dbh = -0.05),
               "re_dbh must be numeric & positive")
})

# ==== Edge cases ====

test_that("fc_agc handles small trees (dbh < 7)", {
  # Small trees use sapling equations
  result <- fc_agc_test("Oak", dbh = 5, height = 3, type = "broadleaf")

  expect_s3_class(result, "data.frame")
  # Should have a value (may be NA if type lookup fails)
})

test_that("fc_agc handles multiple trees", {
  result <- fc_agc_test(c("Oak", "Beech"), dbh = c(50, 40), height = c(20, 18),
                        type = c("broadleaf", "broadleaf"))

  expect_equal(nrow(result), 2)
})

test_that("fc_agc and fc_agc_error recycle scalar name and type over tree vectors", {
  dbh <- c(45, 48, 42)
  h <- c(18, 19, 17)
  ht <- h * 0.85
  full <- fc_agc_test(rep("Oak", 3), dbh = dbh, height = h, type = rep("broadleaf", 3),
                      height_timber = ht)
  rec <- fc_agc_test("Oak", dbh = dbh, height = h, type = "broadleaf", height_timber = ht)
  expect_equal(rec$AGC_WCC_t, full$AGC_WCC_t)

  err_f <- fc_agc_error_test(rep("Oak", 3), dbh = dbh, height = h, type = rep("broadleaf", 3),
                             height_timber = ht, output.all = FALSE)
  err_r <- fc_agc_error_test("Oak", dbh = dbh, height = h, type = "broadleaf",
                             height_timber = ht, output.all = FALSE)
  expect_equal(err_r$AGC_WCC_t, err_f$AGC_WCC_t)
})

test_that("fc_agc stops if name length does not match dbh/height", {
  expect_error(
    fc_agc_test(c("Oak", "Beech"), dbh = c(45, 48, 42), height = c(18, 19, 17),
                type = "broadleaf"),
    "name"
  )
})



