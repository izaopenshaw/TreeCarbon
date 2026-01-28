# Tests for Bunce() function
# Bunce (1968) UK deciduous woodland biomass equation

test_that("Bunce returns correct structure for single tree", {
 result <- Bunce("Oak", 30)

 expect_s3_class(result, "data.frame")
  expect_true("species_name" %in% names(result))
  expect_true("dbh" %in% names(result))
  expect_true("biomass" %in% names(result))
  expect_true("spcode" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("Bunce returns correct structure for multiple trees", {
  result <- Bunce(c("Oak", "Beech", "Ash"), c(30, 40, 25))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$species_name, c("Oak", "Beech", "Ash"))
  expect_equal(result$dbh, c(30, 40, 25))
})

test_that("Bunce biomass increases with DBH", {
  result <- Bunce(rep("Oak", 3), c(10, 30, 50))

  expect_true(result$biomass[1] < result$biomass[2])
  expect_true(result$biomass[2] < result$biomass[3])
})

test_that("Bunce returns positive biomass for valid inputs", {
  result <- Bunce("Oak", 30)

  expect_true(result$biomass > 0)
  expect_false(is.na(result$biomass))
})

test_that("Bunce calculates uncertainty when re_dbh provided", {
  result <- Bunce("Oak", 30, re_dbh = 0.05)

  expect_true("sigma" %in% names(result))
  expect_true(result$sigma > 0)
  expect_false(is.na(result$sigma))
})

test_that("Bunce rich_output returns allometry_result object", {
  result <- Bunce("Oak", 30, re_dbh = 0.05, rich_output = TRUE)

  expect_s3_class(result, "allometry_result")
  expect_true("value" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("citation" %in% names(result))
  expect_true("assumptions" %in% names(result))
  expect_equal(result$method, "Bunce")
})

test_that("Bunce rich_output contains all required metadata fields", {
  result <- Bunce("Oak", 30, re_dbh = 0.05, rich_output = TRUE)

  # Required metadata fields
  expect_true("value" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("citation" %in% names(result))
  expect_true("assumptions" %in% names(result))
  expect_true("validity_warnings" %in% names(result))
  expect_true("flags" %in% names(result))
  expect_true("uncertainty" %in% names(result))
  expect_true("region" %in% names(result))
  expect_true("source_type" %in% names(result))
})

test_that("Bunce rich_output for multiple trees returns list", {
  result <- Bunce(c("Oak", "Beech"), c(30, 40), re_dbh = 0.05, rich_output = TRUE)

  expect_s3_class(result, "bunce_multi_result")
  expect_true("trees" %in% names(result))
  expect_true("summary_table" %in% names(result))
  expect_true("n_trees" %in% names(result))
  expect_equal(result$n_trees, 2)
})

# ==== Error handling ====

test_that("Bunce errors on missing required arguments", {
  expect_error(Bunce(), "Both 'name' and 'dbh' are required")
  expect_error(Bunce("Oak"), "Both 'name' and 'dbh' are required")
})
test_that("Bunce errors on non-character name", {
  expect_error(Bunce(123, 30), "'name' must be a character vector")
})

test_that("Bunce errors on non-numeric dbh", {
  expect_error(Bunce("Oak", "thirty"), "'dbh' must be numeric")
})

test_that("Bunce errors on mismatched lengths", {
  expect_error(Bunce(c("Oak", "Beech"), c(30)), "'name' and 'dbh' must have the same length")
})

# ==== Edge cases ====

test_that("Bunce handles unknown species with fallback coefficients", {
  # Unknown species should use generic broadleaf coefficients
  result <- Bunce("UnknownSpecies123", 30)

  expect_false(is.na(result$biomass))
  expect_true(result$biomass > 0)
})

test_that("Bunce flags extrapolation for large DBH", {
  result <- Bunce("Oak", 200, rich_output = TRUE)

  # DBH 200 cm exceeds Bunce valid range (7-150 cm)
  expect_true(any(grepl("range|extrapolation", result$flags, ignore.case = TRUE)) ||
              any(grepl("range|extrapolation", result$validity_warnings, ignore.case = TRUE)))
})

test_that("Bunce flags extrapolation for small DBH", {
  result <- Bunce("Oak", 5, rich_output = TRUE)

  # DBH 5 cm below Bunce valid range (7-150 cm)
  expect_true(any(grepl("range|extrapolation", result$validity_warnings, ignore.case = TRUE)) ||
              result$validity_warnings[1] != "None")
})



