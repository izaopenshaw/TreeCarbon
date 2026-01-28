# Tests for allometries() function
# Multi-method comparison function

test_that("allometries returns data.frame with all methods", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  result <- allometries("Quercus", "robur", dbh = 30, height = 15)

  expect_s3_class(result, "data.frame")
  expect_true("genus" %in% names(result))
  expect_true("species" %in% names(result))
  expect_true("dbh" %in% names(result))
  expect_true("height" %in% names(result))
})

test_that("allometries returns carbon columns when returnv='AGC'", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  result <- allometries("Quercus", "robur", dbh = 30, height = 15, returnv = "AGC")

  # Should have carbon columns (C) not biomass columns (B)
  expect_true(any(grepl("_C_", names(result))))
})

test_that("allometries returns biomass columns when returnv='AGB'", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  result <- allometries("Quercus", "robur", dbh = 30, height = 15, returnv = "AGB")

  # Should have biomass columns (B)
  expect_true(any(grepl("_B_", names(result))))
})

test_that("allometries rich_output returns comprehensive result", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  result <- allometries("Quercus", "robur", dbh = 30, height = 15, rich_output = TRUE)

  expect_s3_class(result, "allometries_rich_result")
  expect_true("estimates" %in% names(result))
  expect_true("methods" %in% names(result))
  expect_true("inputs" %in% names(result))
})

test_that("allometries rich_output contains all method metadata", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  result <- allometries("Quercus", "robur", dbh = 30, height = 15, rich_output = TRUE)

  expect_true("WCC" %in% names(result$methods))
  expect_true("BIOMASS" %in% names(result$methods))
  expect_true("allodb" %in% names(result$methods))
  expect_true("Bunce" %in% names(result$methods))

  # Each method should have metadata
  for (m_name in names(result$methods)) {
    m <- result$methods[[m_name]]
    expect_true("metadata" %in% names(m), info = paste("Missing metadata for", m_name))
    expect_true("validation" %in% names(m), info = paste("Missing validation for", m_name))
  }
})

# ==== Error handling ====

test_that("allometries errors on non-character genus/species", {
  expect_error(allometries(123, "robur", dbh = 30, height = 15),
               "genus and species must be character")
})

test_that("allometries errors on negative dbh", {
  expect_error(allometries("Quercus", "robur", dbh = -30, height = 15),
               "dbh must be numeric and positive")
})

test_that("allometries errors on negative height", {
  expect_error(allometries("Quercus", "robur", dbh = 30, height = -15),
               "height must be numeric and positive")
})

test_that("allometries errors on invalid coords", {
  expect_error(allometries("Quercus", "robur", dbh = 30, height = 15,
                           coords = c(1, 2, 3)),
               "coords must be a numeric vector of length 2")
})

test_that("allometries errors on invalid method", {
  expect_error(allometries("Quercus", "robur", dbh = 30, height = 15,
                           method = "InvalidMethod"),
               "Invalid method")
})



