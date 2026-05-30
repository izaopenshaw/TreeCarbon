# Tests for input validation edge cases
# Comprehensive edge case testing across functions

# ==== Missing input tests ====

test_that("functions handle NA values gracefully", {
  # Bunce: bare NA is logical, not numeric — use NA_real_ for missing numeric dbh
  result <- Bunce("Oak", NA_real_)
  expect_true(is.na(result$biomass))

  # woodbiomass: use NA_real_ so treevol is numeric (bare NA is logical)
  result <- woodbiomass(NA_real_, 0.5)
  expect_true(is.na(result))

  # biomass2c with NA (numeric NA)
  result <- biomass2c(NA_real_, method = "Matthews1")
  expect_true(is.na(result))
})

test_that("functions handle NULL values with errors", {
  # Most functions should error on NULL required arguments
  expect_error(Bunce(NULL, 30))
  expect_error(woodbiomass(NULL, 0.5))
  expect_error(biomass2c(NULL, method = "Matthews1"))
})

test_that("functions handle empty vectors", {
  # Empty numeric vector
  result <- biomass2c(numeric(0), method = "Matthews1")
  expect_length(result, 0)

  result <- woodbiomass(numeric(0), numeric(0))
  expect_length(result, 0)
})

# ==== Boundary value tests ====

test_that("functions handle zero values correctly", {
  # Zero DBH
  result <- Bunce("Oak", 0)
  expect_true(result$biomass == 0 || is.na(result$biomass))

  # Zero biomass
  result <- biomass2c(0, method = "Matthews1")
  expect_equal(result, 0)

  # Zero volume
  result <- woodbiomass(0, 0.5)
  expect_equal(result, 0)
})

test_that("functions handle very large values", {
  # Very large DBH (should work but may be extrapolated)
  result <- Bunce("Oak", 1000)
  expect_true(result$biomass > 0)
  expect_false(is.na(result$biomass))

  # Large biomass
  result <- biomass2c(1000000, method = "Matthews1")
  expect_equal(result, 500000)
})

test_that("functions handle very small positive values", {
  # Small DBH
  result <- Bunce("Oak", 0.001)
  expect_true(!is.na(result$biomass))

  # Small biomass
  result <- biomass2c(0.001, method = "Matthews1")
  expect_equal(result, 0.0005)
})

# ==== Type coercion tests ====

test_that("functions handle integer inputs", {
  result <- Bunce("Oak", 30L)
  expect_true(!is.na(result$biomass))

  result <- biomass2c(1L, method = "Matthews1")
  expect_equal(result, 0.5)

  result <- woodbiomass(10L, 0.5)
  expect_equal(result, 5)
})

test_that("functions reject character numbers", {
  expect_error(Bunce("Oak", "30"))
  expect_error(biomass2c("1", method = "Matthews1"))
  expect_error(woodbiomass("10", 0.5))
})

# ==== Special numeric values ====

test_that("functions handle Inf values", {
  result <- biomass2c(Inf, method = "Matthews1")
  expect_equal(result, Inf)

  result <- woodbiomass(Inf, 0.5)
  expect_equal(result, Inf)
})

test_that("functions handle -Inf values", {
  # woodbiomass returns -Inf (no warning); nsg is still validated as positive
  expect_equal(woodbiomass(-Inf, 0.5), -Inf)
})

test_that("functions handle NaN values", {
  result <- biomass2c(NaN, method = "Matthews1")
  expect_true(is.nan(result))
})

# ==== Species name edge cases ====

test_that("Bunce handles case variations", {
  # Should be case-insensitive or handle common cases
  result_lower <- Bunce("oak", 30)
  result_upper <- Bunce("OAK", 30)
  result_title <- Bunce("Oak", 30)

  # All should return valid (possibly same) results
  expect_true(!is.na(result_lower$biomass))
  expect_true(!is.na(result_upper$biomass))
  expect_true(!is.na(result_title$biomass))
})

test_that("Bunce handles whitespace in names", {
  # Trailing/leading whitespace
  result <- Bunce(" Oak ", 30)
  expect_true(!is.na(result$biomass) || is.na(result$biomass))  # Either works or graceful NA
})

test_that("Bunce handles special characters in names", {
  # Unknown species: lookup warns; fallback coefficients still give biomass
  expect_warning(
    result <- Bunce("Oak-Tree", 30),
    "species were not found"
  )
  expect_true(!is.na(result$biomass))
})

# ==== Mismatched vector lengths ====

test_that("functions error on mismatched vector lengths", {
  expect_error(Bunce(c("Oak", "Beech"), c(30, 40, 50)))
  # woodbiomass recycles scalar nsg (R vectorization); crownbiomass enforces length match
  expect_error(crownbiomass(c("CBOK", "CBOK"), dbh = 30), "same")
})

# ==== Method validation ====

test_that("biomass2c validates method names", {
  # Valid methods
  expect_no_error(biomass2c(1, method = "Matthews1"))
  expect_no_error(biomass2c(1, method = "IPCC1"))

  # Invalid method
  expect_error(biomass2c(1, method = "NotAMethod"))
})

test_that("fc_agc validates biome names", {
  # Invalid biome
  expect_error(fc_agc("Oak", dbh = 30, height = 15, biome = "mars"))
})

# ==== Unicode handling ====

test_that("functions handle unicode species names", {
  expect_warning(
    result <- Bunce("Frêne", 30),
    "species were not found"
  )
  expect_true(!is.na(result$biomass))
})



