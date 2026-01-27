# Tests for biomass2c() function
# Biomass to carbon conversion using various methods

test_that("biomass2c returns numeric for simple methods", {
  result <- biomass2c(1, method = "Matthews1")

  expect_type(result, "double")
  expect_equal(result, 0.5)  # Matthews1 uses 50% carbon fraction
})

test_that("biomass2c Matthews1 uses 50% carbon fraction", {
  result <- biomass2c(2, method = "Matthews1")

  expect_equal(result, 1.0)  # 2 * 0.5 = 1
})

test_that("biomass2c IPCC1 uses 47.7% carbon fraction", {
  result <- biomass2c(1, method = "IPCC1")

  expect_equal(result, 0.477, tolerance = 0.001)
})

test_that("biomass2c handles vectorized input", {
  result <- biomass2c(c(1, 2, 3), method = "Matthews1")

  expect_length(result, 3)
  expect_equal(result, c(0.5, 1.0, 1.5))
})

test_that("biomass2c IPCC2 requires type", {
  expect_warning(biomass2c(1, method = "IPCC2"),
                 "type is undefined")
})

test_that("biomass2c Thomas requires type", {
  expect_warning(biomass2c(1, method = "Thomas"),
                 "type is undefined")
})

test_that("biomass2c with type returns value", {
  result <- biomass2c(1, method = "IPCC2", type = "broadleaf", biome = "temperate")

  expect_type(result, "double")
  expect_true(result > 0)
  expect_true(result < 1)  # Carbon fraction < 100%
})

test_that("biomass2c returns data.frame when sig_biomass provided", {
  result <- biomass2c(1, method = "Thomas", type = "broadleaf",
                      biome = "temperate", sig_biomass = 0.1)

  expect_s3_class(result, "data.frame")
  expect_true("AGC" %in% names(result))
  expect_true("sig_AGC" %in% names(result))
})

test_that("biomass2c rich_output returns metadata", {
  result <- biomass2c(1, method = "Thomas", type = "broadleaf",
                      biome = "temperate", rich_output = TRUE)

  expect_s3_class(result, "biomass2c_result")
  expect_true("carbon" %in% names(result))
  expect_true("conversion_method" %in% names(result))
  expect_true("citation" %in% names(result))
  expect_true("assumptions" %in% names(result))
})

test_that("biomass2c rich_output contains correct method info", {
  result <- biomass2c(1, method = "Thomas", type = "broadleaf",
                      biome = "temperate", rich_output = TRUE)

  expect_equal(result$conversion_method, "Thomas")
  expect_true(grepl("Thomas", result$citation))
  expect_equal(result$source_type, "peer-reviewed")
})

# ==== Error handling ====

test_that("biomass2c errors on invalid method", {
  expect_error(biomass2c(1, method = "InvalidMethod"),
               "Invalid method")
})

test_that("biomass2c errors on non-numeric biomass", {
  expect_error(biomass2c("one", method = "Matthews1"),
               "biomass values must be numeric")
})

test_that("biomass2c errors on invalid biome", {
  expect_error(biomass2c(1, method = "IPCC2", type = "broadleaf", biome = "arctic"),
               "Invalid biome")
})

test_that("biomass2c errors on mismatched sig_biomass length", {
  expect_error(biomass2c(c(1, 2), method = "Thomas", type = c("broadleaf", "broadleaf"),
                         biome = "temperate", sig_biomass = 0.1),
               "same length")
})

# ==== Known values ====

test_that("biomass2c produces consistent results across methods", {
  biomass <- 1

  m1 <- biomass2c(biomass, "Matthews1")
  m2 <- biomass2c(biomass, "Matthews2", type = "broadleaf")
  ipcc1 <- biomass2c(biomass, "IPCC1")

  # All should be between 0.4 and 0.55 (typical carbon fractions)
  expect_true(m1 >= 0.4 && m1 <= 0.55)
  expect_true(m2 >= 0.4 && m2 <= 0.55)
  expect_true(ipcc1 >= 0.4 && ipcc1 <= 0.55)
})

test_that("biomass2c broadleaf vs conifer differ for Matthews2", {
  broad <- biomass2c(1, "Matthews2", type = "broadleaf")
  conif <- biomass2c(1, "Matthews2", type = "conifer")

  # They should be different (Matthews2 distinguishes by type)
  expect_false(broad == conif)
})



