# Helper functions and setup for TreeCarbon tests
# This file is automatically loaded before running tests

# Skip tests if optional packages are not available
skip_if_no_BIOMASS <- function() {
  skip_if_not_installed("BIOMASS",
                        "BIOMASS package not installed - skipping related tests")
}

skip_if_no_allodb <- function() {
  skip_if_not_installed("allodb",
                        "allodb package not installed - skipping related tests")
}

# Helper to check if result is approximately equal
expect_approx_equal <- function(actual, expected, tolerance = 0.01) {
  expect_equal(actual, expected, tolerance = tolerance)
}

# Helper to check allometry_result structure
expect_allometry_result <- function(obj) {
  expect_s3_class(obj, "allometry_result")
  expect_true("value" %in% names(obj))
  expect_true("method" %in% names(obj))
  expect_true("citation" %in% names(obj))
  expect_true("assumptions" %in% names(obj))
  expect_true("validity_warnings" %in% names(obj))
  expect_true("flags" %in% names(obj))
}

# Helper to check metadata completeness
expect_complete_metadata <- function(result) {
  required_fields <- c("value", "method", "citation", "assumptions",
                       "validity_warnings", "flags", "uncertainty",
                       "region", "source_type")

  for (field in required_fields) {
    expect_true(field %in% names(result),
                info = paste("Missing required field:", field))
  }
}

# Sample test data
sample_tree_data <- function() {
  data.frame(
    species = c("Quercus robur", "Fagus sylvatica", "Fraxinus excelsior"),
    genus = c("Quercus", "Fagus", "Fraxinus"),
    species_epithet = c("robur", "sylvatica", "excelsior"),
    dbh = c(30, 40, 25),
    height = c(15, 18, 12),
    type = c("broadleaf", "broadleaf", "broadleaf")
  )
}

# Sample UK coordinates
sample_uk_coords <- function() {
  c(-1.5, 51.5)  # Near Oxford, UK
}

# Sample US coordinates (for allodb)
sample_us_coords <- function() {
  c(-77.0, 38.9)  # Washington DC area
}

# Known reference values for regression testing
# These values should be stable across package versions
reference_values <- list(
  # Matthews1 carbon fraction
  matthews1_cf = 0.5,

  # IPCC1 carbon fraction
  ipcc1_cf = 0.477,

  # CO2e conversion factor (44/12)
  co2e_factor = 3.666667,

  # Bunce coefficients for Oak (sp code OK)
  bunce_oak_a = 1.868,
  bunce_oak_b = 2.268
)

# Suppress specific warnings during tests
suppress_height_warning <- function(expr) {
  suppressWarnings(expr, classes = "heightWarning")
}

# Print test context info for debugging
debug_test_info <- function() {
  cat("\n=== Test Environment ===\n")
  cat("R version:", R.version.string, "\n")
  cat("TreeCarbon version:", as.character(packageVersion("TreeCarbon")), "\n")
  cat("BIOMASS available:", requireNamespace("BIOMASS", quietly = TRUE), "\n")
  cat("allodb available:", requireNamespace("allodb", quietly = TRUE), "\n")
  cat("========================\n\n")
}



