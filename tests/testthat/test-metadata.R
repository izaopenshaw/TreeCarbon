# Tests for method metadata functions
# get_method_metadata(), validate_inputs_for_method(), print_method_info()

test_that("get_method_metadata returns list for single method", {
  result <- get_method_metadata("WCC")

  expect_type(result, "list")
  expect_true("full_name" %in% names(result))
  expect_true("citation" %in% names(result))
  expect_true("assumptions" %in% names(result))
  expect_true("dbh_range" %in% names(result))
})

test_that("get_method_metadata returns all methods", {
  result <- get_method_metadata("all")

  expect_type(result, "list")
  expect_true("WCC" %in% names(result))
  expect_true("BIOMASS" %in% names(result))
  expect_true("allodb" %in% names(result))
  expect_true("Bunce" %in% names(result))
})

test_that("get_method_metadata contains required fields for each method", {
  methods <- c("WCC", "BIOMASS", "allodb", "Bunce")

  for (m in methods) {
    result <- get_method_metadata(m)

    expect_true("full_name" %in% names(result), info = paste("Missing full_name for", m))
    expect_true("citation" %in% names(result), info = paste("Missing citation for", m))
    expect_true("assumptions" %in% names(result), info = paste("Missing assumptions for", m))
    expect_true("region" %in% names(result), info = paste("Missing region for", m))
    expect_true("source_type" %in% names(result), info = paste("Missing source_type for", m))
    expect_true("dbh_range" %in% names(result), info = paste("Missing dbh_range for", m))
    expect_true("height_required" %in% names(result), info = paste("Missing height_required for", m))
  }
})

test_that("get_method_metadata dbh_range is numeric vector of length 2", {
  methods <- c("WCC", "BIOMASS", "allodb", "Bunce")

  for (m in methods) {
    result <- get_method_metadata(m)

    expect_length(result$dbh_range, 2)
    expect_type(result$dbh_range, "double")
    expect_true(result$dbh_range[1] < result$dbh_range[2],
                info = paste("DBH range invalid for", m))
  }
})

test_that("get_method_metadata errors on invalid method", {
  expect_error(get_method_metadata("InvalidMethod"),
               "Unknown method")
})

test_that("get_method_metadata WCC requires height", {
  result <- get_method_metadata("WCC")

  expect_true(result$height_required)
})

test_that("get_method_metadata Bunce does not require height", {
  result <- get_method_metadata("Bunce")

  expect_false(result$height_required)
})

# ==== validate_inputs_for_method tests ====

test_that("validate_inputs_for_method returns correct structure", {
  result <- validate_inputs_for_method("WCC", dbh = 50, height = 20)

  expect_type(result, "list")
  expect_true("validity_warnings" %in% names(result))
  expect_true("flags" %in% names(result))
  expect_true("extrapolation" %in% names(result))
})

test_that("validate_inputs_for_method flags DBH below range", {
  result <- validate_inputs_for_method("Bunce", dbh = 3)  # Min is 7 cm

  expect_true(result$extrapolation)
  expect_true(any(grepl("below", result$validity_warnings, ignore.case = TRUE)) ||
              any(grepl("DBH", result$flags)))
})

test_that("validate_inputs_for_method flags DBH above range", {
  result <- validate_inputs_for_method("Bunce", dbh = 200)  # Max is 150 cm

  expect_true(result$extrapolation)
  expect_true(any(grepl("above", result$validity_warnings, ignore.case = TRUE)) ||
              any(grepl("DBH", result$flags)))
})

test_that("validate_inputs_for_method flags missing height for WCC", {
  result <- validate_inputs_for_method("WCC", dbh = 50)  # No height provided

  expect_true(any(grepl("height", result$validity_warnings, ignore.case = TRUE)) ||
              any(grepl("Height", result$flags)))
})

test_that("validate_inputs_for_method no warnings for valid inputs", {
  result <- validate_inputs_for_method("Bunce", dbh = 50)  # Valid range

  expect_false(result$extrapolation)
  expect_true(result$validity_warnings[1] == "None" ||
              length(result$validity_warnings) == 0)
})

test_that("validate_inputs_for_method handles vectorized DBH", {
  result <- validate_inputs_for_method("Bunce", dbh = c(5, 50, 200))

  # Should flag both below and above range
  expect_true(result$extrapolation)
})

# ==== method_comparison_table tests ====

test_that("method_comparison_table returns data.frame", {
  result <- method_comparison_table()

  expect_s3_class(result, "data.frame")
})

test_that("method_comparison_table has all methods", {
  result <- method_comparison_table()

  expect_true("WCC" %in% result$method)
  expect_true("BIOMASS" %in% result$method)
  expect_true("allodb" %in% result$method)
  expect_true("Bunce" %in% result$method)
})

test_that("method_comparison_table has required columns", {
  result <- method_comparison_table()

  expect_true("method" %in% names(result))
  expect_true("full_name" %in% names(result))
  expect_true("citation_short" %in% names(result))
  expect_true("height_required" %in% names(result))
})



