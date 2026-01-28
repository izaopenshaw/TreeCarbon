# Tests for compare_allometries() function
# Comprehensive method comparison with statistics

test_that("compare_allometries returns correct class", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  # First get allometries results
  results <- allometries("Quercus", "robur", dbh = 30, height = 15)
  comparison <- compare_allometries(results)

  expect_s3_class(comparison, "allometry_comparison")
})

test_that("compare_allometries has method column", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries("Quercus", "robur", dbh = 30, height = 15)
  comparison <- compare_allometries(results)

  expect_true("method" %in% names(comparison))
})

test_that("compare_allometries has value column", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries("Quercus", "robur", dbh = 30, height = 15)
  comparison <- compare_allometries(results)

  expect_true("value" %in% names(comparison) || "agb_t" %in% names(comparison))
})

test_that("compare_allometries has metadata columns", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries("Quercus", "robur", dbh = 30, height = 15)
  comparison <- compare_allometries(results)

  # Should have metadata columns
  expect_true("citation" %in% names(comparison) ||
              "assumptions" %in% names(comparison) ||
              "source" %in% names(comparison))
})

test_that("compare_allometries has pct_diff_vs_ref column", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries("Quercus", "robur", dbh = 30, height = 15)
  comparison <- compare_allometries(results)

  expect_true("pct_diff_vs_ref" %in% names(comparison))
})

test_that("compare_allometries reference method has 0% difference", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries("Quercus", "robur", dbh = 30, height = 15)
  comparison <- compare_allometries(results, reference = "WCC")

  ref_row <- comparison[comparison$method == "WCC", ]
  expect_equal(ref_row$pct_diff_vs_ref, 0, tolerance = 0.001)
})

test_that("compare_allometries has rank column", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries("Quercus", "robur", dbh = 30, height = 15)
  comparison <- compare_allometries(results)

  expect_true("rank" %in% names(comparison))
})

test_that("compare_allometries has attributes", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries("Quercus", "robur", dbh = 30, height = 15)
  comparison <- compare_allometries(results)

  # Should have summary attributes
  expect_true(!is.null(attr(comparison, "reference_method")) ||
              !is.null(attr(comparison, "cv_across_methods")))
})

test_that("compare_allometries aggregate=FALSE returns per-tree", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries(c("Quercus", "Fagus"), c("robur", "sylvatica"),
                         dbh = c(30, 40), height = c(15, 18))
  comparison <- compare_allometries(results, aggregate = FALSE)

  # Should have tree_id column for per-tree results
  expect_true("tree_id" %in% names(comparison))
})

# ==== Edge cases ====

test_that("compare_allometries handles single tree", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries("Quercus", "robur", dbh = 30, height = 15)
  comparison <- compare_allometries(results)

  expect_s3_class(comparison, "allometry_comparison")
})

test_that("compare_allometries handles different reference methods", {
  skip_if_not_installed("BIOMASS")
  skip_if_not_installed("allodb")

  results <- allometries("Quercus", "robur", dbh = 30, height = 15)

  comp_wcc <- compare_allometries(results, reference = "WCC")
  comp_bunce <- compare_allometries(results, reference = "Bunce")

  # Different references should give different pct_diff values
  expect_false(identical(comp_wcc$pct_diff_vs_ref, comp_bunce$pct_diff_vs_ref))
})



