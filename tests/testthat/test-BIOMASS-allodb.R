# Tests for BIOMASS() and allodb() wrapper functions
#
# BIOMASS() signature is (dbh, height = NULL, genus, species, coords, ...).
# Always name dbh/genus/species/coords so positional args are not matched to height.
#
# Default height avoids BIOMASS::retrieveH(), which downloads large climate rasters
# (~50+ MB) on first use and makes tests slow and network-dependent.

bm_args <- function(...) {
  # merge so overrides replace defaults; c(list, list) duplicates names and
  # do.call() then errors: formal argument "height" matched by multiple actual arguments
  defaults <- list(
    dbh = 30, genus = "Quercus", species = "robur",
    coords = c(-1.5, 51.5), height = 15
  )
  utils::modifyList(defaults, list(...))
}

# ==== BIOMASS tests ====

test_that("BIOMASS returns data.frame", {
  skip_if_not_installed("BIOMASS")

  result <- do.call(BIOMASS, bm_args())

  expect_s3_class(result, "data.frame")
})

test_that("BIOMASS returns correct columns", {
  skip_if_not_installed("BIOMASS")

  result <- do.call(BIOMASS, bm_args())

  expect_true("genus" %in% names(result))
  expect_true("species" %in% names(result))
  expect_true("dbh" %in% names(result))
  expect_true("AGB_Biomass_kg" %in% names(result))
})

test_that("BIOMASS returns positive biomass", {
  skip_if_not_installed("BIOMASS")

  result <- do.call(BIOMASS, bm_args())

  expect_true(result$AGB_Biomass_kg > 0)
})

test_that("BIOMASS handles height when provided", {
  skip_if_not_installed("BIOMASS")

  result <- do.call(BIOMASS, bm_args(height = 22))

  expect_true("height" %in% names(result))
  expect_equal(result$height, 22)
})

test_that("BIOMASS rich_output returns allometry_result", {
  skip_if_not_installed("BIOMASS")

  result <- do.call(BIOMASS, bm_args(rich_output = TRUE))

  expect_s3_class(result, "allometry_result")
  expect_equal(result$method, "BIOMASS")
})

test_that("BIOMASS rich_output contains metadata", {
  skip_if_not_installed("BIOMASS")

  result <- do.call(BIOMASS, bm_args(rich_output = TRUE))

  expect_true("reference" %in% names(result))
  expect_true("assumptions" %in% names(result))
  expect_true("validity_warnings" %in% names(result))
  expect_true("region" %in% names(result))
  expect_true(grepl("Chave|Rejou|Mechain", result$reference, ignore.case = TRUE))
})

test_that("BIOMASS flags height estimation when no height provided", {
  skip_if_not_installed("BIOMASS")

  # Mock retrieveH so we do not download climate data (see file header).
  testthat::local_mocked_bindings(
    retrieveH = function(D, coord) list(H = as.numeric(D) * 0.5 + 10),
    .package = "BIOMASS"
  )

  result <- do.call(BIOMASS, bm_args(rich_output = TRUE, height = NULL))

  expect_true(any(grepl("height|estimated", c(result$flags, result$validity_warnings),
                        ignore.case = TRUE)))
})

# ==== BIOMASS uncertainty estimation ====

test_that("BIOMASS returns uncertainty columns when uncertainty=TRUE", {
  skip_if_not_installed("BIOMASS")

  result <- BIOMASS(dbh = 30, genus = "Quercus", species = "robur",
                    coords = c(-1.5, 51.5), height = 15,
                    uncertainty = TRUE, n_mc = 100, output.all = TRUE)

  expect_true("AGB_mean_kg" %in% names(result))
  expect_true("AGB_sd_kg" %in% names(result))
  expect_true("AGB_CI_low_kg" %in% names(result))
  expect_true("AGB_CI_high_kg" %in% names(result))
  expect_true("AGB_CV_pct" %in% names(result))
})

test_that("BIOMASS uncertainty SD is positive", {
  skip_if_not_installed("BIOMASS")

  result <- BIOMASS(dbh = 30, genus = "Quercus", species = "robur",
                    coords = c(-1.5, 51.5), height = 15,
                    uncertainty = TRUE, n_mc = 100, output.all = TRUE)

  expect_true(result$AGB_sd_kg > 0)
})

test_that("BIOMASS uncertainty CI contains point estimate", {
  skip_if_not_installed("BIOMASS")

  result <- BIOMASS(dbh = 30, genus = "Quercus", species = "robur",
                    coords = c(-1.5, 51.5), height = 15,
                    uncertainty = TRUE, n_mc = 500, output.all = TRUE)

  expect_true(result$AGB_mean_kg >= result$AGB_CI_low_kg * 0.9)
  expect_true(result$AGB_mean_kg <= result$AGB_CI_high_kg * 1.1)
})

test_that("BIOMASS rich_output includes uncertainty when requested", {
  skip_if_not_installed("BIOMASS")

  result <- BIOMASS(dbh = 30, genus = "Quercus", species = "robur",
                    coords = c(-1.5, 51.5), height = 15,
                    rich_output = TRUE, uncertainty = TRUE, n_mc = 100)

  expect_s3_class(result, "allometry_result")
  expect_true(!is.null(result$uncertainty))
  expect_true(result$uncertainty > 0)
})

test_that("BIOMASS n_mc parameter affects result stability", {
  skip_if_not_installed("BIOMASS")

  result_100 <- BIOMASS(dbh = 30, genus = "Quercus", species = "robur",
                        coords = c(-1.5, 51.5), height = 15,
                        uncertainty = TRUE, n_mc = 100, output.all = TRUE)
  result_500 <- BIOMASS(dbh = 30, genus = "Quercus", species = "robur",
                        coords = c(-1.5, 51.5), height = 15,
                        uncertainty = TRUE, n_mc = 500, output.all = TRUE)

  expect_true(!is.na(result_100$AGB_sd_kg))
  expect_true(!is.na(result_500$AGB_sd_kg))
})

test_that("BIOMASS handles multiple trees with uncertainty", {
  skip_if_not_installed("BIOMASS")

  result <- BIOMASS(dbh = c(30, 40), genus = c("Quercus", "Fagus"),
                    species = c("robur", "sylvatica"),
                    coords = c(-1.5, 51.5), height = c(15, 18),
                    uncertainty = TRUE, n_mc = 100, output.all = TRUE)

  expect_equal(nrow(result), 2)
  expect_true(all(!is.na(result$AGB_sd_kg)))
  expect_true(all(!is.na(result$AGB_CV_pct)))
})

# ==== BIOMASS error handling ====

test_that("BIOMASS errors on non-character genus", {
  skip_if_not_installed("BIOMASS")

  expect_error(
    BIOMASS(dbh = 30, genus = 123, species = "robur", coords = c(-1.5, 51.5)),
    "character"
  )
})

test_that("BIOMASS errors on non-numeric dbh", {
  skip_if_not_installed("BIOMASS")

  expect_error(
    BIOMASS(dbh = "thirty", genus = "Quercus", species = "robur", coords = c(-1.5, 51.5)),
    "dbh must be numeric"
  )
})

test_that("BIOMASS errors on missing coords", {
  skip_if_not_installed("BIOMASS")

  expect_error(
    BIOMASS(dbh = 30, genus = "Quercus", species = "robur"),
    "coords"
  )
})

test_that("BIOMASS errors on invalid coords length", {
  skip_if_not_installed("BIOMASS")

  expect_error(
    BIOMASS(dbh = 30, genus = "Quercus", species = "robur", coords = c(1, 2, 3)),
    "coords"
  )
})

# ==== allodb tests ====

test_that("allodb returns data.frame", {
  skip_if_not_installed("allodb")

  result <- allodb("Quercus", "robur", dbh = 30, coords = c(-77, 38.9))

  expect_s3_class(result, "data.frame")
})

test_that("allodb returns correct columns", {
  skip_if_not_installed("allodb")

  result <- allodb("Quercus", "robur", dbh = 30, coords = c(-77, 38.9))

  expect_true("genus" %in% names(result))
  expect_true("species" %in% names(result))
  expect_true("dbh" %in% names(result))
  expect_true("AGB_allodb_kg" %in% names(result))
})

test_that("allodb returns positive biomass", {
  skip_if_not_installed("allodb")

  result <- allodb("Quercus", "robur", dbh = 30, coords = c(-77, 38.9))

  expect_true(result$AGB_allodb_kg > 0)
})

test_that("allodb rich_output returns allometry_result", {
  skip_if_not_installed("allodb")

  result <- allodb("Quercus", "robur", dbh = 30, coords = c(-77, 38.9),
                   rich_output = TRUE)

  expect_s3_class(result, "allometry_result")
  expect_equal(result$method, "allodb")
})

test_that("allodb rich_output contains metadata", {
  skip_if_not_installed("allodb")

  result <- allodb("Quercus", "robur", dbh = 30, coords = c(-77, 38.9),
                   rich_output = TRUE)

  expect_true("reference" %in% names(result))
  expect_true("assumptions" %in% names(result))
  expect_true("validity_warnings" %in% names(result))
  expect_true(
    grepl("Gonzalez|allodb|allometric", result$reference, ignore.case = TRUE) ||
      grepl("Gonzalez|allodb|allometric", result$method_full_name, ignore.case = TRUE)
  )
})

# ==== allodb error handling ====

test_that("allodb errors on non-character genus", {
  skip_if_not_installed("allodb")

  expect_error(
    allodb(123, "robur", dbh = 30, coords = c(-77, 38.9)),
    "character"
  )
})

test_that("allodb errors on non-numeric dbh", {
  skip_if_not_installed("allodb")

  expect_error(
    allodb("Quercus", "robur", dbh = "thirty", coords = c(-77, 38.9)),
    "numeric"
  )
})

test_that("allodb errors on missing coords", {
  skip_if_not_installed("allodb")

  expect_error(
    allodb("Quercus", "robur", dbh = 30),
    "coords"
  )
})
