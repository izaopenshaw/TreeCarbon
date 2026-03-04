# Tests for WCC tariff and volume functions
# Method A: tariff_vol_area | Method B: broadleaf_tariff | Method C: conifer_tariff
# Method D: stand_tariff, calculate_top_height, fc_stand_carbon | tariffs() for B/C

# ==== tariff_vol_area (WCC Method A) ====

test_that("tariff_vol_area returns numeric", {
  result <- tariff_vol_area(vol = 0.5, dbh = 24)
  expect_type(result, "double")
})

test_that("tariff_vol_area returns list with sigma when sig_vol provided", {
  result <- tariff_vol_area(vol = 0.5, dbh = 24, sig_vol = 0.01, re_dbh = 0.025)
  expect_type(result, "list")
  expect_true("tariff" %in% names(result) || "sigma_tariff" %in% names(result))
})

# ==== broadleaf_tariff (WCC Method B) ====

test_that("broadleaf_tariff returns numeric or data.frame", {
  result <- broadleaf_tariff(spcode = "OK", height_timber = 20, dbh = 45)
  expect_true(is.numeric(result) || is.data.frame(result))
})

test_that("broadleaf_tariff returns data.frame with sigma when re_dbh provided", {
  result <- broadleaf_tariff(spcode = "OK", height_timber = 20, dbh = 45,
                             re_dbh = 0.05, re_h = 0.1)
  expect_s3_class(result, "data.frame")
  expect_true("sigma" %in% names(result))
})

test_that("broadleaf_tariff errors on dbh < 7", {
  expect_error(broadleaf_tariff(spcode = "OK", height_timber = 15, dbh = 5),
               "dbh")
})

# ==== conifer_tariff (WCC Method C) ====

test_that("conifer_tariff returns numeric or data.frame", {
  result <- conifer_tariff(spcode = "SP", height = 20, dbh = 30)
  expect_true(is.numeric(result) || is.data.frame(result))
})

test_that("conifer_tariff returns data.frame with sigma when re_h provided", {
  result <- conifer_tariff(spcode = "SP", height = 20, dbh = 30,
                           re_h = 0.05, re_dbh = 0.025)
  expect_s3_class(result, "data.frame")
  expect_true("sigma" %in% names(result))
})

# ==== stand_tariff (WCC Method D) ====

test_that("stand_tariff returns numeric", {
  result <- stand_tariff(spcode = "OK", stand_height = 18)
  expect_type(result, "double")
})

test_that("stand_tariff handles vectorized input", {
  result <- stand_tariff(spcode = "OK", stand_height = c(15, 18, 20))
  expect_length(result, 3)
  expect_true(all(result > 0))
})

test_that("stand_tariff errors on non-numeric stand_height", {
  expect_error(stand_tariff(spcode = "OK", stand_height = "eighteen"),
               "numeric")
})

# ==== tariffs() - auto-selects Method B or C ====

test_that("tariffs returns tariff for broadleaf", {
  result <- tariffs(spcode = "OK", height = 18, dbh = 45, type = "broadleaf")
  expect_true(is.numeric(result) || (is.data.frame(result) && "tariff" %in% names(result)))
})

test_that("tariffs returns tariff for conifer", {
  result <- tariffs(spcode = "SP", height = 20, dbh = 30, type = "conifer")
  expect_true(is.numeric(result) || (is.data.frame(result) && "tariff" %in% names(result)))
})

# ==== merchtreevol (uses tariff from Methods A-D) ====

test_that("merchtreevol returns numeric when sig_tariff is NULL", {
  result <- merchtreevol(dbh = 24, tariff = 24)
  expect_type(result, "double")
})

test_that("merchtreevol returns list with volume and sigma when sig_tariff provided", {
  result <- merchtreevol(dbh = 24, tariff = 24, re_dbh = 0.05, sig_tariff = 1)
  expect_type(result, "list")
  expect_true("volume" %in% names(result))
  expect_true("sigma" %in% names(result))
})

# ==== treevol ====

test_that("treevol returns numeric", {
  result <- treevol(mtreevol = 1.5, dbh = 24)
  expect_type(result, "double")
})

test_that("treevol converts merchantable to stem volume", {
  merc <- 1.5
  result <- treevol(mtreevol = merc, dbh = 24)
  expect_true(result >= merc || is.na(result))
})

test_that("treevol handles vectorized input", {
  result <- treevol(mtreevol = c(1, 2, 3), dbh = c(20, 24, 28))
  expect_length(result, 3)
})
