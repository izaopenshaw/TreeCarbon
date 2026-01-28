# Tests for tariff functions
# stand_tariff(), broadleaf_tariff(), conifer_tariff()

# ==== stand_tariff tests ====

test_that("stand_tariff returns numeric", {
  # Stand tariff from DBH and height
  result <- stand_tariff(dbh = 30, height = 15)

  expect_type(result, "double")
})

test_that("stand_tariff handles vectorized input", {
  result <- stand_tariff(dbh = c(20, 30, 40), height = c(10, 15, 20))

  expect_length(result, 3)
  expect_true(all(result > 0))
})

test_that("stand_tariff errors on non-numeric dbh", {
  expect_error(stand_tariff(dbh = "thirty", height = 15),
               "numeric")
})

test_that("stand_tariff errors on non-numeric height", {
  expect_error(stand_tariff(dbh = 30, height = "fifteen"),
               "numeric")
})

# ==== broadleaf_tariff tests ====

test_that("broadleaf_tariff returns correct columns", {
  result <- broadleaf_tariff(tariff = 2.5, dbh = 30)

  expect_true("mercvol" %in% names(result))
})

test_that("broadleaf_tariff volume is positive", {
  result <- broadleaf_tariff(tariff = 2.5, dbh = 30)

  expect_true(result$mercvol > 0)
})

test_that("broadleaf_tariff includes sigma when provided", {
  result <- broadleaf_tariff(tariff = 2.5, dbh = 30, sig_tariff = 0.1, re_dbh = 0.05)

  expect_true("sig" %in% names(result) || "sigma" %in% names(result))
})

test_that("broadleaf_tariff errors on dbh < 7", {
  expect_error(broadleaf_tariff(tariff = 2.5, dbh = 5),
               "dbh")
})

# ==== conifer_tariff tests ====

test_that("conifer_tariff returns correct columns", {
  result <- conifer_tariff(tariff = 3.0, dbh = 30)

  expect_true("mercvol" %in% names(result))
})

test_that("conifer_tariff volume is positive", {
  result <- conifer_tariff(tariff = 3.0, dbh = 30)

  expect_true(result$mercvol > 0)
})

test_that("conifer_tariff errors on dbh < 7", {
  expect_error(conifer_tariff(tariff = 3.0, dbh = 5),
               "dbh")
})

# ==== treevol tests ====

test_that("treevol returns numeric", {
  result <- treevol(mercvol = 1.5)

  expect_type(result, "double")
})

test_that("treevol converts merchantable to total volume", {
  # Should be slightly larger than mercvol
  merc <- 1.5
  result <- treevol(mercvol = merc)

  expect_true(result >= merc)
})

test_that("treevol handles vectorized input", {
  result <- treevol(mercvol = c(1, 2, 3))

  expect_length(result, 3)
})

# ==== merchtreevol tests ====

test_that("merchtreevol returns list with mercvol and stemvol", {
  result <- merchtreevol(tariff = 2.5, dbh = 30, type = "broadleaf")

  expect_type(result, "list")
  expect_true("mercvol" %in% names(result))
  expect_true("stemvol" %in% names(result))
})

test_that("merchtreevol type broadleaf uses broadleaf_tariff", {
  result <- merchtreevol(tariff = 2.5, dbh = 30, type = "broadleaf")

  expect_true(result$mercvol > 0)
  expect_true(result$stemvol > 0)
})

test_that("merchtreevol type conifer uses conifer_tariff", {
  result <- merchtreevol(tariff = 3.0, dbh = 30, type = "conifer")

  expect_true(result$mercvol > 0)
  expect_true(result$stemvol > 0)
})

test_that("merchtreevol errors on invalid type", {
  expect_error(merchtreevol(tariff = 2.5, dbh = 30, type = "invalid"),
               "type")
})

test_that("merchtreevol errors on dbh < 7", {
  expect_error(merchtreevol(tariff = 2.5, dbh = 5, type = "broadleaf"),
               "dbh")
})



