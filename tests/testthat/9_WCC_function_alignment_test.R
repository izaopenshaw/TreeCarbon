# ==============================================================================
# WCC Function Alignment Test
# ==============================================================================
# Tests that allometries(), fc_agc(), and fc_agc_error() return consistent
# AGC estimates for the same inputs. Creates a grid of input combinations and
# compares outputs. Use plot(out1, out2) to identify misalignments.
#
# KNOWN MISALIGNMENTS (fixed):
#   1. allometries() used WCC$AGB_WCC_t (biomass) for WCC_C_t when returnv=="AGC"
#      instead of WCC$AGC_WCC_t (carbon). Fixed in Compare_allometry.R.
#
# POTENTIAL REMAINING SOURCES OF DIFFERENCE:
#   - allometries pre-resolves height_timber and passes to fc_agc; fc_agc uses
#     tariffs() internally. Slight numeric differences if resolution paths differ.
#   - allometries uses genus/species; fc_agc uses name=paste(genus,species).
#     Lookup should match but verify for edge cases.
#   - Small trees (dbh<7): fc_agc uses sap_seedling2C; ensure same logic.
#
# Run: devtools::load_all() then source this file
#
# For wakehurst_analysis.R: if plot(AGC_WCC_t, WCC_C_t) is off the y=x line,
# ensure (1) devtools::load_all() was run to pick up the AGC fix in allometries,
# (2) use explicit wcc_df$AGC_WCC_t and results$WCC_C_t (see wakehurst diagnostics).
# ==============================================================================

library(TreeCarbon)
devtools::load_all()

# ------------------------------------------------------------------------------
# 1. Build test grid: all combinations of relevant inputs
# ------------------------------------------------------------------------------

# Base inputs (expand via expand.grid)
test_grid <- expand.grid(
  # Species
  species_name = c("Oak", "Quercus robur", "Sitka Spruce", "Scots Pine"),
  # Tree dimensions
  dbh_cm = c(15, 30, 45, 60),
  height_m = c(10, 15, 20, 25),
  # Type (NA = let function infer)
  type = c("broadleaf", "conifer", NA),
  # height_timber: NULL, measured, or NA (partial)
  height_timber_opt = c("NULL", "measured", "NA_partial"),
  # timber_ratio for estimation
  timber_ratio_opt = c("0.85", "0.8", "estimate"),
  # Carbon conversion method
  method = c("Matthews2", "IPCC2", "Thomas"),
  stringsAsFactors = FALSE
)

# Filter invalid combinations: broadleaf species with conifer type and vice versa
valid_type <- with(test_grid, {
  oak_like <- species_name %in% c("Oak", "Quercus robur")
  pine_like <- species_name %in% c("Sitka Spruce", "Scots Pine")
  (oak_like & (type == "broadleaf" | is.na(type))) |
  (pine_like & (type == "conifer" | is.na(type)))
})
test_grid <- test_grid[valid_type, ]

# Reduce grid size for speed (full grid can be 1000+ rows)
# Sample if too large, or use a smaller subset
set.seed(42)
if (nrow(test_grid) > 300) {
  idx <- sample(nrow(test_grid), 300)
  test_grid <- test_grid[idx, ]
}

# ------------------------------------------------------------------------------
# 2. Helper: parse species into genus/species for allometries
# ------------------------------------------------------------------------------
parse_species <- function(name) {
  if (name %in% c("Oak", "Quercus robur")) {
    list(genus = "Quercus", species = "robur")
  } else if (name == "Sitka Spruce") {
    list(genus = "Picea", species = "sitchensis")
  } else if (name == "Scots Pine") {
    list(genus = "Pinus", species = "sylvestris")
  } else {
    list(genus = "Quercus", species = "robur")  # fallback
  }
}

# ------------------------------------------------------------------------------
# 3. Run all three functions and collect AGC
# ------------------------------------------------------------------------------
coords <- c(-0.088837, 51.071610)
results <- data.frame(
  id = seq_len(nrow(test_grid)),
  species_name = test_grid$species_name,
  dbh = test_grid$dbh_cm,
  height = test_grid$height_m,
  type = test_grid$type,
  method = test_grid$method,
  allometries_AGC = NA_real_,
  fc_agc_AGC = NA_real_,
  fc_agc_error_AGC = NA_real_,
  allometries_note = NA_character_,
  fc_agc_note = NA_character_,
  fc_agc_error_note = NA_character_,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(test_grid))) {
  tg <- test_grid[i, ]
  name <- tg$species_name
  dbh <- tg$dbh_cm
  height <- tg$height_m
  type <- if (is.na(tg$type)) NULL else tg$type
  method <- tg$method

  # Resolve height_timber
  if (tg$height_timber_opt == "NULL") {
    ht <- NULL
  } else if (tg$height_timber_opt == "measured") {
    ht <- height * 0.85
  } else {
    ht <- NA_real_  # NA = will be estimated via timber_ratio
  }

  # Resolve timber_ratio
  tr <- switch(tg$timber_ratio_opt,
    "0.85" = 0.85,
    "0.8" = 0.8,
    "estimate" = "estimate",
    0.85
  )

  ps <- parse_species(name)

  # --- allometries (requires BIOMASS, allodb) ---
  tryCatch({
    if (requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)) {
      alm <- allometries(ps$genus, ps$species, dbh, height, type = type,
                         method = method, returnv = "AGC",
                         height_timber = ht, timber_ratio = tr,
                         coords = coords, checkTaxo = FALSE)
      results$allometries_AGC[i] <- if (length(alm$WCC_C_t) == 1) alm$WCC_C_t else alm$WCC_C_t[1]
    } else {
      results$allometries_note[i] <- "BIOMASS/allodb not installed"
    }
  }, error = function(e) {
    results$allometries_note[i] <- substr(conditionMessage(e), 1, 80)
  })

  # --- fc_agc ---
  tryCatch({
    wcc <- fc_agc(name, dbh, height, type = type, method = method,
                  height_timber = ht, timber_ratio = tr, output.all = TRUE)
    results$fc_agc_AGC[i] <- if (length(wcc$AGC_WCC_t) == 1) wcc$AGC_WCC_t else wcc$AGC_WCC_t[1]
  }, error = function(e) {
    results$fc_agc_note[i] <- substr(conditionMessage(e), 1, 80)
  })

  # --- fc_agc_error ---
  tryCatch({
    wcc_err <- fc_agc_error(name, dbh, height, type = type, method = method,
                            height_timber = ht, timber_ratio = tr, output.all = TRUE)
    results$fc_agc_error_AGC[i] <- if (length(wcc_err$AGC_WCC_t) == 1) wcc_err$AGC_WCC_t else wcc_err$AGC_WCC_t[1]
  }, error = function(e) {
    results$fc_agc_error_note[i] <- substr(conditionMessage(e), 1, 80)
  })
}

# ------------------------------------------------------------------------------
# 4. Comparison and plots
# ------------------------------------------------------------------------------
# Valid rows: at least fc_agc and fc_agc_error succeeded
valid_fc <- !is.na(results$fc_agc_AGC) & !is.na(results$fc_agc_error_AGC)
valid <- valid_fc & !is.na(results$allometries_AGC)  # all 3 for full comparison

cat("=== WCC Function Alignment Test ===\n")
cat(sprintf("Total rows: %d\n", nrow(results)))
cat(sprintf("Valid (all 3 succeeded): %d\n", sum(valid)))
cat(sprintf("allometries failed: %d\n", sum(!is.na(results$allometries_note))))
cat(sprintf("fc_agc failed: %d\n", sum(!is.na(results$fc_agc_note))))
cat(sprintf("fc_agc_error failed: %d\n", sum(!is.na(results$fc_agc_error_note))))

# Always plot fc_agc vs fc_agc_error (no BIOMASS needed)
if (sum(valid_fc) > 0) {
  r_fc <- results[valid_fc, ]
  cat("\n--- fc_agc vs fc_agc_error ---\n")
  cat(sprintf("max diff = %.6f\n", max(abs(r_fc$fc_agc_AGC - r_fc$fc_agc_error_AGC), na.rm = TRUE)))
  plot(r_fc$fc_agc_AGC, r_fc$fc_agc_error_AGC,
       xlab = "fc_agc AGC_WCC_t", ylab = "fc_agc_error AGC_WCC_t",
       main = "fc_agc vs fc_agc_error")
  abline(0, 1, col = "red", lty = 2)
}

# If allometries available, full comparison
if (sum(valid) > 0) {
  r <- results[valid, ]
  cat("\n--- Agreement (all 3) ---\n")
  cat(sprintf("allometries vs fc_agc: max diff = %.6f\n",
              max(abs(r$allometries_AGC - r$fc_agc_AGC), na.rm = TRUE)))
  cat(sprintf("allometries vs fc_agc_error: max diff = %.6f\n",
              max(abs(r$allometries_AGC - r$fc_agc_error_AGC), na.rm = TRUE)))
  par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
  plot(r$allometries_AGC, r$fc_agc_AGC, xlab = "allometries WCC_C_t",
       ylab = "fc_agc AGC_WCC_t", main = "allometries vs fc_agc")
  abline(0, 1, col = "red", lty = 2)
  plot(r$allometries_AGC, r$fc_agc_error_AGC, xlab = "allometries WCC_C_t",
       ylab = "fc_agc_error AGC_WCC_t", main = "allometries vs fc_agc_error")
  abline(0, 1, col = "red", lty = 2)
  par(mfrow = c(1, 1))
}

# ------------------------------------------------------------------------------
# 5. Report misalignments
# ------------------------------------------------------------------------------
tol <- 1e-6
if (sum(valid_fc) > 0) {
  r_fc <- results[valid_fc, ]
  fc_vs_err <- abs(r_fc$fc_agc_AGC - r_fc$fc_agc_error_AGC) > tol
  if (any(fc_vs_err)) {
    cat("\n--- Misalignments (diff > 1e-6) ---\n")
    cat("fc_agc vs fc_agc_error:", sum(fc_vs_err), "rows\n")
  } else {
    cat("\nfc_agc and fc_agc_error align within tolerance.\n")
  }
}
if (sum(valid) > 0) {
  r <- results[valid, ]
  alm_vs_fc <- abs(r$allometries_AGC - r$fc_agc_AGC) > tol
  alm_vs_err <- abs(r$allometries_AGC - r$fc_agc_error_AGC) > tol
  if (any(alm_vs_fc) || any(alm_vs_err)) {
    cat("allometries vs fc_agc:", sum(alm_vs_fc), "rows\n")
    cat("allometries vs fc_agc_error:", sum(alm_vs_err), "rows\n")
  } else {
    cat("All three functions align within tolerance.\n")
  }
}

# Export for inspection
tryCatch({
  if (file.exists("inst/examples")) {
    write.csv(results, "inst/examples/9_WCC_alignment_test_results.csv", row.names = FALSE)
  } else if (file.exists("9_WCC_function_alignment_test.R")) {
    write.csv(results, "9_WCC_alignment_test_results.csv", row.names = FALSE)
  }
  cat("\nResults saved to CSV for inspection.\n")
}, error = function(e) message("Could not save CSV: ", conditionMessage(e)))

# ------------------------------------------------------------------------------
# 6. Minimal comparison (fc_agc vs fc_agc_error only - no BIOMASS/allodb needed)
# ------------------------------------------------------------------------------
cat("\n--- Minimal test: fc_agc vs fc_agc_error ---\n")
minimal <- data.frame(
  name = c("Oak", "Oak", "Sitka Spruce"),
  dbh = c(30, 50, 40),
  height = c(15, 22, 18),
  type = c("broadleaf", "broadleaf", "conifer"),
  height_timber = c(12, 18, NA),
  method = c("Matthews2", "IPCC2", "Matthews2")
)
minimal$fc_agc <- NA
minimal$fc_agc_error <- NA
for (i in seq_len(nrow(minimal))) {
  m <- minimal[i, ]
  minimal$fc_agc[i] <- fc_agc(m$name, m$dbh, m$height, type = m$type,
                               height_timber = if (is.na(m$height_timber)) NULL else m$height_timber,
                               method = m$method, output.all = FALSE)$AGC_WCC_t
  minimal$fc_agc_error[i] <- fc_agc_error(m$name, m$dbh, m$height, type = m$type,
                                           height_timber = if (is.na(m$height_timber)) NULL else m$height_timber,
                                           method = m$method, output.all = FALSE)$AGC_WCC_t
}
print(minimal)
cat("fc_agc vs fc_agc_error max diff:", max(abs(minimal$fc_agc - minimal$fc_agc_error)), "\n")
