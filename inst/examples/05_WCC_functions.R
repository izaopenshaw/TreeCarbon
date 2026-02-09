# ==============================================================================
# Example 5: WCC Functions - Step-by-Step Pipeline
# ==============================================================================
# This example demonstrates the complete WCC carbon estimation pipeline
# using individual WCC functions. This is useful for understanding the
# methodology or when you need intermediate values.
#
# This example shows:
# - Species lookup
# - Tariff calculation
# - Volume estimation
# - Biomass calculation
# - Carbon conversion
# - Complete pipeline
# ==============================================================================

library(TreeCarbon)

# ==============================================================================
# Example 5.1: Species Lookup
# ==============================================================================

# Lookup species code
species_name <- "Oak"
spcode_result <- lookupcode(species_name, type = "broadleaf")
spcode <- spcode_result$code

cat("=== Species Lookup ===\n")
cat(sprintf("Species: %s\n", species_name))
cat(sprintf("WCC Code: %s\n", spcode))
cat("\n")

# Get additional information (NSG, Crown, Root codes)
lookup_info <- lookupcode(species_name, code = "all", returnv = "all")
cat("=== Species Information ===\n")
cat(sprintf("NSG (wood density): %.3f\n", lookup_info$NSG))
cat(sprintf("Crown code: %s\n", lookup_info$Crown))
cat(sprintf("Root code: %s\n", lookup_info$Root))
cat("\n")

# ==============================================================================
# Example 5.2: Tariff Calculation
# ==============================================================================

dbh <- 45  # cm
height <- 18  # m

# Calculate tariff number
tariff_result <- tariffs(
  spcode = spcode,
  height = height,
  dbh = dbh,
  type = "broadleaf",
  re_h = 0.05,    # 5% height error
  re_dbh = 0.025  # 2.5% DBH error
)

cat("=== Tariff Calculation ===\n")
cat(sprintf("Tariff number: %.2f ± %.2f\n",
            tariff_result$tariff, tariff_result$sigma))
cat("\n")

# ==============================================================================
# Example 5.3: Volume Estimation
# ==============================================================================

# Step 3a: Merchantable volume
merch_vol <- merchtreevol(
  dbh = dbh,
  tariff = tariff_result$tariff,
  re_dbh = 0.025,
  sig_tariff = tariff_result$sigma
)

cat("=== Merchantable Volume ===\n")
cat(sprintf("Volume: %.3f ± %.3f m³\n",
            merch_vol$volume, merch_vol$sigma))
cat("\n")

# Step 3b: Stem volume
stem_vol <- treevol(
  mtreevol = merch_vol$volume,
  dbh = dbh,
  sig_mtreevol = merch_vol$sigma
)

cat("=== Stem Volume ===\n")
cat(sprintf("Stem volume: %.3f ± %.3f m³\n",
            stem_vol$stemvolume, stem_vol$sigma))
cat("\n")

# ==============================================================================
# Example 5.4: Biomass Calculation
# ==============================================================================

# Step 4a: Wood biomass (using NSG)
wood_bio <- woodbiomass(
  treevol = stem_vol$stemvolume,
  nsg = lookup_info$NSG,
  sig_treevol = stem_vol$sigma,
  sig_nsg = 0.094  # Default NSG uncertainty
)

cat("=== Wood Biomass ===\n")
cat(sprintf("Wood biomass: %.3f ± %.3f t\n",
            wood_bio$woodbiomass, wood_bio$sigma))
cat("\n")

# Step 4b: Crown biomass
crown_bio <- crownbiomass(
  spcode = spcode,
  dbh = dbh,
  re_d = 0.025,
  re = 0.025
)

cat("=== Crown Biomass ===\n")
cat(sprintf("Crown biomass: %.3f ± %.3f t\n",
            crown_bio$biomass, crown_bio$sigma))
cat("\n")

# Step 4c: Total above-ground biomass
agb <- wood_bio$woodbiomass + crown_bio$biomass
agb_sigma <- sqrt(wood_bio$sigma^2 + crown_bio$sigma^2)

cat("=== Total Above-Ground Biomass ===\n")
cat(sprintf("AGB: %.3f ± %.3f t\n", agb, agb_sigma))
cat("\n")

# Step 4d: Root biomass (optional, for below-ground carbon)
root_bio <- rootbiomass(
  spcode = spcode,
  dbh = dbh,
  re_dbh = 0.025,
  re = 0.025
)

cat("=== Root Biomass ===\n")
cat(sprintf("Root biomass: %.3f ± %.3f t\n",
            root_bio$rootbiomass, root_bio$sigma))
cat("\n")

# ==============================================================================
# Example 5.5: Carbon Conversion
# ==============================================================================

# Convert biomass to carbon using Thomas method
carbon <- biomass2c(
  biomass = agb,
  method = "Thomas",
  type = "broadleaf",
  sig_biomass = agb_sigma
)

cat("=== Carbon Conversion ===\n")
cat(sprintf("Above-ground carbon: %.3f ± %.3f t C\n",
            carbon$AGC, carbon$sig_AGC))
cat(sprintf("95%% CI: [%.3f, %.3f] t C\n",
            carbon$AGC - 1.96 * carbon$sig_AGC,
            carbon$AGC + 1.96 * carbon$sig_AGC))
cat("\n")

# ==============================================================================
# Example 5.6: Complete Pipeline Summary
# ==============================================================================

cat("=== COMPLETE PIPELINE SUMMARY ===\n")
cat(sprintf("Tree: %s (DBH: %d cm, Height: %d m)\n", species_name, dbh, height))
cat(sprintf("WCC Code: %s\n", spcode))
cat(sprintf("Tariff: %.2f\n", tariff_result$tariff))
cat(sprintf("Stem volume: %.3f m³\n", stem_vol$stemvolume))
cat(sprintf("Wood biomass: %.3f t\n", wood_bio$woodbiomass))
cat(sprintf("Crown biomass: %.3f t\n", crown_bio$biomass))
cat(sprintf("Total AGB: %.3f t\n", agb))
cat(sprintf("Above-ground carbon: %.3f ± %.3f t C\n",
            carbon$AGC, carbon$sig_AGC))
cat("\n")

# ==============================================================================
# Example 5.7: Comparison with High-Level Function
# ==============================================================================

# Compare step-by-step result with high-level function
result_highlevel <- fc_agc_error(
  name = species_name,
  dbh = dbh,
  height = height,
  type = "broadleaf",
  re_dbh = 0.025,
  re_h = 0.05
)

cat("=== Comparison with High-Level Function ===\n")
cat("Step-by-step result:\n")
cat(sprintf("  Carbon: %.3f ± %.3f t C\n", carbon$AGC, carbon$sig_AGC))
cat("\nHigh-level function result:\n")
cat(sprintf("  Carbon: %.3f ± %.3f t C\n",
            result_highlevel$AGC_WCC_t, result_highlevel$sig_AGC))
cat("\nNote: Small differences may occur due to rounding or implementation details.\n")
