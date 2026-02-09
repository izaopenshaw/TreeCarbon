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

# ==== Species Lookup ===========================================================

# Lookup species code
species_name <- "Oak"
spcode_result <- lookupcode(species_name, type = "broadleaf")
spcode <- spcode_result$code

cat(sprintf("Species: %s\n", species_name))
cat(sprintf("WCC Code: %s\n", spcode))

# Get additional information (NSG, Crown, Root codes) as needed
# Users can call lookupcode with specific code parameters:
# lookupcode(species_name, code = "NSG") for wood density
# lookupcode(species_name, code = "Crown") for crown code
# lookupcode(species_name, code = "Root") for root code

# ==== Tariff Calculation ======================================================

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

cat(sprintf("Tariff number: %.2f ± %.2f\n",
            tariff_result$tariff, tariff_result$sigma))

# ==== Volume Estimation =======================================================

# Step 3a: Merchantable volume
merch_vol <- merchtreevol(
  dbh = dbh,
  tariff = tariff_result$tariff,
  re_dbh = 0.025,
  sig_tariff = tariff_result$sigma
)

cat(sprintf("Volume: %.3f ± %.3f m³\n",
            merch_vol$volume, merch_vol$sigma))

# Step 3b: Stem volume
stem_vol <- treevol(
  mtreevol = merch_vol$volume,
  dbh = dbh,
  sig_mtreevol = merch_vol$sigma
)

cat(sprintf("Stem volume: %.3f ± %.3f m³\n",
            stem_vol$stemvolume, stem_vol$sigma))

# ==== Biomass Calculation =====================================================

# Step 4a: Wood biomass (using NSG)
# Get NSG (wood density) from lookup
nsg_lookup <- lookupcode(species_name, code = "NSG")
nsg <- nsg_lookup$code
wood_bio <- woodbiomass(
  treevol = stem_vol$stemvolume,
  nsg = nsg,
  sig_treevol = stem_vol$sigma,
  sig_nsg = 0.094  # Default NSG uncertainty
)

cat(sprintf("Wood biomass: %.3f ± %.3f t\n",
            wood_bio$woodbiomass, wood_bio$sigma))

# Step 4b: Crown biomass
crown_bio <- crownbiomass(
  spcode = spcode,
  dbh = dbh,
  re_d = 0.025,
  re = 0.025
)

cat(sprintf("Crown biomass: %.3f ± %.3f t\n",
            crown_bio$biomass, crown_bio$sigma))

# Step 4c: Total above-ground biomass
agb <- wood_bio$woodbiomass + crown_bio$biomass
agb_sigma <- sqrt(wood_bio$sigma^2 + crown_bio$sigma^2)

cat(sprintf("AGB: %.3f ± %.3f t\n", agb, agb_sigma))

# Step 4d: Root biomass (optional, for below-ground carbon)
root_bio <- rootbiomass(
  spcode = spcode,
  dbh = dbh,
  re_dbh = 0.025,
  re = 0.025
)

cat(sprintf("Root biomass: %.3f ± %.3f t\n",
            root_bio$rootbiomass, root_bio$sigma))

# ==== Carbon Conversion ========================================================

# Convert biomass to carbon using Thomas method
carbon <- biomass2c(
  biomass = agb,
  method = "Thomas",
  type = "broadleaf",
  sig_biomass = agb_sigma
)

cat(sprintf("Above-ground carbon: %.3f ± %.3f t C\n",
            carbon$AGC, carbon$sig_AGC))
cat(sprintf("95%% CI: [%.3f, %.3f] t C\n",
            carbon$AGC - 1.96 * carbon$sig_AGC,
            carbon$AGC + 1.96 * carbon$sig_AGC))

# ==== Complete Pipeline Summary ===============================================

cat(sprintf("Tree: %s (DBH: %d cm, Height: %d m)\n", species_name, dbh, height))
cat(sprintf("WCC Code: %s\n", spcode))
cat(sprintf("Tariff: %.2f\n", tariff_result$tariff))
cat(sprintf("Stem volume: %.3f m³\n", stem_vol$stemvolume))
cat(sprintf("Wood biomass: %.3f t\n", wood_bio$woodbiomass))
cat(sprintf("Crown biomass: %.3f t\n", crown_bio$biomass))
cat(sprintf("Total AGB: %.3f t\n", agb))
cat(sprintf("Above-ground carbon: %.3f ± %.3f t C\n",
            carbon$AGC, carbon$sig_AGC))

# ==== Comparison with High-Level Function ======================================

# Compare step-by-step result with high-level function
result_highlevel <- fc_agc_error(
  name = species_name,
  dbh = dbh,
  height = height,
  type = "broadleaf",
  re_dbh = 0.025,
  re_h = 0.05
)

cat(sprintf("Step-by-step result: Carbon: %.3f ± %.3f t C\n", carbon$AGC, carbon$sig_AGC))
cat(sprintf("High-level function result: Carbon: %.3f ± %.3f t C\n",
            result_highlevel$AGC_WCC_t, result_highlevel$sig_AGC))
cat("Note: Small differences may occur due to rounding or implementation details.\n")
