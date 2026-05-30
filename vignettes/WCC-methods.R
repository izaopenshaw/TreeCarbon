## ----setup, include = FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)


## ----load-package, message=FALSE, warning=FALSE------------------------------------------------------------
library(TreeCarbon)
library(ggplot2)


## ----lookup-basic------------------------------------------------------------------------------------------
# Lookup by common name
lookupcode("Oak")


## ----lookup-botanical--------------------------------------------------------------------------------------
# Lookup by botanical name
lookupcode("Quercus robur")


## ----lookup-multiple---------------------------------------------------------------------------------------
# Lookup multiple species
species <- c("Oak", "Beech", "Scots Pine", "Ash")
lookupcode(species)


## ----lookup-type-------------------------------------------------------------------------------------------
# Pine could be several species - specify conifer
lookupcode("Pine", type = "conifer")


## ----lookup-specific---------------------------------------------------------------------------------------
# Get NSG (wood density)
lookupcode("Oak", code = "NSG")

# Get Crown code
lookupcode("Oak", code = "Crown")

# Get Root code
lookupcode("Oak", code = "Root")


## ----tariff-vol-area---------------------------------------------------------------------------------------
# Example: Tree with measured volume
tariff <- tariff_vol_area(
  vol = 0.5,      # m³
  dbh = 24,       # cm
  sig_vol = 0.01, # uncertainty in volume
  re_dbh = 0.025  # 2.5% DBH measurement error
)

print(tariff)

## ----tariff-conifer----------------------------------------------------------------------------------------
# Scots Pine
tariff_pine <- conifer_tariff(
  spcode = "PS",  # Pinus sylvestris code
  height = 20,    # m
  dbh = 30,       # cm
  re_h = 0.05,    # 5% height error
  re_dbh = 0.025  # 2.5% DBH error
)
print(tariff_pine)


## ----tariff-broadleaf--------------------------------------------------------------------------------------
# Oak
tariff_oak <- broadleaf_tariff(
  spcode = "OK",  # Quercus code
  height_timber = 15,  # m (timber height - height to first branch, WCC protocol requirement)
  dbh = 45,       # cm
  re_h = 0.05,
  re_dbh = 0.025
)

print(tariff_oak)


## ----tariff-stand------------------------------------------------------------------------------------------
# Stand-level tariff (Method D) - top height required
tariff_stand <- stand_tariff(
  spcode = "OK",
  stand_height = 18,  # Top height in m
  re_h = 0.05
)

print(tariff_stand)


## ----tariff-auto-------------------------------------------------------------------------------------------
# Broadleaf Oak: provide height_timber or use timber_ratio (default 0.85)
tariff_auto <- tariffs(
  spcode = "OK",  # Oak
  height = 18,
  dbh = 45,
  type = "broadleaf",
  height_timber = 15,  # Measured timber height, or omit to use timber_ratio
  re_h = 0.05,
  re_dbh = 0.025
)

print(tariff_auto)
# tariff_method = "broad_timber" when height_timber provided


## ----merch-vol---------------------------------------------------------------------------------------------
# Calculate merchantable volume from tariff
merch_vol <- merchtreevol(
  dbh = 45,
  tariff = tariff_auto$tariff,
  re_dbh = 0.025,
  sig_tariff = tariff_auto$sigma
)

print(merch_vol)


## ----stem-vol----------------------------------------------------------------------------------------------
# Calculate stem volume from merchantable volume
stem_vol <- treevol(
  mtreevol = merch_vol$volume,
  dbh = 45,
  sig_mtreevol = merch_vol$sigma,
  re = 0.025
)

print(stem_vol)


## ----wood-bio----------------------------------------------------------------------------------------------
# Get NSG (wood density) from lookup - lookupcode converts species name to the chosen code
nsg_lookup <- lookupcode("Oak", code = "NSG")
nsg <- nsg_lookup$code

# Calculate wood biomass
wood_bio <- woodbiomass(
  treevol = stem_vol$stemvolume,
  nsg = nsg,
  sig_treevol = stem_vol$sigma,
  sig_nsg = 0.094  # Default NSG uncertainty
)

print(wood_bio)


## ----crown-bio---------------------------------------------------------------------------------------------
# Calculate crown biomass
crown_bio <- crownbiomass(
  spcode = "OK",
  dbh = 45,
  re_d = 0.025,
  re = 0.025
)

print(crown_bio)


## ----root-bio----------------------------------------------------------------------------------------------
# Calculate root biomass
root_bio <- rootbiomass(
  spcode = "OK",
  dbh = 45,
  re_dbh = 0.025,
  re = 0.025
)

print(root_bio)


## ----agb---------------------------------------------------------------------------------------------------
# Sum wood and crown biomass
agb <- wood_bio$woodbiomass + crown_bio$biomass
agb_sigma <- sqrt(wood_bio$sigma^2 + crown_bio$sigma^2)

cat(sprintf("Above-ground biomass: %.3f ± %.3f t\n", agb, agb_sigma))


## ----biomass2c---------------------------------------------------------------------------------------------
# Matthews2 (recommended for WCC) - type-specific carbon fraction
carbon_matthews2 <- biomass2c(biomass = agb, method = "Matthews2", type = "broadleaf")
print(carbon_matthews2)

# Thomas or IPCC2 - for uncertainty propagation (includes sig_AGC)
carbon_thomas <- biomass2c(
  biomass = agb,
  method = "Thomas",
  type = "broadleaf",
  sig_biomass = agb_sigma
)
print(carbon_thomas)

# IPCC2 (alternative with uncertainty)
carbon_ipcc <- biomass2c(
  biomass = agb,
  method = "IPCC2",
  type = "broadleaf",
  sig_biomass = agb_sigma
)
print(carbon_ipcc)


## ----complete-pipeline-------------------------------------------------------------------------------------
# === Step 1: Species lookup ===
species_name <- "Oak"
spcode_result <- lookupcode(species_name, type = "broadleaf")
spcode <- spcode_result$code
# Get NSG (wood density) when needed
nsg_lookup <- lookupcode(species_name, code = "NSG")
nsg <- nsg_lookup$code

cat("=== Step 1: Species Lookup ===\n")
cat(sprintf("Species: %s\n", species_name))
cat(sprintf("WCC Code: %s\n", spcode))
cat(sprintf("NSG: %s\n", nsg))
cat("\n")

# === Step 2: Tariff calculation ===
dbh <- 45  # cm
height <- 18  # m (total height)
height_timber <- 15  # m (height to first branch for broadleaves)
tariff_result <- tariffs(
  spcode = spcode,
  height = height,
  dbh = dbh,
  type = "broadleaf",
  height_timber = height_timber,  # Or use timber_ratio = 0.85 to estimate
  re_h = 0.05,
  re_dbh = 0.025
)

cat("=== Step 2: Tariff Calculation ===\n")
cat(sprintf("Tariff: %.2f ± %.2f (method: %s)\n",
            tariff_result$tariff, tariff_result$sigma, tariff_result$tariff_method))
cat("\n")

# === Step 3: Volume estimation ===
merch_vol <- merchtreevol(
  dbh = dbh,
  tariff = tariff_result$tariff,
  re_dbh = 0.025,
  sig_tariff = tariff_result$sigma
)

stem_vol <- treevol(
  mtreevol = merch_vol$volume,
  dbh = dbh,
  sig_mtreevol = merch_vol$sigma
)

cat("=== Step 3: Volume Estimation ===\n")
cat(sprintf("Merchantable volume: %.3f ± %.3f m³\n",
            merch_vol$volume, merch_vol$sigma))
cat(sprintf("Stem volume: %.3f ± %.3f m³\n",
            stem_vol$stemvolume, stem_vol$sigma))
cat("\n")

# === Step 4: Biomass calculation ===
# Get NSG (wood density) from lookup
nsg_lookup <- lookupcode(species_name, code = "NSG")
nsg <- nsg_lookup$code
wood_bio <- woodbiomass(
  treevol = stem_vol$stemvolume,
  nsg = nsg,
  sig_treevol = stem_vol$sigma,
  sig_nsg = 0.094
)

crown_bio <- crownbiomass(
  spcode = spcode,
  dbh = dbh,
  re_d = 0.025
)

agb <- wood_bio$woodbiomass + crown_bio$biomass
agb_sigma <- sqrt(wood_bio$sigma^2 + crown_bio$sigma^2)

cat("=== Step 4: Biomass Calculation ===\n")
cat(sprintf("Wood biomass: %.3f ± %.3f t\n",
            wood_bio$woodbiomass, wood_bio$sigma))
cat(sprintf("Crown biomass: %.3f ± %.3f t\n",
            crown_bio$biomass, crown_bio$sigma))
cat(sprintf("Total AGB: %.3f ± %.3f t\n", agb, agb_sigma))
cat("\n")

# === Step 5: Carbon conversion ===
# Using Thomas for uncertainty propagation in this demo; Matthews2 is WCC default
carbon <- biomass2c(
  biomass = agb,
  method = "Thomas",
  type = "broadleaf",
  sig_biomass = agb_sigma
)

cat("=== Step 5: Carbon Conversion ===\n")
cat(sprintf("Above-ground carbon: %.3f ± %.3f t C\n",
            carbon$AGC, carbon$sig_AGC))
cat("\n")

# === Summary ===
cat("=== FINAL RESULT ===\n")
cat(sprintf("Tree: %s (DBH: %d cm, Height: %d m)\n", species_name, dbh, height))
cat(sprintf("Above-ground carbon: %.3f ± %.3f t C\n",
            carbon$AGC, carbon$sig_AGC))
cat(sprintf("95%% CI: [%.3f, %.3f] t C\n",
            carbon$AGC - 1.96 * carbon$sig_AGC,
            carbon$AGC + 1.96 * carbon$sig_AGC))


## ----fc-agc-simple-----------------------------------------------------------------------------------------
# Simple calculation - returns carbon only
carbon_simple <- fc_agc(name = "Oak", dbh = 45, height = 18,
                        type = "broadleaf", output.all = FALSE)

cat(sprintf("Carbon: %.3f tonnes\n", carbon_simple))


## ----fc-agc-detailed---------------------------------------------------------------------------------------
# Get all intermediate values
carbon_detailed <- fc_agc(name = "Oak", dbh = 45, height = 18,
                          type = "broadleaf", output.all = TRUE)

print(carbon_detailed)


## ----fc-agc-error------------------------------------------------------------------------------------------
# Include uncertainty estimates
carbon_error <- fc_agc_error(name = "Oak", dbh = 45, height = 18,
                             type = "broadleaf",
                             re_dbh = 0.025, re_h = 0.05, re = 0.025)

# View key results
cat("=== Carbon with Uncertainty ===\n")
cat(sprintf("Carbon: %.3f ± %.3f t C\n",
            carbon_error$AGC_WCC_t, carbon_error$sig_AGC))
cat(sprintf("95%% CI: [%.3f, %.3f] t C\n",
            carbon_error$AGC_WCC_t - 1.96 * carbon_error$sig_AGC,
            carbon_error$AGC_WCC_t + 1.96 * carbon_error$sig_AGC))


## ----fc-agc-rich-------------------------------------------------------------------------------------------
# Get comprehensive metadata
carbon_rich <- fc_agc(name = "Oak", dbh = 45, height = 18,
                      type = "broadleaf", rich_output = TRUE)

print(carbon_rich)


## ----batch-fc-agc------------------------------------------------------------------------------------------
# Create sample data
trees <- data.frame(
  name = c("Oak", "Beech", "Ash", "Birch"),
  dbh = c(45, 38, 52, 28),
  height = c(18, 22, 24, 15),
  type = rep("broadleaf", 4)
)

# Calculate carbon for all trees
results <- fc_agc(name = trees$name, dbh = trees$dbh,
                  height = trees$height, type = trees$type,
                  output.all = TRUE)

# Summary
cat("=== Batch Results ===\n")
cat(sprintf("Total carbon: %.2f tonnes\n", sum(results$AGC_WCC_t, na.rm = TRUE)))
cat(sprintf("Mean per tree: %.3f tonnes\n", mean(results$AGC_WCC_t, na.rm = TRUE)))
print(results[, c("name", "dbh", "height", "AGC_WCC_t")])


## ----batch-fc-agc-error------------------------------------------------------------------------------------
# With uncertainty
results_error <- fc_agc_error(name = trees$name, dbh = trees$dbh,
                              height = trees$height, type = trees$type,
                              re_dbh = 0.025, re_h = 0.05, re = 0.025)

# Calculate total uncertainty (error propagation for sum)
total_carbon <- sum(results_error$AGC_WCC_t, na.rm = TRUE)
total_uncertainty <- sqrt(sum(results_error$sig_AGC^2, na.rm = TRUE))

cat("=== Batch Results with Uncertainty ===\n")
cat(sprintf("Total carbon: %.2f ± %.2f tonnes\n", total_carbon, total_uncertainty))
cat(sprintf("95%% CI: [%.2f, %.2f] tonnes\n",
            total_carbon - 1.96 * total_uncertainty,
            total_carbon + 1.96 * total_uncertainty))


## ----batch-rich--------------------------------------------------------------------------------------------
# Rich output for multiple trees
results_rich <- fc_agc(name = trees$name, dbh = trees$dbh,
                       height = trees$height, type = trees$type,
                       rich_output = TRUE)

print(results_rich)


## ----seedlings---------------------------------------------------------------------------------------------
# Seedling (height 0.5 m = 50 cm)
seedling_carbon <- sap_seedling2C(
  height = 0.5,     # metres (50 cm)
  type = "broadleaf",
  re_h = 0.05,
  re = 0.025
)

cat(sprintf("Seedling carbon: %.4f ± %.4f t C\n",
            seedling_carbon$carbon, seedling_carbon$sd))

# Sapling (height 5 m)
sapling_carbon <- sap_seedling2C(5, "conifer")
cat(sprintf("Sapling carbon: %.4f t C\n", sapling_carbon))


## ----custom-nsg--------------------------------------------------------------------------------------------
# Get wood density from BIOMASS package
if (requireNamespace("BIOMASS", quietly = TRUE)) {
  wd <- BIOMASS::getWoodDensity("Quercus", "robur", region = "Europe")

  # Use custom NSG
  carbon_custom <- fc_agc(name = "Oak", dbh = 45, height = 18,
                          type = "broadleaf", nsg = wd$meanWD)

  cat("=== Using Custom Wood Density ===\n")
  cat(sprintf("NSG from BIOMASS: %.3f\n", wd$meanWD))
  cat(sprintf("Carbon: %.3f tonnes\n", carbon_custom$AGC_WCC_t))
}


## ----plot-dbh, fig.height=5--------------------------------------------------------------------------------
# Create data for plotting
dbh_range <- seq(10, 100, by = 5)
carbon_result <- fc_agc(name = rep("Oak", length(dbh_range)),
                        dbh = dbh_range,
                        height = rep(18, length(dbh_range)),
                        type = rep("broadleaf", length(dbh_range)),
                        output.all = FALSE)

# Extract carbon values from result (handles both data frame and vector return)
if (is.data.frame(carbon_result)) {
  carbon_range <- carbon_result$AGC_WCC_t
} else {
  carbon_range <- carbon_result
}

plot_data <- data.frame(
  dbh = dbh_range,
  carbon = carbon_range
)

ggplot(plot_data, aes(x = dbh, y = carbon)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  labs(title = "Above-Ground Carbon vs DBH (Oak, Height = 18m)",
       x = "DBH (cm)", y = "Above-Ground Carbon (tonnes)") +
  theme_minimal()


## ----plot-uncertainty, fig.height=5------------------------------------------------------------------------
# Calculate with uncertainty
dbh_range <- seq(20, 80, by = 5)
results_unc <- fc_agc_error(name = rep("Oak", length(dbh_range)),
                            dbh = dbh_range,
                            height = rep(18, length(dbh_range)),
                            type = rep("broadleaf", length(dbh_range)),
                            re_dbh = 0.025, re_h = 0.05)

plot_data <- data.frame(
  dbh = dbh_range,
  carbon = results_unc$AGC_WCC_t,
  ci_low = results_unc$AGC_WCC_t - 1.96 * results_unc$sig_AGC,
  ci_high = results_unc$AGC_WCC_t + 1.96 * results_unc$sig_AGC
)

ggplot(plot_data, aes(x = dbh, y = carbon)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              alpha = 0.2, fill = "darkgreen") +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  labs(title = "Above-Ground Carbon vs DBH with 95% Confidence Intervals",
       x = "DBH (cm)", y = "Above-Ground Carbon (tonnes)") +
  theme_minimal()


## ----method-d-stand-carbon---------------------------------------------------------------------------------
# Method D: Stand-level carbon from sample trees
dbh_stand <- c(45, 52, 38, 60, 42, 55, 48, 35, 58, 40)
height_stand <- c(18, 22, 15, 24, 17, 21, 19, 14, 23, 16)
area_ha <- 0.1

stand_carbon <- fc_stand_carbon(name = "Oak", dbh = dbh_stand,
                                height = height_stand, area_ha = area_ha,
                                type = "broadleaf",
                                re_dbh = 0.05, re_h = 0.05,
                                output.all = TRUE)

cat("=== Method D: Stand-Level Carbon ===\n")
print(stand_carbon)


## ----method-e-dbh-class------------------------------------------------------------------------------------
# Method E: DBH class tallies
dbh_classes <- c(5, 15, 25, 35, 45)   # cm midpoints
counts <- c(150, 200, 180, 120, 50)   # trees per class
heights <- c(3, 8, 15, 20, 22)        # representative heights (m)

tally_data <- wcc_process_tally(dbh_class = dbh_classes, count = counts,
                                height = heights, use_midpoints = TRUE)

# Carbon from tally
carbon_tally <- wcc_dbh_class_carbon(name = "Oak", tally_data = tally_data,
                                     type = "broadleaf", area_ha = 1.0,
                                     re_dbh = 0.05, re_h = 0.05)

cat("=== Method E: DBH Class Carbon ===\n")
print(carbon_tally)


## ----stand-inventory---------------------------------------------------------------------------------------
# Example: Forest stand with multiple species
stand_data <- data.frame(
  species = c("Oak", "Beech", "Ash", "Birch", "Oak", "Beech"),
  dbh = c(45, 38, 52, 28, 55, 42),
  height = c(18, 22, 24, 15, 20, 21),
  type = c("broadleaf", "broadleaf", "broadleaf", "broadleaf",
           "broadleaf", "broadleaf")
)

# Calculate carbon
stand_results <- fc_agc_error(name = stand_data$species, dbh = stand_data$dbh,
                              height = stand_data$height,
                              type = stand_data$type,
                              re_dbh = 0.025, re_h = 0.05)

# Summary by species
stand_summary <- aggregate(
  list(AGC = stand_results$AGC_WCC_t,
       Uncertainty = stand_results$sig_AGC),
  by = list(Species = stand_results$name),
  FUN = function(x) c(Total = sum(x, na.rm = TRUE),
                      Mean = mean(x, na.rm = TRUE))
)

cat("=== Stand Carbon Inventory ===\n")
print(stand_summary)

# Total stand carbon
total_stand_carbon <- sum(stand_results$AGC_WCC_t, na.rm = TRUE)
total_stand_uncertainty <- sqrt(sum(stand_results$sig_AGC^2, na.rm = TRUE))

cat(sprintf("\nTotal Stand Carbon: %.2f ± %.2f tonnes C\n",
            total_stand_carbon, total_stand_uncertainty))


## ----per-area----------------------------------------------------------------------------------------------
# Calculate carbon density (tonnes C per hectare)
stand_area_ha <- 0.5  # 0.5 hectares

carbon_density <- total_stand_carbon / stand_area_ha
carbon_density_uncertainty <- total_stand_uncertainty / stand_area_ha

cat("=== Carbon Density ===\n")
cat(sprintf("Stand area: %.2f ha\n", stand_area_ha))
cat(sprintf("Carbon density: %.2f ± %.2f t C/ha\n",
            carbon_density, carbon_density_uncertainty))


## ----wcc-mean-tariff---------------------------------------------------------------------------------------
# Mean tariff from sample trees (WCC protocol requirement).
# Run this chunk from the top: mean_tariff is created by wcc_mean_tariff() below.
spcode <- lookupcode("Oak", type = "broadleaf")$code
sample_heights <- c(18, 20, 19, 22, 17)  # m
sample_dbh <- c(45, 48, 42, 52, 38)      # cm

mean_tariff <- wcc_mean_tariff(
  spcode = spcode,
  height = sample_heights,
  dbh = sample_dbh,
  type = "broadleaf",
  re_h = 0.05,
  re_dbh = 0.025
)

cat("=== Mean Tariff from Sample Trees ===\n")
# wcc_mean_tariff() returns a list(mean_tariff, sigma) when re_h/re_dbh are set;
# use !is.data.frame because is.list() is also TRUE for data.frames.
if (is.list(mean_tariff) && !is.data.frame(mean_tariff)) {
  cat(sprintf("Mean tariff: %.2f ± %.2f\n", mean_tariff$mean_tariff, mean_tariff$sigma))
} else {
  cat(sprintf("Mean tariff: %.2f\n", mean_tariff))
}


## ----wcc-per-hectare---------------------------------------------------------------------------------------
# Scale per-tree carbon to per-hectare
carbon_per_tree <- c(0.5, 0.6, 0.45, 0.55, 0.5)  # t C per tree
trees_per_ha <- 200
plot_area_ha <- 0.5

per_ha_result <- wcc_per_hectare(
  carbon_per_tree = carbon_per_tree,
  trees_per_ha = trees_per_ha,
  area_ha = plot_area_ha,
  sig_carbon_per_tree = 0.05  # optional: propagates uncertainty
)

cat("=== Per-Hectare Scaling ===\n")
cat(sprintf("Carbon per ha: %.2f ± %.2f t C/ha\n",
            per_ha_result$carbon_per_ha, per_ha_result$sig_carbon_per_ha))
cat(sprintf("Total carbon (%.1f ha): %.2f ± %.2f t C\n",
            plot_area_ha, per_ha_result$total_carbon, per_ha_result$sig_total_carbon))


## ----wcc-stratify------------------------------------------------------------------------------------------
# Aggregate carbon across stratified stands (with optional uncertainty)
strata <- data.frame(
  area_ha = c(0.5, 0.3, 0.2),
  carbon_per_ha_t = c(120, 95, 80),
  carbon_per_ha_sig_t = c(15, 12, 10)  # optional: per-stratum uncertainty
)

strata_result <- wcc_stratify(strata, carbon_col = "carbon_per_ha_t")

cat("=== Stratified Carbon Aggregation ===\n")
cat(sprintf("Total area: %.2f ha\n", strata_result$total_area_ha))
cat(sprintf("Total carbon: %.1f ± %.1f t C\n",
            strata_result$total_carbon_t, strata_result$total_sig_carbon_t))
cat(sprintf("Weighted mean: %.1f ± %.1f t C/ha\n",
            strata_result$weighted_mean_carbon_per_ha_t, strata_result$weighted_mean_sig))


## ----fc-wcc-app4-stand-------------------------------------------------------------------------------------
# Compartment 1: Scots Pine - sample tree DBH and heights from plots
# (Representative of the 24 height sample trees used in Appendix 4)
dbh_comp1 <- c(22, 24, 26, 28, 26, 24, 28, 30, 26, 24)  # cm
height_comp1 <- c(14, 15, 16, 17, 15, 14, 16, 18, 15, 14)  # m
area_comp1 <- 5.6  # ha net area

stand1 <- fc_stand_carbon(name = "Scots Pine", dbh = dbh_comp1, height = height_comp1,
                          area_ha = area_comp1, type = "conifer",
                          re_dbh = 0.05, re_h = 0.05, output.all = TRUE)

cat("=== Compartment 1: Scots Pine ===\n")
cat(sprintf("Stand carbon: %.1f ± %.1f t C\n", stand1$stand_AGC_t, stand1$sig_stand_AGC))
cat(sprintf("Carbon per ha: %.1f t C/ha\n\n", stand1$stand_AGC_t / area_comp1))

# Compartment 2: Corsican Pine
dbh_comp2 <- c(16, 18, 20, 18, 16, 20, 18, 19)  # cm
height_comp2 <- c(10, 11, 12, 11, 10, 12, 11, 11)  # m
area_comp2 <- 3.6  # ha net area

stand2 <- fc_stand_carbon(name = "Corsican Pine", dbh = dbh_comp2, height = height_comp2,
                          area_ha = area_comp2, type = "conifer",
                          re_dbh = 0.05, re_h = 0.05, output.all = TRUE)

cat("=== Compartment 2: Corsican Pine ===\n")
cat(sprintf("Stand carbon: %.1f ± %.1f t C\n", stand2$stand_AGC_t, stand2$sig_stand_AGC))
cat(sprintf("Carbon per ha: %.1f t C/ha\n\n", stand2$stand_AGC_t / area_comp2))

# Total project carbon (as in Appendix 4)
total_carbon <- stand1$stand_AGC_t + stand2$stand_AGC_t
total_area <- area_comp1 + area_comp2
cat("=== Total Project (Appendix 4 style) ===\n")
cat(sprintf("Total carbon: %.1f t C\n", total_carbon))
cat(sprintf("Total area: %.1f ha\n", total_area))
cat(sprintf("Carbon per ha: %.1f t C/ha\n", total_carbon / total_area))
cat(sprintf("tCO2e (× 44/12): %.0f tCO2e\n", total_carbon * 44/12))


## ----fc-wcc-app4-dbh-class---------------------------------------------------------------------------------
# Compartment 1: Scots Pine - DBH class midpoints and counts
# (Simplified from Appendix 4 plot data)
dbh_classes <- c(22, 24, 26, 28, 30)  # cm
counts <- c(400, 600, 800, 900, 706)  # trees per class
heights <- c(14, 15, 16, 17, 18)  # representative heights (m)

tally <- wcc_process_tally(dbh_class = dbh_classes, count = counts,
                          height = heights, use_midpoints = TRUE)

carbon_comp1 <- wcc_dbh_class_carbon(name = "Scots Pine", tally_data = tally,
                                     type = "conifer", area_ha = 5.6,
                                     re_dbh = 0.05, re_h = 0.05)

cat("=== Compartment 1: DBH class tally (Method E style) ===\n")
print(carbon_comp1)


## ----fc-wcc-app5-------------------------------------------------------------------------------------------
# Oak and ash/birch groups - individual trees or DBH classes
trees_oak <- data.frame(dbh = c(25, 30, 35, 40, 45), height = c(12, 15, 18, 20, 22),
                        count = c(50, 80, 120, 150, 100))

carbon_oak <- sum(
  fc_agc_error(name = "Oak", dbh = trees_oak$dbh, height = trees_oak$height,
               type = "broadleaf", method = "Matthews2",
               height_timber = trees_oak$height * 0.85)$AGC_WCC_t * trees_oak$count
)

cat(sprintf("Oak carbon (example): %.1f t C\n", carbon_oak))


## ----troubleshooting-species-------------------------------------------------------------------------------
# Unknown species - will use type-based defaults
result_unknown <- fc_agc(name = "Unknown Species", dbh = 45, height = 18,
                         type = "broadleaf")  # Important: specify type

cat("Using type-based defaults for unknown species\n")
print(result_unknown)


## ----troubleshooting-range---------------------------------------------------------------------------------
# Very small tree - may trigger warnings
result_small <- fc_agc(name = "Oak", dbh = 5, height = 3,
                       type = "broadleaf")

# Check for warnings/flags
cat("Small tree calculation (may have validity warnings)\n")


## ----troubleshooting-height--------------------------------------------------------------------------------
# Option 1: Use Bunce method instead (no height needed)
bunce_result <- Bunce(name = "Oak", dbh = 45)

# Option 2: Estimate height from DBH (not recommended for WCC)
# WCC requires measured height for accurate carbon accounting


## ----session-info------------------------------------------------------------------------------------------
sessionInfo()

