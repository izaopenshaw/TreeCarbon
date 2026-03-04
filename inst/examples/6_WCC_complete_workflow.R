# ==============================================================================
# Example 6: Complete WCC Workflow - All Equations and Methods
# ==============================================================================
# Full runnable pipeline for all WCC Methods A-E (Carbon Assessment Protocol v2.0).
#
# SIGNPOSTING:
#   - Vignette: vignette("WCC-methods", package = "TreeCarbon") - narrative guide
#   - Method reference: inst/examples/WCC_method_reference.R - Method | Functions | Section
#   - Method selection: 8_WCC_method_selection.R - which method for your data?
#
# Use this file when: you want executable code for the full WCC pipeline.
# Use the vignette when: you want explanations, troubleshooting, and step-by-step walkthrough.
#
# Covers:
# - WCC Methods A-E: tariff_vol_area (A), broadleaf_tariff (B), conifer_tariff (C),
#   stand_tariff/fc_stand_carbon (D), wcc_process_tally/wcc_dbh_class_carbon (E)
# - Seedlings and saplings (height < 10m or DBH < 7cm)
# - Tariff number calculations (Methods A-D)
# - Volume estimation from basal area
# - Biomass calculations (stem, crown, root)
# - Carbon conversion using Matthews methods
# - Complete single tree workflow
# ==============================================================================

library(TreeCarbon)

# ==============================================================================
# (1) SEEDLINGS AND SAPLINGS
# ==============================================================================
# According to WCC Protocol v2.0:
# - Seedlings: Height < 1m (100cm) or DBH < 7cm
# - Saplings: Height 1-10m (100-1000cm) or DBH 7-10cm
# - Trees: DBH >= 7cm and height >= 10m
#
# For seedlings and saplings, carbon is calculated directly from height
# using lookup tables, without requiring DBH or tariff calculations.
# ==============================================================================

# ==== Sapling Example ====
# Sapling: height = 3m (300cm), conifer
sap_seedling2C(height = 3.0,    type = "conifer")

# ==== Sapling with error ====
# Seedling: height = 0.5m (50cm), broadleaf
sap_seedling2C(height = 0.5, type = "broadleaf",
               re_h = 0.05,  # 5% height measurement error
               re = 0.025)   # 2.5% coefficient error

# ==============================================================================
# (2) TARIFF NUMBER CALCULATIONS
# ==============================================================================
# Tariff is calculated from timber height (height to first branch or
# merchantable top) and DBH (Diameter at Breast Height 1.3 m)

dbh_brdlf <- 45

# ==== 2.1 Method B: Broadleaf Tariff (Equation 4) ====
spcode <- lookupcode("Oak", type = "broadleaf")$code

broadleaf_tariff(spcode = spcode, height_timber = 15, dbh = dbh_brdlf)
# Timber height for broadleaves (WCC protocol requirement)
# Timber height is the usable straight trunk length. Point clinometer or height
# measuring instrument where the trunk becomes too thin to be useful timber
# (where 7 cm diameter including bark).

# ==== 2.2 Method C: Conifer Tariff (Equation 3) ====
spcode_conifer <- lookupcode("Scots Pine", type = "conifer")$code

conifer_tariff(spcode = spcode_conifer,     height = 20,    dbh = 30)

# ==== 2.3 Method D: Stand Tariff (Equation 4) ====
# For stand-level calculations when DBH is not available
# Uses top height (mean height of 100 largest trees per hectare)
# Use calculate_top_height() to derive from plot data

stand_tariff(spcode = spcode, stand_height = 18)  # Top height in metres

# ==== 2.4 Method A: Tariff from Volume and Basal Area (Equation 1) ====
# If you have measured volume from a felled sample tree

tariff_vol_area(vol = 0.5, dbh = dbh_brdlf,  # m³ (from felled tree)
                sig_vol = 0.01, re_dbh = 0.05, re = 0.025)

# ==== 2.5 Automatic Tariff Selection (Methods B/C) ====
# tariffs() automatically selects broadleaf_tariff (B) or conifer_tariff (C)

tariff_auto <- tariffs(spcode = spcode, height = 20, dbh = dbh_brdlf,
                       type = "broadleaf", height_timber = 20,
                       re_h = 0.05, re_dbh = 0.025)

# ==============================================================================
# PART 3: VOLUME ESTIMATION
# ==============================================================================
# Volume is calculated from tariff number and DBH using WCC equations
# ==============================================================================

# ==== 3.1: Merchantable Volume (Equation 5a) ====
# Merchantable volume = volume of main stem up to specified top diameter

merch_vol <- merchtreevol(dbh = dbh_brdlf,          tariff = tariff_auto$tariff,
                          re_dbh = 0.025,    sig_tariff = tariff_auto$sigma)

# ==== 3.2: Stem Volume (Equation 5b) ====
# Stem volume includes full stem including taper
# Conversion factor applied based on DBH

treevol(mtreevol = merch_vol$volume,         dbh = dbh_brdlf,
        sig_mtreevol = merch_vol$sigma)

# ==== 3.3: Volume from Basal Area ====
# If you have basal area measurements instead of individual tree DBH
# Basal area = (π × DBH²) / 40000 (in m²)

basal_area <- (pi * dbh_brdlf^2) / 40000  # m²
# Volume can be calculated from basal area and tariff
# This is used in stand-level calculations (Method E)

# ==============================================================================
# PART 4: BIOMASS CALCULATION
# ==============================================================================
# Biomass is calculated from volume using wood density (NSG) and
# species-specific allometric equations for crown and roots
# ==============================================================================

# ==== 4.1: Stem Wood Biomass ====
# Stem biomass = stem volume × Nominal Specific Gravity (NSG)
# NSG is obtained from WCC lookup tables

nsg_lookup <- lookupcode(species_name, code = "NSG")
nsg <- as.numeric(nsg_lookup$code)

wood_bio <- woodbiomass(treevol = stem_vol$stemvolume, nsg = nsg,
                        sig_treevol = stem_vol$sigma,
                        sig_nsg = 0.094)  # Default NSG uncertainty

# ==== 4.2: Crown Biomass (Equations 6-7) ====
# Crown biomass includes branches, stem tips, and foliage
# Uses species-specific crown factors from WCC lookup

crown_code_lookup <- lookupcode(species_name, code = "Crown")
crown_code <- crown_code_lookup$code

crown_bio <- crownbiomass(spcode = crown_code, dbh = dbh_brdlf,
                          re_d = 0.025, re = 0.025)

# ==== 4.3: Root Biomass (Equations 8-9) ====
# Below-ground biomass estimation
# Uses species-specific root factors from WCC lookup

root_code_lookup <- lookupcode(species_name, code = "Root")
root_code <- root_code_lookup$code

root_bio <- rootbiomass(spcode = root_code, dbh = dbh_brdlf,
                        re_dbh = 0.025, re = 0.025)

# ==== 4.4: Total Above-Ground Biomass ====
# AGB = stem wood biomass + crown biomass

agb <- wood_bio$woodbiomass + crown_bio$biomass
agb_sigma <- sqrt(wood_bio$sigma^2 + crown_bio$sigma^2)

# ==== 4.5: Total Tree Biomass ====
# Total biomass = AGB + root biomass

total_biomass <- agb + root_bio$rootbiomass
total_biomass_sigma <- sqrt(agb_sigma^2 + root_bio$sigma^2)

# ==============================================================================
# PART 5: CARBON CONVERSION
# ==============================================================================
# Convert biomass to carbon using carbon fraction (CF) from chosen method
# WCC protocol recommends Matthews methods for UK applications
# ==============================================================================

# ==== 5.1: Matthews Method 1 (Matthews1) ====
# Simplest method: CF = 50% for all trees
# Suitable for quick estimates

carbon_matthews1 <- biomass2c(biomass = agb, method = "Matthews1",
                              sig_biomass = agb_sigma)

# ==== 5.2: Matthews Method 2 (Matthews2) ====
# CF varies by tree type (broadleaf vs conifer)
# Recommended for UK WCC applications

carbon_matthews2 <- biomass2c(biomass = agb, method = "Matthews2",
                              type = "broadleaf", sig_biomass = agb_sigma)

# ==== 5.3: Other Methods (for comparison) ====
# IPCC1: CF = 47.7% (default IPCC value)
carbon_ipcc1 <- biomass2c(biomass = agb, method = "IPCC1",
                          sig_biomass = agb_sigma)

# IPCC2: CF varies by type and biome
carbon_ipcc2 <- biomass2c(biomass = agb, method = "IPCC2",
                          type = "broadleaf", biome = "temperate",
                          sig_biomass = agb_sigma)

# Thomas: CF from empirical synthesis (most detailed)
carbon_thomas <- biomass2c(biomass = agb, method = "Thomas",
                           type = "broadleaf", biome = "temperate",
                           sig_biomass = agb_sigma)

# ==============================================================================
# PART 6: COMPLETE SINGLE TREE WORKFLOW
# ==============================================================================
# Complete example for one tree following WCC protocol step-by-step
# ==============================================================================

# Input data
tree_name <- "Oak"
tree_dbh <- 45      # cm
tree_height_total <- 18   # m (total height)
tree_height_timber <- 15  # m (timber height - height to first branch, WCC protocol requirement)
tree_type <- "broadleaf"

# Step 1: Species lookup
spcode_lookup <- lookupcode(tree_name, type = tree_type)
tree_spcode <- spcode_lookup$code

# Step 2: Tariff calculation
# For broadleaves, use timber height (WCC protocol requirement)
tree_tariff <- tariffs(spcode = tree_spcode, height = tree_height_total,
                       dbh = tree_dbh, type = tree_type,
                       height_timber = tree_height_timber,
                       re_h = 0.05, re_dbh = 0.025)

# Step 3: Volume estimation
tree_merch_vol <- merchtreevol(dbh = tree_dbh, tariff = tree_tariff$tariff,
                               re_dbh = 0.025, sig_tariff = tree_tariff$sigma)

tree_stem_vol <- treevol(mtreevol = tree_merch_vol$volume, dbh = tree_dbh,
                         sig_mtreevol = tree_merch_vol$sigma)

# Step 4: Biomass calculation
tree_nsg <- as.numeric(lookupcode(tree_name, code = "NSG")$code)
tree_wood_bio <- woodbiomass(treevol = tree_stem_vol$stemvolume, nsg = tree_nsg,
                             sig_treevol = tree_stem_vol$sigma, sig_nsg = 0.094)

tree_crown_code <- lookupcode(tree_name, code = "Crown")$code
tree_crown_bio <- crownbiomass(spcode = tree_crown_code, dbh = tree_dbh,
                               re_d = 0.025, re = 0.025)

tree_agb <- tree_wood_bio$woodbiomass + tree_crown_bio$biomass
tree_agb_sigma <- sqrt(tree_wood_bio$sigma^2 + tree_crown_bio$sigma^2)

# Step 5: Carbon conversion (using Matthews2 as recommended for WCC)
tree_carbon <- biomass2c(biomass = tree_agb, method = "Matthews2",
                         type = tree_type, sig_biomass = tree_agb_sigma)

# ==============================================================================
# PART 7: COMPARISON WITH HIGH-LEVEL FUNCTIONS (Methods B/C)
# ==============================================================================
# The high-level functions (fc_agc, fc_agc_error) perform all steps above
# automatically. Compare results to verify consistency.
# ==============================================================================

# High-level function result
# For broadleaves, provide timber height for WCC protocol compliance
result_highlevel <- fc_agc_error(name = tree_name, dbh = tree_dbh,
                                 height = tree_height_total, type = tree_type,
                                 height_timber = tree_height_timber,
                                 method = "Matthews2",
                                 re_dbh = 0.025, re_h = 0.05, re = 0.025)

# Results should be very similar (small differences may occur due to rounding)
# Step-by-step: tree_carbon$AGC
# High-level: result_highlevel$AGC_WCC_t

# ==============================================================================
# PART 8: METHOD D - STAND-LEVEL CARBON (fc_stand_carbon)
# ==============================================================================
# When you have DBH and height from sample trees and want stand-level carbon.
# Uses calculate_top_height() and stand_tariff internally.
# ==============================================================================

dbh_stand <- c(45, 52, 38, 60, 42, 55, 48, 35, 58, 40)
height_stand <- c(18, 22, 15, 24, 17, 21, 19, 14, 23, 16)
area_ha <- 0.1

# Method D: Stand-level carbon
fc_stand_carbon(name = "Oak", dbh = dbh_stand, height = height_stand,
                area_ha = area_ha, type = "broadleaf",
                re_dbh = 0.05, re_h = 0.05, output.all = TRUE)

# ==============================================================================
# PART 9: METHOD E - DBH CLASS TALLIES (wcc_process_tally, wcc_dbh_class_carbon)
# ==============================================================================
# When trees are tallied by DBH class rather than measured individually.
# ==============================================================================

dbh_classes <- c(5, 15, 25, 35, 45)   # cm midpoints
counts <- c(150, 200, 180, 120, 50)   # trees per class
heights <- c(3, 8, 15, 20, 22)        # representative heights (m)

# Process tally
tally_data <- wcc_process_tally(dbh_class = dbh_classes, count = counts,
                                height = heights, use_midpoints = TRUE)

# Method E: Carbon from DBH class tallies
wcc_dbh_class_carbon(name = "Oak", tally_data = tally_data,
                     type = "broadleaf", area_ha = 1.0,
                     output.all = TRUE)

# ==============================================================================
# PART 10: AVERAGING TREES AND PER-HECTARE SCALING
# ==============================================================================
# WCC protocol often requires averaging trees (e.g. mean tariff) and scaling
# to per-hectare values. Helper functions support these operations.
# ==============================================================================

# ==== 10.1 Mean tariff from sample trees (WCC protocol requirement) ====
mean_tariff <- wcc_mean_tariff(spcode = lookupcode("Oak", type = "broadleaf")$code,
                               height = c(18, 20, 19, 22, 17),
                               dbh = c(45, 48, 42, 52, 38),
                               type = "broadleaf",
                               re_h = 0.05, re_dbh = 0.025)

# ==== 10.2 Scale per-tree carbon to per-hectare (with uncertainty) ====
per_ha <- wcc_per_hectare(carbon_per_tree = c(0.5, 0.6, 0.45, 0.55, 0.5),
                          trees_per_ha = 200, area_ha = 0.5,
                          sig_carbon_per_tree = 0.05)

# ==== 10.3 Aggregate carbon across stratified stands (with uncertainty) ====
strata <- data.frame(area_ha = c(0.5, 0.3, 0.2),
                     carbon_per_ha_t = c(120, 95, 80),
                     carbon_per_ha_sig_t = c(15, 12, 10))
wcc_stratify(strata, carbon_col = "carbon_per_ha_t")
