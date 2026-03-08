# ==============================================================================
# Example 8: WCC Method Selection Guide
# ==============================================================================
# Choose which WCC method to use based on your available data (Carbon Assessment
# Protocol v2.0).
#
# Other examples:
#   - Vignette: vignette("WCC-methods", package = "TreeCarbon") - full guide
#   - Full workflow: 6_WCC_complete_workflow.R - runnable pipeline for all methods
#
# This file is for comparing against the different WCC methods according to the data you have.
#
# ==============================================================================
# METHOD REFERENCE TABLE
# ==============================================================================
#
# -------+---------------+--------------------------------+-----------------------------+----------+--------
# Method | Method name   | Functions                      | Inputs                      | This doc | Vignette: "WCC-methods"
# -------+---------------+--------------------------------+-----------------------------+----------+--------
#  A     | Fell sample   | tariff_vol_area()              | Felled sample: vol (m³),    |   5      | 2.1, 6
#        | trees         | merchtreevol(), treevol(),     | dbh (cm)                    |          |
#        |               | woodbiomass(), crownbiomass(), | Then: NSG, species for      |          |
#        |               | biomass2c()                    | full pipeline*              |          |
# -------+---------------+--------------------------------+-----------------------------+----------+--------
#  B     | Timber height | broadleaf_tariff(), tariffs()  | type="broadleaf", height_timber or timber_ratio | 1 | 2.3, 7, 12.3
#        | (broadleaves) | fc_agc(), fc_agc_error()       | dbh (cm), height_timber (m) |          |
# -------+---------------+--------------------------------+-----------------------------+----------+--------
#  C     | Total height  | conifer_tariff(), tariffs()    | type="conifer", species,    |   1      | 2.2, 7, 12.3
#        | (conifers)    | fc_agc(), fc_agc_error()       | dbh (cm), height (total, m) |          |
# -------+---------------+--------------------------------+-----------------------------+----------+--------
#  D     | Top height    | stand_tariff(),                | stand_height                |   3      | 2.4, 12.1
#        |               | calculate_top_height()         | or dbh and height vectors   |          |
#        |               | fc_stand_carbon()              | area_ha, species, type      |          |
# -------+---------------+--------------------------------+-----------------------------+----------+--------
#  E     | Full trees    | wcc_process_tally(): DBH class | dbh_class, count            |   6      | 12.2
#        | counted by    | wcc_dbh_class_carbon(): indiv- | optional: height, area_ha,  |          |
#        | tallies       |  idual trees measured          | species/name, type          |          |

# ==============================================================================
# Step-by-step pipeline* from tariff onward (merchtreevol, treevol, woodbiomass, crownbiomass, biomass2c).
# End to end carbon: 
#  fc_agc()/fc_agc_error() for X 
#  fc_stand_carbon() for X
#
# Averaging trees (as required in some WCC methods):
#   1. Calculate carbon for each tree individually (fc_agc, fc_agc_error)
#   2. Sum results for totals: total_sigma = sqrt(sum(sig_AGC^2))
#   3. Calculate means by dividing by number of trees
#   4. For uncertainty, propagate errors (see vignette("uncertainty-and-comparison", package = "TreeCarbon"))
#   Helper functions: wcc_mean_tariff(), wcc_per_hectare(), wcc_stratify()
#
# Carbon conversion: Matthews2 recommended for WCC
#
# ==============================================================================
#
# Example 8 sections: 1 (B/C), 2 (height missing), 3 (D), 4 (insufficient data), 5 (A), 6 (E)
#
# ==============================================================================

library(TreeCarbon)

# ==============================================================================
# SECTION 1: Method B (broadleaves) or Method C (conifers)
# ==============================================================================
# You have: Individual tree DBH and height
# Use: fc_agc(), fc_agc_error(), tariffs(), broadleaf_tariff(), conifer_tariff()
#
# - Method B: Broadleaves require timber height (height to first branch)
# - Method C: Conifers use total height
# - For full carbon: fc_agc() or fc_agc_error() handle everything
# ==============================================================================

cat("\n--- Section 1: Method B/C (individual trees with DBH and height) ---\n")

# Single broadleaf tree
fc_agc(name = "Oak", dbh = 45, height = 18, type = "broadleaf")

# Single conifer tree
fc_agc(name = "Scots Pine", dbh = 30, height = 22, type = "conifer")

# Multiple trees with uncertainty
fc_agc_error(name = c("Oak", "Oak", "Beech"), dbh = c(45, 38, 42),
             height = c(18, 15, 20), type = "broadleaf",
             re_dbh = 0.05, re_h = 0.05)

# With timber height for broadleaves (recommended for WCC certification)
fc_agc(name = "Oak", dbh = 45, height = 18,
       height_timber = 15,  # Measured height to first branch
       type = "broadleaf")


# ==============================================================================
# SECTION 2: Height missing - Estimate or use allometric equations
# ==============================================================================
# You have: Individual tree DBH but NOT height
# Options: Use height–DBH models or collect height data
#
# To get Height-from-DBH relationships:
# - Use species-specific height curves from forestry literature
# - Measure height for a subsample and model
# - Use stand-level approach (Method D) if top height is available
# ==============================================================================

cat("\n--- Section 2: Height missing ---\n")

cat("If you have DBH but not height:\n")
cat("  1. Collect height data for representative trees\n")
cat("  2. Use height-DBH models from forestry sources (external to package)\n")
cat("  3. If you have top height from a stand inventory, use Method D instead\n")

# ==============================================================================
# SECTION 3: Method D - Stand-level from top height
# ==============================================================================
# You have: Top height (mean of 100 largest trees/ha) and sample tree DBH/height
# Use: fc_stand_carbon(), stand_tariff(), calculate_top_height()
#
# fc_stand_carbon() needs sample tree dbh and height to compute top height
# and mean DBH. For pre-calculated top height, use stand_tariff() directly.
# ==============================================================================

cat("\n--- Section 3: Method D (stand-level from top height) ---\n")

# Sample trees from a plot
dbh <- c(45, 52, 38, 60, 42, 55, 48, 35, 58, 40)
height <- c(18, 22, 15, 24, 17, 21, 19, 14, 23, 16)
area_ha <- 0.1  # 0.1 ha plot

# Stand carbon (calculates top height and mean DBH internally)
fc_stand_carbon(name = "Oak", dbh = dbh, height = height, area_ha = area_ha,
                type = "broadleaf", re_dbh = 0.05, re_h = 0.05)

# If you already have top height (e.g. from field measurement)
stand_tariff(spcode = "OK", stand_height = 20, re_h = 0.05)

# ==============================================================================
# SECTION 4: Insufficient data - What to collect
# ==============================================================================
# You have: Neither individual tree DBH nor top height
# Action: Collect inventory data before WCC carbon estimation
#
# Minimum for Method D: Top height (mean of 100 largest trees/ha by DBH)
# Minimum for Method B/C: Individual tree DBH and height
# ==============================================================================

cat("\n--- Section 4: Insufficient data ---\n")

cat("To use WCC methods, you need at least one of:\n")
cat("  - Individual tree DBH + height (Methods B/C/E)\n")
cat("  - Top height + sample for mean DBH (Method D)\n")
cat("  - DBH class tallies + representative heights (Method E)\n")
cat("  - Measured volume from felled trees (Method A)\n")


# ==============================================================================
# SECTION 5: Method A - Fell sample trees
# ==============================================================================
# You have: Measured volume from felled trees and DBH
# Use: tariff_vol_area()
#
# Method A uses Equation 1: tariff from volume and basal area.
# Then use merchtreevol(), treevol(), woodbiomass(), crownbiomass(), etc.
# ==============================================================================

cat("\n--- Section 5: Method A (fell sample trees) ---\n")

# Step 1: Tariff from measured volume (felled tree)
tariff_a <- tariff_vol_area(vol = 0.5, dbh = 24,  # m³, cm from felled tree
                            sig_vol = 0.01, re_dbh = 0.025)

# Step 2: Volume from tariff and DBH
dbh_a <- 24
merc_vol <- merchtreevol(dbh = dbh_a, tariff = tariff_a$tariff,
                         sig_tariff = tariff_a$sigma_tariff, re_dbh = 0.025)
stem_vol <- treevol(mtreevol = merc_vol$volume, dbh = dbh_a, sig_mtreevol = merc_vol$sigma)

# Step 3 onwards: biomass and carbon (see Example 6 for full pipeline)
# woodbiomass() -> crownbiomass() -> biomass2c()


# ==============================================================================
# SECTION 6: Method E - Full tree count (DBH class tallies)
# ==============================================================================
# You have: Counts of trees in each DBH class (and optionally heights)
# Use: wcc_process_tally(), wcc_dbh_class_carbon()
#
# For stands where trees are tallied by DBH class rather than measured
# individually.
# ==============================================================================

cat("\n--- Section 6: Method E (DBH class tallies) ---\n")

# DBH class midpoints and counts
dbh_classes <- c(5, 15, 25, 35, 45)   # cm midpoints
counts <- c(150, 200, 180, 120, 50)   # trees per class
heights <- c(3, 8, 15, 20, 22)        # representative heights (m)

# Process tally data
tally_df <- wcc_process_tally(dbh_class = dbh_classes, count = counts,
                              height = heights, use_midpoints = TRUE)

# Carbon from tally (Method E)
wcc_dbh_class_carbon(name = "Oak", tally_data = tally_df, type = "broadleaf",
                     area_ha = 1.0, re_dbh = 0.05, re_h = 0.05,
                     output.all = TRUE)

