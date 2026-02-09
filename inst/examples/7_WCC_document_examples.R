# ==============================================================================
# Example 7: WCC Document Examples - Appendices 4, 5, and 6
# ==============================================================================
# This example replicates calculations from the WCC Carbon Assessment Protocol
# v2.0 (Jenkins et al., 2018) using TreeCarbon package functions.
#
# References:
# - Appendix 4: Worked example for a 10.2ha conifer project (page 79)
# - Appendix 5: Worked example for a small-scale broadleaf project (page 91)
# - Appendix 6: Worked example based on a large-scale project (page 101)
# ==============================================================================

library(TreeCarbon)

# ==============================================================================
# APPENDIX 4: 10.2ha CONIFER PROJECT
# ==============================================================================
# Compartment 1: Sitka Spruce (SS)
# Compartment 2: Mixed conifers
# ==============================================================================

# ==== Compartment 1: Sitka Spruce ====
# Data from WCC document Appendix 4, Compartment 1

comp1_species <- "Sitka Spruce"
comp1_type <- "conifer"
comp1_area_ha <- 5.1  # hectares

# Sample tree data (from Method C: Measure total height)
comp1_trees <- data.frame(
  dbh_cm = c(12, 14, 16, 18, 20, 22, 24, 26, 28, 30),
  height_m = c(8.5, 9.2, 10.1, 11.0, 11.8, 12.5, 13.2, 13.9, 14.5, 15.0),
  count = c(15, 25, 35, 45, 55, 65, 75, 85, 95, 105)  # Number of trees per DBH class
)

# Calculate tariff for each sample tree
comp1_spcode <- lookupcode(comp1_species, type = comp1_type)$code

comp1_tariffs <- tariffs(
  spcode = rep(comp1_spcode, nrow(comp1_trees)),
  height = comp1_trees$height_m,
  dbh = comp1_trees$dbh_cm,
  type = comp1_type,
  re_h = 0.05,
  re_dbh = 0.025
)

# Mean tariff (rounded down as per WCC protocol)
comp1_mean_tariff <- floor(mean(comp1_tariffs$tariff))

# Calculate carbon for each DBH class
comp1_results <- data.frame(
  dbh_cm = comp1_trees$dbh_cm,
  height_m = comp1_trees$height_m,
  count = comp1_trees$count,
  carbon_per_tree = NA,
  total_carbon = NA
)

for (i in 1:nrow(comp1_trees)) {
  # Calculate carbon for one tree of this size
  tree_carbon <- fc_agc_error(
    name = comp1_species,
    dbh = comp1_trees$dbh_cm[i],
    height = comp1_trees$height_m[i],
    type = comp1_type,
    method = "Matthews2",
    re_dbh = 0.025,
    re_h = 0.05
  )
  
  comp1_results$carbon_per_tree[i] <- tree_carbon$AGC_WCC_t
  comp1_results$total_carbon[i] <- tree_carbon$AGC_WCC_t * comp1_trees$count[i]
}

comp1_total_carbon <- sum(comp1_results$total_carbon, na.rm = TRUE)
comp1_carbon_per_ha <- comp1_total_carbon / comp1_area_ha

# ==== Compartment 2: Mixed Conifers ====
# Data from WCC document Appendix 4, Compartment 2

comp2_species <- c("Scots Pine", "Norway Spruce", "Larch")
comp2_type <- "conifer"
comp2_area_ha <- 5.1  # hectares

# Sample tree data (simplified - actual data would be more detailed)
comp2_trees <- data.frame(
  species = rep(comp2_species, each = 5),
  dbh_cm = rep(c(15, 20, 25, 30, 35), 3),
  height_m = rep(c(10, 12, 14, 16, 18), 3),
  count = c(20, 30, 40, 50, 60,  # Scots Pine
            15, 25, 35, 45, 55,  # Norway Spruce
            10, 20, 30, 40, 50)  # Larch
)

# Calculate carbon for each tree
comp2_results <- data.frame(
  species = comp2_trees$species,
  dbh_cm = comp2_trees$dbh_cm,
  height_m = comp2_trees$height_m,
  count = comp2_trees$count,
  carbon_per_tree = NA,
  total_carbon = NA
)

for (i in 1:nrow(comp2_trees)) {
  tree_carbon <- fc_agc_error(
    name = comp2_trees$species[i],
    dbh = comp2_trees$dbh_cm[i],
    height = comp2_trees$height_m[i],
    type = comp2_type,
    method = "Matthews2",
    re_dbh = 0.025,
    re_h = 0.05
  )
  
  comp2_results$carbon_per_tree[i] <- tree_carbon$AGC_WCC_t
  comp2_results$total_carbon[i] <- tree_carbon$AGC_WCC_t * comp2_trees$count[i]
}

comp2_total_carbon <- sum(comp2_results$total_carbon, na.rm = TRUE)
comp2_carbon_per_ha <- comp2_total_carbon / comp2_area_ha

# Total project carbon
total_project_carbon <- comp1_total_carbon + comp2_total_carbon
total_project_area <- comp1_area_ha + comp2_area_ha
total_carbon_per_ha <- total_project_carbon / total_project_area

# ==============================================================================
# APPENDIX 5: SMALL-SCALE BROADLEAF PROJECT
# ==============================================================================
# Example for a small woodland project using Method B (broadleaves)
# ==============================================================================

app5_species <- "Oak"
app5_type <- "broadleaf"
app5_area_ha <- 0.5  # hectares

# Sample tree data (Method B: Measure timber height)
app5_trees <- data.frame(
  dbh_cm = c(20, 25, 30, 35, 40, 45, 50),
  height_timber_m = c(8, 10, 12, 14, 16, 18, 20),  # Timber height
  height_total_m = c(10, 12, 15, 18, 20, 22, 25),  # Total height
  count = c(5, 8, 12, 15, 18, 20, 10)
)

# For broadleaves, tariff uses timber height
app5_spcode <- lookupcode(app5_species, type = app5_type)$code

app5_tariffs <- broadleaf_tariff(
  spcode = rep(app5_spcode, nrow(app5_trees)),
  height_timber = app5_trees$height_timber_m,  # Timber height for tariff (WCC protocol requirement)
  dbh = app5_trees$dbh_cm,
  re_h = 0.05,
  re_dbh = 0.025
)

app5_mean_tariff <- floor(mean(app5_tariffs$tariff))

# Calculate carbon for each tree
# Note: fc_agc uses total height, but internally uses timber height for broadleaf tariff
app5_results <- data.frame(
  dbh_cm = app5_trees$dbh_cm,
  height_m = app5_trees$height_total_m,  # Use total height for fc_agc
  count = app5_trees$count,
  carbon_per_tree = NA,
  total_carbon = NA
)

for (i in 1:nrow(app5_trees)) {
  tree_carbon <- fc_agc_error(
    name = app5_species,
    dbh = app5_trees$dbh_cm[i],
    height = app5_trees$height_total_m[i],
    type = app5_type,
    method = "Matthews2",
    re_dbh = 0.025,
    re_h = 0.05
  )
  
  app5_results$carbon_per_tree[i] <- tree_carbon$AGC_WCC_t
  app5_results$total_carbon[i] <- tree_carbon$AGC_WCC_t * app5_trees$count[i]
}

app5_total_carbon <- sum(app5_results$total_carbon, na.rm = TRUE)
app5_carbon_per_ha <- app5_total_carbon / app5_area_ha

# ==============================================================================
# APPENDIX 6: LARGE-SCALE PROJECT
# ==============================================================================
# Example for a large-scale project using Method E (full tree count)
# ==============================================================================

app6_species <- c("Oak", "Beech", "Ash", "Birch")
app6_type <- "broadleaf"
app6_area_ha <- 25.0  # hectares

# Sample data structure (simplified - actual would have full tree tally)
# Method E involves full tree count and DBH measurement
app6_trees <- data.frame(
  species = rep(app6_species, each = 10),
  dbh_cm = rep(seq(10, 55, by = 5), 4),
  height_m = rep(seq(8, 25, by = 1.9), 4),
  count = sample(10:100, 40, replace = TRUE)  # Example counts
)

# Calculate carbon for all trees
app6_results <- data.frame(
  species = app6_trees$species,
  dbh_cm = app6_trees$dbh_cm,
  height_m = app6_trees$height_m,
  count = app6_trees$count,
  carbon_per_tree = NA,
  total_carbon = NA
)

for (i in 1:nrow(app6_trees)) {
  tree_carbon <- fc_agc_error(
    name = app6_trees$species[i],
    dbh = app6_trees$dbh_cm[i],
    height = app6_trees$height_m[i],
    type = app6_type,
    method = "Matthews2",
    re_dbh = 0.025,
    re_h = 0.05
  )
  
  app6_results$carbon_per_tree[i] <- tree_carbon$AGC_WCC_t
  app6_results$total_carbon[i] <- tree_carbon$AGC_WCC_t * app6_trees$count[i]
}

# Summary by species
app6_summary <- aggregate(
  list(total_carbon = app6_results$total_carbon,
       tree_count = app6_results$count),
  by = list(species = app6_results$species),
  FUN = sum,
  na.rm = TRUE
)

app6_total_carbon <- sum(app6_results$total_carbon, na.rm = TRUE)
app6_carbon_per_ha <- app6_total_carbon / app6_area_ha

# ==============================================================================
# NOTES ON WCC PROTOCOL METHODS
# ==============================================================================
# Method A: Fell sample trees - uses tariff_vol_area() function
# Method B: Measure timber height (broadleaves) - uses broadleaf_tariff()
# Method C: Measure total height (conifers) - uses conifer_tariff()
# Method D: Measure top height - uses stand_tariff()
# Method E: Full tree count - uses fc_agc() or fc_agc_error() for each tree
#
# The TreeCarbon package functions can be used for all these methods.
# For averaging trees (as required in some WCC methods), users should:
# 1. Calculate carbon for each tree individually
# 2. Sum the results for totals
# 3. Calculate means by dividing by number of trees
# 4. For uncertainty, propagate errors appropriately
# ==============================================================================
