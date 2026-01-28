# ============================================================================
# TreeCarbon: A Complete Workflow Guide
# ============================================================================
# This script contains all code examples from the TreeCarbon-workflow vignette.
# Run this script to test the package functionality without knitting.
#
# Author: Isabel Openshaw & Justin Moat
# ============================================================================

# Install package
devtools::install_github("izaopenshaw/TreeCarbon", force = TRUE)

# Load the package
library(TreeCarbon)
library(ggplot2)

# ============================================================================
# PART 1: SINGLE TREE CALCULATIONS
# ============================================================================

# ----------------------------------------------------------------------------
# 1.1 Using the Woodland Carbon Code (WCC)
# ----------------------------------------------------------------------------
# The WCC method is the standard for UK woodland carbon accounting.
# It requires species, DBH, and height.

# A single oak tree
result_wcc <- fc_agc(
  name = "Oak",
  dbh = 45,        # cm
  height = 18,     # m
  type = "broadleaf"
)

print(result_wcc)

# With Full Metadata (Rich Output)
# Use rich_output = TRUE to get comprehensive metadata
result_rich <- fc_agc(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  rich_output = TRUE
)

print(result_rich)

# With Uncertainty Propagation
result_error <- fc_agc_error(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  re_dbh = 0.025,   # 2.5% relative measurement error
  re_h = 0.05       # 5% height measurement error
)

print(result_error[, c("name", "AGC_WCC_t", "sig_AGC")])

# Print 95% confidence interval
agc <- result_error$AGC_WCC_t
sigma <- result_error$sig_AGC
cat(sprintf("\nAGC: %.3f ± %.3f t (95%% CI: [%.3f, %.3f])\n",
            agc, 1.96 * sigma, agc - 1.96*sigma, agc + 1.96*sigma))

# ----------------------------------------------------------------------------
# 1.2 Using the Bunce Equation (UK Deciduous)
# ----------------------------------------------------------------------------
# The Bunce (1968) method only requires DBH and species name - no height needed.

result_bunce <- Bunce(
  name = "Oak",
  dbh = 45,
  re_dbh = 0.025  # Include measurement error
)

print(result_bunce)

# With rich metadata
result_bunce_rich <- Bunce("Oak", dbh = 45, re_dbh = 0.025, rich_output = TRUE)
print(result_bunce_rich)

# ----------------------------------------------------------------------------
# 1.3 Using BIOMASS
# ----------------------------------------------------------------------------
# The BIOMASS package uses Chave et al. pantropical equations.
# The package is used worldwide and requires coordinate inputs.

if (requireNamespace("BIOMASS", quietly = TRUE)) {
  # UK coordinates (Kew Gardens)
  coords <- c(-0.2915, 51.4787)

  result_biomass <- BIOMASS(
    dbh = 45,
    height = 18,
    genus = "Quercus",
    species = "robur",
    coords = coords
  )

  print(result_biomass[, c("genus", "species", "dbh", "height",
                           "Wood_Density", "AGB_Biomass_kg")])

  # Convert to tonnes for comparison
  cat(sprintf("\nAGB: %.3f kg (%.4f t)\n",
              result_biomass$AGB_Biomass_kg,
              result_biomass$AGB_Biomass_kg / 1000))

  # BIOMASS with Monte Carlo Uncertainty
  result_biomass_mc <- BIOMASS(
    dbh = 45,
    height = 18,
    genus = "Quercus",
    species = "robur",
    coords = coords,
    uncertainty = TRUE,
    n_mc = 1000
  )

  print(result_biomass_mc[, c("AGB_Biomass_kg", "AGB_sd_kg", "AGB_CV_pct",
                               "AGB_CI_low_kg", "AGB_CI_high_kg")])
} else {
  message("Install BIOMASS package for pantropical allometries")
}

# ----------------------------------------------------------------------------
# 1.4 Using allodb (Global Extratropical)
# ----------------------------------------------------------------------------

if (requireNamespace("allodb", quietly = TRUE)) {
  result_allodb <- allodb(
    dbh = 45,
    genus = "Quercus",
    species = "robur",
    coords = c(-77.0, 38.9)  # Uses North American equations by default
  )

  print(result_allodb)
} else {
  message("Install allodb package for extratropical allometries")
}


# ============================================================================
# PART 2: BATCH PROCESSING MULTIPLE TREES
# ============================================================================

# ----------------------------------------------------------------------------
# 2.1 Creating Sample Data
# ----------------------------------------------------------------------------

# Create a sample dataset of trees
trees <- data.frame(
  tree_id = 1:10,
  species = c("Oak", "Beech", "Ash", "Birch", "Oak",
              "Sycamore", "Beech", "Oak", "Ash", "Birch"),
  genus = c("Quercus", "Fagus", "Fraxinus", "Betula", "Quercus",
            "Acer", "Fagus", "Quercus", "Fraxinus", "Betula"),
  species_epithet = c("robur", "sylvatica", "excelsior", "pendula", "robur",
                      "pseudoplatanus", "sylvatica", "robur", "excelsior", "pendula"),
  dbh = c(45, 38, 52, 28, 61, 42, 35, 55, 48, 31),
  height = c(18, 22, 24, 15, 26, 20, 19, 23, 21, 14),
  type = rep("broadleaf", 10),
  stringsAsFactors = FALSE
)

print(trees)

# ----------------------------------------------------------------------------
# 2.2 WCC Method for Multiple Trees
# ----------------------------------------------------------------------------

# Apply WCC to all trees
wcc_results <- fc_agc(
  name = trees$species,
  dbh = trees$dbh,
  height = trees$height,
  type = trees$type
)

# Add tree IDs
wcc_results$tree_id <- trees$tree_id

# WCC Results Summary
wcc_summary <- data.frame(
  Metric = c("Total AGC", "Mean AGC per tree", "Min AGC", "Max AGC"),
  Value = c(
    sprintf("%.2f tonnes", sum(wcc_results$AGC_WCC_t, na.rm = TRUE)),
    sprintf("%.3f tonnes", mean(wcc_results$AGC_WCC_t, na.rm = TRUE)),
    sprintf("%.3f tonnes", min(wcc_results$AGC_WCC_t, na.rm = TRUE)),
    sprintf("%.3f tonnes", max(wcc_results$AGC_WCC_t, na.rm = TRUE))
  )
)
print(wcc_summary)

# ----------------------------------------------------------------------------
# 2.3 Bunce Method for Multiple Trees
# ----------------------------------------------------------------------------

bunce_results <- Bunce(
  name = trees$species,
  dbh = trees$dbh,
  re_dbh = 0.025
)

bunce_results$tree_id <- trees$tree_id

# Convert to tonnes (Bunce returns kg)
bunce_results$biomass_t <- bunce_results$biomass / 1000
bunce_results$carbon_t <- bunce_results$biomass_t * 0.5  # 50% carbon fraction

# Bunce Results Summary
print(sprintf("Total Carbon: %.2f tonnes", sum(bunce_results$carbon_t, na.rm = TRUE)))

# ----------------------------------------------------------------------------
# 2.4 BIOMASS Method for Multiple Trees
# ----------------------------------------------------------------------------

if (requireNamespace("BIOMASS", quietly = TRUE)) {
  coords <- c(-0.2915, 51.4787)  # Kew Gardens

  biomass_results <- BIOMASS(
    dbh = trees$dbh,
    height = trees$height,
    genus = trees$genus,
    species = trees$species_epithet,
    coords = coords,
    uncertainty = TRUE,
    n_mc = 500
  )

  biomass_results$tree_id <- trees$tree_id

  # Convert to tonnes
  biomass_results$AGB_t <- biomass_results$AGB_Biomass_kg / 1000
  biomass_results$carbon_t <- biomass_results$AGB_t * 0.47  # IPCC carbon fraction

  # BIOMASS Results Summary
  biomass_summary <- data.frame(
    Metric = c("Total Carbon", "Mean CV"),
    Value = c(
      sprintf("%.2f tonnes", sum(biomass_results$carbon_t, na.rm = TRUE)),
      sprintf("%.1f%%", mean(biomass_results$AGB_CV_pct, na.rm = TRUE))
    )
  )
  print(biomass_summary)
} else {
  message("Install BIOMASS package for batch processing with BIOMASS")
}


# ============================================================================
# PART 3: COMPARING ALLOMETRIC METHODS
# ============================================================================

# ----------------------------------------------------------------------------
# 3.1 Single Tree Comparison
# ----------------------------------------------------------------------------

# Compare all methods for a single oak tree
comparison <- allometries(
  genus = "Quercus",
  species = "robur",
  dbh = 45,
  height = 18,
  coords = c(-0.2915, 51.4787),
  returnv = "AGC"
)

print(comparison)

# ----------------------------------------------------------------------------
# 3.2 Using compare_allometries()
# ----------------------------------------------------------------------------
# The compare_allometries() function provides detailed statistics

# Get allometries results first
results <- allometries(
  genus = "Quercus",
  species = "robur",
  dbh = 45,
  height = 18,
  coords = c(-0.2915, 51.4787),
  returnv = "AGC"
)

# Run comparison
comp <- compare_allometries(results, reference = "WCC")

# View the comparison table
print(comp)

# Summary statistics
summary(comp)

# ----------------------------------------------------------------------------
# 3.3 Batch Comparison
# ----------------------------------------------------------------------------

# Compare methods across all trees - allometries() handles vectors directly
batch_results <- allometries(
  genus = trees$genus,
  species = trees$species_epithet,
  dbh = trees$dbh,
  height = trees$height,
  coords = c(-0.2915, 51.4787),
  returnv = "AGC"
)

# Add tree IDs
batch_results$tree_id <- seq_len(nrow(batch_results))

# Batch Results (first 6 rows)
print(head(batch_results))

# ----------------------------------------------------------------------------
# 3.4 Visualizing Method Differences
# ----------------------------------------------------------------------------

# Extract carbon columns for comparison
if (exists("batch_results") && nrow(batch_results) > 0) {

  # Reshape for plotting
  plot_data <- data.frame(
    tree_id = rep(batch_results$tree_id, 4),
    dbh = rep(batch_results$dbh, 4),
    method = rep(c("WCC", "BIOMASS", "allodb", "Bunce"), each = nrow(batch_results)),
    carbon = c(
      batch_results$WCC_C_t,
      batch_results$biomass_C_t,
      batch_results$allodb_C_t,
      batch_results$Bunce_C_t
    )
  )

  # Remove NA values
  plot_data <- plot_data[!is.na(plot_data$carbon), ]

  # Plot
  p <- ggplot(plot_data, aes(x = dbh, y = carbon, color = method, shape = method)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
    labs(
      title = "Carbon Estimates by Method vs DBH",
      x = "DBH (cm)",
      y = "Above-ground Carbon (tonnes)",
      color = "Method",
      shape = "Method"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set2")

  print(p)
}


# ============================================================================
# PART 4: QUANTIFYING DIFFERENCES BETWEEN METHODS
# ============================================================================

# ----------------------------------------------------------------------------
# 4.1 Method Deviation Statistics
# ----------------------------------------------------------------------------

if (exists("batch_results") && nrow(batch_results) > 0) {

  # Calculate statistics
  methods <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")
  method_names <- c("WCC", "BIOMASS", "allodb", "Bunce")

  stats_table <- data.frame(
    Method = method_names,
    Mean = sapply(methods, function(m) mean(batch_results[[m]], na.rm = TRUE)),
    SD = sapply(methods, function(m) sd(batch_results[[m]], na.rm = TRUE)),
    Min = sapply(methods, function(m) min(batch_results[[m]], na.rm = TRUE)),
    Max = sapply(methods, function(m) max(batch_results[[m]], na.rm = TRUE)),
    N_valid = sapply(methods, function(m) sum(!is.na(batch_results[[m]])))
  )

  # Add CV
  stats_table$CV_pct <- 100 * stats_table$SD / stats_table$Mean

  print(stats_table)
}

# ----------------------------------------------------------------------------
# 4.2 Pairwise Method Differences
# ----------------------------------------------------------------------------

if (exists("batch_results") && nrow(batch_results) > 0) {

  # Calculate pairwise differences (as % of mean)
  wcc <- batch_results$WCC_C_t
  biomass <- batch_results$biomass_C_t
  allodb <- batch_results$allodb_C_t
  bunce <- batch_results$Bunce_C_t

  # Mean absolute percentage difference
  mean_apd <- function(x, y) {
    valid <- !is.na(x) & !is.na(y) & (x + y) > 0
    if (sum(valid) == 0) return(NA)
    mean(abs(x[valid] - y[valid]) / ((x[valid] + y[valid]) / 2) * 100)
  }

  pairwise <- data.frame(
    Comparison = c("WCC vs BIOMASS", "WCC vs allodb", "WCC vs Bunce",
                   "BIOMASS vs allodb", "BIOMASS vs Bunce", "allodb vs Bunce"),
    Mean_Abs_Pct_Diff = c(
      mean_apd(wcc, biomass),
      mean_apd(wcc, allodb),
      mean_apd(wcc, bunce),
      mean_apd(biomass, allodb),
      mean_apd(biomass, bunce),
      mean_apd(allodb, bunce)
    )
  )

  pairwise <- pairwise[order(-pairwise$Mean_Abs_Pct_Diff), ]
  print(pairwise)
}

# ----------------------------------------------------------------------------
# 4.3 Method Agreement Assessment
# ----------------------------------------------------------------------------

if (exists("batch_results") && nrow(batch_results) > 0) {

  # Calculate CV across methods for each tree
  methods <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")

  batch_results$cross_method_mean <- apply(batch_results[, methods], 1, mean, na.rm = TRUE)
  batch_results$cross_method_sd <- apply(batch_results[, methods], 1, sd, na.rm = TRUE)
  batch_results$cross_method_cv <- 100 * batch_results$cross_method_sd / batch_results$cross_method_mean

  # Cross-Method Agreement
  agreement_summary <- data.frame(
    Metric = c("Mean CV across methods", "Min CV", "Max CV"),
    Value = c(
      sprintf("%.1f%%", mean(batch_results$cross_method_cv, na.rm = TRUE)),
      sprintf("%.1f%%", min(batch_results$cross_method_cv, na.rm = TRUE)),
      sprintf("%.1f%%", max(batch_results$cross_method_cv, na.rm = TRUE))
    )
  )
  print(agreement_summary)

  # Trees with high disagreement
  high_cv <- batch_results[batch_results$cross_method_cv > 30, c("tree_id", "genus", "dbh", "cross_method_cv")]
  if (nrow(high_cv) > 0) {
    # Trees with high method disagreement (CV > 30%)
    print(high_cv)
  }
  # Note: If no trees have CV > 30%, all show reasonable agreement
}


# ============================================================================
# PART 5: SENSITIVITY ANALYSIS
# ============================================================================
# "How Sensitive is Carbon to Method Choice?"

# ----------------------------------------------------------------------------
# 5.1 Single Tree Sensitivity
# ----------------------------------------------------------------------------

# Single tree data
single_tree <- data.frame(
  dbh = 45,
  height = 18,
  genus = "Quercus",
  species = "robur",
  name = "Oak"
)

# Run sensitivity analysis
sens_single <- sensitivity_analysis(
  single_tree,
  coords = c(-0.29, 51.48),  # UK coordinates
  type = "broadleaf",
  reference = "WCC"
)

# View results
print(sens_single)

# ----------------------------------------------------------------------------
# 5.2 Batch Sensitivity Analysis
# ----------------------------------------------------------------------------

# Create sample dataset
forest_plot <- data.frame(
  tree_id = 1:15,
  name = c("Oak", "Beech", "Ash", "Birch", "Oak",
           "Sycamore", "Beech", "Oak", "Ash", "Birch",
           "Oak", "Beech", "Ash", "Oak", "Birch"),
  genus = c("Quercus", "Fagus", "Fraxinus", "Betula", "Quercus",
            "Acer", "Fagus", "Quercus", "Fraxinus", "Betula",
            "Quercus", "Fagus", "Fraxinus", "Quercus", "Betula"),
  species = c("robur", "sylvatica", "excelsior", "pendula", "robur",
              "pseudoplatanus", "sylvatica", "robur", "excelsior", "pendula",
              "robur", "sylvatica", "excelsior", "robur", "pendula"),
  dbh = c(45, 38, 52, 28, 61, 42, 35, 55, 48, 31, 40, 33, 47, 58, 26),
  height = c(18, 22, 24, 15, 26, 20, 19, 23, 21, 14, 17, 18, 22, 25, 13),
  type = rep("broadleaf", 15)
)

# Run sensitivity analysis on the whole plot
sens_plot <- sensitivity_analysis(
  forest_plot,
  coords = c(-0.29, 51.48),
  type = "broadleaf",
  reference = "WCC"
)

print(sens_plot)

# ----------------------------------------------------------------------------
# 5.3 Visualizing Sensitivity
# ----------------------------------------------------------------------------

# Bar chart of method totals
plot(sens_plot, type = "comparison")

# Deviation from reference method
plot(sens_plot, type = "deviation")

# ----------------------------------------------------------------------------
# 5.4 Per-Tree Sensitivity
# ----------------------------------------------------------------------------

# Per-tree sensitivity
sens_trees <- sensitivity_analysis(
  forest_plot,
  coords = c(-0.29, 51.48),
  type = "broadleaf",
  aggregate = FALSE
)

# View per-tree results
tree_sens <- sens_trees$by_tree[, c("tree_id", "mean_estimate", "cv_pct",
                                     "spread_pct", "range_ratio")]
tree_sens <- tree_sens[order(-tree_sens$cv_pct), ]

# Trees ranked by method sensitivity (highest first)
print(head(tree_sens, 10))

# ----------------------------------------------------------------------------
# 5.5 Interpreting Sensitivity Results
# ----------------------------------------------------------------------------

# Sensitivity levels:
# | CV (%) | Level      | Interpretation                           |
# |--------|------------|------------------------------------------|
# | < 10   | LOW        | Method choice has minimal impact         |
# | 10-25  | MODERATE   | Typical variation; report range          |
# | 25-50  | HIGH       | Significant impact; justify method choice|
# | > 50   | VERY HIGH  | Investigate data quality/applicability   |

# ----------------------------------------------------------------------------
# 5.6 What Drives High Sensitivity?
# ----------------------------------------------------------------------------

# Check which trees have highest disagreement
if (exists("sens_trees")) {
  high_sens <- sens_trees$by_tree[sens_trees$by_tree$cv_pct > 20, ]

  if (nrow(high_sens) > 0) {
    # Trees with CV > 20%
    high_sens_full <- merge(high_sens[, c("tree_id", "cv_pct", "spread_pct")],
                            forest_plot, by.x = "tree_id", by.y = "tree_id")
    print(high_sens_full[, c("tree_id", "name", "dbh", "height", "cv_pct")])
  }
}


# ============================================================================
# PART 6: UNCERTAINTY ANALYSIS
# ============================================================================

# ----------------------------------------------------------------------------
# 6.1 Using the Monte Carlo Framework
# ----------------------------------------------------------------------------

# Define the Bunce allometric function
bunce_fn <- function(dbh, a, b) {
  exp(a + b * log(pi * dbh))
}

# Define inputs with uncertainties
inputs <- list(
  dbh = list(mean = 45, sd = 1.13, source = "measurement"),  # 2.5% error
  a = list(mean = 1.868, sd = 0.047, source = "parameter"),
  b = list(mean = 2.268, sd = 0.057, source = "parameter")
)

# Run Monte Carlo
mc_result <- mc_uncertainty(bunce_fn, inputs, N = 2000, decompose = TRUE)

# View results
print(mc_result)
summary(mc_result)

# ----------------------------------------------------------------------------
# 6.2 Variance Decomposition
# ----------------------------------------------------------------------------

if (!is.null(mc_result$decomposition)) {
  by_source <- mc_result$decomposition$by_source

  p <- ggplot(by_source, aes(x = reorder(source, contribution_pct),
                       y = contribution_pct, fill = source)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Variance Decomposition by Uncertainty Source",
      x = "Source",
      y = "Contribution (%)"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2")

  print(p)
}

# ----------------------------------------------------------------------------
# 6.3 Method Choice Uncertainty
# ----------------------------------------------------------------------------

# Estimates from different methods for the same tree
estimates <- c(
  WCC = 0.85,
  BIOMASS = 0.78,
  allodb = 0.92,
  Bunce = 0.72
)

method_unc <- method_choice_uncertainty(estimates)

# Method Choice Uncertainty
method_unc_summary <- data.frame(
  Metric = c("Consensus estimate", "Spread", "SD across methods"),
  Value = c(
    sprintf("%.3f t", method_unc$consensus),
    sprintf("%.3f t (%.1f%%)", method_unc$spread, method_unc$spread_pct),
    sprintf("%.3f t (CV = %.1f%%)", method_unc$sd, method_unc$cv)
  )
)
print(method_unc_summary)

# Individual method differences
print(method_unc$individual)


# ----------------------------------------------------------------------------
# 6.4 Bootstrap Sensitivity Analysis (Option B)
# ----------------------------------------------------------------------------

# This analysis asks: "How robust is my sensitivity conclusion to measurement error?"
# It propagates DBH/height measurement uncertainty through all methods via bootstrapping.

# Bootstrap Sensitivity Analysis (Option B)

# Create sample tree data
boot_tree_data <- data.frame(
  dbh = c(30, 45, 60, 25, 55),
  height = c(15, 20, 25, 12, 22),
  genus = c("Quercus", "Fagus", "Fraxinus", "Betula", "Acer"),
  species = c("robur", "sylvatica", "excelsior", "pendula", "pseudoplatanus")
)

# Input tree data
print(boot_tree_data)

# Run bootstrap sensitivity analysis
# Note: Using n_boot = 50 for speed in this example. Use 500+ for real analysis.

boot_result <- bootstrap_sensitivity(
  data = boot_tree_data,
  methods = c("WCC", "BIOMASS", "allodb", "Bunce"),
  coords = c(-0.29, 51.48),  # London area
  type = "broadleaf",
  n_boot = 50,               # Use 500+ for publication-quality analysis
  dbh_cv = 0.02,             # 2% DBH measurement error
  height_cv = 0.05,          # 5% height measurement error
  method = "IPCC2",
  seed = 42,                 # For reproducibility
  verbose = TRUE
)

# View the full results
print(boot_result)

# Key Bootstrap Results
boot_summary <- data.frame(
  Metric = c("CV Point Estimate", "CV 95% CI", "CV Standard Error", "Range Ratio", "Range Ratio 95% CI"),
  Value = c(
    sprintf("%.1f%%", boot_result$cv_observed),
    sprintf("[%.1f%%, %.1f%%]", boot_result$cv_ci[1], boot_result$cv_ci[2]),
    sprintf("%.2f%%", boot_result$cv_se),
    sprintf("%.2fx", boot_result$range_ratio_observed),
    sprintf("[%.2fx - %.2fx]", boot_result$range_ratio_ci[1], boot_result$range_ratio_ci[2])
  )
)
print(boot_summary)

# Interpretation
# ci_width < 10: ROBUST to measurement error (narrow 95% CI)
# ci_width 10-25: MODERATELY STABLE (some uncertainty)
# ci_width > 25: UNCERTAIN (measurement error affects conclusion)

# Plot the bootstrap distribution (if running interactively)
if (interactive()) {
  plot(boot_result, type = "cv")
}

# Comparison: Option A vs Option B
# Option A (Per-tree CV distribution):
#   - Shows which trees are most sensitive to method choice
#   - Fast computation (uses existing results)
#   - Best for: Exploration, identifying outlier trees
#
# Option B (Bootstrap sensitivity):
#   - Shows confidence intervals on overall sensitivity metrics
#   - Computationally intensive (re-runs all methods many times)
#   - Best for: Publications, rigorous uncertainty quantification


# ============================================================================
# PART 7: COMPLETE WORKFLOW EXAMPLE
# ============================================================================

# === Step 1: Load/prepare your data ===
my_trees <- data.frame(
  id = 1:5,
  common_name = c("Oak", "Beech", "Scots Pine", "Ash", "Birch"),
  genus = c("Quercus", "Fagus", "Pinus", "Fraxinus", "Betula"),
  species = c("robur", "sylvatica", "sylvestris", "excelsior", "pendula"),
  dbh_cm = c(45, 38, 42, 52, 28),
  height_m = c(18, 22, 20, 24, 15),
  type = c("broadleaf", "broadleaf", "conifer", "broadleaf", "broadleaf")
)

# Input Data
print(my_trees)

# === Step 2: Calculate carbon using preferred method ===

wcc_out <- fc_agc_error(
  name = my_trees$common_name,
  dbh = my_trees$dbh_cm,
  height = my_trees$height_m,
  type = my_trees$type,
  re_dbh = 0.025,
  re_h = 0.05
)

results <- data.frame(
  Tree = my_trees$common_name,
  DBH = my_trees$dbh_cm,
  Height = my_trees$height_m,
  AGC_t = round(wcc_out$AGC_WCC_t, 3),
  SD_t = round(wcc_out$sig_AGC, 3)
)
results$CV_pct <- round(100 * results$SD_t / results$AGC_t, 1)

print(results)

# === Step 3: Summary statistics ===
final_summary <- data.frame(
  Metric = c("Total AGC", "Total uncertainty", "Mean per-tree CV"),
  Value = c(
    sprintf("%.2f tonnes", sum(results$AGC_t, na.rm = TRUE)),
    sprintf("± %.2f tonnes", sqrt(sum(results$SD_t^2, na.rm = TRUE))),
    sprintf("%.1f%%", mean(results$CV_pct, na.rm = TRUE))
  )
)
print(final_summary)

# === Step 4: Convert to CO2 equivalent ===
results$CO2e_t <- results$AGC_t * (44/12)
print(sprintf("Total CO2 equivalent: %.2f tonnes", sum(results$CO2e_t, na.rm = TRUE)))


# ============================================================================
# SESSION INFO
# ============================================================================

sessionInfo()

# === Workflow script completed successfully ===
