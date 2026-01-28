# ============================================================================
# TreeCarbon: Getting Started Examples
# ============================================================================
# This script contains all code examples from the getting-started vignette.
# Run this script to test the package functionality without knitting.
#
# Author: TreeCarbon package
# ============================================================================

# Install package
devtools::install_github("izaopenshaw/TreeCarbon", force = TRUE)

# Load the package
library(TreeCarbon)

# ============================================================================
# SINGLE TREE - WCC METHOD (UK)
# ============================================================================
# The Woodland Carbon Code

# Calculate above-ground carbon for an oak tree
result <- fc_agc(name = "Oak",
                 dbh = 45,        # cm
                 height = 18,     # metres
                 type = "broadleaf")

# View result
cat(sprintf("Above-ground Carbon: %.3f tonnes\n", result$AGC_WCC_t))

# ============================================================================
# SINGLE TREE - BUNCE METHOD
# ============================================================================
# For UK deciduous woodland when height isn't available

result <- Bunce(name = "Oak", dbh = 45)
cat(sprintf("Biomass: %.1f kg (%.3f tonnes)\n",
            result$biomass, result$biomass/1000))

# ============================================================================
# MULTIPLE TREES
# ============================================================================
# Process a batch of trees

# Sample data
trees <- data.frame(
  species = c("Oak", "Beech", "Ash", "Birch"),
  dbh = c(45, 38, 52, 28),
  height = c(18, 22, 24, 15),
  type = rep("broadleaf", 4)
)

# Calculate carbon for all trees
results <- fc_agc(
  name = trees$species,
  dbh = trees$dbh,
  height = trees$height,
  type = trees$type
)

# Summary
cat(sprintf("Total carbon: %.2f tonnes\n", sum(results$AGC_WCC_t, na.rm = TRUE)))

# ============================================================================
# COMPARE METHODS
# ============================================================================
# Use allometries() to compare all methods at once
# Note: Requires BIOMASS and allodb packages

if (requireNamespace("BIOMASS", quietly = TRUE) &&
    requireNamespace("allodb", quietly = TRUE)) {

  comparison <- allometries(
    genus = "Quercus",
    species = "robur",
    dbh = 45,
    height = 18,
    coords = c(-0.29, 51.48),  # UK coordinates
    returnv = "AGC"
  )

  # View carbon estimates from each method
  print(comparison[, c("genus", "dbh", "WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")])

} else {
  message("Install BIOMASS and allodb packages to compare all methods")
}

# ============================================================================
# WITH UNCERTAINTY
# ============================================================================
# Add measurement error propagation

result <- fc_agc_error(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  re_dbh = 0.025,   # 2.5% DBH error
  re_h = 0.05       # 5% height error
)

cat(sprintf("Carbon: %.3f ± %.3f tonnes\n",
            result$AGC_WCC_t, result$sig_AGC))

# ============================================================================
# RICH OUTPUT (FULL METADATA)
# ============================================================================
# Get comprehensive metadata including assumptions and citations

result <- fc_agc(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  rich_output = TRUE
)

print(result)

# ============================================================================
# NEXT STEPS
# ============================================================================
# - See TreeCarbon-workflow.R for complete workflows
# - Use ?allometries for multi-method comparison
# - Use ?compare_allometries for detailed method comparison statistics
# - Use ?mc_uncertainty for Monte Carlo uncertainty analysis

# === Getting Started script completed successfully ===
