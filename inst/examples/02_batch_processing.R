# ==============================================================================
# Example 2: Batch Processing Multiple Trees
# ==============================================================================
# This example demonstrates processing multiple trees at once, which is
# much more efficient than looping.
#
# This example shows:
# - Vectorized input handling
# - Batch processing with WCC
# - Batch processing with Bunce
# - Rich output for multiple trees
# - Summary statistics
# ==============================================================================

library(TreeCarbon)

# ==============================================================================
# Example 2.1: Create Sample Tree Data
# ==============================================================================

# Create a data frame with multiple trees
trees <- data.frame(
  tree_id = 1:10,
  species = c("Oak", "Beech", "Ash", "Birch", "Oak",
              "Sycamore", "Beech", "Oak", "Ash", "Birch"),
  dbh = c(45, 38, 52, 28, 61, 42, 35, 55, 48, 31),
  height = c(18, 22, 24, 15, 26, 20, 19, 23, 21, 14),
  type = rep("broadleaf", 10),
  stringsAsFactors = FALSE
)

cat("=== Input Data ===\n")
print(trees)
cat("\n")

# ==============================================================================
# Example 2.2: Batch WCC Calculation (Standard Output)
# ==============================================================================

# Process all trees at once - TreeCarbon handles vectors efficiently
results_wcc <- fc_agc(
  name = trees$species,
  dbh = trees$dbh,
  height = trees$height,
  type = trees$type,
  output.all = TRUE
)

cat("=== WCC Batch Results ===\n")
cat(sprintf("Total carbon: %.2f tonnes\n", sum(results_wcc$AGC_WCC_t, na.rm = TRUE)))
cat(sprintf("Mean per tree: %.3f tonnes\n", mean(results_wcc$AGC_WCC_t, na.rm = TRUE)))
cat(sprintf("Range: %.3f - %.3f tonnes\n",
            min(results_wcc$AGC_WCC_t, na.rm = TRUE),
            max(results_wcc$AGC_WCC_t, na.rm = TRUE)))
cat("\n")

# View first few results
cat("First 5 trees:\n")
print(results_wcc[1:5, c("name", "dbh", "height", "AGC_WCC_t")])
cat("\n")

# ==============================================================================
# Example 2.3: Batch Bunce Calculation
# ==============================================================================

# Bunce method for all trees (no height needed)
results_bunce <- Bunce(
  name = trees$species,
  dbh = trees$dbh
)

# Convert to tonnes and add carbon fraction (50%)
results_bunce$biomass_t <- results_bunce$biomass / 1000
results_bunce$carbon_t <- results_bunce$biomass_t * 0.5

cat("=== Bunce Batch Results ===\n")
cat(sprintf("Total carbon: %.2f tonnes\n", sum(results_bunce$carbon_t, na.rm = TRUE)))
cat(sprintf("Mean per tree: %.3f tonnes\n", mean(results_bunce$carbon_t, na.rm = TRUE)))
cat("\n")

# ==============================================================================
# Example 2.4: Batch Processing with Rich Output
# ==============================================================================

# Rich output for multiple trees provides summary and metadata
results_rich <- fc_agc(
  name = trees$species,
  dbh = trees$dbh,
  height = trees$height,
  type = trees$type,
  rich_output = TRUE
)

cat("=== Rich Output (Multiple Trees) ===\n")
print(results_rich)
cat("\n")

# ==============================================================================
# Example 2.5: Summary Statistics by Species
# ==============================================================================

# Combine results with original data
results_wcc$tree_id <- trees$tree_id
results_wcc$species <- trees$species

# Summary by species
species_summary <- aggregate(
  AGC_WCC_t ~ species,
  data = results_wcc,
  FUN = function(x) c(
    Count = length(x),
    Total = sum(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
)

cat("=== Summary by Species ===\n")
print(species_summary)
cat("\n")

# ==============================================================================
# Example 2.6: Export Results
# ==============================================================================

# Create a combined results table
final_results <- data.frame(
  Tree_ID = trees$tree_id,
  Species = trees$species,
  DBH_cm = trees$dbh,
  Height_m = trees$height,
  AGC_WCC_t = results_wcc$AGC_WCC_t,
  AGC_Bunce_t = results_bunce$carbon_t,
  stringsAsFactors = FALSE
)

cat("=== Combined Results Table ===\n")
print(final_results)

# Uncomment to save:
# write.csv(final_results, "tree_carbon_results.csv", row.names = FALSE)
