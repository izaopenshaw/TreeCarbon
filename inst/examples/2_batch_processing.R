# ==============================================================================
# Example 2: Batch Processing Multiple Trees
# ==============================================================================
# This example shows:
# - Vectorized input handling
# - Batch processing with WCC
# - Batch processing with Bunce
# - Rich output for multiple trees
# - Summary statistics
# ==============================================================================

library(TreeCarbon)

# ==== Create Sample Tree Data =================================================

# Create a data frame with multiple trees
trees <- data.frame(tree_id = 1:10,
  species = c("Oak", "Beech", "Ash", "Birch", "Oak", "Sycamore", "Beech", "Oak", "Ash", "Birch"),
  dbh = c(45, 38, 52, 28, 61, 42, 35, 55, 48, 31),
  height = c(18, 22, 24, 15, 26, 20, 19, 23, 21, 14),
  type = rep("broadleaf", 10), stringsAsFactors = FALSE
)

# ==== Batch WCC Calculation ================================================
# Process all trees at once
results_wcc <- fc_agc(name = trees$species,    dbh = trees$dbh,
                      height = trees$height,   type = trees$type,
                      output.all = TRUE)

cat(sprintf("Total carbon: %.2f tonnes\n", sum(results_wcc$AGC_WCC_t, na.rm = TRUE)))
cat(sprintf("Mean per tree: %.3f tonnes\n", mean(results_wcc$AGC_WCC_t, na.rm = TRUE)))
cat(sprintf("Range: %.3f - %.3f tonnes\n",
            min(results_wcc$AGC_WCC_t, na.rm = TRUE),
            max(results_wcc$AGC_WCC_t, na.rm = TRUE)))

print(results_wcc[1:5, c("name", "dbh", "height", "AGC_WCC_t")])

# ==== Batch Bunce Calculation =================================================

# Bunce method for all trees (no height needed)
results_bunce <- Bunce(name = trees$species,   dbh = trees$dbh)

# Convert biomass (kg) to tonnes
results_bunce$biomass_t <- results_bunce$biomass / 1000

# Convert biomass to carbon using biomass2c function
carbon_bunce <- biomass2c(biomass = results_bunce$biomass_t,
                          method = "Matthews1",
                          type = trees$type)

# biomass2c returns a vector when sig_biomass is NULL
results_bunce$carbon_t <- carbon_bunce

cat(sprintf("Total carbon: %.2f tonnes\n", sum(results_bunce$carbon_t, na.rm = TRUE)))
cat(sprintf("Mean per tree: %.3f tonnes\n", mean(results_bunce$carbon_t, na.rm = TRUE)))

# ==== WCC with Error and Rich Output =======================================

# Calculate carbon with error propagation
results_wcc_error <- fc_agc_error(name = trees$species,
                                  dbh = trees$dbh,
                                  height = trees$height,
                                  type = trees$type,
                                  re_dbh = 0.025,  # 2.5% DBH measurement error
                                  re_h = 0.05,     # 5% height measurement error
                                  re = 0.025)      # 2.5% coefficient error

# Rich output for multiple trees provides summary and metadata
results_wcc_rich <- fc_agc_error(name = trees$species,
                                 dbh = trees$dbh,
                                 height = trees$height,
                                 type = trees$type,
                                 re_dbh = 0.025,
                                 re_h = 0.05,
                                 re = 0.025,
                                 rich_output = TRUE)
print(results_wcc_rich)

# ==== Summary Statistics by Species ============================================

# Combine results with original data
results_wcc_error$tree_id <- trees$tree_id
results_wcc_error$species <- trees$species

# Summary by species (using results with uncertainty)
species_summary <- aggregate(AGC_WCC_t ~ species, data = results_wcc_error,
  FUN = function(x) c(Count = as.integer(length(x)),
    Total = sum(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
)

# Calculate total uncertainty by species (error propagation for sum)
species_uncertainty <- aggregate(sig_AGC ~ species, data = results_wcc_error,
  FUN = function(x) sqrt(sum(x^2, na.rm = TRUE))  # Error propagation for sum
)

# Combine summaries
species_summary$Total_Uncertainty <- species_uncertainty$sig_AGC[match(species_summary$species, species_uncertainty$species)]

print(species_summary)

# ==== Summary per Area =========================================================
# Calculate carbon density (tonnes C per hectare)

# Get carbon values and uncertainties from fc_agc_error results
carbon_values <- results_wcc_error$AGC_WCC_t
carbon_uncertainty <- results_wcc_error$sig_AGC

# Define plot area (e.g., 0.5 hectares) with measurement uncertainty
plot_area_ha <- 0.5
area_uncertainty_ha <- 0.05  # 10% uncertainty in area measurement

# Calculate carbon density per hectare
carbon_density <- summary_per_area(
  input = carbon_values,
  sigma_input = carbon_uncertainty,
  area = plot_area_ha,
  sigma_area = area_uncertainty_ha,
  returnv = "sigma"
)

cat(sprintf("Total carbon: %.2f t C\n", sum(carbon_values, na.rm = TRUE)))
cat(sprintf("Plot area: %.2f ± %.2f ha\n", plot_area_ha, area_uncertainty_ha))
cat(sprintf("Carbon density: %.2f ± %.2f t C/ha\n",
            carbon_density$total_per_area, carbon_density$error_per_area))
cat(sprintf("95%% CI: [%.2f, %.2f] t C/ha\n",
            carbon_density$total_per_area - 1.96 * carbon_density$error_per_area,
            carbon_density$total_per_area + 1.96 * carbon_density$error_per_area))

# ==== Export Results ===========================================================

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

print(final_results)

# Uncomment to save:
# write.csv(final_results, "tree_carbon_results.csv", row.names = FALSE)
