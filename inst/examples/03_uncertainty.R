# ==============================================================================
# Example 3: Uncertainty Propagation
# ==============================================================================
# This example demonstrates how to include uncertainty estimates in your
# carbon calculations, showing error propagation through the calculations.
#
# This example shows:
# - WCC uncertainty propagation
# - Bunce uncertainty propagation
# - Confidence intervals
# - Total uncertainty for multiple trees
# - Rich output with uncertainty
# ==============================================================================

library(TreeCarbon)

# ==============================================================================
# Example 3.1: WCC with Uncertainty (Single Tree)
# ==============================================================================

# Calculate carbon with measurement error propagation
result <- fc_agc_error(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  re_dbh = 0.025,  # 2.5% DBH measurement error
  re_h = 0.05,     # 5% height measurement error
  re = 0.025       # 2.5% coefficient error
)

cat("=== WCC with Uncertainty ===\n")
cat(sprintf("Carbon: %.3f ± %.3f t C\n",
            result$AGC_WCC_t, result$sig_AGC))
cat(sprintf("CV: %.1f%%\n", 100 * result$sig_AGC / result$AGC_WCC_t))
cat(sprintf("95%% CI: [%.3f, %.3f] t C\n",
            result$AGC_WCC_t - 1.96 * result$sig_AGC,
            result$AGC_WCC_t + 1.96 * result$sig_AGC))
cat("\n")

# ==============================================================================
# Example 3.2: Bunce with Uncertainty
# ==============================================================================

# Bunce uncertainty uses CO lookup from Table 6
result_bunce <- Bunce(
  name = "Oak",
  dbh = 45,
  re_dbh = 0.025  # Only DBH error needed
)

cat("=== Bunce with Uncertainty ===\n")
cat(sprintf("Biomass: %.1f ± %.1f kg\n",
            result_bunce$biomass, result_bunce$sigma))
cat(sprintf("CV: %.1f%%\n",
            100 * result_bunce$sigma / result_bunce$biomass))
cat(sprintf("95%% CI: [%.1f, %.1f] kg\n",
            result_bunce$biomass - 1.96 * result_bunce$sigma,
            result_bunce$biomass + 1.96 * result_bunce$sigma))
cat("\n")

# ==============================================================================
# Example 3.3: Rich Output with Uncertainty
# ==============================================================================

# Rich output includes uncertainty information
result_rich <- Bunce(
  name = "Oak",
  dbh = 45,
  re_dbh = 0.025,
  rich_output = TRUE
)

cat("=== Rich Output with Uncertainty ===\n")
print(result_rich)
cat("\n")

# ==============================================================================
# Example 3.4: Batch Processing with Uncertainty
# ==============================================================================

# Multiple trees with uncertainty
trees <- data.frame(
  species = c("Oak", "Beech", "Ash", "Birch"),
  dbh = c(45, 38, 52, 28),
  height = c(18, 22, 24, 15),
  type = rep("broadleaf", 4)
)

# WCC with uncertainty for all trees
results_wcc <- fc_agc_error(
  name = trees$species,
  dbh = trees$dbh,
  height = trees$height,
  type = trees$type,
  re_dbh = 0.025,
  re_h = 0.05
)

cat("=== Batch Results with Uncertainty ===\n")
print(results_wcc[, c("name", "dbh", "AGC_WCC_t", "sig_AGC")])
cat("\n")

# ==============================================================================
# Example 3.5: Total Uncertainty for Multiple Trees
# ==============================================================================

# Calculate total carbon and total uncertainty
# For sums, uncertainty propagates as: sqrt(sum(sigma^2))
total_carbon <- sum(results_wcc$AGC_WCC_t, na.rm = TRUE)
total_uncertainty <- sqrt(sum(results_wcc$sig_AGC^2, na.rm = TRUE))

cat("=== Total Uncertainty (Error Propagation for Sum) ===\n")
cat(sprintf("Total carbon: %.2f ± %.2f t C\n",
            total_carbon, total_uncertainty))
cat(sprintf("95%% CI: [%.2f, %.2f] t C\n",
            total_carbon - 1.96 * total_uncertainty,
            total_carbon + 1.96 * total_uncertainty))
cat(sprintf("CV: %.1f%%\n", 100 * total_uncertainty / total_carbon))
cat("\n")

# ==============================================================================
# Example 3.6: Rich Output for Multiple Trees with Uncertainty
# ==============================================================================

# Rich output for multiple trees includes total uncertainty
results_rich_multi <- Bunce(
  name = trees$species,
  dbh = trees$dbh,
  re_dbh = 0.025,
  rich_output = TRUE
)

cat("=== Rich Output (Multiple Trees with Uncertainty) ===\n")
print(results_rich_multi)
cat("\n")

# ==============================================================================
# Example 3.7: Visualizing Uncertainty
# ==============================================================================

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  
  # Create data for plotting
  plot_data <- data.frame(
    species = results_wcc$name,
    dbh = results_wcc$dbh,
    carbon = results_wcc$AGC_WCC_t,
    ci_low = results_wcc$AGC_WCC_t - 1.96 * results_wcc$sig_AGC,
    ci_high = results_wcc$AGC_WCC_t + 1.96 * results_wcc$sig_AGC
  )
  
  # Plot with error bars
  p <- ggplot(plot_data, aes(x = dbh, y = carbon, color = species)) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 2, alpha = 0.6) +
    geom_point(size = 3) +
    labs(
      title = "Carbon Estimates with 95% Confidence Intervals",
      x = "DBH (cm)",
      y = "Above-Ground Carbon (tonnes)",
      color = "Species"
    ) +
    theme_minimal()
  
  print(p)
}
