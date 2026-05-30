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


## ----measurement-unc---------------------------------------------------------------------------------------
# Example: DBH measurement with 2.5% error
dbh_measured <- 45  # cm
dbh_error_pct <- 2.5
dbh_sd <- dbh_measured * (dbh_error_pct / 100)

cat(sprintf("DBH: %.1f ± %.2f cm (%.1f%% error)\n",
            dbh_measured, dbh_sd, dbh_error_pct))
cat(sprintf("95%% CI: [%.1f, %.1f] cm\n",
            dbh_measured - 1.96 * dbh_sd,
            dbh_measured + 1.96 * dbh_sd))


## ----parameter-unc-----------------------------------------------------------------------------------------
# Bunce equation: biomass = exp(a + b * ln(π * dbh))
# Coefficients have standard errors from regression
a_mean <- -5.445  # Intercept
a_se <- 0.05      # Standard error of intercept
b_mean <- 2.505   # Slope
b_se <- 0.06      # Standard error of slope

cat("=== Parameter Uncertainty ===\n")
cat(sprintf("Intercept (a): %.3f ± %.3f\n", a_mean, a_se))
cat(sprintf("Slope (b): %.3f ± %.3f\n", b_mean, b_se))


## ----model-unc---------------------------------------------------------------------------------------------
# Residual error (model fit uncertainty)
# Typically expressed as relative error (CV)
residual_cv <- 0.10  # 10% residual CV

cat("=== Model Uncertainty ===\n")
cat(sprintf("Residual CV: %.1f%%\n", residual_cv * 100))


## ----wcc-error-prop----------------------------------------------------------------------------------------
# Calculate carbon with error propagation
result <- fc_agc_error(name = "Oak", dbh = 45, height = 18,
                       type = "broadleaf",
                       re_dbh = 0.025, re_h = 0.05, re = 0.025)

cat("=== WCC Error Propagation ===\n")
cat(sprintf("Carbon: %.3f ± %.3f t C\n",
            result$AGC_WCC_t, result$sig_AGC))
cat(sprintf("CV: %.1f%%\n", 100 * result$sig_AGC / result$AGC_WCC_t))
cat(sprintf("95%% CI: [%.3f, %.3f] t C\n",
            result$AGC_WCC_t - 1.96 * result$sig_AGC,
            result$AGC_WCC_t + 1.96 * result$sig_AGC))


## ----bunce-error-prop--------------------------------------------------------------------------------------
# Bunce with error propagation
result_bunce <- Bunce(name = "Oak", dbh = 45, re_dbh = 0.025)

cat("=== Bunce Error Propagation ===\n")
cat(sprintf("Biomass: %.1f ± %.1f kg\n",
            result_bunce$biomass, result_bunce$sigma))
cat(sprintf("CV: %.1f%%\n",
            100 * result_bunce$sigma / result_bunce$biomass))


## ----bunce-method-detail-----------------------------------------------------------------------------------
# With rich output to see methodology
result_bunce_rich <- Bunce(name = "Oak", dbh = 45, re_dbh = 0.025,
                           rich_output = TRUE)

print(result_bunce_rich)


## ----biomass-mc, eval=requireNamespace("BIOMASS", quietly = TRUE)------------------------------------------
# BIOMASS with Monte Carlo uncertainty
result_biomass <- BIOMASS(
  dbh = 45,
  height = 18,
  genus = "Quercus",
  species = "robur",
  coords = c(-0.29, 51.48),
  uncertainty = TRUE,
  n_mc = 1000  # Number of Monte Carlo iterations
)

cat("=== BIOMASS Monte Carlo Uncertainty ===\n")
cat(sprintf("AGB: %.1f ± %.1f kg\n",
            result_biomass$AGB_Biomass_kg,
            result_biomass$AGB_sd_kg))
cat(sprintf("CV: %.1f%%\n", result_biomass$AGB_CV_pct))
cat(sprintf("95%% CI: [%.1f, %.1f] kg\n",
            result_biomass$AGB_CI_low_kg,
            result_biomass$AGB_CI_high_kg))


## ----area-sd-gps-------------------------------------------------------------------------------------------
# GPS-traced boundary: perimeter 200 m, RMSE 3 m
sigma_area_band <- sd_area(perimeter = 200, RMSE = 3)
cat(sprintf("Band method: sigma_area = %.4f ha\n", sigma_area_band))

# GPS corners only: 4 vertices, same perimeter
sigma_area_vertex <- sd_area(perimeter = 200, RMSE = 3, n_vertices = 4)
cat(sprintf("Vertex method: sigma_area = %.4f ha\n", sigma_area_vertex))


## ----area-sd-measure---------------------------------------------------------------------------------------
# Circular plot: radius 12.62 m (≈0.05 ha), tape error 0.1 m
sigma_circle <- sd_area_measure("circle", 12.62, 0.1)
cat(sprintf("Circular: sigma_area = %.4f ha\n", sigma_circle))

# Rectangular plot: 40 m × 25 m, tape error 0.1 m per side
sigma_rect <- sd_area_measure("rect", c(40, 25), c(0.1, 0.1))
cat(sprintf("Rectangular: sigma_area = %.4f ha\n", sigma_rect))


## ----area-total-density------------------------------------------------------------------------------------
# Tree-level carbon with uncertainty
carbon_vals <- c(0.45, 0.62, 0.38)
carbon_sig <- c(0.05, 0.06, 0.04)

# 1. Obtain sigma_area from your measurement method
sigma_area_ha <- sd_area_measure("circle", sqrt(0.5 * 10000 / pi), 0.1)  # 0.5 ha circle

# 2. Calculate density with propagated error
result <- total_density(
  input = carbon_vals,
  sigma_input = carbon_sig,
  area = 0.5,
  sigma_area = sigma_area_ha
)

cat(sprintf("Carbon density: %.2f ± %.2f t C/ha\n",
            result$total_per_area, result$error_per_area))


## ----area-negligible---------------------------------------------------------------------------------------
result <- total_density(carbon_vals, carbon_sig, area = 0.5, sigma_area = 0)
cat(sprintf("Density (no area error): %.2f ± %.2f t C/ha\n",
            result$total_per_area, result$error_per_area))


## ----compare-unc-methods, fig.height=6---------------------------------------------------------------------
# Create data for multiple DBH values
dbh_range <- seq(20, 80, by = 5)

# WCC uncertainty
wcc_results <- fc_agc_error(name = rep("Oak", length(dbh_range)),
                            dbh = dbh_range,
                            height = rep(18, length(dbh_range)),
                            type = rep("broadleaf", length(dbh_range)),
                            re_dbh = 0.025, re_h = 0.05)

# Bunce uncertainty
bunce_results <- Bunce(
  name = rep("Oak", length(dbh_range)),
  dbh = dbh_range,
  re_dbh = 0.025
)

# Combine for plotting
plot_data <- data.frame(
  dbh = rep(dbh_range, 2),
  method = rep(c("WCC", "Bunce"), each = length(dbh_range)),
  carbon = c(wcc_results$AGC_WCC_t, bunce_results$biomass / 1000 * 0.5),
  uncertainty = c(wcc_results$sig_AGC, bunce_results$sigma / 1000 * 0.5)
)

plot_data$cv <- 100 * plot_data$uncertainty / plot_data$carbon

ggplot(plot_data, aes(x = dbh, y = cv, color = method)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Coefficient of Variation (CV) vs DBH",
       subtitle = "Comparing WCC and Bunce uncertainty methods",
       x = "DBH (cm)", y = "CV (%)", color = "Method") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

## ----uncertainty-decomp, eval=requireNamespace("BIOMASS", quietly = TRUE)----------------------------------
# Use mc_uncertainty for detailed decomposition
bunce_fn <- function(dbh, a, b) {
  exp(a + b * log(pi * dbh))
}

# Define inputs with uncertainties
inputs <- list(
  dbh = list(mean = 45, sd = 1.13, source = "measurement"),
  a = list(mean = -5.445, sd = 0.05, source = "parameter"),
  b = list(mean = 2.505, sd = 0.06, source = "parameter")
)

# Run Monte Carlo with decomposition
mc_result <- mc_uncertainty(
  bunce_fn,
  inputs,
  N = 2000,
  decompose = TRUE
)

# View decomposition
if (!is.null(mc_result$decomposition)) {
  cat("=== Uncertainty Decomposition ===\n")
  print(mc_result$decomposition$by_source)
}


## ----allometries-compare, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Compare all methods for a single tree
comparison <- allometries(
  genus = "Quercus",
  species = "robur",
  dbh = 45,
  height = 18,
  coords = c(-0.29, 51.48),
  returnv = "AGC"
)

cat("=== Method Comparison (Single Tree) ===\n")
print(comparison[, c("genus", "species", "dbh", "height",
                      "WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")])


## ----method-diff-abs---------------------------------------------------------------------------------------
# Extract estimates
estimates <- c(
  WCC = comparison$WCC_C_t,
  BIOMASS = comparison$biomass_C_t,
  allodb = comparison$allodb_C_t,
  Bunce = comparison$Bunce_C_t
)

# Remove NA values
estimates <- estimates[!is.na(estimates)]

cat("=== Absolute Differences ===\n")
cat(sprintf("Range: %.3f - %.3f t C\n", min(estimates), max(estimates)))
cat(sprintf("Spread: %.3f t C (%.1f%% of mean)\n",
            max(estimates) - min(estimates),
            100 * (max(estimates) - min(estimates)) / mean(estimates)))
cat(sprintf("Mean: %.3f t C\n", mean(estimates)))
cat(sprintf("SD: %.3f t C (CV = %.1f%%)\n",
            sd(estimates), 100 * sd(estimates) / mean(estimates)))


## ----method-diff-rel---------------------------------------------------------------------------------------
# Calculate relative differences from mean
mean_estimate <- mean(estimates)
relative_diff <- 100 * (estimates - mean_estimate) / mean_estimate

diff_table <- data.frame(
  Method = names(estimates),
  Estimate_tC = estimates,
  Difference_pct = relative_diff
)

cat("=== Relative Differences (from mean) ===\n")
print(diff_table)


## ----compare-allometries-func, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Get comparison statistics
comp_stats <- compare_allometries(
  comparison,
  reference = "WCC"  # Use WCC as reference
)

print(comp_stats)


## ----batch-compare, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Sample tree data
trees <- data.frame(
  genus = c("Quercus", "Fagus", "Fraxinus", "Betula"),
  species = c("robur", "sylvatica", "excelsior", "pendula"),
  dbh = c(45, 38, 52, 28),
  height = c(18, 22, 24, 15)
)

# Compare all methods
batch_comparison <- allometries(
  genus = trees$genus,
  species = trees$species,
  dbh = trees$dbh,
  height = trees$height,
  coords = c(-0.29, 51.48),
  returnv = "AGC"
)

cat("=== Batch Method Comparison ===\n")
print(batch_comparison[, c("genus", "species", "dbh",
                           "WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")])


## ----method-agreement--------------------------------------------------------------------------------------
# Calculate cross-method statistics for each tree
methods <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")

batch_comparison$mean_estimate <- apply(
  batch_comparison[, methods], 1, mean, na.rm = TRUE
)
batch_comparison$sd_estimate <- apply(
  batch_comparison[, methods], 1, sd, na.rm = TRUE
)
batch_comparison$cv_pct <- 100 * batch_comparison$sd_estimate / batch_comparison$mean_estimate

cat("=== Method Agreement by Tree ===\n")
print(batch_comparison[, c("genus", "species", "dbh", "mean_estimate", "cv_pct")])

cat("\n=== Overall Agreement ===\n")
cat(sprintf("Mean CV across methods: %.1f%%\n",
            mean(batch_comparison$cv_pct, na.rm = TRUE)))
cat(sprintf("Range of CV: %.1f%% - %.1f%%\n",
            min(batch_comparison$cv_pct, na.rm = TRUE),
            max(batch_comparison$cv_pct, na.rm = TRUE)))


## ----plot-method-diff, fig.height=6, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Reshape data for plotting
library(tidyr)

plot_data <- batch_comparison %>%
  select(genus, species, dbh, WCC_C_t, biomass_C_t, allodb_C_t, Bunce_C_t) %>%
  pivot_longer(cols = c(WCC_C_t, biomass_C_t, allodb_C_t, Bunce_C_t),
               names_to = "method", values_to = "carbon")

plot_data$method <- gsub("_C_t", "", plot_data$method)
plot_data$method <- gsub("biomass", "BIOMASS", plot_data$method)

ggplot(plot_data, aes(x = dbh, y = carbon, color = method, shape = method)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  labs(title = "Carbon Estimates by Method vs DBH",
       x = "DBH (cm)", y = "Above-Ground Carbon (tonnes)",
       color = "Method", shape = "Method") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")


## ----sensitivity-single, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Single tree sensitivity
single_tree <- data.frame(
  dbh = 45,
  height = 18,
  genus = "Quercus",
  species = "robur"
)

# Get estimates from all methods
estimates_all <- allometries(
  genus = single_tree$genus,
  species = single_tree$species,
  dbh = single_tree$dbh,
  height = single_tree$height,
  coords = c(-0.29, 51.48),
  returnv = "AGC"
)

# Calculate sensitivity metrics
carbon_vals <- c(
  estimates_all$WCC_C_t,
  estimates_all$biomass_C_t,
  estimates_all$allodb_C_t,
  estimates_all$Bunce_C_t
)
carbon_vals <- carbon_vals[!is.na(carbon_vals)]

mean_carbon <- mean(carbon_vals)
sd_carbon <- sd(carbon_vals)
cv_carbon <- 100 * sd_carbon / mean_carbon
spread_pct <- 100 * (max(carbon_vals) - min(carbon_vals)) / mean_carbon
range_ratio <- max(carbon_vals) / min(carbon_vals)

cat("=== Single Tree Sensitivity Analysis ===\n")
cat(sprintf("Mean estimate: %.3f t C\n", mean_carbon))
cat(sprintf("SD: %.3f t C\n", sd_carbon))
cat(sprintf("CV: %.1f%%\n", cv_carbon))
cat(sprintf("Spread: %.1f%%\n", spread_pct))
cat(sprintf("Range ratio: %.2fx\n", range_ratio))
cat(sprintf("Range: %.3f - %.3f t C\n", min(carbon_vals), max(carbon_vals)))

# Interpretation
if (cv_carbon < 10) {
  interpretation <- "LOW sensitivity - method choice has minimal impact"
} else if (cv_carbon < 25) {
  interpretation <- "MODERATE sensitivity - typical variation"
} else if (cv_carbon < 50) {
  interpretation <- "HIGH sensitivity - method choice matters"
} else {
  interpretation <- "VERY HIGH sensitivity - investigate data quality"
}

cat(sprintf("\nInterpretation: %s\n", interpretation))


## ----sensitivity-batch, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Multiple trees
forest_data <- data.frame(
  tree_id = 1:10,
  genus = rep(c("Quercus", "Fagus", "Fraxinus", "Betula", "Acer"), 2),
  species = rep(c("robur", "sylvatica", "excelsior", "pendula", "pseudoplatanus"), 2),
  dbh = c(45, 38, 52, 28, 42, 50, 35, 48, 40, 30),
  height = c(18, 22, 24, 15, 20, 21, 19, 23, 18, 14)
)

# Compare all methods
batch_results <- allometries(
  genus = forest_data$genus,
  species = forest_data$species,
  dbh = forest_data$dbh,
  height = forest_data$height,
  coords = c(-0.29, 51.48),
  returnv = "AGC"
)

# Calculate per-tree sensitivity
methods <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")
batch_results$mean_est <- apply(batch_results[, methods], 1, mean, na.rm = TRUE)
batch_results$sd_est <- apply(batch_results[, methods], 1, sd, na.rm = TRUE)
batch_results$cv_pct <- 100 * batch_results$sd_est / batch_results$mean_est

cat("=== Batch Sensitivity Analysis ===\n")
cat("Per-tree CV across methods:\n")
print(batch_results[, c("genus", "species", "dbh", "cv_pct")])

cat(sprintf("\nOverall mean CV: %.1f%%\n",
            mean(batch_results$cv_pct, na.rm = TRUE)))


## ----total-uncertainty, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# For a single tree, get estimates with uncertainty from each method
tree_dbh <- 45
tree_height <- 18

# WCC with measurement uncertainty
wcc_result <- fc_agc_error(name = "Oak", dbh = tree_dbh,
                           height = tree_height, type = "broadleaf",
                           re_dbh = 0.025, re_h = 0.05)

# Bunce with measurement uncertainty
bunce_result <- Bunce(
  name = "Oak",
  dbh = tree_dbh,
  re_dbh = 0.025
)

# Method uncertainty (spread across methods)
all_methods <- allometries(
  genus = "Quercus",
  species = "robur",
  dbh = tree_dbh,
  height = tree_height,
  coords = c(-0.29, 51.48),
  returnv = "AGC"
)

method_estimates <- c(
  all_methods$WCC_C_t,
  all_methods$biomass_C_t,
  all_methods$allodb_C_t,
  all_methods$Bunce_C_t
)
method_estimates <- method_estimates[!is.na(method_estimates)]
method_uncertainty <- sd(method_estimates)

cat("=== Total Uncertainty Breakdown ===\n")
cat("WCC (with measurement error):\n")
cat(sprintf("  Estimate: %.3f ± %.3f t C\n",
            wcc_result$AGC_WCC_t, wcc_result$sig_AGC))

cat("\nBunce (with measurement error):\n")
cat(sprintf("  Estimate: %.3f ± %.3f t C\n",
            bunce_result$biomass / 1000 * 0.5,
            bunce_result$sigma / 1000 * 0.5))

cat("\nMethod uncertainty (spread across methods):\n")
cat(sprintf("  SD across methods: %.3f t C\n", method_uncertainty))
cat(sprintf("  Mean estimate: %.3f t C\n", mean(method_estimates)))

# Combined uncertainty (rough approximation)
# This is a simplified approach - for rigorous analysis, use Monte Carlo
mean_estimate <- mean(method_estimates)
measurement_unc <- mean(c(wcc_result$sig_AGC, bunce_result$sigma / 1000 * 0.5))
total_unc <- sqrt(measurement_unc^2 + method_uncertainty^2)

cat("\nCombined uncertainty (approximate):\n")
cat(sprintf("  Total SD: %.3f t C\n", total_unc))
cat(sprintf("  Total CV: %.1f%%\n", 100 * total_unc / mean_estimate))


## ----session-info------------------------------------------------------------------------------------------
sessionInfo()

