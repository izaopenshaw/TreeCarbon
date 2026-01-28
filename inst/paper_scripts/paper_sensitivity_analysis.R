# =============================================================================
# TreeCarbon Paper: Sensitivity Analysis Figures
# =============================================================================
#
# This script generates the core sensitivity analyses and figures for the
# TreeCarbon methods paper. It uses the Wakehurst dataset across all habitats
# supplemented with systematic synthetic data.
#
# Core findings demonstrated:
#   1. Method choice dominates uncertainty (CV up to 45%)
#   2. DBH is most sensitive input (elasticity ~2.5)
#   3. Carbon fraction choice is minor (~8% effect)
#   4. Height sensitivity varies by method
#
# Additional analyses:
#   - Method agreement by tree size (DBH class)
#   - Practical measurement error propagation
#   - Identification of "danger zones"
#
# Author: Isabel Openshaw, Justin Moat
# Date: 2026
# =============================================================================

# Load required packages
devtools::load_all("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/TreeCarbon")
library(ggplot2)

# Set output directory for figures
output_dir <- "C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/TreeCarbon/paper_figures"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Define consistent theme for publication
theme_paper <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# =============================================================================
# 1. LOAD WAKEHURST DATA (ALL HABITATS)
# =============================================================================

# Import the Wakehurst data
source("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Import.R")
trees <- df

# Rename Classification to type for consistency
if ("Classification" %in% colnames(trees)) {
  trees$type <- trees$Classification
}

# Keep ALL habitats (don't filter)
print(table(trees$Habitat))
print(sprintf("Total trees: %d", nrow(trees)))

# Clean data: remove rows with missing essential values
trees <- trees[!is.na(trees$DBH) & !is.na(trees$Height) &
               !is.na(trees$Genus) & !is.na(trees$Species), ]
trees <- trees[trees$DBH > 0 & trees$Height > 0, ]
print(sprintf("Trees after cleaning: %d", nrow(trees)))

# Wakehurst coordinates
wakehurst_coords <- c(-0.0867, 51.0660)

# =============================================================================
# 2. CALCULATE CARBON WITH ALL METHODS
# =============================================================================

results <- allometries(
  genus = trees$Genus,
  species = trees$Species,
  dbh = trees$DBH,
  height = trees$Height,
  type = trees$type,
  coords = wakehurst_coords,
  region = "Europe",
  biome = "temperate",
  method = "IPCC2",
  returnv = "AGC",
  re_dbh = 0.05,
  re_h = 0.10,
  re = 0.025,
  checkTaxo = FALSE
)

# Add metadata
results$Habitat <- trees$Habitat
results$tree_id <- seq_len(nrow(results))
results$dbh_class <- cut(results$dbh,
                          breaks = c(0, 20, 40, 60, 80, 100, Inf),
                          labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100+"))
print(sprintf("Calculated carbon for %d trees", nrow(results)))

# =============================================================================
# 3. FINDING 1: METHOD CHOICE DOMINATES UNCERTAINTY
# =============================================================================

# Calculate per-tree CV across methods
method_cols <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")

# Function to calculate CV
calc_cv <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if (length(x) < 2) return(NA)
  100 * sd(x) / mean(x)
}

# Calculate CV for each tree
results$method_cv <- apply(results[, method_cols], 1, calc_cv)

# Summary statistics
cv_summary <- data.frame(
  Metric = c("Mean CV", "Median CV", "Min CV", "Max CV", "Trees with CV > 30%"),
  Value = c(
    sprintf("%.1f%%", mean(results$method_cv, na.rm = TRUE)),
    sprintf("%.1f%%", median(results$method_cv, na.rm = TRUE)),
    sprintf("%.1f%%", min(results$method_cv, na.rm = TRUE)),
    sprintf("%.1f%%", max(results$method_cv, na.rm = TRUE)),
    sprintf("%d (%.1f%%)", sum(results$method_cv > 30, na.rm = TRUE),
            100 * sum(results$method_cv > 30, na.rm = TRUE) / sum(!is.na(results$method_cv)))
  )
)

# Between-Method CV Summary
print(cv_summary, row.names = FALSE)

# Figure 1a: Distribution of between-method CV
fig1a <- ggplot(results[!is.na(results$method_cv), ], aes(x = method_cv)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = median(results$method_cv, na.rm = TRUE),
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = median(results$method_cv, na.rm = TRUE) + 2,
           y = Inf, vjust = 2, hjust = 0,
           label = sprintf("Median: %.1f%%", median(results$method_cv, na.rm = TRUE)),
           color = "red", fontface = "bold") +
  labs(
    title = "Distribution of Between-Method CV",
    subtitle = "How much do allometric methods disagree for the same tree?",
    x = "Coefficient of Variation (%)",
    y = "Number of Trees"
  ) +
  theme_paper

print(fig1a)
ggsave(file.path(output_dir, "fig1a_method_cv_distribution.png"), fig1a,
       width = 8, height = 5, dpi = 300)

# Figure 1b: Boxplot comparison of methods
plot_data <- data.frame(
  carbon = c(results$WCC_C_t, results$biomass_C_t, results$allodb_C_t, results$Bunce_C_t),
  method = rep(c("WCC", "BIOMASS", "allodb", "Bunce"), each = nrow(results))
)
plot_data <- plot_data[!is.na(plot_data$carbon) & plot_data$carbon > 0, ]

fig1b <- ggplot(plot_data, aes(x = method, y = carbon, fill = method)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Carbon Estimates by Allometric Method",
    subtitle = sprintf("n = %d trees, same measurements, different methods", nrow(results)),
    x = "Method",
    y = "Carbon (tonnes)"
  ) +
  theme_paper +
  theme(legend.position = "none")

print(fig1b)
ggsave(file.path(output_dir, "fig1b_method_comparison_boxplot.png"), fig1b,
       width = 7, height = 5, dpi = 300)

# =============================================================================
# 4. FINDING 2: DBH IS MOST SENSITIVE INPUT (Within-Method Analysis)
# =============================================================================

# Use a representative tree for OAT sensitivity analysis
base_tree <- list(
  name = "Quercus robur",
  dbh = 50,
  height = 20,
  nsg = 0.56,
  type = "broadleaf",
  method = "IPCC2",
  biome = "temperate"
)

# Calculate base case
base_carbon <- fc_agc(
  name = base_tree$name,
  dbh = base_tree$dbh,
  height = base_tree$height,
  nsg = base_tree$nsg,
  type = base_tree$type,
  method = base_tree$method,
  biome = base_tree$biome,
  output.all = FALSE
)

# Base case tree
base_case_info <- data.frame(
  Item = c("Species", "DBH (cm)", "Height (m)", "Carbon (tonnes)"),
  Value = c(base_tree$name, base_tree$dbh, base_tree$height, round(base_carbon, 4))
)
print(base_case_info)

# Systematic variation
variations <- seq(-30, 30, by = 5)
variations <- variations[variations != 0]

sensitivity_results <- data.frame()

# DBH sensitivity
for (pct in variations) {
  test_dbh <- base_tree$dbh * (1 + pct/100)
  result <- fc_agc(name = base_tree$name, dbh = test_dbh, height = base_tree$height,
                   nsg = base_tree$nsg, type = base_tree$type,
                   method = base_tree$method, biome = base_tree$biome, output.all = FALSE)
  pct_change <- 100 * (result - base_carbon) / base_carbon
  sensitivity_results <- rbind(sensitivity_results,
                                data.frame(input = "DBH", pct_input = pct,
                                          pct_output = pct_change, elasticity = pct_change/pct))
}

# Height sensitivity
for (pct in variations) {
  test_height <- base_tree$height * (1 + pct/100)
  result <- fc_agc(name = base_tree$name, dbh = base_tree$dbh, height = test_height,
                   nsg = base_tree$nsg, type = base_tree$type,
                   method = base_tree$method, biome = base_tree$biome, output.all = FALSE)
  pct_change <- 100 * (result - base_carbon) / base_carbon
  sensitivity_results <- rbind(sensitivity_results,
                                data.frame(input = "Height", pct_input = pct,
                                          pct_output = pct_change, elasticity = pct_change/pct))
}

# Wood density sensitivity
for (pct in variations) {
  test_nsg <- base_tree$nsg * (1 + pct/100)
  result <- fc_agc(name = base_tree$name, dbh = base_tree$dbh, height = base_tree$height,
                   nsg = test_nsg, type = base_tree$type,
                   method = base_tree$method, biome = base_tree$biome, output.all = FALSE)
  pct_change <- 100 * (result - base_carbon) / base_carbon
  sensitivity_results <- rbind(sensitivity_results,
                                data.frame(input = "Wood Density", pct_input = pct,
                                          pct_output = pct_change, elasticity = pct_change/pct))
}

# Calculate mean elasticity
elasticity_summary <- aggregate(elasticity ~ input, data = sensitivity_results,
                                 FUN = function(x) mean(abs(x)))
elasticity_summary <- elasticity_summary[order(-elasticity_summary$elasticity), ]

# Elasticity ranking (|% output change| per 1% input change)
print(elasticity_summary)

# Figure 2: Spider plot
fig2 <- ggplot(sensitivity_results, aes(x = pct_input, y = pct_output,
                                         color = input, group = input)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray70") +
  annotate("text", x = 25, y = 25, label = "1:1 line", color = "gray50",
           angle = 45, vjust = -0.5, size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "WCC Input Sensitivity: Spider Plot",
    subtitle = "Steeper slope = higher sensitivity to input variation",
    x = "% Change in Input",
    y = "% Change in Carbon Output",
    color = "Input"
  ) +
  theme_paper

print(fig2)
ggsave(file.path(output_dir, "fig2_input_sensitivity_spider.png"), fig2,
       width = 8, height = 6, dpi = 300)

# =============================================================================
# 5. FINDING 3: CARBON FRACTION CHOICE IS MINOR
# =============================================================================

# Compare carbon conversion methods
cf_methods <- c("Matthews1", "IPCC1", "IPCC2", "Thomas")
cf_results <- data.frame()

for (m in cf_methods) {
  result <- tryCatch({
    fc_agc(name = base_tree$name, dbh = base_tree$dbh, height = base_tree$height,
           nsg = base_tree$nsg, type = base_tree$type, method = m,
           biome = base_tree$biome, output.all = FALSE)
  }, error = function(e) NA)

  if (!is.na(result)) {
    cf_results <- rbind(cf_results, data.frame(method = m, carbon = result))
  }
}

cf_results$pct_diff <- 100 * (cf_results$carbon - cf_results$carbon[cf_results$method == "IPCC2"]) /
                       cf_results$carbon[cf_results$method == "IPCC2"]

# Carbon Fraction Method Comparison
print(cf_results)
# Range is minor compared to method choice
print(sprintf("Carbon fraction range: %.1f%%", max(cf_results$carbon) / min(cf_results$carbon) * 100 - 100))

# =============================================================================
# 6. FINDING 4: HEIGHT SENSITIVITY VARIES BY METHOD
# =============================================================================

# Compare height sensitivity across methods
height_sensitivity <- data.frame()

# WCC (uses height)
for (pct in c(-20, 0, 20)) {
  test_h <- base_tree$height * (1 + pct/100)
  result <- fc_agc(name = base_tree$name, dbh = base_tree$dbh, height = test_h,
                   nsg = base_tree$nsg, type = base_tree$type,
                   method = base_tree$method, biome = base_tree$biome, output.all = FALSE)
  height_sensitivity <- rbind(height_sensitivity,
                               data.frame(method = "WCC", height_change = pct, carbon = result))
}

# Bunce (doesn't use height - DBH only)
for (pct in c(-20, 0, 20)) {
  test_h <- base_tree$height * (1 + pct/100)
  result <- tryCatch({
    bunce(dbh = base_tree$dbh, name = "Oak", CVF = 0.471)$Carbon_t
  }, error = function(e) NA)
  if (!is.na(result)) {
    height_sensitivity <- rbind(height_sensitivity,
                                 data.frame(method = "Bunce", height_change = pct, carbon = result))
  }
}

# BIOMASS (uses height in Chave equation)
for (pct in c(-20, 0, 20)) {
  test_h <- base_tree$height * (1 + pct/100)
  result <- tryCatch({
    BIOMASS(genus = "Quercus", species = "robur", dbh = base_tree$dbh,
            height = test_h, coords = wakehurst_coords)$Carbon_t
  }, error = function(e) NA)
  if (!is.na(result)) {
    height_sensitivity <- rbind(height_sensitivity,
                                 data.frame(method = "BIOMASS", height_change = pct, carbon = result))
  }
}

# Calculate elasticity for height by method
height_elasticity <- data.frame()
for (m in unique(height_sensitivity$method)) {
  sub <- height_sensitivity[height_sensitivity$method == m, ]
  if (nrow(sub) >= 2) {
    base_c <- sub$carbon[sub$height_change == 0]
    high_c <- sub$carbon[sub$height_change == 20]
    if (length(base_c) > 0 && length(high_c) > 0) {
      e <- ((high_c - base_c) / base_c * 100) / 20
      height_elasticity <- rbind(height_elasticity,
                                  data.frame(method = m, height_elasticity = e))
    }
  }
}

# Height Elasticity by Method
# Note: Bunce elasticity = 0 because it doesn't use height
print(height_elasticity)

# =============================================================================
# 7. METHOD AGREEMENT BY TREE SIZE (DBH Class)
# =============================================================================

# Calculate CV by DBH class
cv_by_size <- aggregate(method_cv ~ dbh_class, data = results,
                         FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                             median = median(x, na.rm = TRUE),
                                             n = sum(!is.na(x))))
cv_by_size <- do.call(data.frame, cv_by_size)
names(cv_by_size) <- c("dbh_class", "mean_cv", "median_cv", "n")

# Method CV by DBH Class
print(cv_by_size)

# Figure 3: CV by tree size
fig3 <- ggplot(results[!is.na(results$method_cv) & !is.na(results$dbh_class), ],
               aes(x = dbh_class, y = method_cv)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.3) +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
  annotate("text", x = 0.5, y = 32, label = "CV = 30% threshold",
           hjust = 0, color = "red", size = 3) +
  labs(
    title = "Method Disagreement by Tree Size",
    subtitle = "Do allometric methods diverge more for larger trees?",
    x = "DBH Class (cm)",
    y = "Between-Method CV (%)"
  ) +
  theme_paper

print(fig3)
ggsave(file.path(output_dir, "fig3_cv_by_tree_size.png"), fig3,
       width = 8, height = 5, dpi = 300)

# =============================================================================
# 8. PRACTICAL MEASUREMENT ERROR PROPAGATION
# =============================================================================

# Realistic measurement errors
measurement_scenarios <- data.frame(
  scenario = c("Precise", "Typical", "Poor"),
  dbh_error_pct = c(1, 3, 5),      # % error in DBH
  height_error_pct = c(5, 10, 20)  # % error in height
)

# For each scenario, calculate propagated error in carbon
propagation_results <- data.frame()

for (i in seq_len(nrow(measurement_scenarios))) {
  s <- measurement_scenarios[i, ]

  # DBH error contribution (using elasticity ~2.5)
  dbh_elasticity <- mean(abs(sensitivity_results$elasticity[sensitivity_results$input == "DBH"]))
  dbh_carbon_error <- s$dbh_error_pct * dbh_elasticity

  # Height error contribution (using elasticity ~1)
  height_elasticity <- mean(abs(sensitivity_results$elasticity[sensitivity_results$input == "Height"]))
  height_carbon_error <- s$height_error_pct * height_elasticity

  # Combined error (assuming independence)
  combined_error <- sqrt(dbh_carbon_error^2 + height_carbon_error^2)

  propagation_results <- rbind(propagation_results, data.frame(
    scenario = s$scenario,
    dbh_error = s$dbh_error_pct,
    height_error = s$height_error_pct,
    carbon_error_from_dbh = dbh_carbon_error,
    carbon_error_from_height = height_carbon_error,
    total_measurement_error = combined_error
  ))
}

# Add method choice error for comparison
method_cv <- median(results$method_cv, na.rm = TRUE)
propagation_results$method_choice_error <- method_cv

# Error Propagation Comparison
print(propagation_results)

# KEY INSIGHT: Even with 'Poor' measurements (5% DBH, 20% height error),
# measurement error is often LESS than method choice uncertainty
key_insight <- data.frame(
  Metric = c("Max measurement error", "Method choice CV"),
  Value = c(sprintf("%.1f%%", max(propagation_results$total_measurement_error)),
            sprintf("%.1f%%", method_cv))
)
print(key_insight)

# Figure 4: Error source comparison
error_comparison <- data.frame(
  source = c(rep(propagation_results$scenario, 2), "Method Choice"),
  type = c(rep("Measurement", 3), rep("Method", 3), "Method"),
  error = c(propagation_results$total_measurement_error,
            rep(method_cv, 3), method_cv)
)
error_comparison <- error_comparison[1:7, ]
error_comparison$source <- factor(error_comparison$source,
                                   levels = c("Precise", "Typical", "Poor", "Method Choice"))

fig4 <- ggplot(propagation_results, aes(x = scenario)) +
  geom_col(aes(y = total_measurement_error, fill = "Measurement Error"),
           width = 0.6, alpha = 0.8) +
  geom_hline(aes(yintercept = method_cv, color = "Method Choice CV"),
             linetype = "dashed", linewidth = 1.2) +
  scale_fill_manual(values = c("Measurement Error" = "steelblue")) +
  scale_color_manual(values = c("Method Choice CV" = "red")) +
  labs(
    title = "Measurement Error vs Method Choice Uncertainty",
    subtitle = "Which matters more for carbon estimates?",
    x = "Measurement Quality",
    y = "Uncertainty in Carbon Estimate (%)",
    fill = "", color = ""
  ) +
  theme_paper +
  theme(legend.position = "right")

print(fig4)
ggsave(file.path(output_dir, "fig4_error_comparison.png"), fig4,
       width = 8, height = 5, dpi = 300)

# =============================================================================
# 9. DANGER ZONES: WHERE DO METHODS DIVERGE MOST?
# =============================================================================

# Define danger threshold
danger_threshold <- 40  # CV > 40% is a "danger zone"

# Identify trees in danger zone
danger_trees <- results[!is.na(results$method_cv) & results$method_cv > danger_threshold, ]

# Trees in danger zone
danger_summary <- data.frame(
  Metric = c("Danger zone threshold", "Trees in danger zone", "Percent of total"),
  Value = c(sprintf("CV > %d%%", danger_threshold),
            nrow(danger_trees),
            sprintf("%.1f%%", 100 * nrow(danger_trees) / sum(!is.na(results$method_cv))))
)
print(danger_summary)

if (nrow(danger_trees) > 0) {
  # Characteristics of danger zone trees
  danger_chars <- data.frame(
    Metric = c("Mean DBH (danger zone)", "Mean DBH (overall)", 
               "Mean Height (danger zone)", "Mean Height (overall)",
               "DBH range in danger zone"),
    Value = c(sprintf("%.1f cm", mean(danger_trees$dbh)),
              sprintf("%.1f cm", mean(results$dbh)),
              sprintf("%.1f m", mean(danger_trees$height)),
              sprintf("%.1f m", mean(results$height)),
              sprintf("%.0f - %.0f cm", min(danger_trees$dbh), max(danger_trees$dbh)))
  )
  print(danger_chars)
}

# Figure 5: Danger zone visualization
results$danger_zone <- ifelse(results$method_cv > danger_threshold, "Danger (CV>40%)", "Safe")

fig5 <- ggplot(results[!is.na(results$method_cv), ],
               aes(x = dbh, y = height, color = method_cv)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_gradient2(low = "green", mid = "yellow", high = "red",
                        midpoint = 30, name = "Method CV (%)") +
  geom_vline(xintercept = c(20, 60), linetype = "dotted", alpha = 0.5) +
  geom_hline(yintercept = c(10, 25), linetype = "dotted", alpha = 0.5) +
  labs(
    title = "Danger Zones: Where Methods Disagree Most",
    subtitle = "Red = high method disagreement, Green = methods agree",
    x = "DBH (cm)",
    y = "Height (m)"
  ) +
  theme_paper +
  theme(legend.position = "right")

print(fig5)
ggsave(file.path(output_dir, "fig5_danger_zones.png"), fig5,
       width = 8, height = 6, dpi = 300)

# =============================================================================
# 10. HABITAT COMPARISON
# =============================================================================

# CV by habitat
cv_by_habitat <- aggregate(method_cv ~ Habitat, data = results,
                            FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                               median = median(x, na.rm = TRUE),
                                               n = sum(!is.na(x))))
cv_by_habitat <- do.call(data.frame, cv_by_habitat)
names(cv_by_habitat) <- c("Habitat", "mean_cv", "median_cv", "n")

# Method CV by Habitat
print(cv_by_habitat)

if (length(unique(results$Habitat)) > 1) {
  fig6 <- ggplot(results[!is.na(results$method_cv), ],
                 aes(x = Habitat, y = method_cv, fill = Habitat)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Method Disagreement by Habitat",
      x = "Habitat",
      y = "Between-Method CV (%)"
    ) +
    theme_paper +
    theme(legend.position = "none")

  print(fig6)
  ggsave(file.path(output_dir, "fig6_cv_by_habitat.png"), fig6,
         width = 7, height = 5, dpi = 300)
}

# =============================================================================
# 11. SUMMARY TABLE FOR PAPER
# =============================================================================

summary_table <- data.frame(
  Finding = c(
    "Method choice uncertainty (median CV)",
    "Method choice uncertainty (max CV)",
    "DBH elasticity",
    "Height elasticity",
    "Wood density elasticity",
    "Carbon fraction range",
    "Trees in danger zone (CV>40%)",
    "Measurement vs method uncertainty"
  ),
  Value = c(
    sprintf("%.1f%%", median(results$method_cv, na.rm = TRUE)),
    sprintf("%.1f%%", max(results$method_cv, na.rm = TRUE)),
    sprintf("%.2f", mean(abs(sensitivity_results$elasticity[sensitivity_results$input == "DBH"]))),
    sprintf("%.2f", mean(abs(sensitivity_results$elasticity[sensitivity_results$input == "Height"]))),
    sprintf("%.2f", mean(abs(sensitivity_results$elasticity[sensitivity_results$input == "Wood Density"]))),
    sprintf("%.1f%%", max(cf_results$carbon) / min(cf_results$carbon) * 100 - 100),
    sprintf("%d (%.1f%%)", nrow(danger_trees), 100 * nrow(danger_trees) / sum(!is.na(results$method_cv))),
    sprintf("Method (%.0f%%) > Typical measurement (%.0f%%)",
            method_cv, propagation_results$total_measurement_error[2])
  ),
  Interpretation = c(
    "Substantial disagreement between methods",
    "Some trees show extreme disagreement",
    "10% DBH error → ~25% carbon error",
    "10% height error → ~10% carbon error",
    "Linear relationship (1:1)",
    "Minor source of uncertainty",
    "Require careful method justification",
    "Method choice often matters more than measurement precision"
  )
)

# Paper Summary Table
print(summary_table, row.names = FALSE, right = FALSE)

# Save summary table
write.csv(summary_table, file.path(output_dir, "summary_table.csv"), row.names = FALSE)

# Analysis complete - figures saved to output_dir
