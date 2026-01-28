# =============================================================================
# WCC Input Sensitivity Analysis
# =============================================================================
#
# This script performs a one-at-a-time sensitivity analysis on the
# Woodland Carbon Code (WCC) allometric method to determine which input
# parameter has the most influence on the carbon estimate.
#
# Q: "Which input is the most sensitive part of the equation?"
#
# Inputs analyzed:
#   - DBH (diameter at breast height)
#   - Height
#   - Wood density (NSG - nominal specific gravity)
#   - Carbon fraction (via method/biome)
#
# Author: Isabel Openshaw, Justin Moat
# =============================================================================

library(TreeCarbon)
library(ggplot2)

# =============================================================================
# 1. SETUP: Define Base Case Tree
# =============================================================================

# Base case: A typical mature oak tree
base_case <- list(
  name = "Quercus robur",
  dbh = 50,           # cm
  height = 20,        # m
  nsg = 0.56,         # wood density (g/cm³) - typical oak
  type = "broadleaf",
  method = "IPCC2",
  biome = "temperate"
)

# Base case tree details
base_case_df <- data.frame(
  Parameter = c("Species", "DBH (cm)", "Height (m)", "Wood density (g/cm³)", "Type", "Carbon method"),
  Value = c(base_case$name, base_case$dbh, base_case$height, base_case$nsg, 
            base_case$type, paste(base_case$method, base_case$biome, sep = " - "))
)
print(base_case_df)

# Calculate base case carbon
base_result <- fc_agc(
  name = base_case$name,
  dbh = base_case$dbh,
  height = base_case$height,
  nsg = base_case$nsg,
  type = base_case$type,
  method = base_case$method,
  biome = base_case$biome,
  output.all = FALSE
)

base_carbon <- base_result
print(sprintf("Base case carbon: %.4f tonnes", base_carbon))

# =============================================================================
# 2. ONE-AT-A-TIME SENSITIVITY ANALYSIS
# =============================================================================

# Define percentage variations to test
variations <- c(-30, -20, -10, -5, 5, 10, 20, 30)  # % change from base

# Storage for results
sensitivity_results <- data.frame()

# -----------------------------------------------------------------------------
# 2.1 DBH Sensitivity
# -----------------------------------------------------------------------------
# Analyzing DBH sensitivity

for (pct in variations) {
  test_dbh <- base_case$dbh * (1 + pct/100)

  result <- fc_agc(
    name = base_case$name,
    dbh = test_dbh,
    height = base_case$height,
    nsg = base_case$nsg,
    type = base_case$type,
    method = base_case$method,
    biome = base_case$biome,
    output.all = FALSE
  )

  pct_change_output <- 100 * (result - base_carbon) / base_carbon

  sensitivity_results <- rbind(sensitivity_results, data.frame(
    input = "DBH",
    pct_change_input = pct,
    input_value = test_dbh,
    carbon = result,
    pct_change_output = pct_change_output,
    elasticity = pct_change_output / pct
  ))
}

# -----------------------------------------------------------------------------
# 2.2 Height Sensitivity
# -----------------------------------------------------------------------------
# Analyzing Height sensitivity

for (pct in variations) {
  test_height <- base_case$height * (1 + pct/100)

  result <- fc_agc(
    name = base_case$name,
    dbh = base_case$dbh,
    height = test_height,
    nsg = base_case$nsg,
    type = base_case$type,
    method = base_case$method,
    biome = base_case$biome,
    output.all = FALSE
  )

  pct_change_output <- 100 * (result - base_carbon) / base_carbon

  sensitivity_results <- rbind(sensitivity_results, data.frame(
    input = "Height",
    pct_change_input = pct,
    input_value = test_height,
    carbon = result,
    pct_change_output = pct_change_output,
    elasticity = pct_change_output / pct
  ))
}

# -----------------------------------------------------------------------------
# 2.3 Wood Density (NSG) Sensitivity
# -----------------------------------------------------------------------------
# Analyzing Wood Density sensitivity

for (pct in variations) {
  test_nsg <- base_case$nsg * (1 + pct/100)

  result <- fc_agc(
    name = base_case$name,
    dbh = base_case$dbh,
    height = base_case$height,
    nsg = test_nsg,
    type = base_case$type,
    method = base_case$method,
    biome = base_case$biome,
    output.all = FALSE
  )

  pct_change_output <- 100 * (result - base_carbon) / base_carbon

  sensitivity_results <- rbind(sensitivity_results, data.frame(
    input = "Wood Density",
    pct_change_input = pct,
    input_value = test_nsg,
    carbon = result,
    pct_change_output = pct_change_output,
    elasticity = pct_change_output / pct
  ))
}

# -----------------------------------------------------------------------------
# 2.4 Carbon Fraction Sensitivity (via different methods)
# -----------------------------------------------------------------------------
# Analyzing Carbon Fraction sensitivity

# Get carbon fractions for different methods
cf_methods <- data.frame(
  method = c("Matthews1", "IPCC1", "IPCC2", "Thomas"),
  cf = c(0.50, 0.477, 0.471, 0.463)  # Approximate CF values for temperate broadleaf
)

base_cf <- 0.471  # IPCC2 temperate broadleaf

for (i in seq_len(nrow(cf_methods))) {
  m <- cf_methods$method[i]
  cf <- cf_methods$cf[i]

  result <- tryCatch({
    fc_agc(
      name = base_case$name,
      dbh = base_case$dbh,
      height = base_case$height,
      nsg = base_case$nsg,
      type = base_case$type,
      method = m,
      biome = base_case$biome,
      output.all = FALSE
    )
  }, error = function(e) NA)

  if (!is.na(result)) {
    pct_change_input <- 100 * (cf - base_cf) / base_cf
    pct_change_output <- 100 * (result - base_carbon) / base_carbon

    sensitivity_results <- rbind(sensitivity_results, data.frame(
      input = "Carbon Fraction",
      pct_change_input = pct_change_input,
      input_value = cf,
      carbon = result,
      pct_change_output = pct_change_output,
      elasticity = if (abs(pct_change_input) > 0.01) pct_change_output / pct_change_input else NA
    ))
  }
}

# =============================================================================
# 3. CALCULATE ELASTICITY (Sensitivity Index)
# =============================================================================

# Elasticity = (% change in output) / (% change in input)
# Elasticity > 1 means output is MORE sensitive than input change
# Elasticity = 1 means proportional (1:1)
# Elasticity < 1 means output is LESS sensitive than input change

# Calculate mean elasticity for each input
elasticity_summary <- aggregate(
  elasticity ~ input,
  data = sensitivity_results[!is.na(sensitivity_results$elasticity), ],
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

# Flatten the matrix output
elasticity_df <- data.frame(
  input = elasticity_summary$input,
  mean_elasticity = elasticity_summary$elasticity[, "mean"],
  sd_elasticity = elasticity_summary$elasticity[, "sd"]
)

# Sort by absolute elasticity
elasticity_df <- elasticity_df[order(-abs(elasticity_df$mean_elasticity)), ]

# Elasticity ranking (% output change per 1% input change)
elasticity_df$interpretation <- sapply(elasticity_df$mean_elasticity, function(e) {
  if (abs(e) > 2) "VERY HIGH"
  else if (abs(e) > 1) "HIGH"
  else if (abs(e) > 0.5) "MODERATE"
  else "LOW"
})
print(elasticity_df)

# =============================================================================
# 4. TORNADO DIAGRAM
# =============================================================================

# For tornado diagram, we need min/max carbon for ±20% change in each input
tornado_data <- data.frame()

for (inp in unique(sensitivity_results$input)) {
  subset_data <- sensitivity_results[sensitivity_results$input == inp, ]

  # Get carbon at -20% and +20% (or closest available)
  low_idx <- which.min(abs(subset_data$pct_change_input - (-20)))
  high_idx <- which.min(abs(subset_data$pct_change_input - 20))

  if (length(low_idx) > 0 && length(high_idx) > 0) {
    tornado_data <- rbind(tornado_data, data.frame(
      input = inp,
      carbon_low = subset_data$carbon[low_idx],
      carbon_high = subset_data$carbon[high_idx],
      range = subset_data$carbon[high_idx] - subset_data$carbon[low_idx]
    ))
  }
}

# Sort by range
tornado_data <- tornado_data[order(-tornado_data$range), ]

# Tornado data (Carbon range for ±20% input change)
print(tornado_data)

# =============================================================================
# 5. VISUALIZATION
# =============================================================================

# 5.1 Spider Plot: % Change in Output vs % Change in Input
spider_plot <- ggplot(sensitivity_results[sensitivity_results$input != "Carbon Fraction", ],
                      aes(x = pct_change_input, y = pct_change_output,
                          color = input, group = input)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray70") +
  labs(
    title = "WCC Input Sensitivity: Spider Plot",
    subtitle = "Steeper slope = higher sensitivity (dotted line shows 1:1 relationship)",
    x = "% Change in Input",
    y = "% Change in Carbon Output",
    color = "Input Parameter"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set1")

print(spider_plot)

# 5.2 Tornado Diagram
tornado_data$input <- factor(tornado_data$input,
                              levels = tornado_data$input[order(tornado_data$range)])

tornado_plot <- ggplot(tornado_data) +
  geom_segment(aes(x = input, xend = input,
                   y = carbon_low, yend = carbon_high),
               linewidth = 8, color = "steelblue") +
  geom_point(aes(x = input, y = carbon_low), size = 3, color = "darkred") +
  geom_point(aes(x = input, y = carbon_high), size = 3, color = "darkgreen") +
  geom_hline(yintercept = base_carbon, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 0.5, y = base_carbon,
           label = sprintf("Base: %.3f t", base_carbon),
           hjust = 0, vjust = -0.5, color = "red", fontface = "bold") +
  coord_flip() +
  labs(
    title = "WCC Input Sensitivity: Tornado Diagram",
    subtitle = "Longer bars = higher sensitivity (±20% input variation)",
    x = "",
    y = "Carbon (tonnes)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(tornado_plot)

# 5.3 Elasticity Bar Chart
elasticity_df$input <- factor(elasticity_df$input,
                               levels = elasticity_df$input[order(abs(elasticity_df$mean_elasticity))])

elasticity_plot <- ggplot(elasticity_df, aes(x = input, y = mean_elasticity, fill = mean_elasticity)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = mean_elasticity - sd_elasticity,
                    ymax = mean_elasticity + sd_elasticity),
                width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 0.5, y = 1, label = "1:1 proportional",
           hjust = 0, vjust = -0.5, color = "red", size = 3) +
  coord_flip() +
  labs(
    title = "WCC Input Elasticity",
    subtitle = "Elasticity = % change in carbon per 1% change in input",
    x = "",
    y = "Elasticity",
    fill = "Elasticity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "none"
  ) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1)

print(elasticity_plot)

# =============================================================================
# 6. SUMMARY AND CONCLUSIONS
# =============================================================================

# Rank by elasticity
most_sensitive <- elasticity_df$input[1]
most_sensitive_e <- elasticity_df$mean_elasticity[1]

# Summary: Ranking (most to least sensitive)
ranking_df <- data.frame(
  Rank = seq_len(nrow(elasticity_df)),
  Input = elasticity_df$input,
  Elasticity = round(elasticity_df$mean_elasticity, 2)
)
print(ranking_df)

# KEY FINDINGS:
# 1. The MOST sensitive input is typically DBH
#    - Elasticity ~2.5 (a 1% change in DBH causes ~2.5% change in carbon)
#
# 2. DBH has the highest sensitivity because:
#    - It enters the WCC equations raised to a power (typically ~2.5)
#    - It affects both stem volume AND crown biomass calculations
#    - Measurement errors in DBH propagate strongly to the output
#
# 3. Height is typically moderately sensitive because:
#    - It enters linearly in some components of the WCC
#    - It affects both tariff number and volume calculations
#
# 4. Wood density (NSG) is linearly proportional (elasticity ~ 1):
#    - A 10% error in wood density causes ~10% error in biomass
#    - Species mis-identification can significantly affect this
#
# 5. Carbon fraction has the lowest sensitivity:
#    - The range across methods is small (46-50%)
#    - Method choice matters less than measurement accuracy
#
# PRACTICAL IMPLICATIONS:
#    - Prioritize accurate DBH measurement (callipers, tape at exactly 1.3m)
#    - Height errors are significant but less critical than DBH
#    - Correct species identification matters for wood density lookup
#    - Carbon conversion method choice is relatively unimportant
