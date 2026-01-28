# ============================================================================
# Wakehurst Tree Carbon Analysis
# ============================================================================
# This script demonstrates the TreeCarbon package workflow using Wakehurst data
#
# Features demonstrated:
#   - Loading and preparing tree inventory data
#   - Calculating carbon with multiple allometric methods
#   - Error propagation for uncertainty quantification
#   - Sensitivity analysis: How does method choice affect results?
#   - Habitat-level summaries with carbon per hectare
#   - Visualization comparing methods
#
# ============================================================================

# Load required packages
# Use local development version (includes latest fixes)
devtools::load_all("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/TreeCarbon")

# Or install from GitHub (may not have latest local fixes):
# devtools::install_github("izaopenshaw/TreeCarbon", force = TRUE)

library(ggplot2)

# ============================================================================
# 1. LOAD AND PREPARE DATA
# ============================================================================
source("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Import.R")

# Select relevant columns from imported data
# Expected columns: Name, Genus, Species, DBH_Manual, DBH, ID, Height, Habitat, Plot, Classification
trees <- df

# Rename Classification to type for consistency with package functions
if ("Classification" %in% colnames(trees)) {
  trees$type <- trees$Classification
}

# Select Habitat
trees <- trees[trees$Habitat == "broadleaf",]

# Show data structure
print(names(trees))

# Show available habitats
print(table(trees$Habitat))

# Total trees
print(sprintf("Total trees: %d", nrow(trees)))

# ============================================================================
# 2. WAKEHURST COORDINATES AND HABITAT AREAS
# ============================================================================

# Wakehurst Place, Sussex, UK coordinates
wakehurst_coords <- c(-0.0867, 51.0660)  # Longitude, Latitude

# Define habitat areas (hectares) - UPDATE THESE WITH ACTUAL VALUES
# These are example values - replace with actual measured areas
habitat_areas <- data.frame(
  Habitat = c("broadleaf", "conifer", "coppice"),
  area_ha = c(15.5, 8.2, 3.8),          # Habitat areas in hectares
  sigma_area_ha = c(0.5, 0.3, 0.15)     # Uncertainty in area measurement
)

# If you have perimeter measurements, calculate sigma_area using sd_area():
# habitat_areas$sigma_area_ha <- sd_area(perimeter_m = c(1500, 1100, 750), RMSE = 0.5)

print(habitat_areas)

# Set working directory to save graphs
setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Wakehurst_analysis/broadleaf")

# ============================================================================
# 3. CALCULATE CARBON WITH ALL METHODS
# ============================================================================

# Use allometries() to calculate all methods at once
# This includes WCC, BIOMASS, allodb, and Bunce

results <- tryCatch({
  allometries(
    genus = trees$Genus,
    species = trees$Species,
    dbh = trees$DBH,
    height = trees$Height,
    type = trees$type,
    coords = wakehurst_coords,
    region = "Europe",
    biome = "temperate",
    method = "IPCC2",          # Carbon conversion method
    returnv = "AGC",           # Return Above-Ground Carbon
    re_dbh = 0.05,             # 5% DBH measurement error
    re_h = 0.10,               # 10% height measurement error
    re = 0.025,                # 2.5% coefficient error
    checkTaxo = FALSE          # Don't check taxonomy (faster)
  )
}, error = function(e) {
  warning("Error in allometries(): ", e$message)
  NULL
})

# ============================================================================
# 3B. CALCULATE TLS-BASED CARBON
# ============================================================================
# TLS (Terrestrial Laser Scanning) provides direct volume measurements
# Convert: Volume → Biomass (via wood density) → Carbon

# Check if TLS data exists

if ("TotalVolume_opt" %in% colnames(trees)) {

  # TLS data check: count non-NA values

  # Check if there's actual data
  if (sum(!is.na(trees$TotalVolume_opt)) == 0) {
    warning("TotalVolume_opt column exists but contains only NA values")
    results$TLS_C_t <- NA
    results$TLS_C_sig <- NA
  } else {

    # TLS volume - check units (might be in litres/dm³ or already m³)
    # If values are large (>100), assume dm³ and convert to m³
    median_vol <- median(trees$TotalVolume_opt, na.rm = TRUE)

    if (median_vol > 100) {
      # Likely in dm³ (litres), convert to m³
      tls_volume_m3 <- trees$TotalVolume_opt / 1000
    } else {
      # Already in m³
      tls_volume_m3 <- trees$TotalVolume_opt
    }

    # Get wood density for each tree (use species-specific or default)
    # Using NSG (Nominal Specific Gravity) from lookup if available
    spcodes <- lookupcode(trees$Name, trees$type, code = "short")

    # Get wood density from lookup table
    nsg_idx <- match(spcodes$code, lookup_df$short)
    wood_density <- lookup_df$NSG[nsg_idx]
    wood_density[is.na(wood_density)] <- 0.55  # Default for missing

    # Calculate TLS biomass: Volume (m³) × Wood Density (t/m³)
    # Note: NSG is in g/cm³ which equals t/m³
    tls_biomass_t <- tls_volume_m3 * wood_density

    # Estimate TLS uncertainty (if TotalVolume_std exists, use it; otherwise assume 10% CV)
    if ("TotalVolume_std" %in% colnames(trees)) {
      tls_biomass_sigma <- (trees$TotalVolume_std / 1000) * wood_density
    } else {
      # Assume 10% CV for TLS volume measurement
      tls_biomass_sigma <- tls_biomass_t * 0.10
    }

    # Convert biomass to carbon using biomass2c
    tls_carbon <- biomass2c(
      tls_biomass_t,
      method = "IPCC2",
      type = trees$type,
      biome = "temperate",
      sig_biomass = tls_biomass_sigma
    )

    # Add TLS results to the results dataframe
    results$TLS_C_t <- tls_carbon$AGC
    results$TLS_C_sig <- tls_carbon$sig_AGC
  }

} else {
  warning("TotalVolume_opt column not found - TLS comparison will be skipped")
  results$TLS_C_t <- NA
  results$TLS_C_sig <- NA
}

# ============================================================================
# 4. SUMMARIZE RESULTS BY METHOD
# ============================================================================

# Extract carbon columns (now including TLS)
carbon_cols <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t", "TLS_C_t")
error_cols <- c("WCC_C_sig", "biomass_C_sig", "allodb_C_sig", "Bunce_C_sig", "TLS_C_sig")

# Rename for cleaner output
method_names <- c("WCC", "BIOMASS", "allodb", "Bunce", "TLS")

# Create summary table
method_summary <- data.frame(
  Method = method_names,
  Total_Carbon_t = sapply(carbon_cols, function(col) {
    if (col %in% colnames(results)) sum(results[[col]], na.rm = TRUE) else NA
  }),
  Mean_per_Tree_t = sapply(carbon_cols, function(col) {
    if (col %in% colnames(results)) mean(results[[col]], na.rm = TRUE) else NA
  }),
  SD_per_Tree = sapply(carbon_cols, function(col) {
    if (col %in% colnames(results)) sd(results[[col]], na.rm = TRUE) else NA
  }),
  N_Valid = sapply(carbon_cols, function(col) {
    if (col %in% colnames(results)) sum(!is.na(results[[col]])) else 0
  })
)

# Add total propagated error (sum of individual errors in quadrature)
method_summary$Total_Error_t <- sapply(seq_along(carbon_cols), function(i) {
  err_col <- error_cols[i]
  if (err_col %in% colnames(results)) {
    sqrt(sum(results[[err_col]]^2, na.rm = TRUE))
  } else NA
})

# Calculate CV
method_summary$CV_pct <- round(100 * method_summary$SD_per_Tree / method_summary$Mean_per_Tree_t, 1)

# Round for display
method_summary$Total_Carbon_t <- round(method_summary$Total_Carbon_t, 2)
method_summary$Mean_per_Tree_t <- round(method_summary$Mean_per_Tree_t, 4)
method_summary$SD_per_Tree <- round(method_summary$SD_per_Tree, 4)
method_summary$Total_Error_t <- round(method_summary$Total_Error_t, 2)

# Total Carbon by Method
print(method_summary)

# Calculate cross-method statistics
valid_totals <- method_summary$Total_Carbon_t[!is.na(method_summary$Total_Carbon_t)]
if (length(valid_totals) > 1) {
  # Cross-Method Comparison
  cross_method_stats <- data.frame(
    Metric = c("Mean total", "SD", "CV", "Range min", "Range max", "Spread", "Range ratio"),
    Value = c(
      sprintf("%.2f tonnes", mean(valid_totals)),
      sprintf("%.2f tonnes", sd(valid_totals)),
      sprintf("%.1f%%", 100 * sd(valid_totals) / mean(valid_totals)),
      sprintf("%.2f tonnes", min(valid_totals)),
      sprintf("%.2f tonnes", max(valid_totals)),
      sprintf("%.2f tonnes", max(valid_totals) - min(valid_totals)),
      sprintf("%.2fx", max(valid_totals) / min(valid_totals))
    )
  )
  print(cross_method_stats)
}

# ============================================================================
# 4B. SUMMARIZE BY HABITAT
# ============================================================================

# ============================================================================
# 4B. SUMMARIZE BY HABITAT
# ============================================================================

# Get unique habitats from data
habitats <- unique(trees$Habitat)
habitats <- habitats[!is.na(habitats)]

# Create habitat summary for each method
habitat_summary <- data.frame()

for (hab in habitats) {
  # Get trees in this habitat
  hab_idx <- which(trees$Habitat == hab)

  if (length(hab_idx) == 0) next

  # Get area info for this habitat (if available)
  area_info <- habitat_areas[habitat_areas$Habitat == hab, ]
  if (nrow(area_info) == 0) {
    # Default values if habitat not in habitat_areas
    hab_area <- NA
    hab_sigma_area <- NA
  } else {
    hab_area <- area_info$area_ha[1]
    hab_sigma_area <- area_info$sigma_area_ha[1]
  }

  # Calculate totals for each method in this habitat
  for (i in seq_along(carbon_cols)) {
    col <- carbon_cols[i]
    err_col <- error_cols[i]
    method <- method_names[i]

    if (col %in% colnames(results)) {
      # Get carbon values for this habitat
      carbon_vals <- results[[col]][hab_idx]
      error_vals <- if (err_col %in% colnames(results)) results[[err_col]][hab_idx] else rep(NA, length(hab_idx))

      # Total carbon for habitat
      total_carbon <- sum(carbon_vals, na.rm = TRUE)
      # Propagated error (sum in quadrature)
      total_error <- sqrt(sum(error_vals^2, na.rm = TRUE))

      # Calculate carbon per hectare using summary_per_area
      if (!is.na(hab_area) && hab_area > 0) {
        per_ha_result <- summary_per_area(
          input = carbon_vals,
          sigma_input = error_vals,
          area = hab_area,
          sigma_area = hab_sigma_area
        )
        carbon_per_ha <- per_ha_result$total_per_area
        error_per_ha <- per_ha_result$error_per_area
      } else {
        carbon_per_ha <- NA
        error_per_ha <- NA
      }

      # Add row to summary
      hab_row <- data.frame(
        Habitat = hab,
        Method = method,
        N_Trees = sum(!is.na(carbon_vals)),
        Total_Carbon_t = round(total_carbon, 3),
        Total_Error_t = round(total_error, 3),
        Area_ha = hab_area,
        Carbon_per_ha = round(carbon_per_ha, 3),
        Error_per_ha = round(error_per_ha, 3)
      )
      habitat_summary <- rbind(habitat_summary, hab_row)
    }
  }
}

# Print habitat summary
print(habitat_summary)

# Flag if Bunce error is very large
if (any(habitat_summary$Total_Error_t > habitat_summary$Total_Carbon_t, na.rm = TRUE)) {
  bunce_issues <- habitat_summary[habitat_summary$Method == "Bunce" &
                                   habitat_summary$Total_Error_t > habitat_summary$Total_Carbon_t, ]
  if (nrow(bunce_issues) > 0) {
    # NOTE: Bunce method shows very large uncertainty estimates (error > estimate).
    # This is due to the original Bunce (1968) equation lacking published standard errors.
    # For visualizations, error bars are capped at the carbon value.
    # Consider using WCC, BIOMASS, or allodb methods for more reliable uncertainty estimates.
    warning("Bunce method shows very large uncertainty (error > estimate). Consider using WCC, BIOMASS, or allodb.")
  }
}

# Reshape to wide format for easier comparison
if (nrow(habitat_summary) > 0) {
  # Carbon per Hectare by Habitat (Wide Format)
  # Create wide table
  wide_table <- reshape(
    habitat_summary[, c("Habitat", "Method", "Carbon_per_ha")],
    idvar = "Habitat",
    timevar = "Method",
    direction = "wide"
  )
  colnames(wide_table) <- gsub("Carbon_per_ha.", "", colnames(wide_table))
  print(wide_table)

  # Site-Wide Totals

  # Sum carbon totals directly
  site_totals <- aggregate(
    Total_Carbon_t ~ Method,
    data = habitat_summary,
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  # Sum errors in quadrature (sqrt of sum of squared errors)
  site_errors <- aggregate(
    Total_Error_t ~ Method,
    data = habitat_summary,
    FUN = function(x) sqrt(sum(x^2, na.rm = TRUE))
  )

  site_totals$Total_Error_t <- round(site_errors$Total_Error_t, 3)
  site_totals$Total_Carbon_t <- round(site_totals$Total_Carbon_t, 3)
  print(site_totals)
}

# ============================================================================
# 5. SENSITIVITY ANALYSIS
# ============================================================================

# Prepare data for sensitivity analysis
sens_data <- data.frame(
  dbh = trees$DBH,
  height = trees$Height,
  genus = trees$Genus,
  species = trees$Species,
  name = trees$Name,
  type = trees$type,
  stringsAsFactors = FALSE
)

# Run sensitivity analysis (note: TLS is not included as it's measured, not modeled)
sens_result <- tryCatch({
  sensitivity_analysis(
    data = sens_data,
    methods = c("WCC", "BIOMASS", "allodb", "Bunce"),  # Allometric methods only
    coords = wakehurst_coords,
    type = "broadleaf",  # Default for mixed data
    carbon_fraction = 0.5,
    aggregate = TRUE,
    reference = "WCC"
  )
}, error = function(e) {
  warning("Sensitivity analysis error: ", e$message)
  NULL
})

# Note: TLS is excluded from sensitivity analysis because it's a direct measurement,
# not an allometric model. TLS serves as a validation/comparison benchmark.

if (!is.null(sens_result)) {
  # Sensitivity Analysis Results
  print(paste("INTERPRETATION:", sens_result$interpretation))
  print(paste("RECOMMENDATION:", sens_result$recommendation))

  # Sensitivity Metrics
  sens_metrics <- data.frame(
    Metric = c("Sensitivity Level", "CV across methods", "Spread", "Range ratio"),
    Value = c(
      sens_result$sensitivity_level,
      sprintf("%.1f%%", sens_result$sensitivity_metrics$cv_across_methods),
      sprintf("%.1f%%", sens_result$sensitivity_metrics$spread_pct),
      sprintf("%.2fx", sens_result$sensitivity_metrics$range_ratio)
    )
  )
  print(sens_metrics)

  # Method Summary
  print(sens_result$method_summary)
}

# ============================================================================
# 5B. TLS COMPARISON - Allometric Methods vs TLS "Ground Truth"
# ============================================================================

# ============================================================================
# 5B. TLS COMPARISON - Allometric Methods vs TLS "Ground Truth"
# ============================================================================

# Compare each allometric method to TLS
if ("TLS_C_t" %in% colnames(results) && sum(!is.na(results$TLS_C_t)) > 0) {

  tls_total <- sum(results$TLS_C_t, na.rm = TRUE)
  tls_error <- sqrt(sum(results$TLS_C_sig^2, na.rm = TRUE))

  # TLS Total Carbon
  print(sprintf("TLS Total Carbon: %.2f ± %.2f tonnes", tls_total, tls_error))

  # Compare each method to TLS
  allometric_methods <- c("WCC", "BIOMASS", "allodb", "Bunce")
  allometric_cols <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")

  tls_comparison <- data.frame(
    Method = allometric_methods,
    Total_Carbon_t = sapply(allometric_cols, function(col) {
      if (col %in% colnames(results)) sum(results[[col]], na.rm = TRUE) else NA
    }),
    stringsAsFactors = FALSE
  )

  tls_comparison$TLS_Carbon_t <- tls_total
  tls_comparison$Difference_t <- tls_comparison$Total_Carbon_t - tls_total
  tls_comparison$Difference_pct <- round(100 * tls_comparison$Difference_t / tls_total, 1)
  tls_comparison$Ratio_to_TLS <- round(tls_comparison$Total_Carbon_t / tls_total, 3)

  # Allometric Methods vs TLS
  print(tls_comparison)

  # Per-tree correlation with TLS
  tls_correlations <- data.frame()
  for (i in seq_along(allometric_cols)) {
    col <- allometric_cols[i]
    method <- allometric_methods[i]
    if (col %in% colnames(results)) {
      valid <- !is.na(results[[col]]) & !is.na(results$TLS_C_t)
      if (sum(valid) > 2) {
        cor_val <- cor(results[[col]][valid], results$TLS_C_t[valid], use = "complete.obs")
        rmse <- sqrt(mean((results[[col]][valid] - results$TLS_C_t[valid])^2))
        bias <- mean(results[[col]][valid] - results$TLS_C_t[valid])
        tls_correlations <- rbind(tls_correlations, data.frame(
          Method = method, r = round(cor_val, 3),
          RMSE_t = round(rmse, 4), Bias_t = round(bias, 4)
        ))
      }
    }
  }
  # Note: Positive bias means allometric method overestimates relative to TLS
  print(tls_correlations)
}

# ============================================================================
# 6. VISUALIZATIONS
# ============================================================================

# ============================================================================
# 6. VISUALIZATIONS
# ============================================================================

# Color palette matching the Shiny app (now including TLS)
method_colors <- c("WCC" = "#E69F00", "BIOMASS" = "#56B4E9",
                   "allodb" = "#009E73", "Bunce" = "#CC79A7",
                   "TLS" = "#D55E00")  # Orange-red for TLS

# -----------------------------------------------------------------------------
# Plot 1: Bar chart of total carbon by method
# -----------------------------------------------------------------------------

p1 <- ggplot(method_summary[!is.na(method_summary$Total_Carbon_t), ],
             aes(x = reorder(Method, -Total_Carbon_t), y = Total_Carbon_t, fill = Method)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = Total_Carbon_t - Total_Error_t,
                    ymax = Total_Carbon_t + Total_Error_t),
                width = 0.2, color = "gray30") +
  geom_text(aes(label = paste0(Total_Carbon_t, " t")), vjust = -0.5, size = 4) +
  scale_fill_manual(values = method_colors) +
  labs(
    title = "Total Above-Ground Carbon by Allometric Method",
    subtitle = paste0("n = ", nrow(trees), " trees"),
    x = "Method",
    y = "Total Carbon (tonnes)",
    caption = "Error bars show propagated measurement uncertainty"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  ) +
  ylim(0, max(method_summary$Total_Carbon_t, na.rm = TRUE) * 1.15)

print(p1)
ggsave("wakehurst_carbon_by_method.png", p1, width = 10, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# Plot 2: Deviation from reference (WCC)
# -----------------------------------------------------------------------------

# Calculate deviations from WCC
ref_value <- method_summary$Total_Carbon_t[method_summary$Method == "WCC"]
method_summary$Deviation_t <- method_summary$Total_Carbon_t - ref_value
method_summary$Deviation_pct <- 100 * method_summary$Deviation_t / ref_value

plot_data_dev <- method_summary[!is.na(method_summary$Deviation_pct), ]

p2 <- ggplot(plot_data_dev,
             aes(x = reorder(Method, Deviation_pct), y = Deviation_pct,
                 fill = Deviation_pct > 0)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Labels inside bars: hjust=1 for positive (right-aligned inside), hjust=0 for negative (left-aligned inside)
  geom_text(aes(label = paste0(ifelse(Deviation_pct > 0, "+", ""),
                                round(Deviation_pct, 1), "%"),
                hjust = ifelse(Deviation_pct > 0, 1.5, -0.5)),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728"), guide = "none") +
  coord_flip() +
  labs(
    title = "Deviation from WCC Reference Method",
#    subtitle = paste0("Reference: WCC = ", round(ref_value, 1), " tonnes"),
    x = "Method",
    y = "Deviation from WCC (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(p2)

ggsave("wakehurst_deviation_from_wcc.png", p2, width = 10, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# Plot 3: Boxplot of per-tree estimates by method
# -----------------------------------------------------------------------------

# Reshape data for boxplot
plot_data <- data.frame()
for (i in seq_along(carbon_cols)) {
  col <- carbon_cols[i]
  if (col %in% colnames(results)) {
    temp <- data.frame(
      Method = method_names[i],
      Carbon_t = results[[col]]
    )
    plot_data <- rbind(plot_data, temp)
  }
}

# Remove NAs
plot_data <- plot_data[!is.na(plot_data$Carbon_t), ]

p3 <- ggplot(plot_data, aes(x = Method, y = Carbon_t, fill = Method)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_manual(values = method_colors) +
  labs(
    title = "Distribution of Carbon Estimates by Method",
    x = "Method",
    y = "Carbon per Tree (tonnes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(p3)

ggsave("wakehurst_carbon_distribution.png", p3, width = 10, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# Plot 4: Carbon vs DBH by method
# -----------------------------------------------------------------------------

# Reshape for scatter plot
# Use trees$DBH as the x-axis (results may have different column name)
scatter_data <- data.frame(
  dbh = rep(trees$DBH, length(carbon_cols)),
  Method = rep(method_names, each = nrow(results)),
  Carbon_t = unlist(lapply(carbon_cols, function(col) {
    if (col %in% colnames(results)) results[[col]] else rep(NA, nrow(results))
  }))
)
scatter_data <- scatter_data[!is.na(scatter_data$Carbon_t), ]

p4 <- ggplot(scatter_data, aes(x = dbh, y = Carbon_t, color = Method)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  scale_color_manual(values = method_colors) +
  labs(
    title = "Carbon vs DBH by Allometric Method",
    subtitle = "Showing allometric curve differences",
    x = "DBH (cm)",
    y = "Above-Ground Carbon (tonnes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom"
  )

print(p4)

ggsave("wakehurst_carbon_vs_dbh.png", p4, width = 10, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# Plot 4a: Carbon vs DBH including TLS measurements
# -----------------------------------------------------------------------------

# Check if TLS data exists
if ("TLS_C_t" %in% colnames(results) && sum(!is.na(results$TLS_C_t)) > 0) {

  # Create data for allometric methods (with smoothing lines)
  allometric_scatter <- data.frame(
    dbh = rep(trees$DBH, 4),
    Method = rep(c("WCC", "BIOMASS", "allodb", "Bunce"), each = nrow(results)),
    Carbon_t = c(results$WCC_C_t, results$biomass_C_t, results$allodb_C_t, results$Bunce_C_t),
    Type = "Allometric"
  )

  # Create data for TLS (points only, no smoothing)
  tls_scatter <- data.frame(
    dbh = trees$DBH,
    Method = "TLS",
    Carbon_t = results$TLS_C_t,
    Type = "TLS Measured"
  )

  # Combine
  all_scatter <- rbind(allometric_scatter, tls_scatter)
  all_scatter <- all_scatter[!is.na(all_scatter$Carbon_t) & !is.na(all_scatter$dbh), ]

  # Separate data for different geom layers
  allometric_data <- all_scatter[all_scatter$Type == "Allometric", ]
  tls_data <- all_scatter[all_scatter$Type == "TLS Measured", ]

  p4a <- ggplot() +
    # Allometric methods: points + smoothed lines
    geom_point(data = allometric_data,
               aes(x = dbh, y = Carbon_t, color = Method),
               alpha = 0.3, size = 1.5) +
    geom_smooth(data = allometric_data,
                aes(x = dbh, y = Carbon_t, color = Method),
                method = "loess", se = FALSE, linewidth = 1) +
    # TLS: points only (larger, more visible)
    geom_point(data = tls_data,
               aes(x = dbh, y = Carbon_t, color = Method),
               alpha = 0.7, size = 2.5, shape = 17) +  # Triangle for TLS
    scale_color_manual(values = method_colors) +
    coord_cartesian(xlim = c(60, 130)) +  # Limit x-axis to 60-130 cm DBH
    labs(
      title = "Carbon vs DBH: Allometric Methods and TLS Measurements",
      subtitle = "Focused on larger trees (DBH 60-130 cm)",
      x = "DBH (cm)",
      y = "Above-Ground Carbon (tonnes)" #,
      # caption = "TLS provides direct volume measurements; allometric methods estimate from DBH/height"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom"
    )

  print(p4a)

  ggsave("wakehurst_carbon_vs_dbh_with_tls.png", p4a, width = 10, height = 6, dpi = 300)
}

# -----------------------------------------------------------------------------
# Plot 4b: Error vs DBH - Shows uncertainty increases with tree size
# -----------------------------------------------------------------------------

# Creating Error vs DBH plot

# Reshape error data for plotting
error_plot_data <- data.frame()
for (i in seq_along(error_cols)) {
  err_col <- error_cols[i]
  if (err_col %in% colnames(results) && sum(!is.na(results[[err_col]])) > 0) {
    temp <- data.frame(
      DBH = trees$DBH,
      Error_t = results[[err_col]],
      Method = method_names[i],
      stringsAsFactors = FALSE
    )
    error_plot_data <- rbind(error_plot_data, temp)
  }
}

# Remove NAs and exclude Bunce (its error is too large and skews the plot)
error_plot_data <- error_plot_data[!is.na(error_plot_data$Error_t) &
                                    !is.na(error_plot_data$DBH) &
                                    error_plot_data$Method != "Bunce", ]

if (nrow(error_plot_data) > 0) {

  # Plot: Error vs DBH by method (excluding Bunce)
  p4b <- ggplot(error_plot_data, aes(x = DBH, y = Error_t, color = Method)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 1) +
    scale_color_manual(values = method_colors) +
    labs(
      title = "Uncertainty with Tree Size",
      subtitle = "Propagated error (σ) vs DBH",
      x = "Diameter at Breast Height (cm)",
      y = "Propagated Error σ (tonnes)",
      caption = "Larger trees have higher absolute uncertainty due to error propagation through allometric equations"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom"
    )

  print(p4b)

  ggsave("wakehurst_error_vs_dbh.png", p4b, width = 10, height = 6, dpi = 300)

  # Also create a relative error (CV) vs DBH plot to show if relative uncertainty is constant
  cv_plot_data <- data.frame()
  for (i in seq_along(carbon_cols)) {
    c_col <- carbon_cols[i]
    err_col <- error_cols[i]
    if (c_col %in% colnames(results) && err_col %in% colnames(results)) {
      valid <- !is.na(results[[c_col]]) & !is.na(results[[err_col]]) & results[[c_col]] > 0
      if (sum(valid) > 0) {
        temp <- data.frame(
          DBH = trees$DBH[valid],
          CV_pct = 100 * results[[err_col]][valid] / results[[c_col]][valid],
          Method = method_names[i],
          stringsAsFactors = FALSE
        )
        cv_plot_data <- rbind(cv_plot_data, temp)
      }
    }
  }

  # Filter extreme CVs and exclude Bunce for better visualization
  cv_plot_data <- cv_plot_data[cv_plot_data$CV_pct < 100 &
                                cv_plot_data$Method != "Bunce", ]

  if (nrow(cv_plot_data) > 0) {
    p4c <- ggplot(cv_plot_data, aes(x = DBH, y = CV_pct, color = Method)) +
      geom_point(alpha = 0.4, size = 1.5) +
      geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 1) +
      scale_color_manual(values = method_colors) +
      labs(
        title = "Relative Uncertainty (CV%) vs Tree Size",
        subtitle = "Coefficient of Variation by method (Bunce excluded)",
        x = "Diameter at Breast Height (cm)",
        y = "Coefficient of Variation (%)",
#        caption = "Relatively constant CV suggests proportional error; increasing CV suggests larger trees have higher relative uncertainty"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        legend.position = "bottom"
      ) +
      ylim(0, min(50, max(cv_plot_data$CV_pct, na.rm = TRUE) * 1.1))

    print(p4c)

    ggsave("wakehurst_cv_vs_dbh.png", p4c, width = 10, height = 6, dpi = 300)
  }
}

# -----------------------------------------------------------------------------
# Plot 4d: Large Trees Dominate Carbon - Shows carbon concentration in big trees
# -----------------------------------------------------------------------------

# Creating 'Large Trees Dominate Carbon' plots

# Use WCC carbon for this analysis (or first available method)
carbon_col <- "WCC_C_t"
if (!carbon_col %in% colnames(results)) {
  carbon_col <- carbon_cols[carbon_cols %in% colnames(results)][1]
}

if (!is.null(carbon_col) && carbon_col %in% colnames(results)) {

  # Create analysis dataframe
  tree_carbon_df <- data.frame(
    DBH = trees$DBH,
    Carbon_t = results[[carbon_col]],
    stringsAsFactors = FALSE
  )
  tree_carbon_df <- tree_carbon_df[!is.na(tree_carbon_df$Carbon_t) & !is.na(tree_carbon_df$DBH), ]

  # Sort by DBH (largest first)
  tree_carbon_df <- tree_carbon_df[order(-tree_carbon_df$DBH), ]

  # Calculate cumulative carbon (from largest to smallest)
  tree_carbon_df$Cumulative_Carbon <- cumsum(tree_carbon_df$Carbon_t)
  tree_carbon_df$Cumulative_Pct <- 100 * tree_carbon_df$Cumulative_Carbon / sum(tree_carbon_df$Carbon_t)
  tree_carbon_df$Tree_Rank <- 1:nrow(tree_carbon_df)
  tree_carbon_df$Tree_Pct <- 100 * tree_carbon_df$Tree_Rank / nrow(tree_carbon_df)

  # Find key statistics
  total_trees <- nrow(tree_carbon_df)
  total_carbon <- sum(tree_carbon_df$Carbon_t)

  # How many trees for 50% of carbon?
  trees_for_50pct <- min(which(tree_carbon_df$Cumulative_Pct >= 50))
  pct_trees_for_50pct <- round(100 * trees_for_50pct / total_trees, 1)

  # How much carbon in top 10% of trees?
  top_10pct_idx <- ceiling(0.1 * total_trees)
  carbon_in_top_10pct <- round(tree_carbon_df$Cumulative_Pct[top_10pct_idx], 1)

  # How much carbon in top 20% of trees?
  top_20pct_idx <- ceiling(0.2 * total_trees)
  carbon_in_top_20pct <- round(tree_carbon_df$Cumulative_Pct[top_20pct_idx], 1)

  # Key findings about large trees
  large_tree_findings <- data.frame(
    Finding = c(
      sprintf("Top 10%% of trees (%d trees)", top_10pct_idx),
      sprintf("Top 20%% of trees (%d trees)", top_20pct_idx),
      sprintf("Trees needed for 50%% carbon")
    ),
    Value = c(
      sprintf("contain %.1f%% of total carbon", carbon_in_top_10pct),
      sprintf("contain %.1f%% of total carbon", carbon_in_top_20pct),
      sprintf("%.1f%% (%d trees)", pct_trees_for_50pct, trees_for_50pct)
    )
  )
  print(large_tree_findings)

  # Plot 1: Cumulative carbon curve
  p4d <- ggplot(tree_carbon_df, aes(x = Tree_Pct, y = Cumulative_Pct)) +
    geom_line(color = "#E69F00", linewidth = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 50, linetype = "dotted", color = "red", linewidth = 0.8) +
    geom_vline(xintercept = pct_trees_for_50pct, linetype = "dotted", color = "red", linewidth = 0.8) +
    annotate("text", x = pct_trees_for_50pct + 2, y = 45,
             label = sprintf("%.1f%% of trees\nhold 50%% of carbon", pct_trees_for_50pct),
             hjust = 0, size = 4, color = "red") +
    annotate("text", x = 10, y = carbon_in_top_10pct + 5,
             label = sprintf("Top 10%% of trees\nhold %.0f%% of carbon", carbon_in_top_10pct),
             hjust = 0, size = 4, color = "#0072B2") +
    geom_point(aes(x = 10, y = carbon_in_top_10pct), color = "#0072B2", size = 3) +
    labs(
      title = "Large Trees Dominate Carbon Storage",
      subtitle = "Cumulative carbon curve (trees ranked largest to smallest by DBH)",
      x = "Cumulative % of Trees (largest first)",
      y = "Cumulative % of Total Carbon",
      caption = "Dashed line = equal distribution; curve above line = carbon concentrated in large trees"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40")
    ) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))

  print(p4d)

  ggsave("wakehurst_large_trees_dominate.png", p4d, width = 10, height = 8, dpi = 300)

  # Plot 2: Carbon by DBH class (bar chart)
  tree_carbon_df$DBH_Class <- cut(tree_carbon_df$DBH,
                                   breaks = c(0, 10, 20, 30, 40, 50, 75, 100, Inf),
                                   labels = c("0-10", "10-20", "20-30", "30-40",
                                              "40-50", "50-75", "75-100", ">100"),
                                   right = FALSE)

  # Summarize by DBH class
  dbh_summary <- aggregate(
    Carbon_t ~ DBH_Class,
    data = tree_carbon_df,
    FUN = function(x) c(Total = sum(x), Count = length(x), Mean = mean(x))
  )
  dbh_summary <- do.call(data.frame, dbh_summary)
  colnames(dbh_summary) <- c("DBH_Class", "Total_Carbon", "Tree_Count", "Mean_Carbon")
  dbh_summary$Pct_Carbon <- round(100 * dbh_summary$Total_Carbon / total_carbon, 1)
  dbh_summary$Pct_Trees <- round(100 * dbh_summary$Tree_Count / total_trees, 1)

  # Create comparison data for dodged bar chart
  dbh_long <- data.frame(
    DBH_Class = rep(dbh_summary$DBH_Class, 2),
    Percentage = c(dbh_summary$Pct_Trees, dbh_summary$Pct_Carbon),
    Type = rep(c("% of Trees", "% of Carbon"), each = nrow(dbh_summary))
  )

  p4e <- ggplot(dbh_long, aes(x = DBH_Class, y = Percentage, fill = Type)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    geom_text(aes(label = paste0(round(Percentage, 0), "%")),
              position = position_dodge(width = 0.8), vjust = -0.3, size = 3) +
    scale_fill_manual(values = c("% of Trees" = "#56B4E9", "% of Carbon" = "#E69F00")) +
    labs(
      title = "Carbon Distribution by Tree Size Class",
#      subtitle = "Large trees: few in number but high carbon contribution",
      x = "DBH Class (cm)",
      y = "Percentage (%)",
      fill = "",
#      caption = "Large DBH classes have fewer trees but disproportionately more carbon"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom"
    )

  print(p4e)

  ggsave("wakehurst_carbon_by_dbh_class.png", p4e, width = 10, height = 6, dpi = 300)

  # Carbon Distribution by DBH Class
  print(dbh_summary[, c("DBH_Class", "Tree_Count", "Pct_Trees", "Total_Carbon", "Pct_Carbon")])
}

# -----------------------------------------------------------------------------
# Plot 5: Method correlation heatmap
# -----------------------------------------------------------------------------

# Calculate correlation matrix
carbon_matrix <- results[, carbon_cols[carbon_cols %in% colnames(results)]]
colnames(carbon_matrix) <- method_names[carbon_cols %in% colnames(results)]
cor_matrix <- cor(carbon_matrix, use = "pairwise.complete.obs")

# Convert to long format for ggplot
cor_long <- as.data.frame(as.table(cor_matrix))
colnames(cor_long) <- c("Method1", "Method2", "Correlation")

p5 <- ggplot(cor_long, aes(x = Method1, y = Method2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 5) +
  scale_fill_gradient2(low = "#d62728", mid = "white", high = "#2ca02c",
                       midpoint = 0.5, limits = c(0, 1)) +
  labs(
    title = "Correlation Between Allometric Methods",
    subtitle = "Per-tree carbon estimates",
    x = "", y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p5)

ggsave("wakehurst_method_correlation.png", p5, width = 8, height = 6, dpi = 300)

# -----------------------------------------------------------------------------
# Plot 5b: TLS vs Allometric Methods - Scatter plots
# -----------------------------------------------------------------------------

if ("TLS_C_t" %in% colnames(results) && sum(!is.na(results$TLS_C_t)) > 0) {

  # Create comparison data for TLS vs each method
  tls_compare_data <- data.frame()
  allometric_cols_no_tls <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")
  allometric_names <- c("WCC", "BIOMASS", "allodb", "Bunce")

  for (i in seq_along(allometric_cols_no_tls)) {
    col <- allometric_cols_no_tls[i]
    if (col %in% colnames(results)) {
      temp <- data.frame(
        TLS = results$TLS_C_t,
        Allometric = results[[col]],
        Method = allometric_names[i]
      )
      tls_compare_data <- rbind(tls_compare_data, temp)
    }
  }

  # Remove NAs
  tls_compare_data <- tls_compare_data[!is.na(tls_compare_data$TLS) &
                                        !is.na(tls_compare_data$Allometric), ]

  if (nrow(tls_compare_data) > 0) {
    # Calculate max for equal axis limits
    max_val <- max(c(tls_compare_data$TLS, tls_compare_data$Allometric), na.rm = TRUE)

    p5b <- ggplot(tls_compare_data, aes(x = TLS, y = Allometric, color = Method)) +
      geom_point(alpha = 0.5, size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
      scale_color_manual(values = method_colors[1:4]) +
      coord_fixed(ratio = 1, xlim = c(0, max_val * 1.1), ylim = c(0, max_val * 1.1)) +
      labs(
        title = "Allometric Methods vs TLS Measurements",
        subtitle = "Dashed line = 1:1 (perfect agreement)",
        x = "TLS Carbon (tonnes)",
        y = "Allometric Carbon (tonnes)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        legend.position = "bottom"
      )

    print(p5b)

    ggsave("wakehurst_tls_comparison.png", p5b, width = 10, height = 10, dpi = 300)

    # Faceted version for clearer comparison
    p5c <- ggplot(tls_compare_data, aes(x = TLS, y = Allometric)) +
      geom_point(alpha = 0.5, size = 2, color = "#0072B2") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      geom_smooth(method = "lm", se = TRUE, color = "#D55E00", fill = "#D55E00", alpha = 0.2) +
      facet_wrap(~ Method, scales = "free") +
      labs(
        title = "Allometric Methods vs TLS: Per-Method Comparison",
        subtitle = "Red dashed = 1:1 line; Orange = linear fit",
        x = "TLS Carbon (tonnes)",
        y = "Allometric Carbon (tonnes)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        strip.text = element_text(face = "bold")
      )

    print(p5c)

    ggsave("wakehurst_tls_faceted.png", p5c, width = 12, height = 10, dpi = 300)
  }
}

# -----------------------------------------------------------------------------
# Plot 6: Carbon by Habitat and Method
# -----------------------------------------------------------------------------

if (nrow(habitat_summary) > 0) {

  # Cap error bars to prevent them from skewing the plot
  # Error capped at the carbon value (so ymin >= 0 and ymax <= 2 * carbon)
  habitat_summary_plot <- habitat_summary[!is.na(habitat_summary$Total_Carbon_t), ]
  habitat_summary_plot$Error_Capped <- pmin(habitat_summary_plot$Total_Error_t,
                                             habitat_summary_plot$Total_Carbon_t)
  habitat_summary_plot$ymin <- pmax(0, habitat_summary_plot$Total_Carbon_t - habitat_summary_plot$Error_Capped)
  habitat_summary_plot$ymax <- habitat_summary_plot$Total_Carbon_t + habitat_summary_plot$Error_Capped

  # Flag if error was capped (for caption)
  bunce_capped <- any(habitat_summary_plot$Total_Error_t > habitat_summary_plot$Total_Carbon_t &
                       habitat_summary_plot$Method == "Bunce")

  p6 <- ggplot(habitat_summary_plot,
               aes(x = Habitat, y = Total_Carbon_t, fill = Method)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    geom_errorbar(
      aes(ymin = ymin, ymax = ymax),
      position = position_dodge(width = 0.8),
      width = 0.2, color = "gray30"
    ) +
    scale_fill_manual(values = method_colors) +
    labs(
      title = "Total Carbon by Habitat and Allometric Method",
      subtitle = "Comparing methods across different habitat types",
      x = "Habitat",
      y = "Total Carbon (tonnes)",
      caption = if(bunce_capped) "Error bars capped at carbon value for visualization (Bunce error exceeds estimate)"
                else "Error bars show propagated measurement uncertainty"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom"
    )

  print(p6)

  ggsave("wakehurst_carbon_by_habitat.png", p6, width = 12, height = 6, dpi = 300)

  # -----------------------------------------------------------------------------
  # Plot 7: Carbon per Hectare by Habitat
  # -----------------------------------------------------------------------------

  # Only plot if we have per-hectare data
  has_per_ha <- sum(!is.na(habitat_summary$Carbon_per_ha)) > 0

  if (has_per_ha) {
    # Cap error bars for per-hectare plot too
    habitat_perha_plot <- habitat_summary[!is.na(habitat_summary$Carbon_per_ha), ]
    habitat_perha_plot$Error_Capped <- pmin(habitat_perha_plot$Error_per_ha,
                                             habitat_perha_plot$Carbon_per_ha)
    habitat_perha_plot$ymin <- pmax(0, habitat_perha_plot$Carbon_per_ha - habitat_perha_plot$Error_Capped)
    habitat_perha_plot$ymax <- habitat_perha_plot$Carbon_per_ha + habitat_perha_plot$Error_Capped

    bunce_capped_perha <- any(habitat_perha_plot$Error_per_ha > habitat_perha_plot$Carbon_per_ha &
                               habitat_perha_plot$Method == "Bunce")

    p7 <- ggplot(habitat_perha_plot,
                 aes(x = Habitat, y = Carbon_per_ha, fill = Method)) +
      geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
      geom_errorbar(
        aes(ymin = ymin, ymax = ymax),
        position = position_dodge(width = 0.8),
        width = 0.2, color = "gray30"
      ) +
      scale_fill_manual(values = method_colors) +
      labs(
        title = "Carbon Density by Habitat and Allometric Method",
        subtitle = "Tonnes of carbon per hectare",
        x = "Habitat",
        y = "Carbon (t/ha)",
        caption = if(bunce_capped_perha) "Error bars capped at carbon value for visualization (Bunce error exceeds estimate)"
                  else "Error bars include propagated uncertainty from measurements and area"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        legend.position = "bottom"
      )

    print(p7)

    ggsave("wakehurst_carbon_per_ha.png", p7, width = 12, height = 6, dpi = 300)
  }

  # -----------------------------------------------------------------------------
  # Plot 8: Faceted comparison - Carbon per hectare by method, faceted by habitat
  # -----------------------------------------------------------------------------

  if (has_per_ha) {
    # Use the already-capped data from Plot 7 (habitat_perha_plot)
    p8 <- ggplot(habitat_perha_plot,
                 aes(x = reorder(Method, -Carbon_per_ha), y = Carbon_per_ha, fill = Method)) +
      geom_col(alpha = 0.8) +
      geom_errorbar(
        aes(ymin = ymin, ymax = ymax),
        width = 0.2, color = "gray30"
      ) +
      geom_text(aes(label = paste0(round(Carbon_per_ha, 1), " t/ha")),
                vjust = -0.5, size = 3) +
      scale_fill_manual(values = method_colors) +
      facet_wrap(~ Habitat, scales = "free_y") +
      labs(
        title = "Carbon Density Comparison Across Habitats",
        subtitle = "Method comparison within each habitat type",
        x = "Method",
        y = "Carbon (t/ha)",
        caption = if(bunce_capped_perha) "Error bars capped for visualization (Bunce error exceeds estimate)" else NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 12)
      )

    print(p8)

    ggsave("wakehurst_carbon_faceted_habitat.png", p8, width = 12, height = 8, dpi = 300)
  }
}

# ============================================================================
# 7. EXPORT RESULTS
# ============================================================================

# ============================================================================
# 7. EXPORT RESULTS
# ============================================================================

# Export full results
write.csv(results, "wakehurst_carbon_results.csv", row.names = FALSE)

# Export method summary
write.csv(method_summary, "wakehurst_method_summary.csv", row.names = FALSE)

# Export habitat summary
if (nrow(habitat_summary) > 0) {
  write.csv(habitat_summary, "wakehurst_habitat_summary.csv", row.names = FALSE)
}

# ============================================================================
# 8. FINAL SUMMARY
# ============================================================================

# ============================================================================
# 8. FINAL SUMMARY
# ============================================================================

# Analysis summary
analysis_summary <- data.frame(
  Item = c("Trees analyzed", "Location", "Methods compared", "Habitats",
           "Total carbon (WCC)", "WCC uncertainty"),
  Value = c(
    nrow(trees),
    "Wakehurst Place, Sussex, UK",
    paste(method_names, collapse = ", "),
    paste(habitats, collapse = ", "),
    sprintf("%.2f tonnes", method_summary$Total_Carbon_t[method_summary$Method == "WCC"]),
    sprintf("%.2f tonnes", method_summary$Total_Error_t[method_summary$Method == "WCC"])
  )
)
print(analysis_summary)

# Cross-method variation
if (length(valid_totals) > 1) {
  method_variation <- data.frame(
    Metric = c("CV across methods", "Range"),
    Value = c(
      sprintf("%.1f%%", 100 * sd(valid_totals) / mean(valid_totals)),
      sprintf("%.2f - %.2f tonnes", min(valid_totals), max(valid_totals))
    )
  )
  print(method_variation)
}

# Sensitivity level
if (!is.null(sens_result)) {
  print(sprintf("Sensitivity level: %s", sens_result$sensitivity_level))
}

# Carbon by Habitat (WCC method)
if (nrow(habitat_summary) > 0) {
  wcc_hab <- habitat_summary[habitat_summary$Method == "WCC",
                             c("Habitat", "Carbon_per_ha", "Error_per_ha", "Total_Carbon_t")]
  print(wcc_hab)
}

# Script completed

