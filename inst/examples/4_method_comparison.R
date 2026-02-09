# ==============================================================================
# Example 4: Comparing All Allometric Methods
# ==============================================================================
# This example demonstrates how to compare different allometric methods
# to understand how method choice affects carbon estimates.
#
# - Use allometries() to run all methods
# - Quantify method differences
# - Visualise comparisons
# - Rich output
# ==============================================================================

library(TreeCarbon)

# Check and install optional packages if needed
if (!requireNamespace("BIOMASS", quietly = TRUE)) {
  cat("BIOMASS package not found. Attempting to install...\n")
  install.packages("BIOMASS")
  if (!requireNamespace("BIOMASS", quietly = TRUE)) {
    stop("Failed to install BIOMASS package. Please install manually: install.packages('BIOMASS')")
  }
}

if (!requireNamespace("allodb", quietly = TRUE)) {
  cat("allodb package not found. Attempting to install from GitHub...\n")
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("ropensci/allodb")
  if (!requireNamespace("allodb", quietly = TRUE)) {
    stop("Failed to install allodb package. Please install manually: remotes::install_github('ropensci/allodb')")
  }
}

# ==== Single Tree Method Comparison ===========================================

# Compare all methods for a single tree
comparison <- allometries(genus = "Quercus",    species = "robur",
                          dbh = 45,             height = 18,
                          coords = c(-0.29, 51.48),  # UK coordinates
                          returnv = "AGC")

print(comparison[, c("genus", "species", "dbh", "height",
                      "WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")])

# Extract estimates
estimates <- c(
  WCC = comparison$WCC_C_t,
  BIOMASS = comparison$biomass_C_t,
  allodb = comparison$allodb_C_t,
  Bunce = comparison$Bunce_C_t
)
estimates <- estimates[!is.na(estimates)]

cat(sprintf("Mean: %.3f t C\n", mean(estimates)))
cat(sprintf("SD: %.3f t C\n", sd(estimates)))
cat(sprintf("CV: %.1f%%\n", 100 * sd(estimates) / mean(estimates)))
cat(sprintf("Range: %.3f - %.3f t C\n", min(estimates), max(estimates)))
cat(sprintf("Spread: %.1f%% of mean\n",
            100 * (max(estimates) - min(estimates)) / mean(estimates)))

# ==== Batch Method Comparison =================================================

# Sample tree data
trees <- data.frame(
  genus = c("Quercus", "Fagus", "Fraxinus", "Betula"),
  species = c("robur", "sylvatica", "excelsior", "pendula"),
  dbh = c(45, 38, 52, 28),
  height = c(18, 22, 24, 15)
)

# Compare all methods for all trees
batch_comparison <- allometries(
  genus = trees$genus,
  species = trees$species,
  dbh = trees$dbh,
  height = trees$height,
  coords = c(-0.29, 51.48),
  returnv = "AGC"
)

print(batch_comparison[, c("genus", "species", "dbh",
                           "WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")])

# ==== Method Agreement Analysis ===============================================

# Calculate cross-method statistics
methods <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")

batch_comparison$mean_estimate <- apply(
  batch_comparison[, methods], 1, mean, na.rm = TRUE
)
batch_comparison$sd_estimate <- apply(
  batch_comparison[, methods], 1, sd, na.rm = TRUE
)
batch_comparison$cv_pct <- 100 * batch_comparison$sd_estimate / batch_comparison$mean_estimate

print(batch_comparison[, c("genus", "species", "dbh", "mean_estimate", "cv_pct")])

cat(sprintf("Mean CV across methods: %.1f%%\n",
            mean(batch_comparison$cv_pct, na.rm = TRUE)))
cat(sprintf("Range of CV: %.1f%% - %.1f%%\n",
            min(batch_comparison$cv_pct, na.rm = TRUE),
            max(batch_comparison$cv_pct, na.rm = TRUE)))

# ==== Using compare_allometries() ==============================================

if (exists("comparison")) {
  # Detailed comparison statistics
  comp_stats <- compare_allometries(
    comparison,
    reference = "WCC"  # Use WCC as reference method
  )

  print(comp_stats)
}

# ==== Visualizing Method Differences ===========================================

if (exists("batch_comparison") && requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Reshape data for plotting
  plot_data <- data.frame(
    tree_id = rep(1:nrow(batch_comparison), 4),
    dbh = rep(batch_comparison$dbh, 4),
    method = rep(c("WCC", "BIOMASS", "allodb", "Bunce"),
                 each = nrow(batch_comparison)),
    carbon = c(
      batch_comparison$WCC_C_t,
      batch_comparison$biomass_C_t,
      batch_comparison$allodb_C_t,
      batch_comparison$Bunce_C_t
    )
  )

  # Remove NA values
  plot_data <- plot_data[!is.na(plot_data$carbon), ]

  # Create comparison plot
  p <- ggplot(plot_data, aes(x = dbh, y = carbon, color = method, shape = method)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
    labs(
      title = "Carbon Estimates by Method vs DBH",
      x = "DBH (cm)",
      y = "Above-Ground Carbon (tonnes)",
      color = "Method",
      shape = "Method"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set2")

  print(p)
}

# ==== Rich Output for Method Comparison ========================================

# Note: allometries() doesn't currently support rich_output, but you can
# get rich output from individual methods and compare

# Get rich output from WCC
wcc_rich <- fc_agc(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  rich_output = TRUE
)

print(wcc_rich)

# Get rich output from Bunce
bunce_rich <- Bunce(
  name = "Oak",
  dbh = 45,
  rich_output = TRUE
)

print(bunce_rich)
