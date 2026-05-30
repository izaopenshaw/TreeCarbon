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


## ----single-wcc--------------------------------------------------------------------------------------------
# A single oak tree (timber height estimated via timber_ratio = 0.85)
result_wcc <- fc_agc(name = "Oak", dbh = 45, height = 18, type = "broadleaf")

print(result_wcc)


## ----single-wcc-rich---------------------------------------------------------------------------------------
result_rich <- fc_agc(name = "Oak", dbh = 45, height = 18, type = "broadleaf",
                      rich_output = TRUE)

# View the complete result
print(result_rich)


## ----single-wcc-error--------------------------------------------------------------------------------------
result_error <- fc_agc_error(name = "Oak", dbh = 45, height = 18,
                             type = "broadleaf",
                             re_dbh = 0.025, re_h = 0.05)

print(result_error[, c("name", "AGC_WCC_t", "sig_AGC")])

# Calculate 95% confidence interval
agc <- result_error$AGC_WCC_t
sigma <- result_error$sig_AGC
cat(sprintf("\nAGC: %.3f ± %.3f t (95%% CI: [%.3f, %.3f])\n",
            agc, 1.96 * sigma, agc - 1.96*sigma, agc + 1.96*sigma))


## ----single-bunce------------------------------------------------------------------------------------------
result_bunce <- Bunce(name = "Oak", dbh = 45, re_dbh = 0.025)

print(result_bunce)


## ----single-bunce-rich-------------------------------------------------------------------------------------
# With rich metadata
result_bunce_rich <- Bunce("Oak", dbh = 45, re_dbh = 0.025, rich_output = TRUE)
print(result_bunce_rich)


## ----single-biomass, eval=requireNamespace("BIOMASS", quietly = TRUE)--------------------------------------
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


## ----single-biomass-mc, eval=requireNamespace("BIOMASS", quietly = TRUE)-----------------------------------
result_biomass_mc <- BIOMASS(
  dbh = 45,
  height = 18,
  genus = "Quercus",
  species = "robur",
  coords = coords,
  uncertainty = TRUE,
  n_mc = 1000
)
## doesnt work due to uncertainty
print(result_biomass_mc[, c("AGB_Biomass_kg", "AGB_sd_kg", "AGB_CV_pct",
                             "AGB_CI_low_kg", "AGB_CI_high_kg")])


## ----single-allodb, eval=requireNamespace("allodb", quietly = TRUE)----------------------------------------
result_allodb <- allodb(
  dbh = 45,
  genus = "Quercus",
  species = "robur",
  coords = c(-77.0, 38.9)  # Uses North American equations by default
)

print(result_allodb)


## ----batch-data--------------------------------------------------------------------------------------------
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


## ----batch-wcc---------------------------------------------------------------------------------------------
# Apply WCC to all trees
wcc_results <- fc_agc(name = trees$species, dbh = trees$dbh,
                      height = trees$height, type = trees$type)

# Add tree IDs
wcc_results$tree_id <- trees$tree_id

# Summary
cat("=== WCC Results Summary ===\n")
cat(sprintf("Total AGC: %.2f tonnes\n", sum(wcc_results$AGC_WCC_t, na.rm = TRUE)))
cat(sprintf("Mean AGC per tree: %.3f tonnes\n", mean(wcc_results$AGC_WCC_t, na.rm = TRUE)))
cat(sprintf("Range: %.3f - %.3f tonnes\n", 
            min(wcc_results$AGC_WCC_t, na.rm = TRUE),
            max(wcc_results$AGC_WCC_t, na.rm = TRUE)))


## ----batch-bunce-------------------------------------------------------------------------------------------
bunce_results <- Bunce(name = trees$species, dbh = trees$dbh,
                       re_dbh = 0.025)

bunce_results$tree_id <- trees$tree_id

# Convert to tonnes (Bunce returns kg)
bunce_results$biomass_t <- bunce_results$biomass / 1000
bunce_results$carbon_t <- bunce_results$biomass_t * 0.5  # 50% carbon fraction

cat("=== Bunce Results Summary ===\n")
cat(sprintf("Total Carbon: %.2f tonnes\n", sum(bunce_results$carbon_t, na.rm = TRUE)))


## ----batch-biomass, eval=requireNamespace("BIOMASS", quietly = TRUE)---------------------------------------
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

cat("=== BIOMASS Results Summary ===\n")
cat(sprintf("Total Carbon: %.2f tonnes\n", sum(biomass_results$carbon_t, na.rm = TRUE)))
cat(sprintf("Mean CV: %.1f%%\n", mean(biomass_results$AGB_CV_pct, na.rm = TRUE)))


## ----compare-single, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Compare all methods for a single oak tree
comparison <- allometries(genus = "Quercus", species = "robur", dbh = 45,
                          height = 18, coords = c(-0.2915, 51.4787), 
                          returnv = "AGC")

print(comparison)


## ----compare-detailed, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Get allometries results first
results <- allometries(genus = "Quercus", species = "robur", dbh = 45,
                       height = 18, coords = c(-0.2915, 51.4787),
                       returnv = "AGC")

# Run comparison
comp <- compare_allometries(results, reference = "WCC")

# View the comparison table
print(comp)

# Summary statistics
summary(comp)


## ----compare-batch, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Compare methods across all trees - allometries() handles vectors directly
batch_results <- allometries(genus = trees$genus, 
                             species = trees$species_epithet,
                             dbh = trees$dbh,
                             height = trees$height,
                             coords = c(-0.2915, 51.4787),
                             returnv = "AGC")

# Add tree IDs
batch_results$tree_id <- seq_len(nrow(batch_results))

cat("=== Batch Results (first 6 rows) ===\n")
print(head(batch_results))


## ----plot-comparison, fig.height=6, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
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
  ggplot(plot_data, aes(x = dbh, y = carbon, color = method, shape = method)) +
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
}


## ----method-stats, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
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


## ----pairwise-diff, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
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


## ----agreement, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
if (exists("batch_results") && nrow(batch_results) > 0) {
  
  # Calculate CV across methods for each tree
  methods <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")
  
  batch_results$cross_method_mean <- apply(batch_results[, methods], 1, mean, na.rm = TRUE)
  batch_results$cross_method_sd <- apply(batch_results[, methods], 1, sd, na.rm = TRUE)
  batch_results$cross_method_cv <- 100 * batch_results$cross_method_sd / batch_results$cross_method_mean
  
  cat("=== Cross-Method Agreement ===\n\n")
  cat(sprintf("Mean CV across methods: %.1f%%\n", mean(batch_results$cross_method_cv, na.rm = TRUE)))
  cat(sprintf("Range of CV: %.1f%% - %.1f%%\n", 
              min(batch_results$cross_method_cv, na.rm = TRUE),
              max(batch_results$cross_method_cv, na.rm = TRUE)))
  
  # Trees with high disagreement
  high_cv <- batch_results[batch_results$cross_method_cv > 30, c("tree_id", "genus", "dbh", "cross_method_cv")]
  if (nrow(high_cv) > 0) {
    cat("\nTrees with high method disagreement (CV > 30%):\n")
    print(high_cv)
  } else {
    cat("\nAll trees show reasonable agreement across methods (CV < 30%)\n")
  }
}


## ----sensitivity-single, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
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


## ----sensitivity-batch, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
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


## ----sensitivity-plot, fig.height=5, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Bar chart of method totals
plot(sens_plot, type = "comparison")


## ----sensitivity-deviation, fig.height=4, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Deviation from reference method
plot(sens_plot, type = "deviation")


## ----sensitivity-per-tree, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
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

cat("Trees ranked by method sensitivity (highest first):\n")
print(head(tree_sens, 10))


## ----sensitivity-investigate, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
# Check which trees have highest disagreement
if (exists("sens_trees")) {
  high_sens <- sens_trees$by_tree[sens_trees$by_tree$cv_pct > 20, ]
  
  if (nrow(high_sens) > 0) {
    cat("Trees with CV > 20%:\n")
    # Merge with original data to see characteristics
    high_sens_full <- merge(high_sens[, c("tree_id", "cv_pct", "spread_pct")],
                            forest_plot, by.x = "tree_id", by.y = "tree_id")
    print(high_sens_full[, c("tree_id", "name", "dbh", "height", "cv_pct")])
  }
}


## ----mc-example, eval=requireNamespace("BIOMASS", quietly = TRUE)------------------------------------------
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


## ----variance-decomp, fig.height=4, eval=requireNamespace("BIOMASS", quietly = TRUE)-----------------------
if (!is.null(mc_result$decomposition)) {
  by_source <- mc_result$decomposition$by_source
  
  ggplot(by_source, aes(x = reorder(source, contribution_pct), 
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
}


## ----method-choice-unc, eval=requireNamespace("BIOMASS", quietly = TRUE)-----------------------------------
# Estimates from different methods for the same tree
estimates <- c(
  WCC = 0.85,
  BIOMASS = 0.78,
  allodb = 0.92,
  Bunce = 0.72
)

method_unc <- method_choice_uncertainty(estimates)

cat("=== Method Choice Uncertainty ===\n\n")
cat(sprintf("Consensus estimate: %.3f t\n", method_unc$consensus))
cat(sprintf("Spread: %.3f t (%.1f%%)\n", method_unc$spread, method_unc$spread_pct))
cat(sprintf("SD across methods: %.3f t (CV = %.1f%%)\n", method_unc$sd, method_unc$cv))

cat("\nIndividual method differences:\n")
print(method_unc$individual)


## ----bootstrap-sensitivity, eval=FALSE---------------------------------------------------------------------
# # Create sample tree data
# tree_data <- data.frame(
#   dbh = c(30, 45, 60, 25, 55),
#   height = c(15, 20, 25, 12, 22),
#   genus = c("Quercus", "Fagus", "Fraxinus", "Betula", "Acer"),
#   species = c("robur", "sylvatica", "excelsior", "pendula", "pseudoplatanus")
# )
# 
# # Run bootstrap sensitivity analysis
# # Note: This is computationally intensive - use n_boot = 100 for testing
# boot_result <- bootstrap_sensitivity(
#   data = tree_data,
#   methods = c("WCC", "BIOMASS", "allodb", "Bunce"),
#   coords = c(-0.29, 51.48),  # London area
#   type = "broadleaf",
#   n_boot = 200,              # Use 500+ for publication
#   dbh_cv = 0.02,             # 2% DBH measurement error
#   height_cv = 0.05,          # 5% height measurement error
#   method = "IPCC2",
#   seed = 42                  # For reproducibility
# )
# 
# # View results
# print(boot_result)


## ----bootstrap-interpret, eval=FALSE-----------------------------------------------------------------------
# # The print method shows:
# # - Point estimate of CV
# # - Bootstrap mean CV
# # - 95% confidence interval on CV
# # - Same for range ratio
# 
# # Key interpretation:
# # If 95% CI is narrow (e.g., [12%, 18%]) -> Sensitivity conclusion is robust
# # If 95% CI is wide (e.g., [5%, 45%]) -> Measurement error affects conclusion
# 
# # Visualize the bootstrap distribution
# plot(boot_result, type = "cv")         # Histogram of CVs
# plot(boot_result, type = "range_ratio") # Histogram of range ratios


## ----complete-workflow-------------------------------------------------------------------------------------
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

cat("=== Input Data ===\n")
print(my_trees)

# === Step 2: Calculate carbon using preferred method ===
cat("\n=== WCC Carbon Estimates ===\n")

wcc_out <- fc_agc_error(name = my_trees$common_name, dbh = my_trees$dbh_cm,
                        height = my_trees$height_m, type = my_trees$type,
                        re_dbh = 0.025, re_h = 0.05)

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
cat("\n=== Summary ===\n")
cat(sprintf("Total AGC: %.2f ± %.2f tonnes\n", 
            sum(results$AGC_t, na.rm = TRUE),
            sqrt(sum(results$SD_t^2, na.rm = TRUE))))
cat(sprintf("Mean per-tree CV: %.1f%%\n", mean(results$CV_pct, na.rm = TRUE)))

# === Step 4: Convert to CO2 equivalent ===
results$CO2e_t <- results$AGC_t * (44/12)
cat(sprintf("\nTotal CO2 equivalent: %.2f tonnes\n", sum(results$CO2e_t, na.rm = TRUE)))


## ----read-csv----------------------------------------------------------------------------------------------
# Example: Reading tree data from CSV file
# Your CSV should have columns: name (or genus/species), dbh, height, type

# Example data structure:
example_data <- data.frame(
  name = c("Oak", "Beech", "Ash"),
  dbh = c(45, 38, 52),
  height = c(18, 22, 24),
  type = rep("broadleaf", 3)
)

# Uncomment to read from file:
# tree_data <- read.csv("your_trees.csv", stringsAsFactors = FALSE)

# Process the data
results <- fc_agc(name = example_data$name, dbh = example_data$dbh,
                  height = example_data$height, type = example_data$type)

print(results)


## ----missing-data------------------------------------------------------------------------------------------
# Example with missing values
trees_with_na <- data.frame(
  name = c("Oak", "Beech", NA, "Ash"),
  dbh = c(45, 38, 52, NA),
  height = c(18, 22, 24, 15),
  type = c("broadleaf", "broadleaf", "broadleaf", "broadleaf")
)

# TreeCarbon handles NA values gracefully
results_na <- fc_agc(name = trees_with_na$name, dbh = trees_with_na$dbh,
                     height = trees_with_na$height, type = trees_with_na$type)

cat("=== Results with Missing Data ===\n")
cat("Note: Trees with missing data will have NA results\n")
print(results_na[, c("name", "dbh", "height", "AGC_WCC_t")])


## ----data-validation---------------------------------------------------------------------------------------
# Check data before processing
validate_data <- function(trees) {
  issues <- character()
  
  if (any(trees$dbh <= 0, na.rm = TRUE)) {
    issues <- c(issues, "Some DBH values are <= 0")
  }
  if (any(trees$height <= 0, na.rm = TRUE)) {
    issues <- c(issues, "Some height values are <= 0")
  }
  if (any(trees$dbh > 200, na.rm = TRUE)) {
    issues <- c(issues, "Some DBH values are very large (>200 cm)")
  }
  if (any(trees$height > 60, na.rm = TRUE)) {
    issues <- c(issues, "Some height values are very large (>60 m)")
  }
  
  if (length(issues) > 0) {
    warning("Data validation issues found:\n", paste(issues, collapse = "\n"))
  } else {
    cat("Data validation passed\n")
  }
}

# Validate example data
validate_data(trees)


## ----custom-wood-density-----------------------------------------------------------------------------------
# Get wood density from BIOMASS package
if (requireNamespace("BIOMASS", quietly = TRUE)) {
  wd <- BIOMASS::getWoodDensity("Quercus", "robur", region = "Europe")
  
  # Use custom NSG (Nominal Specific Gravity)
  result_custom <- fc_agc(name = "Oak", dbh = 45, height = 18,
                          type = "broadleaf", nsg = wd$meanWD)
  
  cat("=== Using Custom Wood Density ===\n")
  cat(sprintf("NSG from BIOMASS: %.3f\n", wd$meanWD))
  cat(sprintf("Carbon: %.3f tonnes\n", result_custom$AGC_WCC_t))
}


## ----seedlings-saplings------------------------------------------------------------------------------------
# Seedling/sapling carbon (height in metres)
seedling_result <- sap_seedling2C(
  height = 0.5,     # 0.5 m = 50 cm seedling
  type = "broadleaf",
  re_h = 0.05,
  re = 0.025
)

cat("=== Seedling/Sapling Carbon ===\n")
cat(sprintf("Carbon: %.4f ± %.4f t C\n",
            seedling_result$carbon, seedling_result$sd))


## ----co2-conversion----------------------------------------------------------------------------------------
# Carbon to CO2 conversion (1 t C = 3.67 t CO2)
carbon_t <- 1.0
co2_t <- ctoco2e(carbon_t)

cat("=== Carbon to CO2 Conversion ===\n")
cat(sprintf("%.2f tonnes C = %.2f tonnes CO2 equivalent\n", carbon_t, co2_t))


## ----rich-output-details-----------------------------------------------------------------------------------
# Get rich output
result_rich <- fc_agc(name = "Oak", dbh = 45, height = 18,
                      type = "broadleaf", rich_output = TRUE)

# Access individual components
cat("=== Rich Output Components ===\n")
cat(sprintf("Value: %.3f %s\n", result_rich$value, result_rich$unit))
cat(sprintf("Method: %s\n", result_rich$method_full_name))
cat(sprintf("Reference: %s\n", result_rich$reference))
if (!is.null(result_rich$doi)) {
  cat(sprintf("DOI: %s\n", result_rich$doi))
}
cat(sprintf("Region: %s\n", result_rich$region))
cat("\nAssumptions:\n")
for (i in seq_along(result_rich$assumptions)) {
  cat(sprintf("  %d. %s\n", i, result_rich$assumptions[i]))
}


## ----troubleshooting-species-------------------------------------------------------------------------------
# Unknown species - will use type-based defaults
result_unknown <- fc_agc(name = "Unknown Species", dbh = 45, height = 18,
                         type = "broadleaf")

cat("=== Unknown Species Handling ===\n")
cat("Using type-based defaults for unknown species\n")
print(result_unknown)


## ----troubleshooting-ranges--------------------------------------------------------------------------------
# Very small or large trees may trigger warnings
result_small <- fc_agc(name = "Oak", dbh = 5, height = 3,
                       type = "broadleaf")

# Check for warnings/flags in rich output
result_small_rich <- fc_agc(name = "Oak", dbh = 5, height = 3,
                            type = "broadleaf", rich_output = TRUE)

cat("=== Small Tree (may have validity warnings) ===\n")
print(result_small_rich)


## ----troubleshooting-height--------------------------------------------------------------------------------
# Option 1: Use Bunce method instead (no height needed)
bunce_no_height <- Bunce(name = "Oak", dbh = 45)

cat("=== Bunce Method (No Height Required) ===\n")
cat(sprintf("Biomass: %.1f kg\n", bunce_no_height$biomass))

# Option 2: Estimate height (not recommended for WCC)
# WCC requires measured height for accurate carbon accounting


## ----troubleshooting-deps----------------------------------------------------------------------------------
# Check if optional packages are available
cat("=== Package Availability ===\n")
cat(sprintf("BIOMASS: %s\n", requireNamespace("BIOMASS", quietly = TRUE)))
cat(sprintf("allodb: %s\n", requireNamespace("allodb", quietly = TRUE)))
cat(sprintf("shiny: %s\n", requireNamespace("shiny", quietly = TRUE)))

# Install missing packages if needed:
# install.packages(c("BIOMASS", "allodb", "shiny"))


## ----performance-vectorized--------------------------------------------------------------------------------
# GOOD: Vectorized (fast)
trees <- data.frame(
  name = rep("Oak", 100),
  dbh = rnorm(100, 45, 5),
  height = rnorm(100, 18, 2),
  type = rep("broadleaf", 100)
)

system.time({
  results <- fc_agc(name = trees$name, dbh = trees$dbh,
                    height = trees$height, type = trees$type)
})

cat("Vectorized processing: efficient for large datasets\n")


## ----performance-rich--------------------------------------------------------------------------------------
# Standard output (faster)
system.time({
  results_std <- fc_agc(name = trees$name[1:10], dbh = trees$dbh[1:10],
                        height = trees$height[1:10],
                        type = trees$type[1:10], rich_output = FALSE)
})

# Rich output (slightly slower, more information)
system.time({
  results_rich <- fc_agc(name = trees$name[1:10], dbh = trees$dbh[1:10],
                         height = trees$height[1:10],
                         type = trees$type[1:10], rich_output = TRUE)
})

cat("Rich output provides metadata but is slightly slower\n")


## ----export-csv--------------------------------------------------------------------------------------------
# Calculate results
results <- fc_agc(name = trees$species, dbh = trees$dbh,
                  height = trees$height, type = trees$type,
                  output.all = TRUE)

# Create export table
export_table <- data.frame(
  Species = results$name,
  DBH_cm = results$dbh,
  Height_m = results$height,
  AGC_tonnes = results$AGC_WCC_t,
  stringsAsFactors = FALSE
)

# Export (uncomment to save)
# write.csv(export_table, "carbon_results.csv", row.names = FALSE)

cat("=== Export Table ===\n")
print(head(export_table))


## ----create-report-----------------------------------------------------------------------------------------
# Summary statistics for reporting
summary_stats <- data.frame(
  Metric = c("Total trees", "Total carbon (t C)", "Mean carbon (t C)",
             "Min carbon (t C)", "Max carbon (t C)"),
  Value = c(
    nrow(results),
    sum(results$AGC_WCC_t, na.rm = TRUE),
    mean(results$AGC_WCC_t, na.rm = TRUE),
    min(results$AGC_WCC_t, na.rm = TRUE),
    max(results$AGC_WCC_t, na.rm = TRUE)
  )
)

cat("=== Summary Statistics for Report ===\n")
print(summary_stats)


## ----session-info------------------------------------------------------------------------------------------
sessionInfo()

