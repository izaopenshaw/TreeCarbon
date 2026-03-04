# =============================================================================
# Analytical vs Monte Carlo Error Propagation Comparison
# =============================================================================
# This script systematically compares analytical and Monte Carlo error
# propagation methods across a range of DBH/height values to determine
# whether analytical methods consistently underestimate uncertainty.
#
# Tests Bunce method across tree sizes to identify:
# - Whether analytical underestimates error compared to MC
# - How the difference varies with tree size
# - Whether the pattern is consistent across DBH/height ranges
#
# Author: Isabel Openshaw, Justin Moat
# Date: 2026
# =============================================================================

setwd("C:/git/TreeCarbon")
devtools::load_all()

# Load required packages
library(ggplot2)

# Set output directory
if (!exists("output_dir")) {
  output_dir <- file.path(getwd(), "paper_figures")
}
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Set seed for reproducibility
set.seed(12345)

# Define consistent theme
theme_paper <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.title = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# =============================================================================
# PARAMETERS
# =============================================================================

test_name <- "Oak"
re_dbh <- 0.025  # 2.5% DBH measurement error
n_mc <- 5000     # Number of Monte Carlo iterations

# Get Bunce coefficients for Oak
lookup_oak <- lookupcode(test_name, code = "short")
spcode_oak <- lookup_oak$code
if (spcode_oak == "MX") spcode_oak <- "XB"
matched_idx <- match(spcode_oak, buncedf$spcode)
if (is.na(matched_idx)) matched_idx <- 6

a_mean <- buncedf$a[matched_idx]
b_mean <- buncedf$b[matched_idx]
a_se <- 0.05  # Standard error of intercept
b_se <- 0.06  # Standard error of slope
re_residual <- buncedf$re[matched_idx]
if (is.na(re_residual)) re_residual <- 0.15

# =============================================================================
# SYSTEMATIC COMPARISON ACROSS TREE SIZES
# =============================================================================

# Define test ranges
dbh_range <- seq(10, 100, by = 5)
height_range <- dbh_range * 0.4  # Typical H:DBH ratio

comparison_results <- data.frame()

cat("Running systematic comparison across tree sizes...\n")
cat(sprintf("Testing %d DBH values from %.0f to %.0f cm\n", 
            length(dbh_range), min(dbh_range), max(dbh_range)))

for (i in seq_along(dbh_range)) {
  dbh_val <- dbh_range[i]
  height_val <- height_range[i]
  
  cat(sprintf("Processing DBH = %d cm...\n", dbh_val))
  
  # ===== ANALYTICAL ERROR PROPAGATION =====
  bunce_analytical <- tryCatch({
    Bunce(name = test_name, dbh = dbh_val, re_dbh = re_dbh)
  }, error = function(e) NULL)
  
  if (is.null(bunce_analytical) || is.na(bunce_analytical$biomass)) {
    next
  }
  
  analytical_mean <- bunce_analytical$biomass
  analytical_sd <- bunce_analytical$sigma
  analytical_cv <- 100 * analytical_sd / analytical_mean
  
  # ===== MONTE CARLO ERROR PROPAGATION =====
  set.seed(12345 + i)  # Different seed per tree to avoid correlation
  mc_biomass <- numeric(n_mc)
  
  for (j in seq_len(n_mc)) {
    # Sample DBH with measurement error
    dbh_sampled <- max(rnorm(1, mean = dbh_val, sd = dbh_val * re_dbh), 0.1)
    
    # Sample coefficients with parameter uncertainty
    a_sampled <- rnorm(1, mean = a_mean, sd = a_se)
    b_sampled <- rnorm(1, mean = b_mean, sd = b_se)
    
    # Calculate biomass with sampled parameters
    biomass_pred <- exp(a_sampled + b_sampled * log(pi * dbh_sampled))
    
    # Add residual error (model prediction uncertainty)
    residual_error <- rnorm(1, mean = 0, sd = biomass_pred * re_residual)
    mc_biomass[j] <- max(biomass_pred + residual_error, 0)
  }
  
  mc_mean <- mean(mc_biomass)
  mc_sd <- sd(mc_biomass)
  mc_cv <- 100 * mc_sd / mc_mean
  mc_ci_low <- quantile(mc_biomass, 0.025)
  mc_ci_high <- quantile(mc_biomass, 0.975)
  
  # Calculate differences
  mean_diff_pct <- 100 * (mc_mean - analytical_mean) / analytical_mean
  sd_diff_pct <- 100 * (mc_sd - analytical_sd) / analytical_sd
  cv_diff_pct <- mc_cv - analytical_cv
  
  # Store results
  comparison_results <- rbind(comparison_results,
                               data.frame(
                                 dbh = dbh_val,
                                 height = height_val,
                                 analytical_mean = analytical_mean,
                                 analytical_sd = analytical_sd,
                                 analytical_cv = analytical_cv,
                                 mc_mean = mc_mean,
                                 mc_sd = mc_sd,
                                 mc_cv = mc_cv,
                                 mean_diff_pct = mean_diff_pct,
                                 sd_diff_pct = sd_diff_pct,
                                 cv_diff_pct = cv_diff_pct,
                                 analytical_underestimates_cv = cv_diff_pct > 0
                               ))
}

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")
cat(sprintf("Total trees tested: %d\n", nrow(comparison_results)))
cat(sprintf("Analytical underestimates CV: %d (%.1f%%)\n",
            sum(comparison_results$analytical_underestimates_cv, na.rm = TRUE),
            100 * sum(comparison_results$analytical_underestimates_cv, na.rm = TRUE) / 
            nrow(comparison_results)))
cat(sprintf("Mean CV difference (MC - Analytical): %.2f%%\n",
            mean(comparison_results$cv_diff_pct, na.rm = TRUE)))
cat(sprintf("Median CV difference: %.2f%%\n",
            median(comparison_results$cv_diff_pct, na.rm = TRUE)))
cat(sprintf("Max CV difference: %.2f%%\n",
            max(comparison_results$cv_diff_pct, na.rm = TRUE)))
cat(sprintf("Min CV difference: %.2f%%\n",
            min(comparison_results$cv_diff_pct, na.rm = TRUE)))

# Test if analytical consistently underestimates
underestimate_rate <- mean(comparison_results$analytical_underestimates_cv, na.rm = TRUE)
cat(sprintf("\nUnderestimate rate: %.1f%%\n", underestimate_rate * 100))

if (underestimate_rate > 0.95) {
  cat("CONCLUSION: Analytical method CONSISTENTLY underestimates uncertainty (>95% of cases)\n")
} else if (underestimate_rate > 0.75) {
  cat("CONCLUSION: Analytical method USUALLY underestimates uncertainty (>75% of cases)\n")
} else if (underestimate_rate > 0.50) {
  cat("CONCLUSION: Analytical method OFTEN underestimates uncertainty (>50% of cases)\n")
} else {
  cat("CONCLUSION: Analytical method does NOT consistently underestimate uncertainty\n")
}

# =============================================================================
# FIGURE 1: CV COMPARISON ACROSS TREE SIZES
# =============================================================================

fig1_data <- data.frame(
  dbh = rep(comparison_results$dbh, 2),
  cv = c(comparison_results$analytical_cv, comparison_results$mc_cv),
  method = rep(c("Analytical", "Monte Carlo"), each = nrow(comparison_results))
)

fig1 <- ggplot(fig1_data, aes(x = dbh, y = cv, color = method, linetype = method)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("Analytical" = "steelblue", "Monte Carlo" = "darkred")) +
  scale_linetype_manual(values = c("Analytical" = "solid", "Monte Carlo" = "dashed")) +
  labs(
    title = "Coefficient of Variation: Analytical vs Monte Carlo",
    subtitle = sprintf("Comparison across DBH range (%d-%d cm), re_dbh = %.1f%%", 
                       min(dbh_range), max(dbh_range), re_dbh * 100),
    x = "DBH (cm)",
    y = "Coefficient of Variation (%)",
    color = "Method",
    linetype = "Method"
  ) +
  theme_paper

print(fig1)
ggsave(file.path(output_dir, "fig_analytical_vs_mc_cv_comparison.png"), 
       fig1, width = 8, height = 5, dpi = 300)

# =============================================================================
# FIGURE 2: CV DIFFERENCE (MC - Analytical) BY TREE SIZE
# =============================================================================

fig2 <- ggplot(comparison_results, aes(x = dbh, y = cv_diff_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_point(color = "darkred", size = 2) +
  geom_ribbon(aes(ymin = 0, ymax = cv_diff_pct), fill = "darkred", alpha = 0.2) +
  annotate("text", x = mean(dbh_range), y = max(comparison_results$cv_diff_pct, na.rm = TRUE) * 0.8,
           label = "MC > Analytical\n(underestimation)", color = "darkred", fontface = "bold") +
  labs(
    title = "CV Difference: Monte Carlo - Analytical",
    subtitle = sprintf("Positive values indicate analytical underestimation (n = %d trees)", 
                       nrow(comparison_results)),
    x = "DBH (cm)",
    y = "CV Difference (%)"
  ) +
  theme_paper

print(fig2)
ggsave(file.path(output_dir, "fig_analytical_vs_mc_cv_difference.png"), 
       fig2, width = 8, height = 5, dpi = 300)

# =============================================================================
# FIGURE 3: RELATIVE DIFFERENCE IN SD
# =============================================================================

fig3 <- ggplot(comparison_results, aes(x = dbh, y = sd_diff_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_line(color = "darkorange", linewidth = 1.2) +
  geom_point(color = "darkorange", size = 2) +
  geom_ribbon(aes(ymin = 0, ymax = sd_diff_pct), fill = "darkorange", alpha = 0.2) +
  labs(
    title = "Relative Difference in Standard Deviation",
    subtitle = sprintf("%% difference: (MC - Analytical) / Analytical × 100", 
                       nrow(comparison_results)),
    x = "DBH (cm)",
    y = "SD Difference (%)"
  ) +
  theme_paper

print(fig3)
ggsave(file.path(output_dir, "fig_analytical_vs_mc_sd_difference.png"), 
       fig3, width = 8, height = 5, dpi = 300)

# =============================================================================
# FIGURE 4: UNDERESTIMATION PATTERN BY TREE SIZE
# =============================================================================

# Create binary indicator for visualization
comparison_results$underestimate <- ifelse(comparison_results$analytical_underestimates_cv, 
                                           "Yes", "No")

fig4 <- ggplot(comparison_results, aes(x = dbh, y = cv_diff_pct, fill = underestimate)) +
  geom_col(alpha = 0.7, width = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "steelblue"),
                    labels = c("Yes" = "Analytical underestimates", 
                              "No" = "Analytical overestimates")) +
  labs(
    title = "Analytical Underestimation Pattern by Tree Size",
    subtitle = sprintf("Red bars: Analytical underestimates CV (%.1f%% of cases)", 
                       underestimate_rate * 100),
    x = "DBH (cm)",
    y = "CV Difference: MC - Analytical (%)",
    fill = "Underestimation"
  ) +
  theme_paper

print(fig4)
ggsave(file.path(output_dir, "fig_analytical_underestimation_pattern.png"), 
       fig4, width = 8, height = 5, dpi = 300)

# =============================================================================
# STATISTICAL TEST
# =============================================================================

# Test if mean difference is significantly different from zero
if (nrow(comparison_results) > 2) {
  t_test <- t.test(comparison_results$cv_diff_pct, mu = 0, alternative = "greater")
  cat("\n=== STATISTICAL TEST ===\n")
  cat(sprintf("One-sided t-test (H0: mean difference = 0, H1: mean difference > 0)\n"))
  cat(sprintf("t-statistic: %.3f\n", t_test$statistic))
  cat(sprintf("p-value: %.4f\n", t_test$p.value))
  cat(sprintf("95%% CI lower bound: %.3f\n", t_test$conf.int[1]))
  
  if (t_test$p.value < 0.05) {
    cat("CONCLUSION: Analytical method significantly underestimates CV (p < 0.05)\n")
  } else {
    cat("CONCLUSION: No significant evidence that analytical underestimates CV (p >= 0.05)\n")
  }
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

write.csv(comparison_results, 
          file.path(output_dir, "analytical_vs_mc_comparison_results.csv"), 
          row.names = FALSE)

cat("\n=== RESULTS SAVED ===\n")
cat(sprintf("Results saved to: %s\n", 
            file.path(output_dir, "analytical_vs_mc_comparison_results.csv")))
cat(sprintf("Figures saved to: %s\n", output_dir))

# =============================================================================
# SUMMARY TABLE
# =============================================================================

summary_table <- data.frame(
  Metric = c(
    "Trees tested",
    "Analytical underestimates CV (%)",
    "Mean CV difference (MC - Analytical)",
    "Median CV difference",
    "Max CV difference",
    "Min CV difference",
    "Mean SD difference (%)",
    "Statistical significance (p-value)"
  ),
  Value = c(
    nrow(comparison_results),
    sprintf("%.1f%%", underestimate_rate * 100),
    sprintf("%.2f%%", mean(comparison_results$cv_diff_pct, na.rm = TRUE)),
    sprintf("%.2f%%", median(comparison_results$cv_diff_pct, na.rm = TRUE)),
    sprintf("%.2f%%", max(comparison_results$cv_diff_pct, na.rm = TRUE)),
    sprintf("%.2f%%", min(comparison_results$cv_diff_pct, na.rm = TRUE)),
    sprintf("%.1f%%", mean(comparison_results$sd_diff_pct, na.rm = TRUE)),
    if (exists("t_test")) sprintf("%.4f", t_test$p.value) else "N/A"
  )
)

cat("\n=== SUMMARY TABLE ===\n")
print(summary_table, row.names = FALSE)

write.csv(summary_table, 
          file.path(output_dir, "analytical_vs_mc_summary.csv"), 
          row.names = FALSE)

cat("\nAnalysis complete!\nf