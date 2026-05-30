# ===== Wakehurst Tree Carbon Analysis by Stand ====================================
#
# Follows FC Woodland Carbon Code (FC_WCC) and WCC Survey Protocol 24 (SP).
# SP preferred where protocols conflict.
#
# Protocol workflow (FC_WCC 4.1.5, SP 9.2):
# 1. Per stratum: use real plots (broadleaf) or create synthetic plots (conifer, coppice)
# 2. Per plot: measure all tree DBHs; identify 2 height sample trees per species (SP 8.3)
# 3. Office work: total trees, total area, trees_per_ha, N_stratum; QMD from all trees;
#    tariff from height sample trees only; volume → biomass → carbon
#
# ================================================================================

setwd("C:/git/TreeCarbon")
source("inst/paper_scripts/wakehurst_C_WCC.R")

# QSM

# FC_WCC Appendix 1 A1.2: all DBH rounded down to nearest whole cm
trees$DBH_floor <- floor(trees$DBH)

# Plot-stratum carbon: fc_plot_stratum_carbon() in TreeCarbon (FC_WCC 4.1.5, SP 9.2)


# ================================================================================
# PLOT ADEQUACY & PLOT DIMENSIONS (SP Section 2, 7–20 trees ≥7 cm per plot)
# ================================================================================

trees_per_plot <- aggregate(DBH ~ Plot + Habitat, data = trees, FUN = function(x) sum(!is.na(x) & x >= 7))
colnames(trees_per_plot)[3] <- "trees_per_plot"

cat("\n=== Plot adequacy (SP: 7–20 trees ≥7 cm dbh per plot) ===\n")
print(trees_per_plot)

mean_plot_m2 <- mean(plot_areas$plot_area_m.2[plot_areas$Plot_ID %in% plots], na.rm = TRUE)
mean_plot_ha <- mean_plot_m2 / 10000

# ================================================================================
# QUADRATIC MEAN DBH (FC_WCC 4.1.5 step 2, A1.2.2)
# ================================================================================

dbh_valid <- trees$DBH_floor[!is.na(trees$DBH_floor) & trees$DBH_floor > 0]
qmd_overall <- sqrt(mean(dbh_valid^2))
arith_overall <- mean(dbh_valid)

cat("\n=== DBH: Quadratic mean (QMD) vs arithmetic [WCC 4.1.5, A1.2.2] ===\n")
cat(sprintf("Overall: N=%d, arithmetic=%.2f cm, QMD=%.2f cm\n",
            length(dbh_valid), arith_overall, qmd_overall))

# ================================================================================
# METHOD B/C/E: Carbon by stratum (FC_WCC 4.1.5, 4.2; SP 9.2)
# ================================================================================
# Broadleaf: real plots (Method B). - is this not method D stand ??
# Conifer: Method E (≤1.25 ha) or synthetic plots (C).
# Coppice: synthetic plots (Method B).

method_bc_by_hab <- data.frame()
method_bc_by_species <- data.frame()

# ------ BROADLEAF: actual plots (5, 6, 8, 9, 10, 11), Method B ------
# Plots 8 and 9 double-sized → split into half-plots (SP Table 3: 6 units for 0.5–2 ha)
#
idx_bl <- which(trees$Habitat == "broadleaf")
sub <- trees[idx_bl, ]
area_hab <- habitat_areas$area_ha[habitat_areas$Habitat == "broadleaf"][1]
plot_list <- list()
plot_areas_vec <- numeric(0)

for (pl in unique(sub$Plot)) {
    pl_trees <- sub[sub$Plot == pl, ]
    if (nrow(pl_trees) == 0) next
    pl_area <- area_hab / 6
    pid <- which(plot_areas$Plot_ID == pl)
    if (length(pid) > 0) {
      pl_area <- plot_areas$plot_area_ha[pid[1]]
    }
    if (pl %in% c(8, 9)) {
      mid <- ceiling(nrow(pl_trees) / 2)
      plot_list[[paste0(pl, "a")]] <- pl_trees[1:mid, ]
      plot_list[[paste0(pl, "b")]] <- pl_trees[(mid + 1):nrow(pl_trees), ]
      plot_areas_vec[paste0(pl, "a")] <- pl_area / 2
      plot_areas_vec[paste0(pl, "b")] <- pl_area / 2
    } else {
      plot_list[[as.character(pl)]] <- pl_trees
      plot_areas_vec[as.character(pl)] <- pl_area
    }
}

res <- fc_plot_stratum_carbon(plot_list, plot_areas_vec, area_hab,
                              type = "broadleaf", re_dbh = re_dbh, re_h = re_h, seed = 42L)
method_bc_by_hab <- rbind(method_bc_by_hab,
  data.frame(Habitat = "broadleaf", Method = "B", Area_ha = area_hab,
             N_trees = length(idx_bl), Stand_AGC_t = res$carbon_t))
if (nrow(res$by_species) > 0) {
  method_bc_by_species <- rbind(method_bc_by_species,
    cbind(data.frame(Habitat = "broadleaf"), res$by_species))
}

# --- CONIFER: Method E (≤1.25 ha) or synthetic plots (Method C, >1.25 ha) ---
#
idx_cf <- which(trees$Habitat == "conifer")
sub <- trees[idx_cf, ]
area_conifer <- habitat_areas$area_ha[habitat_areas$Habitat == "conifer"][1]

if (area_conifer <= 1.25) {
    res_e <- fc_method_e_carbon(
      name = paste(sub$Genus, sub$Species),
      dbh = sub$DBH,
      height = sub$Height,
      area_ha = area_conifer,
      type = "conifer",
      classification = as.character(sub$Classification),
      output.all = TRUE,
      re_dbh = re_dbh, re_h = re_h
    )
    method_bc_by_hab <- rbind(method_bc_by_hab,
      data.frame(Habitat = "conifer", Method = "E", Area_ha = area_conifer,
                 N_trees = res_e$n_trees, Stand_AGC_t = res_e$stand_AGC_t))
    bs <- res_e$by_species[[1]]
    for (i in seq_len(nrow(bs))) {
        method_bc_by_species <- rbind(method_bc_by_species,
          data.frame(Habitat = "conifer", Species = bs$species[i], N_sample = bs$n_trees[i],
                     QMD_cm = bs$qmd_cm[i], Mean_tariff = bs$mean_tariff[i], Carbon_t = bs$carbon_t[i]))
    }
  } else {
    n_plots <- min(if (area_conifer <= 2) 6 else if (area_conifer <= 10) 8 else 10, nrow(sub))
    set.seed(42)
    sub$synth_plot <- sample(seq_len(n_plots), size = nrow(sub), replace = TRUE)
    plot_list <- split(sub, sub$synth_plot)
    plot_areas_vec <- setNames(rep(area_conifer / n_plots, n_plots), names(plot_list))
    res <- fc_plot_stratum_carbon(plot_list, plot_areas_vec, area_conifer,
                                  type = "conifer", re_dbh = re_dbh, re_h = re_h, seed = 42L)
    method_bc_by_hab <- rbind(method_bc_by_hab,
      data.frame(Habitat = "conifer", Method = "C", Area_ha = area_conifer,
                 N_trees = length(idx_cf), Stand_AGC_t = res$carbon_t))
    if (nrow(res$by_species) > 0) {
      method_bc_by_species <- rbind(method_bc_by_species,
        cbind(data.frame(Habitat = "conifer"), res$by_species))
    }
  }


# ================================================================================
# STAND-LEVEL CARBON (Method D) – for comparison
# ================================================================================

stand_carbon_by_hab <- data.frame()
for (hab in as.character(habitat_areas$Habitat)) {
  idx <- which(as.character(trees$Habitat) == hab)
  if (length(idx) < 5) next
  area_hab <- habitat_areas$area_ha[as.character(habitat_areas$Habitat) == hab][1]
  sp_tab <- table(paste(trees$Genus[idx], trees$Species[idx]))
  dom_sp <- names(which.max(sp_tab))

  res <- fc_stand_carbon(name = dom_sp,
                        dbh = trees$DBH[idx], height = trees$Height[idx],
                        area_ha = area_hab,
                        type = as.character(trees$Classification[idx[1]]),
                        re_dbh = re_dbh, re_h = re_h, output.all = TRUE)
  sig_val <- if (!is.null(res$sig_stand_AGC) && length(res$sig_stand_AGC) > 0)
    res$sig_stand_AGC else NA_real_
  stand_carbon_by_hab <- rbind(stand_carbon_by_hab,
    data.frame(Habitat = hab, Species = dom_sp, Area_ha = area_hab,
               Stand_AGC_t = res$stand_AGC_t,
               sig_Stand_AGC_t = sig_val,
               Carbon_per_ha = res$stand_AGC_t / area_hab))
}

cat("\n=== Stand-level carbon (Method D) by habitat ===\n")
print(stand_carbon_by_hab)

# ================================================================================
# INDIVIDUAL TREE SUM: Per habitat with carbon per ha and area error propagation
# ================================================================================
# Each tree's carbon (fc_agc_error) is summed per habitat. Carbon per ha = total / area.
#
# Area denominator (per FC_WCC + SP):
#   Broadleaf: trees from real plots → area = sum of plot areas (Import2: sum of plots 5,6,8,9,10,11).
#     Correct because carbon density = total carbon in plots / total plot area.
#   Conifer/coppice: full inventory → area = stratum area.
#
# Error propagation: sigma_C_per_ha = C_per_ha * sqrt((sigma_C/C)^2 + (sigma_A/A)^2).
#

indiv_by_hab <- aggregate(
  cbind(AGC_WCC_t, AGC_sig_sq = sig_AGC^2) ~ Habitat, data = trees,
  FUN = function(x) sum(x, na.rm = TRUE))
indiv_by_hab$Individual_tree_sum_t_C <- indiv_by_hab$AGC_WCC_t
indiv_by_hab$sig_Total_t_C <- sqrt(indiv_by_hab$AGC_sig_sq)
indiv_by_hab$AGC_WCC_t <- indiv_by_hab$AGC_sig_sq <- NULL

# Merge area and area uncertainty (sd_area from perimeter-based estimation)
indiv_by_hab <- merge(indiv_by_hab,
  habitat_areas[, c("Habitat", "area_ha", "sd_area")],
  by = "Habitat", all.x = TRUE)
colnames(indiv_by_hab)[colnames(indiv_by_hab) == "area_ha"] <- "Area_ha"
colnames(indiv_by_hab)[colnames(indiv_by_hab) == "sd_area"] <- "sigma_Area_ha"

# Carbon per ha and error propagation: C_per_ha = C_total / A
# sigma_C_per_ha = C_per_ha * sqrt((sigma_C/C)^2 + (sigma_A/A)^2)
indiv_by_hab$Carbon_per_ha <- indiv_by_hab$Individual_tree_sum_t_C / indiv_by_hab$Area_ha
sigma_C <- indiv_by_hab$sig_Total_t_C
sigma_A <- indiv_by_hab$sigma_Area_ha
C <- indiv_by_hab$Individual_tree_sum_t_C
A <- indiv_by_hab$Area_ha
indiv_by_hab$sig_Carbon_per_ha <- ifelse(
  !is.na(sigma_C) & !is.na(sigma_A) & C > 0 & A > 0,
  indiv_by_hab$Carbon_per_ha * sqrt((sigma_C / C)^2 + (sigma_A / A)^2),
  ifelse(!is.na(sigma_C) & C > 0 & A > 0, sigma_C / A, NA_real_)
)

cat("\n=== Individual tree sum per habitat (carbon per ha with area error propagation) ===\n")
cat("Broadleaf: area = sum of plot areas. Conifer/coppice: area = stratum area.\n")
print_cols <- c("Habitat", "Individual_tree_sum_t_C", "sig_Total_t_C",
                "Area_ha", "sigma_Area_ha", "Carbon_per_ha", "sig_Carbon_per_ha")
print(indiv_by_hab[, intersect(print_cols, names(indiv_by_hab))])

# ================================================================================
# COMPARISON: Individual-tree sum vs stand vs Method B/C
# ================================================================================

comparison <- merge(
  data.frame(Habitat = habitat_areas$Habitat, Area_ha = habitat_areas$area_ha),
  indiv_by_hab[, c("Habitat", "Individual_tree_sum_t_C", "sig_Total_t_C",
                  "Carbon_per_ha", "sig_Carbon_per_ha")],
  by = "Habitat", all.x = TRUE
)

comparison <- merge(comparison,
                    stand_carbon_by_hab[, c("Habitat", "Stand_AGC_t", "sig_Stand_AGC_t")],
                    by = "Habitat", all.x = TRUE)
colnames(comparison)[colnames(comparison) == "Stand_AGC_t"] <- "Stand_Method_D_t_C"
colnames(comparison)[colnames(comparison) == "sig_Stand_AGC_t"] <- "sig_Stand_Method_D_t_C"

comparison <- merge(comparison,
                    method_bc_by_hab[, c("Habitat", "Stand_AGC_t")], by = "Habitat",
                    all.x = TRUE)
colnames(comparison)[colnames(comparison) == "Stand_AGC_t"] <- "Method_BC_t_C"

cat("\n=== Comparison: Individual-tree sum vs stand vs Method B/C (per habitat) ===\n")
print(comparison)

# ================================================================================
# MONTE CARLO: Synthetic plot allocation (coppice) – FC_WCC + SP Table 3
# ================================================================================
coppice_trees <- trees[trees$Habitat == "coppice", ]
area_coppice <- habitat_areas$area_ha[habitat_areas$Habitat == "coppice"][1]
n_plots_mc <- min(if (area_coppice <= 2) 6 else if (area_coppice <= 10) 8 else 10, nrow(coppice_trees))

n_mc <- 1000
set.seed(12345)
mc_carbon <- numeric(n_mc)
for (i in seq_len(n_mc)) {
  sub_mc <- coppice_trees
  sub_mc$synth_plot <- sample(seq_len(n_plots_mc), size = nrow(sub_mc), replace = TRUE)
  plot_list_mc <- split(sub_mc, sub_mc$synth_plot)
  plot_areas_mc <- setNames(rep(area_coppice / n_plots_mc, n_plots_mc), names(plot_list_mc))
  res_mc <- fc_plot_stratum_carbon(plot_list_mc, plot_areas_mc, area_coppice,
                                   type = "broadleaf", re_dbh = re_dbh, re_h = re_h, seed = NULL)
  mc_carbon[i] <- res_mc$carbon_t
}
mc_mean <- mean(mc_carbon)
mc_sd <- sd(mc_carbon)
mc_cv <- if (mc_mean > 0) 100 * mc_sd / mc_mean else NA
mc_ci <- quantile(mc_carbon, c(0.025, 0.975))

cat("\n=== Monte Carlo: coppice synthetic plot allocation (FC_WCC Method B, SP Table 3) ===\n")
cat(sprintf("n_plots = %d (SP Table 3: 0.5–2 ha → 6, 2–10 ha → 8). %d iterations.\n",
            n_plots_mc, n_mc))
cat(sprintf("Carbon: %.2f ± %.2f t C (CV %.1f%%), 95%% CI [%.2f, %.2f]\n",
            mc_mean, mc_sd, mc_cv, mc_ci[1], mc_ci[2]))
cat("(Simulates sampling variability when plot locations are random.)\n")

# ================================================================================
# SUMMARY: Carbon totals with uncertainty (error propagation)
# ================================================================================
# Error propagation: for sums of independent estimates, total sigma = sqrt(sum(sigma_i^2)).
# 95% CI: mean ± 1.96 * sigma (assuming approximate normality).
#

# --- Per-method totals and uncertainties ---
totals <- data.frame(
  Method = character(),
  Total_t_C = numeric(),
  sig_Total_t_C = numeric(),
  CI_95_low = numeric(),
  CI_95_high = numeric(),
  stringsAsFactors = FALSE
)

# 1. Individual-tree sum (fc_agc_error): sigma = sqrt(sum(sig_AGC^2)) per tree
indiv_total <- sum(trees$AGC_WCC_t, na.rm = TRUE)
indiv_sig <- sqrt(sum(trees$sig_AGC^2, na.rm = TRUE))
totals <- rbind(totals, data.frame(
  Method = "Individual-tree sum (fc_agc_error)",
  Total_t_C = indiv_total,
  sig_Total_t_C = indiv_sig,
  CI_95_low = if (!is.na(indiv_sig)) indiv_total - 1.96 * indiv_sig else NA_real_,
  CI_95_high = if (!is.na(indiv_sig)) indiv_total + 1.96 * indiv_sig else NA_real_,
  stringsAsFactors = FALSE
))

# 2. Stand Method D: sigma from fc_stand_carbon (propagated through tariff, volume, biomass)
stand_d_total <- sum(stand_carbon_by_hab$Stand_AGC_t)
stand_d_sig <- sqrt(sum(stand_carbon_by_hab$sig_Stand_AGC_t^2, na.rm = TRUE))
totals <- rbind(totals, data.frame(
  Method = "Stand Method D (fc_stand_carbon)",
  Total_t_C = stand_d_total,
  sig_Total_t_C = stand_d_sig,
  CI_95_low = stand_d_total - 1.96 * stand_d_sig,
  CI_95_high = stand_d_total + 1.96 * stand_d_sig,
  stringsAsFactors = FALSE
))

# 3. Method B/C (plot-based): uncertainty not yet propagated in fc_plot_stratum_carbon
method_bc_total <- sum(method_bc_by_hab$Stand_AGC_t)
totals <- rbind(totals, data.frame(
    Method = "Method B/C (plot-based, FC_WCC + SP)",
    Total_t_C = method_bc_total,
    sig_Total_t_C = NA_real_,
    CI_95_low = NA_real_,
    CI_95_high = NA_real_,
    stringsAsFactors = FALSE
))

# 4. Monte Carlo: coppice synthetic plot allocation – SD from resampling
totals <- rbind(totals, data.frame(
    Method = "MC plot-based (coppice, synthetic plots)",
    Total_t_C = mc_mean,
    sig_Total_t_C = mc_sd,
    CI_95_low = mc_ci[1],
    CI_95_high = mc_ci[2],
    stringsAsFactors = FALSE
))

cat("\n=== Carbon totals by method with uncertainty (all habitats) ===\n")
print(totals)

# --- Formatted report ---
cat("\n=== REPORT: Carbon estimates with 95%% CI ===\n")
for (i in seq_len(nrow(totals))) {
  r <- totals[i, ]
  if (is.na(r$sig_Total_t_C) || r$sig_Total_t_C == 0) {
    cat(sprintf("%s: %.2f t C (uncertainty not available)\n", r$Method, r$Total_t_C))
  } else {
    cat(sprintf("%s: %.2f ± %.2f t C (95%% CI: [%.2f, %.2f] t C)\n",
                r$Method, r$Total_t_C, r$sig_Total_t_C, r$CI_95_low, r$CI_95_high))
  }
}

# --- Per-habitat summary with uncertainties ---
cat("\n=== Per-habitat carbon and uncertainty ===\n")
# Ensure Habitat is character for consistent merging (avoids factor/coppice NA)
indiv_by_hab$Habitat <- as.character(indiv_by_hab$Habitat)
stand_carbon_by_hab$Habitat <- as.character(stand_carbon_by_hab$Habitat)
method_bc_by_hab$Habitat <- as.character(method_bc_by_hab$Habitat)
habitat_report <- merge(
  indiv_by_hab,
  stand_carbon_by_hab[, c("Habitat", "Stand_AGC_t", "sig_Stand_AGC_t")],
  by = "Habitat", all.x = TRUE
)
colnames(habitat_report)[colnames(habitat_report) == "Stand_AGC_t"] <- "Stand_Method_D_t_C"
colnames(habitat_report)[colnames(habitat_report) == "sig_Stand_AGC_t"] <- "sig_Stand_Method_D_t_C"
habitat_report <- merge(
  habitat_report,
  method_bc_by_hab[, c("Habitat", "Stand_AGC_t")],
  by = "Habitat", all.x = TRUE
)
colnames(habitat_report)[colnames(habitat_report) == "Stand_AGC_t"] <- "Method_BC_t_C"

habitat_report1 <- habitat_report[,-c(9)]
print(habitat_report1)
