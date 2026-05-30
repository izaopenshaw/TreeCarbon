# ==== Calculate AGB by WCC with QSM DBH ====
trees$DBH_qsm <- trees$DBH_qsm*100
wcc_q  <- fc_agc_error(name = name, dbh = trees$DBH_qsm, height = height, type = type,
                       height_timber = height_timber, method = method,
                       re_dbh = re_dbh, re_h = re_h, re = re, rich_output = TRUE)

wcc_df_q <- wcc_q$detailed_output

qsm_dbh <- cbind(trees, wcc_df_q)

setwd("~/SOF/Phil's paper")
write.csv(qsm_dbh, "qsm_dbh.csv")

# ==== Carbon comparison: exclude manual DBH = 0 or NA =================
valid_manual <- !is.na(trees$DBH) & trees$DBH != 0

carbon_comparison <- data.frame(
  manual_AGC_t = manual_dbh$AGC_WCC_t,
  qsm_AGC_t = qsm_dbh$AGC_WCC_t,
  DBH_manual = trees$DBH,
  DBH_qsm = trees$DBH_qsm,
  name = manual_dbh$Name,
  name_q = qsm_dbh$Name
)[valid_manual, ]

# ==== Comparison statistics ===================================================
cat("\n--- Paired t-test ---\n")
print(t.test(carbon_comparison$manual_AGC_t, carbon_comparison$qsm_AGC_t, paired = TRUE))

cat("\n--- Wilcoxon signed-rank test ---\n")
print(wilcox.test(carbon_comparison$manual_AGC_t, carbon_comparison$qsm_AGC_t, paired = TRUE))

cat("\n--- Pearson correlation ---\n")
print(cor.test(carbon_comparison$manual_AGC_t, carbon_comparison$qsm_AGC_t, method = "pearson"))

# ==== Diagnostic plots ========================================================
# 1:1 comparison
plot(carbon_comparison$qsm_AGC_t, carbon_comparison$manual_AGC_t,
     xlab = "Carbon (QSM DBH) [t]", ylab = "Carbon (manual DBH) [t]")
abline(0, 1, lty = 2)

# Bland-Altman (difference vs mean)
diff_c <- carbon_comparison$manual_AGC_t - carbon_comparison$qsm_AGC_t
mean_c <- (carbon_comparison$manual_AGC_t + carbon_comparison$qsm_AGC_t) / 2
plot(mean_c, diff_c, xlab = "Mean carbon [t]", ylab = "Difference (manual - QSM) [t]")
abline(h = mean(diff_c), col = "blue", lty = 2)
abline(h = mean(diff_c) + 1.96 * sd(diff_c), col = "gray", lty = 2)
abline(h = mean(diff_c) - 1.96 * sd(diff_c), col = "gray", lty = 2)

# Difference vs QSM carbon
plot(carbon_comparison$qsm_AGC_t, diff_c,
     xlab = "Carbon (QSM DBH) [t]", ylab = "Difference (manual - QSM) [t]")
abline(h = 0, lty = 2)

###

trees$AGC_WCC_t <- wcc_df$AGC_WCC_t
trees$AGC_sig <- wcc_df$sig_AGC

# allometries() function includes WCC, BIOMASS, allodb, and Bunce
results <- allometries(genus = genus, species = species, dbh = dbh,
                       height = height, type = type, method = method, returnv = "AGC",
                       coords = wakehurst_coords, height_timber = height_timber,
                       re_dbh = re_dbh, re_h = re_h, re = re, checkTaxo = FALSE)
#trees$allo_WCC_C_t <- results$WCC_C_t
#trees$allo_WCC_C_sd <- results$WCC_C_sig
#trees$allo_species <- results$species
#trees$allo_H <- results$height
#trees$allo_d <- results$dbh

# ==== Total carbon per habitat by area (manual vs QSM) =======================
# Uses all available values for each method; broadleaf area = sum of plot areas
valid_manual <- !is.na(trees$DBH) & trees$DBH != 0
valid_qsm <- !is.na(trees$DBH_qsm) & trees$DBH_qsm != 0

carbon_by_habitat <- data.frame()
for (hab in as.character(habitat_areas$Habitat)) {
  area_ha <- habitat_areas$area_ha[habitat_areas$Habitat == hab][1]
  idx_manual <- valid_manual & as.character(trees$Habitat) == hab
  idx_qsm <- valid_qsm & as.character(trees$Habitat) == hab

  total_manual <- sum(manual_dbh$AGC_WCC_t[idx_manual], na.rm = TRUE)
  total_qsm <- sum(qsm_dbh$AGC_WCC_t[idx_qsm], na.rm = TRUE)
  n_manual <- sum(idx_manual)
  n_qsm <- sum(idx_qsm)

  carbon_by_habitat <- rbind(carbon_by_habitat,
    data.frame(
      Habitat = hab,
      Area_ha = area_ha,
      Method = "manual",
      N_trees = n_manual,
      Total_carbon_t = total_manual,
      Carbon_per_ha = total_manual / area_ha
    ),
    data.frame(
      Habitat = hab,
      Area_ha = area_ha,
      Method = "QSM",
      N_trees = n_qsm,
      Total_carbon_t = total_qsm,
      Carbon_per_ha = total_qsm / area_ha
    )
  )
}

cat("\n=== Total carbon per habitat (manual vs QSM, all available values) ===\n")
print(carbon_by_habitat)

# ==== Total carbon per habitat by area (PAIRED: overlapping manual and QSM) ==========
valid <- !is.na(trees$DBH) & trees$DBH != 0 & !is.na(trees$DBH_qsm) & trees$DBH_qsm != 0

carbon_by_habitat_2 <- data.frame()
for (hab in as.character(habitat_areas$Habitat)) {
  area_ha <- habitat_areas$area_ha[habitat_areas$Habitat == hab][1]
  idx <- valid & as.character(trees$Habitat) == hab

  total_manual <- sum(manual_dbh$AGC_WCC_t[idx], na.rm = TRUE)
  total_qsm <- sum(qsm_dbh$AGC_WCC_t[idx], na.rm = TRUE)
  n_trees <- sum(idx)

  carbon_by_habitat_2 <- rbind(carbon_by_habitat_2,
    data.frame(
      Habitat = hab,
      Area_ha = area_ha,
      Method = "manual",
      N_trees = n_trees,
      Total_carbon_t = total_manual,
      Carbon_per_ha = total_manual / area_ha
    ),
    data.frame(
      Habitat = hab,
      Area_ha = area_ha,
      Method = "QSM",
      N_trees = n_trees,
      Total_carbon_t = total_qsm,
      Carbon_per_ha = total_qsm / area_ha
    )
  )
}

cat("\n=== Total carbon per habitat (manual vs QSM, overlapping trees) ===\n")
print(carbon_by_habitat_2)

# ==== Summary stats: manual vs QSM comparison per habitat ============================
paired_df <- data.frame(
  manual_AGC_t = manual_dbh$AGC_WCC_t,
  qsm_AGC_t = qsm_dbh$AGC_WCC_t,
  Habitat = trees$Habitat
)[valid, ]

stats_by_habitat <- data.frame()
for (hab in as.character(habitat_areas$Habitat)) {
  idx <- paired_df$Habitat == hab
  if (sum(idx) < 2) next

  m <- paired_df$manual_AGC_t[idx]
  q <- paired_df$qsm_AGC_t[idx]
  diff_vals <- m - q

  tt <- tryCatch(t.test(m, q, paired = TRUE), error = function(e) list(p.value = NA, estimate = NA, conf.int = c(NA, NA)))
  corr <- tryCatch(cor.test(m, q, method = "pearson"), error = function(e) list(estimate = NA))

  # Habitat-level totals from carbon_by_habitat_2
  hab_rows <- carbon_by_habitat_2[carbon_by_habitat_2$Habitat == hab, ]
  tot_manual <- hab_rows$Total_carbon_t[hab_rows$Method == "manual"][1]
  tot_qsm <- hab_rows$Total_carbon_t[hab_rows$Method == "QSM"][1]
  cph_manual <- hab_rows$Carbon_per_ha[hab_rows$Method == "manual"][1]
  cph_qsm <- hab_rows$Carbon_per_ha[hab_rows$Method == "QSM"][1]

  stats_by_habitat <- rbind(stats_by_habitat,
    data.frame(
      Habitat = hab,
      N_trees = sum(idx),
      Total_manual_t = tot_manual,
      Total_QSM_t = tot_qsm,
      Diff_total_t = tot_manual - tot_qsm,
      Pct_diff_total = 100 * (tot_manual - tot_qsm) / tot_qsm,
      Carbon_per_ha_manual = cph_manual,
      Carbon_per_ha_QSM = cph_qsm,
      Diff_per_ha = cph_manual - cph_qsm,
      Mean_diff_per_tree = mean(diff_vals),
      SD_diff = sd(diff_vals),
      Paired_t_p = if (inherits(tt, "htest")) tt$p.value else NA,
      Pearson_r = if (inherits(corr, "htest")) corr$estimate else NA,
      stringsAsFactors = FALSE
    )
  )
}

cat("\n=== Summary: manual vs QSM carbon comparison (overlapping trees) ===\n")
print(stats_by_habitat)

