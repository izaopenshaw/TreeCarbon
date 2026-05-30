# Protocol Compliance Recommendations for WCC_stand_Wakehurst.R

Based on **FC Woodland Carbon Code: Carbon Assessment Protocol (FC_WCC)** and **WCC Survey Protocol 24 (SP)**, with checks of `WCC_stand_Wakehurst.R`, `fc_plot_stratum_carbon`, `fc_method_e`, and related functions.

---

## 1. Plot number (SP Table 3 / FC_WCC Table 4.1.4)

**Protocol:** Minimum number of plots depends on stratum area and variability (uniform vs variable).

| Stratum area | Uniform | Variable |
|--------------|---------|----------|
| 0.5–2.0 ha   | 6       | 8        |
| 2.0–10.0 ha  | 8       | 12       |
| >10.0 ha     | 10      | 16       |

**Current script:** Uses 6 for 0.5–2 ha, 8 for 2–10 ha, capped by `nrow(sub)`. No handling for >10 ha; no distinction between uniform and variable strata.

**Recommendations:**
- Add stratum variability (uniform vs variable) as an input or metadata.
- For >10 ha, use 10 (uniform) or 16 (variable) plots.
- Do not cap `n_plots` below the protocol minimum when using synthetic plots; if `nrow(sub) < n_plots`, document that the stratum has fewer trees than the minimum plot count.

---

## 2. Height sample trees: nearest to plot centre (SP 8.3, FC_WCC 4.1.4)

**Protocol:** “Identify the two trees of each species **nearest to the plot centre**; these will be your height sample trees.”

**Current script (`fc_plot_stratum_carbon`):** Uses random sampling:
```r
samp_idx <- sample(seq_len(nrow(pl_trees)), size = n_samp, replace = ...)
```

**Recommendations:**
- If distance from plot centre is available, select the 2 trees per species **closest** to the plot centre.
- Add an optional `distance_from_centre` (or similar) column to the tree data and use it when present.
- If distance is not available, document that random selection is a pragmatic substitute and note this as a protocol deviation.

---

## 3. Minor species grouping (<10%) (SP 8.3, FC_WCC 4.1.4, Appendix 3)

**Protocol:** Species with <10% of total tree numbers should be grouped with the most similar alternative (by genus, then genus group, then phylum).

**Current script:**
- `fc_method_e` implements minor species grouping.
- `fc_plot_stratum_carbon` does **not**; it processes each species separately.

**Recommendations:**
- In `fc_plot_stratum_carbon`, add minor species grouping before the species loop:
  1. Count trees per species.
  2. Species with <10% of total → assign to dominant species of same type (conifer/broadleaf).
  3. Use the grouped species codes for tariff/volume when processing minor species.

---

## 4. DBH minimum: 7 cm (FC_WCC, SP 8.3)

**Protocol:** Trees are stems with DBH ≥ 7 cm.

**Current script:** `Import2.R` filters to `DBH >= 10`:
```r
df <- df[!is.na(df$DBH) & df$DBH >= 10, ]
```

**Recommendations:**
- For protocol compliance, change the filter to `DBH >= 7` in `Import2.R`, or add a parameter to choose 7 vs 10.
- Document that the current 10 cm filter excludes trees in the 7–9.9 cm range.

---

## 5. Plot adequacy: 7–20 trees ≥7 cm per plot (SP Section 2, FC_WCC 4.1.2)

**Protocol:** Plot size should yield 7–20 trees ≥7 cm per plot.

**Current script:** `trees_per_plot` uses `x >= 7` correctly. If `Import2.R` is changed to 7 cm, this will remain correct.

---

## 6. Method E: 1.25 ha and 3000 trees (FC_WCC 4.2.1)

**Protocol:** Method E applies to stands ≤1.25 ha or ≤3000 trees.

**Current script:** Correctly uses `area_conifer <= 1.25` for Method E vs C. `fc_method_e` checks both area and tree count and warns if exceeded.

---

## 7. Method E: height sampling (FC_WCC Table 4.2.2)

**Protocol:** Height sampling fraction depends on estimated number of individuals (Table 4.2.2).

**Current script (`fc_method_e`):** `height_frac()` matches Table 4.2.2 for 10–20 (1:1) through 101–200+ (1:10). Uses `seq(1, n_sp, by = frac)` for systematic sampling.

**Recommendation:** Confirm that “every nth tree” is interpreted as systematic (1st, 1+nth, 1+2nth, …) rather than random. The protocol wording (“every nth”) supports systematic sampling; current implementation is consistent.

---

## 8. Tariff rounding (FC_WCC 4.1.5, 4.2.3)

**Protocol:** Mean tariff rounded **down** to the nearest whole number.

**Current script:** Uses `floor(mean(tariff_vals, na.rm = TRUE))` in both `fc_plot_stratum_carbon` and `fc_method_e`. Correct.

---

## 9. QMD from rounded-down DBH (FC_WCC 4.1.5 step 2, A1.2.2)

**Protocol:** DBH rounded down to nearest whole cm; QMD from rounded-down values.

**Current script:** Uses `DBH_floor <- floor(all_trees$DBH)` and `qmd <- sqrt(mean(all_trees$DBH_floor[sp_idx]^2, na.rm = TRUE))`. Correct.

---

## 10. Broadleaf: timber height vs total height (SP 8.3, FC_WCC Method B)

**Protocol:** For broadleaves, use **timber height** (to lowest point where diameter ≥7 cm). For conifers, use **total height**.

**Current script:** Uses `timber_height` when present, otherwise `Height * 0.85`. `wakehurst_C_WCC.R` passes `height_timber = trees$timber_height` to `fc_agc_error`. `fc_plot_stratum_carbon` uses `pl_trees$timber_height` or `Height * 0.85` fallback. Correct where timber height is available.

---

## 11. Plots 8 and 9: double-sized plots (SP Table 3)

**Protocol:** For 0.5–2 ha, 6 plot units. Double-sized plots (8, 9) are split into half-plots (8a, 8b, 9a, 9b).

**Current script:** Correctly splits plots 8 and 9 into half-plots and halves their areas.

---

## 12. Coppice: Method B and broadleaf type

**Protocol:** Coppice assessed as broadleaf (Method B). SP 9.2 refers to Methods B and C.

**Current script:** Uses `type = "broadleaf"` for coppice in `fc_plot_stratum_carbon` and Monte Carlo. Correct.

---

## 13. Seedlings and saplings

**Protocol:** SP 8.1–8.2 and FC_WCC 4.1.4 define seedlings (<50 cm) and saplings (50 cm to <7 cm DBH). Carbon is calculated separately for these categories.

**Current script:** Only trees (DBH ≥7 cm) are processed. No seedling or sapling carbon.

**Recommendations:**
- If seedlings/saplings are present, add `sap_seedling2C()` (or equivalent) and include their carbon in stratum totals.
- Document that the current analysis is trees-only.

---

## 14. Stand Method D: applicability (FC_WCC 4.1.4)

**Protocol:** Method D is for “pure even-aged stands” where height measurement is difficult (e.g. dense, dark stands).

**Current script:** Uses `fc_stand_carbon` (Method D) for all habitats with ≥5 trees, regardless of even-aged or mixed structure.

**Recommendation:** Add a note that Method D is used for comparison only and may not be appropriate for mixed/uneven-aged stands. Consider restricting Method D to clearly even-aged, single-species strata.

---

## 15. `fc_method_e` by_species structure

**Protocol:** N/A (implementation detail).

**Current script:** `fc_method_e` returns `by_species` as a list with one element: a data.frame of all species (rbind of per-species rows). `res_e$by_species[[1]]` therefore contains all species; the loop over rows processes each. Correct.

---

## 16. Individual-tree sum area denominator

**Protocol:** Carbon per ha = total carbon / stratum area. For broadleaf with real plots, area = sum of plot areas (as used for scaling).

**Current script:** Broadleaf uses `sum(plot_areas)`; conifer/coppice use stratum area. Matches protocol for this setup.

---

## Summary of priority actions

| Priority | Item | File(s) |
|----------|------|---------|
| High | Add minor species grouping to `fc_plot_stratum_carbon` | WCC_stand_Wakehurst.R |
| High | Use nearest-to-centre for height sample trees when distance available | WCC_stand_Wakehurst.R, fc_plot_stratum_carbon |
| Medium | Add plot number logic for >10 ha and uniform vs variable | WCC_stand_Wakehurst.R |
| Medium | Consider DBH ≥7 cm filter in Import2.R | Import2.R |
| Medium | Document seedlings/saplings exclusion or add their carbon | WCC_stand_Wakehurst.R |
| Low | Document Method D applicability for mixed stands | WCC_stand_Wakehurst.R |
| Low | Document random vs nearest height sampling when distance unavailable | WCC_stand_Wakehurst.R |
