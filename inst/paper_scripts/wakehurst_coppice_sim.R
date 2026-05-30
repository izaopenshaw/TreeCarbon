# ===== Wakehurst Coppice: Protocol-Compliant Spatial Plot Simulation ============
#
# Simulates plot-based carbon assessment for coppice stratum using spatial plots
# that align with FC_WCC and SP: 7-20 trees per plot, circular plots of known area.
#
# Method: Place n_plots circular plots randomly; choose radius from stocking density
# to target 7-20 trees/plot; assign trees to nearest centre if within radius;
# scale sample to stratum.
#
# ================================================================================

setwd("C:/git/TreeCarbon")
devtools::load_all()

# ---------- Load data ----------
source("inst/paper_scripts/Import2.R")
trees <- df
trees <- trees[!is.na(trees$DBH) & trees$DBH >= 7, ]
trees$DBH_floor <- floor(trees$DBH)

summary(trees$DBH)

# fc_plot_stratum_carbon from TreeCarbon (devtools::load_all above)

# Re-extract after WCC_stand_Wakehurst has run (it may have modified trees)
# We need fresh coppice subset with coordinates
idx_cp <- which(as.character(trees$Habitat) == "coppice")
coppice_trees <- trees[idx_cp, ]

if (!all(c("x_m", "y_m") %in% names(coppice_trees)) ||
    any(is.na(coppice_trees$x_m)) || any(is.na(coppice_trees$y_m))) {
  stop("Coppice trees require x_m and y_m coordinates. Check Import2.R includes them.")
}

stratum_area_ha <- habitat_areas$area_ha[habitat_areas$Habitat == "coppice"][1]
re_dbh <- 0.05
re_h <- 0.10

# ---------- Protocol parameters (SP Table 3, FC_WCC 4.1.2) ----------
# n_plots: 6 for 0.5-2 ha, 8 for 2-10 ha, 10 for >10 ha
# Plot size: 7-20 trees per plot
n_plots <- if (stratum_area_ha <= 2) 6 else if (stratum_area_ha <= 10) 8 else 10

# ---------- Choose plot radius from stocking density ----------
# trees_per_ha = n_trees / stratum_area
# Target 10-15 trees/plot (midpoint of 7-20)
# area_per_plot_ha = target_trees / trees_per_ha
# r_m = sqrt(area_per_plot_ha * 10000 / pi)
trees_per_ha <- nrow(coppice_trees) / stratum_area_ha
target_trees_per_plot <- 12  # midpoint of 7-20
area_per_plot_ha <- target_trees_per_plot / trees_per_ha
r_m <- sqrt(area_per_plot_ha * 10000 / pi)
plot_area_ha <- pi * r_m^2 / 10000

# Clamp radius if it would give <7 or >20 trees on average
min_r <- sqrt((7 / trees_per_ha) * 10000 / pi)
max_r <- sqrt((20 / trees_per_ha) * 10000 / pi)
if (r_m < min_r) r_m <- min_r
if (r_m > max_r) r_m <- max_r
plot_area_ha <- pi * r_m^2 / 10000

# ---------- Place plot centres with minimum separation (no overlap) ----------
# min_sep = 2*r ensures circles do not overlap
place_centres_min_sep <- function(n, x_lo, x_hi, y_lo, y_hi, min_sep, max_tries = 500) {
  centres_x <- numeric(n)
  centres_y <- numeric(n)
  for (i in seq_len(n)) {
    found <- FALSE
    for (tries in seq_len(max_tries)) {
      cx <- runif(1, x_lo, x_hi)
      cy <- runif(1, y_lo, y_hi)
      if (i == 1) { found <- TRUE; break }
      d_to_existing <- sqrt((cx - centres_x[seq_len(i - 1)])^2 +
                            (cy - centres_y[seq_len(i - 1)])^2)
      if (all(d_to_existing >= min_sep)) { found <- TRUE; break }
    }
    if (!found) stop("Could not place plot centres with min separation; extent may be too small.")
    centres_x[i] <- cx
    centres_y[i] <- cy
  }
  data.frame(plot_id = seq_len(n), x = centres_x, y = centres_y)
}

assign_tree_to_plot <- function(tx, ty, cx, cy, r) {
  d <- sqrt((tx - cx)^2 + (ty - cy)^2)
  nearest <- which.min(d)
  if (d[nearest] <= r) nearest else NA
}

# Iterative widening: if any plot has <7 trees, increase radius and re-place centres (max 10 tries)
x_range <- range(coppice_trees$x_m, na.rm = TRUE)
y_range <- range(coppice_trees$y_m, na.rm = TRUE)
r_current <- r_m
max_widen <- 10
set.seed(42)

for (iter in seq_len(max_widen)) {
  margin <- min(r_current * 1.5, 0.3 * diff(x_range), 0.3 * diff(y_range))
  x_lo <- x_range[1] + margin
  x_hi <- x_range[2] - margin
  y_lo <- y_range[1] + margin
  y_hi <- y_range[2] - margin
  if (x_hi <= x_lo) { x_lo <- x_range[1]; x_hi <- x_range[2] }
  if (y_hi <= y_lo) { y_lo <- y_range[1]; y_hi <- y_range[2] }
  min_sep <- 2 * r_current
  if ((x_hi - x_lo) < min_sep || (y_hi - y_lo) < min_sep) {
    warning("Extent too small for non-overlapping plots; using overlapping placement.")
    centres <- data.frame(plot_id = seq_len(n_plots),
                         x = runif(n_plots, x_lo, x_hi),
                         y = runif(n_plots, y_lo, y_hi))
  } else {
    centres <- place_centres_min_sep(n_plots, x_lo, x_hi, y_lo, y_hi, min_sep)
  }

  plot_assign <- integer(nrow(coppice_trees))
  for (i in seq_len(nrow(coppice_trees))) {
    plot_assign[i] <- assign_tree_to_plot(
      coppice_trees$x_m[i], coppice_trees$y_m[i],
      centres$x, centres$y, r_current
    )
  }
  coppice_in_plots <- coppice_trees[!is.na(plot_assign), ]
  coppice_in_plots$synth_plot <- plot_assign[!is.na(plot_assign)]
  plot_list_tmp <- split(coppice_in_plots, coppice_in_plots$synth_plot)
  plot_list_tmp <- setNames(plot_list_tmp, paste0("p", names(plot_list_tmp)))
  n_per_plot <- sapply(seq_len(n_plots), function(i) {
    p <- paste0("p", i)
    if (p %in% names(plot_list_tmp)) nrow(plot_list_tmp[[p]]) else 0L
  })
  n_nonzero <- n_per_plot[n_per_plot > 0]
  if (length(n_nonzero) > 0 && min(n_nonzero) >= 7) break
  if (all(n_per_plot == 0)) break  # no trees in any plot, stop
  r_current <- r_current * 1.2
  if (iter == max_widen) {
    warning("Max widening iterations reached; some plots may still have <7 trees.")
  }
}
r_m <- r_current
plot_area_ha <- pi * r_m^2 / 10000

n_outside <- sum(is.na(plot_assign))
coppice_in_plots <- coppice_trees[!is.na(plot_assign), ]
coppice_in_plots$synth_plot <- plot_assign[!is.na(plot_assign)]

# ---------- Reject plots outside 7-20 (protocol FC_WCC 4.1.2) ----------
plot_list <- split(coppice_in_plots, coppice_in_plots$synth_plot)
plot_list <- setNames(plot_list, paste0("p", names(plot_list)))
trees_per_plot <- data.frame(
  Plot = paste0("p", seq_len(n_plots)),
  n_trees = sapply(seq_len(n_plots), function(i) {
    p <- paste0("p", i)
    if (p %in% names(plot_list)) nrow(plot_list[[p]]) else 0L
  }),
  stringsAsFactors = FALSE
)
trees_per_plot$valid_7_20 <- trees_per_plot$n_trees >= 7 & trees_per_plot$n_trees <= 20
valid <- trees_per_plot$valid_7_20
plots_rejected <- sum(!valid)
if (plots_rejected > 0) {
  valid_plots <- trees_per_plot$Plot[valid]
  plot_list <- plot_list[names(plot_list) %in% valid_plots]
  cat(sprintf("Note: Rejected %d plot(s) with <7 or >20 trees (protocol 7-20). Using %d valid plots.\n",
              plots_rejected, length(plot_list)))
}
# Areas only for plots in plot_list (so total_area matches sampled area)
plot_areas_vec <- setNames(rep(plot_area_ha, length(plot_list)), names(plot_list))

min_plots <- if (stratum_area_ha <= 2) 6 else if (stratum_area_ha <= 10) 8 else 10
if (length(plot_list) < min_plots) {
  warning(sprintf("Only %d valid plots (protocol recommends %d). Results may be less precise.",
                  length(plot_list), min_plots))
}

# ---------- Run protocol carbon ----------
res <- fc_plot_stratum_carbon(plot_list, plot_areas_vec, stratum_area_ha,
                              type = "broadleaf", re_dbh = re_dbh, re_h = re_h, seed = 42L)

# ---------- Summary ----------
cat("\n")
cat("=== Coppice: Protocol-compliant spatial plot simulation ===\n")
cat(sprintf("Stratum area: %.2f ha\n", stratum_area_ha))
cat(sprintf("Total coppice trees: %d\n", nrow(coppice_trees)))
cat(sprintf("Trees per ha (census): %.0f\n", trees_per_ha))
cat(sprintf("n_plots: %d (SP Table 3)\n", n_plots))
cat(sprintf("Plot radius: %.1f m (min separation 2r = %.1f m, no overlap)\n", r_m, 2 * r_m))
cat(sprintf("Plot area: %.4f ha (%.0f m²)\n", plot_area_ha, plot_area_ha * 10000))
cat(sprintf("Target trees/plot: 7-20 (chose r for ~%d)\n", target_trees_per_plot))
n_trees_valid <- sum(sapply(plot_list, nrow))
cat(sprintf("Trees in plots: %d total (%d in valid plots, %.0f%% of census)\n",
            nrow(coppice_in_plots), n_trees_valid,
            100 * n_trees_valid / nrow(coppice_trees)))
cat(sprintf("Trees outside all plots: %d\n", n_outside))
cat("\nTrees per plot:\n")
print(trees_per_plot)
cat(sprintf("\nCarbon (scaled to stratum): %.2f t C\n", res$carbon_t))
carbon_per_ha <- res$carbon_t / stratum_area_ha
cat(sprintf("Carbon per hectare: %.2f t C/ha\n", carbon_per_ha))
cat(sprintf("Estimated n_stratum: %.0f\n", res$n_stratum))
cat("\nBy species:\n")
print(res$by_species)

# ---------- Monte Carlo: plot placement uncertainty ----------
# Fixed r_m from reference run; each iteration places centres with different seed
# return_diag=TRUE: for diagnostic (failure reasons); return_metrics=TRUE: full metrics for output df
run_one_mc <- function(seed, return_diag = FALSE, return_metrics = FALSE) {
  set.seed(seed)
  margin <- min(r_m * 1.5, 0.3 * diff(x_range), 0.3 * diff(y_range))
  x_lo <- x_range[1] + margin
  x_hi <- x_range[2] - margin
  y_lo <- y_range[1] + margin
  y_hi <- y_range[2] - margin
  if (x_hi <= x_lo) { x_lo <- x_range[1]; x_hi <- x_range[2] }
  if (y_hi <= y_lo) { y_lo <- y_range[1]; y_hi <- y_range[2] }
  min_sep <- 2 * r_m
  err_msg <- NULL
  placement_overlapping <- FALSE
  tryCatch({
    if ((x_hi - x_lo) < min_sep || (y_hi - y_lo) < min_sep) {
      placement_overlapping <- TRUE
      centres <- data.frame(plot_id = seq_len(n_plots),
                           x = runif(n_plots, x_lo, x_hi),
                           y = runif(n_plots, y_lo, y_hi))
    } else {
      centres <- tryCatch(
        place_centres_min_sep(n_plots, x_lo, x_hi, y_lo, y_hi, min_sep, max_tries = 2000),
        error = function(e) {
          placement_overlapping <<- TRUE
          data.frame(plot_id = seq_len(n_plots),
                     x = runif(n_plots, x_lo, x_hi),
                     y = runif(n_plots, y_lo, y_hi))
        }
      )
    }
    plot_assign <- integer(nrow(coppice_trees))
    for (i in seq_len(nrow(coppice_trees))) {
      plot_assign[i] <- assign_tree_to_plot(
        coppice_trees$x_m[i], coppice_trees$y_m[i],
        centres$x, centres$y, r_m
      )
    }
    coppice_in <- coppice_trees[!is.na(plot_assign), ]
    coppice_in$synth_plot <- plot_assign[!is.na(plot_assign)]
    pl <- split(coppice_in, coppice_in$synth_plot)
    pl <- setNames(pl, paste0("p", names(pl)))
    n_per_pl <- sapply(pl, nrow)
    n_0 <- n_plots - length(pl)
    n_1_6 <- sum(n_per_pl >= 1 & n_per_pl <= 6)
    n_7_20 <- sum(n_per_pl >= 7 & n_per_pl <= 20)
    n_21_plus <- sum(n_per_pl > 20)

    valid_pl <- names(pl)[n_per_pl >= 7 & n_per_pl <= 20]
    if (length(valid_pl) == 0) {
      err_msg <- sprintf("no_valid_plots (n_0=%d n_1_6=%d n_7_20=%d n_21+=%d)",
                        n_0, n_1_6, n_7_20, n_21_plus)
      if (return_diag) return(list(carbon_t = NA_real_, reason = "no_valid_plots",
                                   reason_full = err_msg,
                                   n_0 = n_0, n_1_6 = n_1_6, n_7_20 = n_7_20, n_21_plus = n_21_plus))
      if (return_metrics) return(list(run = seed, carbon_t = NA_real_, n_trees = NA_integer_,
        area_ha = NA_real_, carbon_per_ha = NA_real_, n_valid_plots = 0L, n_plots_rejected = n_plots,
        n_stratum = NA_real_, trees_per_ha_sample = NA_real_, stratum_area_ha = stratum_area_ha,
        plot_area_ha = plot_area_ha, placement_overlapping = placement_overlapping,
        valid = FALSE, protocol_compliant = FALSE))
      return(NA_real_)
    }
    pl <- pl[names(pl) %in% valid_pl]
    areas <- setNames(rep(plot_area_ha, length(pl)), names(pl))
    res <- fc_plot_stratum_carbon(pl, areas, stratum_area_ha,
                                  type = "broadleaf", re_dbh = re_dbh, re_h = re_h, seed = NULL)
    n_trees <- sum(sapply(pl, nrow))
    area_ha <- sum(areas)
    carbon_per_ha <- res$carbon_t / stratum_area_ha
    trees_per_ha_sample <- n_trees / area_ha

    if (return_diag) return(list(carbon_t = res$carbon_t, reason = NA_character_))
    nvp <- length(pl)
    protocol_compliant <- nvp >= min_plots
    if (return_metrics) return(list(run = seed, carbon_t = res$carbon_t, n_trees = n_trees,
      area_ha = area_ha, carbon_per_ha = carbon_per_ha, n_valid_plots = nvp,
      n_plots_rejected = n_plots - nvp, n_stratum = res$n_stratum,
      trees_per_ha_sample = trees_per_ha_sample, stratum_area_ha = stratum_area_ha,
      plot_area_ha = plot_area_ha, placement_overlapping = placement_overlapping,
      valid = TRUE, protocol_compliant = protocol_compliant))
    res$carbon_t
  }, error = function(e) {
    err_msg <<- paste0("error: ", conditionMessage(e))
    if (return_diag) return(list(carbon_t = NA_real_, reason = "error", reason_full = err_msg))
    if (return_metrics) return(list(run = seed, carbon_t = NA_real_, n_trees = NA_integer_,
      area_ha = NA_real_, carbon_per_ha = NA_real_, n_valid_plots = 0L, n_plots_rejected = NA_integer_,
      n_stratum = NA_real_, trees_per_ha_sample = NA_real_, stratum_area_ha = stratum_area_ha,
      plot_area_ha = plot_area_ha, placement_overlapping = NA, valid = FALSE,
      protocol_compliant = FALSE))
    NA_real_
  })
}

# Diagnostic: identify why runs fail
n_diag <- min(200, n_mc <- 500)
cat(sprintf("\nDiagnostic: running %d MC iterations to identify failure causes...\n", n_diag))
diag_out <- lapply(seq_len(n_diag), function(i) run_one_mc(i, return_diag = TRUE))
diag_carbon <- vapply(diag_out, function(x) x$carbon_t, numeric(1))
diag_reason <- vapply(diag_out, function(x) if (is.na(x$reason)) "ok" else x$reason, character(1))
n_ok <- sum(!is.na(diag_carbon))
n_fail <- n_diag - n_ok
fail_reasons <- table(diag_reason[!is.na(diag_reason) & diag_reason != "ok"])
cat(sprintf("Diagnostic: %d ok, %d failed (%.1f%% fail rate)\n", n_ok, n_fail, 100 * n_fail / n_diag))
if (length(fail_reasons) > 0) {
  cat("Failure reasons:\n")
  print(fail_reasons)
  err_idx <- which(diag_reason == "error")
  if (length(err_idx) > 0) {
    cat(sprintf("  First error: %s\n", diag_out[[err_idx[1]]]$reason_full))
  }
  no_valid_idx <- which(diag_reason == "no_valid_plots")
  if (length(no_valid_idx) > 0) {
    n0 <- mean(sapply(diag_out[no_valid_idx], function(x) if (is.null(x$n_0)) 0 else x$n_0))
    n16 <- mean(sapply(diag_out[no_valid_idx], function(x) if (is.null(x$n_1_6)) 0 else x$n_1_6))
    n720 <- mean(sapply(diag_out[no_valid_idx], function(x) if (is.null(x$n_7_20)) 0 else x$n_7_20))
    n21 <- mean(sapply(diag_out[no_valid_idx], function(x) if (is.null(x$n_21_plus)) 0 else x$n_21_plus))
    cat(sprintf("  no_valid_plots: mean (n_0=%.1f n_1_6=%.1f n_7_20=%.1f n_21+=%.1f)\n",
                n0, n16, n720, n21))
    cat("  (n_0=empty, n_1_6=under 7, n_7_20=valid, n_21+=over 20; high n_21+ -> r too large; high n_0/n_1_6 -> r too small or sparse)\n")
  }
}
cat(sprintf("\nMonte Carlo: %d runs (plot placement uncertainty)...\n", n_mc))
mc_out <- lapply(seq_len(n_mc), function(i) run_one_mc(i, return_metrics = TRUE))
mc_df <- do.call(rbind, lapply(mc_out, function(x) {
  data.frame(run = x$run, carbon_t = x$carbon_t, n_trees = x$n_trees,
             area_ha = x$area_ha, carbon_per_ha = x$carbon_per_ha,
             n_valid_plots = x$n_valid_plots, n_plots_rejected = x$n_plots_rejected,
             n_stratum = x$n_stratum, trees_per_ha_sample = x$trees_per_ha_sample,
             stratum_area_ha = x$stratum_area_ha, plot_area_ha = x$plot_area_ha,
             placement_overlapping = x$placement_overlapping,
             overlapping = x$placement_overlapping, valid = x$valid,
             protocol_compliant = x$protocol_compliant, stringsAsFactors = FALSE)
}))
carbon_mc_valid <- mc_df$carbon_t[mc_df$valid]
n_protocol_compliant <- sum(mc_df$protocol_compliant, na.rm = TRUE)
dir.create("inst/paper_scripts/results", showWarnings = FALSE, recursive = TRUE)
write.csv(mc_df, "inst/paper_scripts/results/wakehurst_coppice_mc_carbon.csv", row.names = FALSE)
cat(sprintf("Stored all %d runs: inst/paper_scripts/results/wakehurst_coppice_mc_carbon.csv\n",
            n_mc))
n_overlap <- sum(mc_df$placement_overlapping, na.rm = TRUE)
cat(sprintf("Columns: run, carbon_t, n_trees, area_ha, carbon_per_ha, n_valid_plots, n_plots_rejected, n_stratum, trees_per_ha_sample, stratum_area_ha, plot_area_ha, placement_overlapping, overlapping, valid, protocol_compliant\n"))
cat(sprintf("Placement: %d runs used overlapping fallback (%.1f%%)\n", n_overlap, 100 * n_overlap / n_mc))
if (n_overlap > 0) {
  cat("  (Paper: report 'Non-overlapping placement attempted; overlapping fallback used when placement failed to converge')\n")
}
cat(sprintf("Valid runs: %d (%.1f%%)\n", length(carbon_mc_valid), 100 * length(carbon_mc_valid) / n_mc))
cat(sprintf("WCC protocol-compliant runs (n_valid_plots >= %d): %d (%.1f%%)\n",
            min_plots, n_protocol_compliant, 100 * n_protocol_compliant / n_mc))
if (length(carbon_mc_valid) > 0) {
  cat(sprintf("Carbon (all valid): mean = %.2f t C, SD = %.2f t C\n",
              mean(carbon_mc_valid), sd(carbon_mc_valid)))
  valid_df <- mc_df[mc_df$valid, ]
  n_min_sep <- sum(!valid_df$placement_overlapping, na.rm = TRUE)
  n_overlap_valid <- sum(valid_df$placement_overlapping, na.rm = TRUE)
  if (n_overlap_valid > 0) {
    cat(sprintf("  Of valid: %d min-sep, %d overlapping\n", n_min_sep, n_overlap_valid))
  }
  if (n_protocol_compliant > 0) {
    cph_pc <- mc_df$carbon_per_ha[mc_df$protocol_compliant]
    cat(sprintf("Carbon (protocol-compliant only): mean = %.2f t C, 95%% CI [%.2f, %.2f]\n",
                mean(cph_pc), quantile(cph_pc, 0.025), quantile(cph_pc, 0.975)))
  } else {
    cat("  WARNING: No protocol-compliant runs. Final result uses all valid runs.\n")
  }
}

# ---------- Final result: carbon per ha (MC method, WCC protocol-compliant only) ----------
cph_pc <- mc_df$carbon_per_ha[mc_df$protocol_compliant]
use_fallback <- FALSE
if (length(cph_pc) == 0) {
  cph_pc <- mc_df$carbon_per_ha[mc_df$valid]
  use_fallback <- TRUE
  cat("\n=== Final result: Coppice carbon (Monte Carlo) ===\n")
  cat(sprintf("WARNING: No protocol-compliant runs (n_valid_plots >= %d). Using all valid runs.\n", min_plots))
} else {
  cat("\n=== Final result: Coppice carbon (Monte Carlo, WCC protocol-compliant) ===\n")
}
if (length(cph_pc) == 0) {
  cat("No valid runs to report.\n")
} else {
mc_mean_cph <- mean(cph_pc)
mc_sd_cph <- sd(cph_pc)
mc_ci_lo <- quantile(cph_pc, 0.025)
mc_ci_hi <- quantile(cph_pc, 0.975)
cat(sprintf("Carbon per hectare: %.2f t C/ha (mean of %d runs%s)\n",
            mc_mean_cph, length(cph_pc),
            if (use_fallback) ", all valid" else sprintf(", n_valid_plots >= %d", min_plots)))
cat(sprintf("95%% CI: [%.2f, %.2f] t C/ha\n", mc_ci_lo, mc_ci_hi))
cat(sprintf("SD: %.2f t C/ha\n", mc_sd_cph))
cat(sprintf("(Paper: report as %.2f [%.2f–%.2f] t C/ha, 95%% CI from %d MC runs%s)\n",
            mc_mean_cph, mc_ci_lo, mc_ci_hi, length(cph_pc),
            if (use_fallback) " [non-compliant]" else " [protocol-compliant]"))

# ---------- Distribution plot: carbon per ha (protocol-compliant runs) ----------
}

setwd("C:/git/TreeCarbon")


if (length(cph_pc) > 0) {
  png("inst/paper_scripts/results/plots/wakehurst_coppice_mc_carbon_dist.png",
      width = 700, height = 500, res = 120)
  par(mar = c(4, 4, 3, 1))
  main_txt <- if (use_fallback)
    sprintf("Coppice: Monte Carlo carbon (all valid, n=%d)", length(cph_pc))
  else
    sprintf("Coppice: Monte Carlo carbon (n_valid_plots >= %d, n=%d)", min_plots, length(cph_pc))
  h <- hist(cph_pc, breaks = max(15, min(30, length(cph_pc) %/% 5)), col = "grey85", border = "grey50",
           xlab = "Carbon per hectare (t C/ha)", main = main_txt, ylab = "Frequency")
  abline(v = mc_mean_cph, col = "darkgreen", lwd = 2, lty = 1)
  abline(v = c(mc_ci_lo, mc_ci_hi), col = "darkgreen", lwd = 1.5, lty = 2)
  legend("topright", legend = c(sprintf("Mean = %.2f", mc_mean_cph),
                               sprintf("95%% CI = [%.2f, %.2f]", mc_ci_lo, mc_ci_hi)),
         lty = c(1, 2), lwd = c(2, 1.5), col = "darkgreen", bty = "n")
  dev.off()
  cat(sprintf("\nDistribution plot saved: inst/paper_scripts/results/plots/wakehurst_coppice_mc_carbon_dist.png\n"))
} else {
  cat("\nSkipping distribution plot (no valid runs).\n")
}

# ---------- Map: plots as circles ----------
dir.create("inst/paper_scripts/results/plots", showWarnings = FALSE, recursive = TRUE)
png("inst/paper_scripts/results/plots/wakehurst_coppice_plot_map.png",
    width = 800, height = 800, res = 120)
par(mar = c(4, 4, 3, 1))
x_all <- c(coppice_trees$x_m, centres$x)
y_all <- c(coppice_trees$y_m, centres$y)
pad <- max(r_m * 1.5, 5)
xlim <- range(x_all, na.rm = TRUE) + c(-pad, pad)
ylim <- range(y_all, na.rm = TRUE) + c(-pad, pad)
plot(NA, xlim = xlim, ylim = ylim, asp = 1, xlab = "x (m)", ylab = "y (m)",
     main = "Coppice stratum: plot circles and trees")
# Trees outside plots
outside <- is.na(plot_assign)
if (any(outside)) {
  points(coppice_trees$x_m[outside], coppice_trees$y_m[outside],
         pch = 16, col = "grey70", cex = 0.6)
}
# Trees inside plots (coloured by plot)
pal <- if (n_plots <= 10) {
  c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
    "#e6ab02", "#a6761d", "#666666", "#1f78b4", "#b2df8a")[seq_len(n_plots)]
} else {
  rainbow(n_plots)
}
for (i in seq_len(n_plots)) {
  in_p <- which(plot_assign == i)
  if (length(in_p) > 0) {
    points(coppice_trees$x_m[in_p], coppice_trees$y_m[in_p],
           pch = 16, col = pal[i], cex = 0.7)
  }
}
# Plot circles
draw_circle <- function(cx, cy, r, ...) {
  theta <- seq(0, 2 * pi, length.out = 100)
  lines(cx + r * cos(theta), cy + r * sin(theta), ...)
}
for (i in seq_len(n_plots)) {
  v <- trees_per_plot$valid_7_20[i]
  draw_circle(centres$x[i], centres$y[i], r_m,
              col = if (v) pal[i] else "grey50",
              lwd = if (v) 2 else 1, lty = if (v) 1 else 2)
}
# Plot centres
points(centres$x, centres$y, pch = 3, cex = 1.2, lwd = 2, col = "black")
legend("topright",
       legend = c("Trees outside plots", "Valid plot circle (7-20 trees)", "Rejected plot circle"),
       pch = c(16, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 2, 1),
       col = c("grey70", "forestgreen", "grey50"), bty = "n", cex = 0.85)
dev.off()
cat(sprintf("\nMap saved: inst/paper_scripts/results/plots/wakehurst_coppice_plot_map.png\n"))

# Overall

mean(mc_df$carbon_per_ha, na.rm=TRUE)
summary(mc_df$n_valid_plots)

