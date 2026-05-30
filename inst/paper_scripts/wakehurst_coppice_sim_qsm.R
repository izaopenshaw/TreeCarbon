# ===== Wakehurst Coppice: Protocol-Compliant Spatial Plot Simulation ============
#
# Simulates plot-based carbon assessment for coppice stratum using spatial plots
# that align with FC_WCC and SP: 7-20 trees per plot, circular plots of known area.
#
# Method: Place n_plots circular plot centres with separation >= 2*r_max so that
# with each r_i <= r_max circles do not overlap; per-plot radii are tuned to local
# stocking; trees assign to the nearest plot among those with d <= r_i; areas are
# pi*r_i^2 per plot and passed to fc_plot_stratum_carbon for correct scaling.
#
# ================================================================================

setwd("C:/git/TreeCarbon")
devtools::load_all()

# ---------- Load data ----------
source("inst/paper_scripts/Import2.R")

trees <- df
trees <- trees[!is.na(trees$DBH) & trees$DBH >= 7, ]
trees$DBH_floor <- floor(trees$DBH)

# Re-extract coppice subset with coordinates (fresh, after WCC may have modified)
idx_cp <- which(as.character(trees$Habitat) == "coppice")
coppice_trees <- trees[idx_cp, ]

if (!all(c("x_m", "y_m") %in% names(coppice_trees)) ||
    any(is.na(coppice_trees$x_m)) || any(is.na(coppice_trees$y_m))) {
  stop("Coppice trees require x_m and y_m coordinates. Check Import2.R includes them.")
}

stratum_area_ha <- habitat_areas$area_ha[habitat_areas$Habitat == "coppice"][1]
re_dbh <- 0.05
re_h <- 0.10

# ---------- Config: protocol and simulation ----------
# SP Table 3: plot counts by stratum size; FC_WCC 4.1.2: 7-20 trees per plot
trees_per_plot_min <- 7L
trees_per_plot_max <- 20L
target_trees_per_plot <- 12L
n_plots_requested <- if (stratum_area_ha <= 2) 6L else if (stratum_area_ha <= 10) 8L else 10L
min_plots_protocol <- n_plots_requested
mc_max_attempts_per_run <- 25L
n_tune_iter <- 25L
n_mc <- 500L
n_diag <- min(200L, n_mc)

trees_per_ha <- nrow(coppice_trees) / stratum_area_ha

# Radius (m) that would yield n trees in a plot if stocking matched stratum average
r_from_n_trees <- function(n_t, tph) sqrt((n_t / tph) * 10000 / pi)
r_min <- r_from_n_trees(trees_per_plot_min, trees_per_ha)
r_max <- r_from_n_trees(trees_per_plot_max, trees_per_ha)
r_mid <- r_from_n_trees(target_trees_per_plot, trees_per_ha)
r_mid <- min(max(r_mid, r_min), r_max)

cat(sprintf("Coppice density: %.0f trees/ha (n=%d, area=%.2f ha)\n",
            trees_per_ha, nrow(coppice_trees), stratum_area_ha))
cat(sprintf("Radius bounds (7-20 trees/plot at stratum density): r_min=%.2f m, r_max=%.2f m (placement uses 2*r_max separation)\n",
            r_min, r_max))

# ---------- Spatial helpers ----------
place_centres_min_sep <- function(n, x_lo, x_hi, y_lo, y_hi, min_sep, max_tries = 2000) {
  centres_x <- numeric(n)
  centres_y <- numeric(n)
  for (i in seq_len(n)) {
    found <- FALSE
    for (tries in seq_len(max_tries)) {
      cx <- runif(1, x_lo, x_hi)
      cy <- runif(1, y_lo, y_hi)
      if (i == 1L) {
        found <- TRUE
        break
      }
      d_to_existing <- sqrt((cx - centres_x[seq_len(i - 1L)])^2 +
                              (cy - centres_y[seq_len(i - 1L)])^2)
      if (all(d_to_existing >= min_sep)) {
        found <- TRUE
        break
      }
    }
    if (!found) stop("Could not place plot centres with min separation; extent may be too small.")
    centres_x[i] <- cx
    centres_y[i] <- cy
  }
  data.frame(plot_id = seq_len(n), x = centres_x, y = centres_y)
}

# Among plots j with d_j <= r_vec[j], assign to argmin d (generalises single-r case)
assign_tree_to_plot_var <- function(tx, ty, cx, cy, r_vec) {
  d <- sqrt((tx - cx)^2 + (ty - cy)^2)
  in_rng <- which(d <= r_vec)
  if (length(in_rng) == 0L) return(NA_integer_)
  in_rng[which.min(d[in_rng])]
}

assign_all_trees <- function(tx, ty, cx, cy, r_vec) {
  n <- length(tx)
  out <- rep(NA_integer_, n)
  for (k in seq_len(n)) {
    out[k] <- assign_tree_to_plot_var(tx[k], ty[k], cx, cy, r_vec)
  }
  out
}

# Iteratively adjust each r_i in [r_min, r_max]; keeps r_i + r_j <= d_ij if d_ij >= 2*r_max
tune_plot_radii <- function(cx, cy, tx, ty, r_init, r_min, r_max, n_plots,
                            min_trees, max_trees, max_iter = 25) {
  r <- rep(r_init, n_plots)
  for (iter in seq_len(max_iter)) {
    r <- pmin(pmax(r, r_min), r_max)
    pa <- assign_all_trees(tx, ty, cx, cy, r)
    npp <- vapply(seq_len(n_plots), function(i) sum(pa == i, na.rm = TRUE), integer(1))
    changed <- FALSE
    for (i in seq_len(n_plots)) {
      ni <- npp[i]
      if (ni < min_trees) {
        rn <- if (ni == 0L) min(r_max, r[i] * 1.12) else min(r_max, r[i] * 1.08)
        if (abs(rn - r[i]) > 1e-6) {
          r[i] <- rn
          changed <- TRUE
        }
      } else if (ni > max_trees) {
        rn <- max(r_min, r[i] / 1.08)
        if (abs(rn - r[i]) > 1e-6) {
          r[i] <- rn
          changed <- TRUE
        }
      }
    }
    if (!changed) break
  }
  pmin(pmax(r, r_min), r_max)
}

x_range <- range(coppice_trees$x_m, na.rm = TRUE)
y_range <- range(coppice_trees$y_m, na.rm = TRUE)

# ---------- Reference run (seed 42): placement + per-plot radii ----------
set.seed(42)
margin_ref <- min(r_max * 1.5, 0.3 * diff(x_range), 0.3 * diff(y_range))
x_lo <- x_range[1] + margin_ref
x_hi <- x_range[2] - margin_ref
y_lo <- y_range[1] + margin_ref
y_hi <- y_range[2] - margin_ref
if (x_hi <= x_lo) {
  x_lo <- x_range[1]
  x_hi <- x_range[2]
}
if (y_hi <= y_lo) {
  y_lo <- y_range[1]
  y_hi <- y_range[2]
}
min_sep <- 2 * r_max
placement_overlapping_ref <- FALSE
if ((x_hi - x_lo) < min_sep || (y_hi - y_lo) < min_sep) {
  warning("Extent too small for non-overlapping plots; using overlapping placement (reference run).")
  placement_overlapping_ref <- TRUE
  centres <- data.frame(plot_id = seq_len(n_plots_requested),
                        x = runif(n_plots_requested, x_lo, x_hi),
                        y = runif(n_plots_requested, y_lo, y_hi))
} else {
  centres <- tryCatch(
    place_centres_min_sep(n_plots_requested, x_lo, x_hi, y_lo, y_hi, min_sep),
    error = function(e) {
      placement_overlapping_ref <<- TRUE
      data.frame(plot_id = seq_len(n_plots_requested),
                   x = runif(n_plots_requested, x_lo, x_hi),
                   y = runif(n_plots_requested, y_lo, y_hi))
    }
  )
}

r_vec_ref <- tune_plot_radii(centres$x, centres$y, coppice_trees$x_m, coppice_trees$y_m,
                             r_mid, r_min, r_max, n_plots_requested,
                             trees_per_plot_min, trees_per_plot_max, max_iter = n_tune_iter)

plot_assign <- assign_all_trees(coppice_trees$x_m, coppice_trees$y_m, centres$x, centres$y, r_vec_ref)
n_outside <- sum(is.na(plot_assign))
coppice_in_plots <- coppice_trees[!is.na(plot_assign), ]
coppice_in_plots$synth_plot <- plot_assign[!is.na(plot_assign)]

plot_list <- split(coppice_in_plots, coppice_in_plots$synth_plot)
plot_list <- setNames(plot_list, paste0("p", names(plot_list)))

trees_per_plot <- data.frame(
  Plot = paste0("p", seq_len(n_plots_requested)),
  n_trees = vapply(seq_len(n_plots_requested), function(i) {
    p <- paste0("p", i)
    if (p %in% names(plot_list)) nrow(plot_list[[p]]) else 0L
  }, integer(1)),
  radius_m = r_vec_ref,
  area_ha = pi * r_vec_ref^2 / 10000,
  stringsAsFactors = FALSE
)
trees_per_plot$valid_7_20 <- trees_per_plot$n_trees >= trees_per_plot_min &
  trees_per_plot$n_trees <= trees_per_plot_max

plots_rejected <- sum(!trees_per_plot$valid_7_20)
if (plots_rejected > 0) {
  valid_plots <- trees_per_plot$Plot[trees_per_plot$valid_7_20]
  plot_list <- plot_list[names(plot_list) %in% valid_plots]
  cat(sprintf("Note: Rejected %d plot(s) with <7 or >20 trees. Using %d valid plots.\n",
              plots_rejected, length(plot_list)))
}

plot_areas_vec <- setNames(
  trees_per_plot$area_ha[match(names(plot_list), trees_per_plot$Plot)],
  names(plot_list)
)

if (length(plot_list) < min_plots_protocol) {
  warning(sprintf("Only %d valid plots (protocol recommends %d). Results may be less precise.",
                  length(plot_list), min_plots_protocol))
}

dir.create("inst/paper_scripts/results", showWarnings = FALSE, recursive = TRUE)
plot_areas_table_ref <- data.frame(
  stratum = "coppice",
  run_type = "reference",
  seed = 42L,
  plot_id = trees_per_plot$Plot,
  centre_x_m = centres$x,
  centre_y_m = centres$y,
  radius_m = trees_per_plot$radius_m,
  area_ha = trees_per_plot$area_ha,
  n_trees = trees_per_plot$n_trees,
  valid_7_20 = trees_per_plot$valid_7_20,
  stringsAsFactors = FALSE
)
write.csv(plot_areas_table_ref, "inst/paper_scripts/results/wakehurst_coppice_plot_areas_reference.csv",
          row.names = FALSE)
cat("Saved per-plot areas (reference): inst/paper_scripts/results/wakehurst_coppice_plot_areas_reference.csv\n")

# ---------- Run protocol carbon ----------
res <- fc_plot_stratum_carbon(plot_list, plot_areas_vec, stratum_area_ha,
                              type = "broadleaf", re_dbh = re_dbh, re_h = re_h, seed = 42L)

# ---------- Summary ----------
cat("\n")
cat("=== Coppice: Protocol-compliant spatial plot simulation ===\n")
cat(sprintf("Stratum area: %.2f ha\n", stratum_area_ha))
cat(sprintf("Total coppice trees: %d\n", nrow(coppice_trees)))
cat(sprintf("Trees per ha (census): %.0f\n", trees_per_ha))
cat(sprintf("n_plots requested: %d (SP Table 3)\n", n_plots_requested))
cat(sprintf("Placement: min separation 2*r_max = %.1f m; overlapping fallback = %s\n",
            2 * r_max, placement_overlapping_ref))
cat(sprintf("Per-plot radius: min=%.2f m, max=%.2f m (among all %d plots)\n",
            min(r_vec_ref), max(r_vec_ref), n_plots_requested))
cat(sprintf("Per-plot area: min=%.4f ha, max=%.4f ha, sum(all circles)=%.4f ha\n",
            min(trees_per_plot$area_ha), max(trees_per_plot$area_ha),
            sum(trees_per_plot$area_ha)))
cat(sprintf("Target trees/plot: %d-%d (tuned from ~%d)\n",
            trees_per_plot_min, trees_per_plot_max, target_trees_per_plot))
n_trees_valid <- sum(sapply(plot_list, nrow))
cat(sprintf("Trees in plots: %d total (%d in valid plots, %.0f%% of census)\n",
            sum(trees_per_plot$n_trees), n_trees_valid,
            100 * n_trees_valid / nrow(coppice_trees)))
cat(sprintf("Trees outside all plots: %d\n", n_outside))
cat("\nTrees per plot (with radius and area used for carbon scaling):\n")
print(trees_per_plot)
cat(sprintf("\nCarbon (scaled to stratum): %.2f t C\n", res$carbon_t))
carbon_per_ha <- res$carbon_t / stratum_area_ha
cat(sprintf("Carbon per hectare: %.2f t C/ha\n", carbon_per_ha))
cat(sprintf("Estimated n_stratum: %.0f\n", res$n_stratum))
cat("\nBy species:\n")
print(res$by_species)

# ---------- Monte Carlo: placement uncertainty; per-plot radii; retries ----------
run_one_mc <- function(seed, return_diag = FALSE, return_metrics = FALSE) {
  if (return_diag) {
    set.seed(seed)
    margin <- min(r_max * 1.5, 0.3 * diff(x_range), 0.3 * diff(y_range))
    x_lo <- x_range[1] + margin
    x_hi <- x_range[2] - margin
    y_lo <- y_range[1] + margin
    y_hi <- y_range[2] - margin
    if (x_hi <= x_lo) {
      x_lo <- x_range[1]
      x_hi <- x_range[2]
    }
    if (y_hi <= y_lo) {
      y_lo <- y_range[1]
      y_hi <- y_range[2]
    }
    min_sep <- 2 * r_max
    placement_overlapping <- FALSE
    tryCatch({
      if ((x_hi - x_lo) < min_sep || (y_hi - y_lo) < min_sep) {
        placement_overlapping <- TRUE
        centres <- data.frame(plot_id = seq_len(n_plots_requested),
                              x = runif(n_plots_requested, x_lo, x_hi),
                              y = runif(n_plots_requested, y_lo, y_hi))
      } else {
        centres <- tryCatch(
          place_centres_min_sep(n_plots_requested, x_lo, x_hi, y_lo, y_hi, min_sep, max_tries = 2000),
          error = function(e) {
            placement_overlapping <<- TRUE
            data.frame(plot_id = seq_len(n_plots_requested),
                       x = runif(n_plots_requested, x_lo, x_hi),
                       y = runif(n_plots_requested, y_lo, y_hi))
          }
        )
      }
      r_vec <- tune_plot_radii(centres$x, centres$y, coppice_trees$x_m, coppice_trees$y_m,
                               r_mid, r_min, r_max, n_plots_requested,
                               trees_per_plot_min, trees_per_plot_max, max_iter = n_tune_iter)
      pa <- assign_all_trees(coppice_trees$x_m, coppice_trees$y_m, centres$x, centres$y, r_vec)
      n_per_pl <- vapply(seq_len(n_plots_requested), function(i) sum(pa == i, na.rm = TRUE), integer(1))
      n_0 <- sum(n_per_pl == 0L)
      n_1_6 <- sum(n_per_pl >= 1L & n_per_pl <= 6L)
      n_7_20 <- sum(n_per_pl >= 7L & n_per_pl <= 20L)
      n_21_plus <- sum(n_per_pl > 20L)
      valid_idx <- which(n_per_pl >= trees_per_plot_min & n_per_pl <= trees_per_plot_max)
      if (length(valid_idx) == 0L) {
        return(list(carbon_t = NA_real_, reason = "no_valid_plots",
                    reason_full = sprintf("no_valid_plots (n_0=%d n_1_6=%d n_7_20=%d n_21+=%d)",
                                          n_0, n_1_6, n_7_20, n_21_plus),
                    n_0 = n_0, n_1_6 = n_1_6, n_7_20 = n_7_20, n_21_plus = n_21_plus))
      }
      coppice_in <- coppice_trees[!is.na(pa), ]
      coppice_in$synth_plot <- pa[!is.na(pa)]
      pl <- split(coppice_in, coppice_in$synth_plot)
      pl <- setNames(pl, paste0("p", names(pl)))
      valid_pl <- paste0("p", valid_idx)
      pl <- pl[names(pl) %in% valid_pl]
      areas <- setNames(pi * r_vec[valid_idx]^2 / 10000, valid_pl)
      res <- fc_plot_stratum_carbon(pl, areas, stratum_area_ha,
                                    type = "broadleaf", re_dbh = re_dbh, re_h = re_h, seed = NULL)
      return(list(carbon_t = res$carbon_t, reason = NA_character_))
    }, error = function(e) {
      list(carbon_t = NA_real_, reason = "error", reason_full = paste0("error: ", conditionMessage(e)))
    })
  }

  last_metrics <- NULL
  err_msg <- NULL
  for (attempt in seq_len(mc_max_attempts_per_run)) {
    set.seed(seed + 10007L * attempt)
    margin <- min(r_max * 1.5, 0.3 * diff(x_range), 0.3 * diff(y_range))
    x_lo <- x_range[1] + margin
    x_hi <- x_range[2] - margin
    y_lo <- y_range[1] + margin
    y_hi <- y_range[2] - margin
    if (x_hi <= x_lo) {
      x_lo <- x_range[1]
      x_hi <- x_range[2]
    }
    if (y_hi <= y_lo) {
      y_lo <- y_range[1]
      y_hi <- y_range[2]
    }
    min_sep <- 2 * r_max
    placement_overlapping <- FALSE
    centres <- tryCatch(
      {
        if ((x_hi - x_lo) < min_sep || (y_hi - y_lo) < min_sep) {
          placement_overlapping <<- TRUE
          data.frame(plot_id = seq_len(n_plots_requested),
                     x = runif(n_plots_requested, x_lo, x_hi),
                     y = runif(n_plots_requested, y_lo, y_hi))
        } else {
          place_centres_min_sep(n_plots_requested, x_lo, x_hi, y_lo, y_hi, min_sep, max_tries = 2000)
        }
      },
      error = function(e) {
        placement_overlapping <<- TRUE
        data.frame(plot_id = seq_len(n_plots_requested),
                     x = runif(n_plots_requested, x_lo, x_hi),
                     y = runif(n_plots_requested, y_lo, y_hi))
      }
    )

    r_vec <- tune_plot_radii(centres$x, centres$y, coppice_trees$x_m, coppice_trees$y_m,
                             r_mid, r_min, r_max, n_plots_requested,
                             trees_per_plot_min, trees_per_plot_max, max_iter = n_tune_iter)
    pa <- assign_all_trees(coppice_trees$x_m, coppice_trees$y_m, centres$x, centres$y, r_vec)
    n_per_pl <- vapply(seq_len(n_plots_requested), function(i) sum(pa == i, na.rm = TRUE), integer(1))
    n_empty <- sum(n_per_pl == 0L)
    n_1_6 <- sum(n_per_pl >= 1L & n_per_pl <= 6L)
    n_7_20_count <- sum(n_per_pl >= 7L & n_per_pl <= 20L)
    n_21_plus <- sum(n_per_pl > 20L)
    n_not_valid <- n_empty + n_1_6 + n_21_plus

    valid_idx <- which(n_per_pl >= trees_per_plot_min & n_per_pl <= trees_per_plot_max)
    if (length(valid_idx) == 0L) {
      last_metrics <- list(
        run = seed, carbon_t = NA_real_, n_trees = NA_integer_,
        area_ha = NA_real_, carbon_per_ha = NA_real_, n_valid_plots = 0L,
        n_plots_not_in_range = n_not_valid, n_empty = n_empty, n_1_6 = n_1_6,
        n_7_20 = n_7_20_count, n_21_plus = n_21_plus,
        n_stratum = NA_real_, trees_per_ha_sample = NA_real_,
        stratum_area_ha = stratum_area_ha,
        mean_plot_area_ha = NA_real_, min_plot_area_ha = NA_real_, max_plot_area_ha = NA_real_,
        plot_area_ha = NA_real_, placement_overlapping = placement_overlapping,
        valid = FALSE, protocol_compliant = FALSE, n_attempts = attempt
      )
      next
    }

    valid_pl <- paste0("p", valid_idx)
    coppice_in <- coppice_trees[!is.na(pa), ]
    coppice_in$synth_plot <- pa[!is.na(pa)]
    pl <- split(coppice_in, coppice_in$synth_plot)
    pl <- setNames(pl, paste0("p", names(pl)))
    pl <- pl[names(pl) %in% valid_pl]
    areas <- setNames(pi * r_vec[valid_idx]^2 / 10000, valid_pl)

    res <- tryCatch(
      fc_plot_stratum_carbon(pl, areas, stratum_area_ha,
                             type = "broadleaf", re_dbh = re_dbh, re_h = re_h, seed = NULL),
      error = function(e) {
        err_msg <<- paste0("error: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(res)) next

    n_trees <- sum(sapply(pl, nrow))
    area_ha <- sum(areas)
    carbon_per_ha <- res$carbon_t / stratum_area_ha
    trees_per_ha_sample <- n_trees / area_ha
    mean_pa <- mean(areas)
    min_pa <- min(areas)
    max_pa <- max(areas)
    nvp <- length(pl)
    protocol_compliant <- nvp >= min_plots_protocol

    m <- list(
      run = seed, carbon_t = res$carbon_t, n_trees = n_trees,
      area_ha = area_ha, carbon_per_ha = carbon_per_ha, n_valid_plots = nvp,
      n_plots_not_in_range = n_not_valid, n_empty = n_empty, n_1_6 = n_1_6,
      n_7_20 = n_7_20_count, n_21_plus = n_21_plus,
      n_stratum = res$n_stratum, trees_per_ha_sample = trees_per_ha_sample,
      stratum_area_ha = stratum_area_ha,
      mean_plot_area_ha = mean_pa, min_plot_area_ha = min_pa, max_plot_area_ha = max_pa,
      plot_area_ha = mean_pa, placement_overlapping = placement_overlapping,
      valid = TRUE, protocol_compliant = protocol_compliant, n_attempts = attempt
    )

    if (protocol_compliant) return(m)
    last_metrics <- m
  }

  if (!is.null(err_msg) && is.null(last_metrics)) {
    return(list(
      run = seed, carbon_t = NA_real_, n_trees = NA_integer_,
      area_ha = NA_real_, carbon_per_ha = NA_real_, n_valid_plots = 0L,
      n_plots_not_in_range = NA_integer_, n_empty = NA_integer_, n_1_6 = NA_integer_,
      n_7_20 = NA_integer_, n_21_plus = NA_integer_,
      n_stratum = NA_real_, trees_per_ha_sample = NA_real_,
      stratum_area_ha = stratum_area_ha,
      mean_plot_area_ha = NA_real_, min_plot_area_ha = NA_real_, max_plot_area_ha = NA_real_,
      plot_area_ha = NA_real_, placement_overlapping = NA, valid = FALSE,
      protocol_compliant = FALSE, n_attempts = mc_max_attempts_per_run
    ))
  }
  if (!is.null(last_metrics)) return(last_metrics)
  list(
    run = seed, carbon_t = NA_real_, n_trees = NA_integer_,
    area_ha = NA_real_, carbon_per_ha = NA_real_, n_valid_plots = 0L,
    n_plots_not_in_range = NA_integer_, n_empty = NA_integer_, n_1_6 = NA_integer_,
    n_7_20 = NA_integer_, n_21_plus = NA_integer_,
    n_stratum = NA_real_, trees_per_ha_sample = NA_real_,
    stratum_area_ha = stratum_area_ha,
    mean_plot_area_ha = NA_real_, min_plot_area_ha = NA_real_, max_plot_area_ha = NA_real_,
    plot_area_ha = NA_real_, placement_overlapping = NA, valid = FALSE,
    protocol_compliant = FALSE, n_attempts = mc_max_attempts_per_run
  )
}

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
    cat("  (n_0=empty, n_1_6=under 7, n_7_20=valid, n_21+=over 20)\n")
  }
}

cat(sprintf("\nMonte Carlo: %d runs (plot placement uncertainty)...\n", n_mc))
mc_out <- lapply(seq_len(n_mc), function(i) run_one_mc(i, return_metrics = TRUE))
mc_df <- do.call(rbind, lapply(mc_out, function(x) {
  data.frame(
    run = x$run, carbon_t = x$carbon_t, n_trees = x$n_trees,
    area_ha = x$area_ha, carbon_per_ha = x$carbon_per_ha,
    n_valid_plots = x$n_valid_plots, n_plots_not_in_range = x$n_plots_not_in_range,
    n_empty = x$n_empty, n_1_6 = x$n_1_6, n_7_20 = x$n_7_20, n_21_plus = x$n_21_plus,
    n_stratum = x$n_stratum, trees_per_ha_sample = x$trees_per_ha_sample,
    stratum_area_ha = x$stratum_area_ha,
    mean_plot_area_ha = x$mean_plot_area_ha,
    min_plot_area_ha = x$min_plot_area_ha,
    max_plot_area_ha = x$max_plot_area_ha,
    plot_area_ha = x$plot_area_ha,
    n_attempts = x$n_attempts,
    placement_overlapping = x$placement_overlapping,
    overlapping = x$placement_overlapping, valid = x$valid,
    protocol_compliant = x$protocol_compliant, stringsAsFactors = FALSE
  )
}))
carbon_mc_valid <- mc_df$carbon_t[mc_df$valid]
n_protocol_compliant <- sum(mc_df$protocol_compliant, na.rm = TRUE)
write.csv(mc_df, "inst/paper_scripts/results/wakehurst_coppice_mc_carbon.csv", row.names = FALSE)
cat(sprintf("Stored all %d runs: inst/paper_scripts/results/wakehurst_coppice_mc_carbon.csv\n", n_mc))

# MC runs meeting WCC plot layout: only plots with 7-20 trees feed carbon; and
# n_valid_plots >= min_plots_protocol (SP Table 3: 6 / 8 / 10 by stratum ha).
mc_df_protocol <- mc_df[mc_df$protocol_compliant %in% TRUE, , drop = FALSE]
rownames(mc_df_protocol) <- NULL
prot_csv <- "inst/paper_scripts/results/wakehurst_coppice_mc_protocol_compliant.csv"
write.csv(mc_df_protocol, prot_csv, row.names = FALSE)
cat(sprintf("Protocol-compliant MC subset (%d runs, n_valid_plots >= %d, 7-20 trees/plot): %s\n",
            nrow(mc_df_protocol), min_plots_protocol, prot_csv))
n_overlap <- sum(mc_df$placement_overlapping, na.rm = TRUE)
cat(sprintf("Placement: %d runs used overlapping fallback (%.1f%%)\n", n_overlap, 100 * n_overlap / n_mc))
if (n_overlap > 0) {
  cat("  (Paper: report overlapping fallback when non-overlapping placement failed)\n")
}
cat(sprintf("Valid runs: %d (%.1f%%)\n", length(carbon_mc_valid), 100 * length(carbon_mc_valid) / n_mc))
cat(sprintf("WCC protocol-compliant runs (n_valid_plots >= %d): %d (%.1f%%)\n",
            min_plots_protocol, n_protocol_compliant, 100 * n_protocol_compliant / n_mc))
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

# ---------- Final result: carbon per ha ----------
cph_pc <- mc_df$carbon_per_ha[mc_df$protocol_compliant]
use_fallback <- FALSE
if (length(cph_pc) == 0) {
  cph_pc <- mc_df$carbon_per_ha[mc_df$valid]
  use_fallback <- TRUE
  cat("\n=== Final result: Coppice carbon (Monte Carlo) ===\n")
  cat(sprintf("WARNING: No protocol-compliant runs (n_valid_plots >= %d). Using all valid runs.\n",
              min_plots_protocol))
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
              if (use_fallback) ", all valid" else sprintf(", n_valid_plots >= %d", min_plots_protocol)))
  cat(sprintf("95%% CI: [%.2f, %.2f] t C/ha\n", mc_ci_lo, mc_ci_hi))
  cat(sprintf("SD: %.2f t C/ha\n", mc_sd_cph))
  cat(sprintf("(Paper: report as %.2f [%.2f–%.2f] t C/ha, 95%% CI from %d MC runs%s)\n",
              mc_mean_cph, mc_ci_lo, mc_ci_hi, length(cph_pc),
              if (use_fallback) " [non-compliant]" else " [protocol-compliant]"))
}

setwd("C:/git/TreeCarbon")

if (length(cph_pc) > 0) {
  png("inst/paper_scripts/results/plots/wakehurst_coppice_mc_carbon_dist.png",
      width = 700, height = 500, res = 120)
  par(mar = c(4, 4, 3, 1))
  main_txt <- if (use_fallback) {
    sprintf("Coppice: Monte Carlo carbon (all valid, n=%d)", length(cph_pc))
  } else {
    sprintf("Coppice: Monte Carlo carbon (n_valid_plots >= %d, n=%d)", min_plots_protocol, length(cph_pc))
  }
  hist(cph_pc, breaks = max(15, min(30, length(cph_pc) %/% 5)), col = "grey85", border = "grey50",
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

# ---------- Map: plots as circles (per-plot radius) ----------
dir.create("inst/paper_scripts/results/plots", showWarnings = FALSE, recursive = TRUE)
png("inst/paper_scripts/results/plots/wakehurst_coppice_plot_map.png",
    width = 800, height = 800, res = 120)
par(mar = c(4, 4, 3, 1))
x_all <- c(coppice_trees$x_m, centres$x - r_vec_ref, centres$x + r_vec_ref)
y_all <- c(coppice_trees$y_m, centres$y - r_vec_ref, centres$y + r_vec_ref)
pad <- max(max(r_vec_ref) * 0.15, 2)
xlim <- range(x_all, na.rm = TRUE) + c(-pad, pad)
ylim <- range(y_all, na.rm = TRUE) + c(-pad, pad)
plot(NA, xlim = xlim, ylim = ylim, asp = 1, xlab = "x (m)", ylab = "y (m)",
     main = "Coppice stratum: plot circles and trees (per-plot radius)")
outside <- is.na(plot_assign)
if (any(outside)) {
  points(coppice_trees$x_m[outside], coppice_trees$y_m[outside],
         pch = 16, col = "grey70", cex = 0.6)
}
pal <- if (n_plots_requested <= 10) {
  c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
    "#e6ab02", "#a6761d", "#666666", "#1f78b4", "#b2df8a")[seq_len(n_plots_requested)]
} else {
  rainbow(n_plots_requested)
}
for (i in seq_len(n_plots_requested)) {
  in_p <- which(plot_assign == i)
  if (length(in_p) > 0) {
    points(coppice_trees$x_m[in_p], coppice_trees$y_m[in_p],
           pch = 16, col = pal[i], cex = 0.7)
  }
}
draw_circle <- function(cx, cy, r, ...) {
  theta <- seq(0, 2 * pi, length.out = 100)
  lines(cx + r * cos(theta), cy + r * sin(theta), ...)
}
for (i in seq_len(n_plots_requested)) {
  v <- trees_per_plot$valid_7_20[i]
  draw_circle(centres$x[i], centres$y[i], r_vec_ref[i],
              col = if (v) pal[i] else "grey50",
              lwd = if (v) 2 else 1, lty = if (v) 1 else 2)
}
points(centres$x, centres$y, pch = 3, cex = 1.2, lwd = 2, col = "black")
legend("topright",
       legend = c("Trees outside plots", "Valid plot circle (7-20 trees)", "Rejected plot circle"),
       pch = c(16, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 2, 1),
       col = c("grey70", "forestgreen", "grey50"), bty = "n", cex = 0.85)
dev.off()
cat(sprintf("\nMap saved: inst/paper_scripts/results/plots/wakehurst_coppice_plot_map.png\n"))

cat("\n=== Full MC summary ===\n")
print(c(all_runs = nrow(mc_df),
        protocol_compliant_runs = nrow(mc_df_protocol),
        min_plots_required = min_plots_protocol))
mean(mc_df$carbon_per_ha, na.rm = TRUE)
summary(mc_df$n_valid_plots)
if (nrow(mc_df_protocol) > 0) {
  cat("\n=== mc_df_protocol: only runs with >= min_plots_protocol plots, each plot 7-20 trees ===\n")
  print(summary(mc_df_protocol))
} else {
  cat("\nmc_df_protocol: 0 rows (no protocol-compliant MC runs).\n")
}

