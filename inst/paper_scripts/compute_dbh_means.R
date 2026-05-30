# =============================================================================
# Compute arithmetic and quadratic mean DBH from saved Wakehurst data
# Run after loading saved R environment (e.g. before_rounding_environment.RData)
# =============================================================================

# Load saved environment (adjust path if your .RData is elsewhere)
# Common locations: inst/paper_scripts/Plots/, inst/paper_scripts/, or project root
load_path <- "before_rounding_environment.RData"
if (file.exists(load_path)) {
  load(load_path)
  cat("Loaded:", load_path, "\n")
} else {
  # Try alternative paths
  alt_paths <- c(
    "inst/paper_scripts/Plots/before_rounding_environment.RData",
    "inst/paper_scripts/before_rounding_environment.RData",
    file.path("C:/git/TreeCarbon", "inst", "paper_scripts", "Plots", "before_rounding_environment.RData")
  )
  found <- FALSE
  for (p in alt_paths) {
    if (file.exists(p)) {
      load(p)
      cat("Loaded:", p, "\n")
      found <- TRUE
      break
    }
  }
  if (!found) stop("Could not find saved RData. Set load_path to your file location.")
}

# Get DBH - use trees if available, else df
dbh <- if (exists("trees") && "DBH" %in% names(trees)) {
  trees$DBH
} else if (exists("df") && "DBH" %in% names(df)) {
  df$DBH
} else {
  stop("No trees$DBH or df$DBH found in loaded environment.")
}

# Remove NA and invalid
dbh_valid <- dbh[!is.na(dbh) & dbh > 0]

# Arithmetic mean (overall)
arith_mean <- mean(dbh_valid)
qmd <- sqrt(mean(dbh_valid^2))

cat("\n=== DBH summary (overall) ===\n")
cat("N trees:", length(dbh_valid), "\n")
cat("Arithmetic mean DBH (cm):", round(arith_mean, 4), "\n")
cat("Quadratic mean DBH (QMD, cm):", round(qmd, 4), "\n")
cat("Ratio QMD / arithmetic:", round(qmd / arith_mean, 4), "\n")

# By habitat if available
if (exists("trees") && "Habitat" %in% names(trees)) {
  hab <- trees$Habitat[!is.na(dbh) & dbh > 0]
  dbh_by_hab <- split(dbh_valid, hab)
  cat("\n=== DBH by habitat ===\n")
  out <- data.frame(
    Habitat = names(dbh_by_hab),
    N = sapply(dbh_by_hab, length),
    Arithmetic_mean_cm = sapply(dbh_by_hab, mean),
    QMD_cm = sapply(dbh_by_hab, function(x) sqrt(mean(x^2))),
    stringsAsFactors = FALSE
  )
  out$QMD_over_arith <- round(out$QMD_cm / out$Arithmetic_mean_cm, 4)
  print(out)
}

# Skewness (if moments package available)
if (requireNamespace("moments", quietly = TRUE)) {
  cat("\nSkewness (moments::skewness):", round(moments::skewness(dbh_valid), 4), "\n")
}
