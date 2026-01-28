# ==============================================================================
# TreeCarbon - Sensitivity functions file
# ==============================================================================
#
# Functions included:
#   - Method Sensitivity Analysis
#   - Bootstrap Sensitivity Analysis
#
# Authors: Justin Moat (J.Moat@kew.org), Isabel Openshaw (I.Openshaw@kew.org)
#
################### Method Sensitivity Analysis ###################
#' Sensitivity Analysis: How Sensitive is Carbon to Method Choice?
#'
#' @description
#' Quantifies how sensitive carbon/biomass estimates are to the choice of
#' allometric method. This helps answer the question: "How much does my
#' reported carbon change depending on which method I use?"
#'
#' This function can accept either:
#' \itemize{
#'   \item Output from \code{allometries()} directly (recommended for consistency)
#'   \item Raw tree data (for backward compatibility)
#' }
#'
#' @param data Either:
#'   \itemize{
#'     \item A data frame output from \code{allometries()} containing columns
#'       like \code{WCC_C_t}, \code{biomass_C_t}, etc.
#'     \item A data frame with tree measurements (dbh, height, genus, species)
#'       for calculating estimates internally
#'   }
#' @param methods Character vector of methods to compare. Default:
#'   \code{c("WCC", "BIOMASS", "allodb", "Bunce")}. Must match column prefixes
#'   in allometries output (e.g., "WCC" matches "WCC_C_t").
#' @param returnv Character. "AGC" for carbon or "AGB" for biomass. Only used
#'   when detecting columns from allometries output. Default "AGC".
#' @param coords Coordinates for BIOMASS/allodb (longitude, latitude).
#'   Only required if \code{data} is raw tree data.
#' @param type Tree type: "broadleaf" or "conifer". Only used if \code{data}
#'   is raw tree data. Default "broadleaf".
#' @param method Carbon conversion method (Thomas, IPCC2, etc.). Only used if
#'   \code{data} is raw tree data. Default "IPCC2".
#' @param biome Biome for carbon conversion. Only used if \code{data} is raw
#'   tree data. Default "temperate".
#' @param aggregate Logical. If TRUE, returns plot-level sensitivity. If FALSE,
#'   returns per-tree sensitivity. Default TRUE.
#' @param reference Reference method for percent difference calculations.
#'   Default "WCC".
#'
#' @return An S3 object of class "sensitivity_analysis" containing:
#'   \describe{
#'     \item{method_summary}{Data frame with per-method totals and rankings}
#'     \item{by_tree}{Per-tree estimates from each method}
#'     \item{sensitivity_metrics}{Key sensitivity indicators (CV, spread, etc.)}
#'     \item{sensitivity_level}{Interpretation level (LOW/MODERATE/HIGH/VERY HIGH)}
#'     \item{interpretation}{Plain-language interpretation}
#'     \item{recommendation}{Suggested actions based on sensitivity}
#'   }
#'
#' @details
#' \strong{Recommended Usage:}
#' For consistent results, first run \code{allometries()} with your desired
#' parameters, then pass that output to \code{sensitivity_analysis()}:
#'
#' \preformatted{
#' results <- allometries(genus, species, dbh, height, method = "IPCC2", ...)
#' sens <- sensitivity_analysis(results)
#' }
#'
#' \strong{Sensitivity Metrics Computed:}
#' \describe{
#'   \item{range_ratio}{Max / Min estimate (fold difference) - \strong{most robust metric}}
#'   \item{spread_pct}{Spread (max - min) as percentage of mean}
#'   \item{cv}{Coefficient of variation across methods}
#'   \item{spread}{Maximum - minimum estimate across methods (absolute)}
#' }
#'
#' \strong{Statistical Note:}
#' The sensitivity metrics are calculated across the 4 fixed allometric methods
#' (WCC, BIOMASS, allodb, Bunce). These methods are not random samples from a
#' population of possible methods - they are specific, commonly-used approaches
#' selected for their relevance and validation. Therefore:
#' \itemize{
#'   \item The CV describes "how much these specific methods disagree"
#'   \item It does NOT describe sampling uncertainty from a population of methods
#'   \item The \strong{range_ratio} (max/min) is the most robust metric with n=4 methods,
#'     as it doesn't assume any distribution
#'   \item Adding more methods would increase statistical precision but only if
#'     those methods are equally valid for your application
#' }
#'
#' \strong{Interpretation Guidelines:}
#' \itemize{
#'   \item CV < 0.1: Low sensitivity - method choice has minor impact
#'   \item CV 0.1-0.25: Moderate sensitivity - report range or use ensemble
#'   \item CV 0.25-0.5: High sensitivity - method choice significantly affects results
#'   \item CV > 0.5: Very high sensitivity - investigate cause, consider data quality
#' }
#'
#' @examples
#' \dontrun{
#' # Recomended: Use allometries() output for consistency
#' results <- allometries(genus = c("Quercus", "Fagus"), species = c("robur", "sylvatica"),
#'   dbh = c(45, 38), height = c(18, 15), method = "IPCC2")
#'
#' sens <- sensitivity_analysis(results)
#' print(sens)
#' plot(sens)
#'
#' # Alternative: Raw tree data (uses defaults for carbon conversion)
#' tree_data <- data.frame(
#'   dbh = c(45, 38), height = c(18, 15),
#'   genus = c("Quercus", "Fagus"),
#'   species = c("robur", "sylvatica")
#' )
#' sens <- sensitivity_analysis(tree_data, coords = c(-0.29, 51.48))
#' }
#'
#' @export
sensitivity_analysis <- function(data,  methods = c("WCC", "BIOMASS", "allodb", "Bunce"),
                                 returnv = "AGC", coords = NULL, type = "broadleaf",
                                 method = "IPCC2", biome = "temperate", aggregate = TRUE,
                                 reference = "WCC") {

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  n_trees <- nrow(data)

  ###### DETECT INPUT TYPE: allometries() output OR raw tree data

  # Check if this looks like allometries() output by looking for method columns
  suffix <- if (returnv == "AGC") "_C_t" else "_B_t"
  possible_cols <- paste0(c("WCC", "biomass", "allodb", "Bunce"), suffix)
  has_allometry_cols <- any(possible_cols %in% names(data))

  if (has_allometry_cols) {
    ###### Option A #####
    # Data is from allometries() - extract directly (Recomended)

    # Map method names to column names
    col_map <- c(
      "WCC" = paste0("WCC", suffix),
      "BIOMASS" = paste0("biomass", suffix),
      "allodb" = paste0("allodb", suffix),
      "Bunce" = paste0("Bunce", suffix)
    )

    # Initialize results storage
    estimates <- data.frame(tree_id = seq_len(n_trees))
    method_results <- list()
    method_success <- character()

    # Extract each requested method from the data
    for (m in methods) {
      col_name <- col_map[m]
      if (!is.null(col_name) && col_name %in% names(data)) {
        vals <- data[[col_name]]
        if (!all(is.na(vals))) {
          estimates[[m]] <- vals
          method_success <- c(method_success, m)
          method_results[[m]] <- vals
        }
      } else {
        warning(sprintf("Method %s column '%s' not found in data", m, col_name))
      }
    }

  } else {
    ##### Option B #####
    # Raw tree data - calculate using allometries() internally

    if (!"dbh" %in% names(data)) {
      stop("'data' must contain a 'dbh' column")
    }
    if (!"height" %in% names(data)) {
      stop("'data' must contain a 'height' column for allometry calculations")
    }
    if (!"genus" %in% names(data) || !"species" %in% names(data)) {
      stop("'data' must contain 'genus' and 'species' columns")
    }
    if (is.null(coords)) {
      stop("'coords' must be provided when using raw tree data")
    }

    # Get type vector
    type_vec <- if ("type" %in% names(data)) data$type else rep(type, n_trees)

    # Call allometries() to ensure consistent calculations
    allo_results <- tryCatch({
      allometries(
        genus = data$genus,
        species = data$species,
        dbh = data$dbh,
        height = data$height,
        type = type_vec,
        method = method,
        returnv = returnv,
        coords = coords,
        biome = biome
      )
    }, error = function(e) {
      stop("Failed to calculate allometries: ", e$message)
    })

    # Now extract from the allometries results
    col_map <- c(
      "WCC" = paste0("WCC", suffix),
      "BIOMASS" = paste0("biomass", suffix),
      "allodb" = paste0("allodb", suffix),
      "Bunce" = paste0("Bunce", suffix)
    )

    estimates <- data.frame(tree_id = seq_len(n_trees))
    method_results <- list()
    method_success <- character()

    for (m in methods) {
      col_name <- col_map[m]
      if (!is.null(col_name) && col_name %in% names(allo_results)) {
        vals <- allo_results[[col_name]]
        if (!all(is.na(vals))) {
          estimates[[m]] <- vals
          method_success <- c(method_success, m)
          method_results[[m]] <- vals
        }
      }
    }
  }

  #################### Calculate Sensitivity Metrics ###################
  if (length(method_success) < 2) {
    stop("Need at least 2 successful methods for sensitivity analysis. ",
         "Only found: ", paste(method_success, collapse = ", "))
  }

  method_cols <- method_success
  estimate_matrix <- as.matrix(estimates[, method_cols, drop = FALSE])

  # Per-tree metrics
  tree_means <- rowMeans(estimate_matrix, na.rm = TRUE)
  tree_sds <- apply(estimate_matrix, 1, sd, na.rm = TRUE)
  tree_mins <- apply(estimate_matrix, 1, min, na.rm = TRUE)
  tree_maxs <- apply(estimate_matrix, 1, max, na.rm = TRUE)
  tree_spreads <- tree_maxs - tree_mins
  tree_cvs <- 100 * tree_sds / tree_means
  tree_range_ratios <- tree_maxs / tree_mins

  estimates$mean_estimate <- tree_means
  estimates$sd_across_methods <- tree_sds
  estimates$cv_pct <- tree_cvs
  estimates$spread <- tree_spreads
  estimates$spread_pct <- 100 * tree_spreads / tree_means
  estimates$range_ratio <- tree_range_ratios

  # Overall (plot-level) metrics
  if (aggregate) {
    total_by_method <- colSums(estimate_matrix, na.rm = TRUE)
    overall_mean <- mean(total_by_method)
    overall_sd <- sd(total_by_method)
    overall_cv <- 100 * overall_sd / overall_mean
    overall_spread <- max(total_by_method) - min(total_by_method)
    overall_spread_pct <- 100 * overall_spread / overall_mean
    overall_range_ratio <- max(total_by_method) / min(total_by_method)
  } else {
    overall_cv <- mean(tree_cvs, na.rm = TRUE)
    overall_spread_pct <- mean(estimates$spread_pct, na.rm = TRUE)
    overall_range_ratio <- mean(tree_range_ratios, na.rm = TRUE)
  }

  # Reference comparison
  if (reference %in% method_success) {
    ref_vals <- estimates[[reference]]
    for (m in method_success) {
      if (m != reference) {
        pct_diff <- 100 * (estimates[[m]] - ref_vals) / ref_vals
        estimates[[paste0(m, "_vs_", reference, "_pct")]] <- pct_diff
      }
    }
  }

  # Build method summary
  method_summary <- data.frame(
    method = method_success,
    total = sapply(method_cols, function(m) sum(estimates[[m]], na.rm = TRUE)),
    mean_per_tree = sapply(method_cols, function(m) mean(estimates[[m]], na.rm = TRUE)),
    sd_per_tree = sapply(method_cols, function(m) sd(estimates[[m]], na.rm = TRUE)),
    stringsAsFactors = FALSE
  )

  if (reference %in% method_success) {
    ref_total <- method_summary$total[method_summary$method == reference]
    method_summary$pct_diff_vs_ref <- 100 * (method_summary$total - ref_total) / ref_total
  }

  method_summary <- method_summary[order(-method_summary$total), ]
  method_summary$rank <- seq_len(nrow(method_summary))

  # Sensitivity metrics summary
  sensitivity_metrics <- list(
    cv_across_methods = overall_cv,
    spread_pct = overall_spread_pct,
    range_ratio = overall_range_ratio,
    n_methods = length(method_success),
    methods_used = method_success,
    n_trees = n_trees,
    reference = reference,
    measure = if (returnv == "AGC") "carbon" else "biomass",
    unit = "tonnes"
  )

  # Generate interpretation
  interpretation <- .interpret_sensitivity(overall_cv, overall_spread_pct,
                                           overall_range_ratio, method_summary)

  # Build result object
  result <- list(
    method_summary = method_summary,
    by_tree = estimates,
    sensitivity_metrics = sensitivity_metrics,
    interpretation = interpretation$text,
    sensitivity_level = interpretation$level,
    recommendation = interpretation$recommendation,
    method_results = method_results
  )

  class(result) <- c("sensitivity_analysis", "list")
  return(result)
}


#' @keywords internal
.interpret_sensitivity <- function(cv, spread_pct, range_ratio, method_summary) {

  # Determine sensitivity level
  if (cv < 10) {
    level <- "LOW"
    text <- sprintf(
      "Method sensitivity is LOW (CV = %.1f%%). The choice of allometric method has minimal impact on reported carbon. All methods produce similar estimates within a %.1f%% spread.",
      cv, spread_pct
    )
    recommendation <- "Any of the tested methods can be used with confidence. Choose based on regional appropriateness or data availability."

  } else if (cv < 25) {
    level <- "MODERATE"
    text <- sprintf(
      "Method sensitivity is MODERATE (CV = %.1f%%). Method choice affects estimates by up to %.1f%% (range ratio: %.2fx). This variation is typical for forest carbon estimation.",
      cv, spread_pct, range_ratio
    )
    recommendation <- "Report the estimate range across methods, or use the method most appropriate for your region. Consider using an ensemble (mean) of methods."

  } else if (cv < 50) {
    level <- "HIGH"
    text <- sprintf(
      "Method sensitivity is HIGH (CV = %.1f%%). Estimates vary by %.1f%% depending on method choice (range ratio: %.2fx). Method selection significantly impacts reported carbon.",
      cv, spread_pct, range_ratio
    )
    recommendation <- "Carefully justify method selection. Report uncertainty bounds. Consider whether tree characteristics (size, species) fall outside method calibration ranges."

  } else {
    level <- "VERY HIGH"
    text <- sprintf(
      "Method sensitivity is VERY HIGH (CV = %.1f%%). Estimates differ by more than %.1f%% (range ratio: %.2fx). This suggests potential issues with data or method applicability.",
      cv, spread_pct, range_ratio
    )
    recommendation <- "Investigate the cause of high variation. Check: (1) Are inputs within method valid ranges? (2) Are species appropriately matched? (3) Is height data accurate? Consider using only the most regionally-appropriate method."
  }

  # Add method-specific notes
  if (nrow(method_summary) >= 2) {
    highest <- method_summary$method[1]
    lowest <- method_summary$method[nrow(method_summary)]
    diff_pct <- 100 * (method_summary$total[1] - method_summary$total[nrow(method_summary)]) /
      method_summary$total[nrow(method_summary)]

    text <- paste0(text, sprintf(
      " %s gives the highest estimates and %s the lowest (%.1f%% difference).",
      highest, lowest, diff_pct
    ))
  }

  list(
    level = level,
    text = text,
    recommendation = recommendation
  )
}


#' @export
print.sensitivity_analysis <- function(x, ...) {

  cat("------------ SENSITIVITY ANALYSIS ------------\n")
  cat("Method Choice Impact on Carbon \n\n")

  # Key metrics
  m <- x$sensitivity_metrics
  cat(sprintf("Trees analyzed:     %d\n", m$n_trees))
  cat(sprintf("Methods compared:   %s\n", paste(m$methods_used, collapse = ", ")))
  cat(sprintf("Reference method:   %s\n", m$reference))
  cat(sprintf("Output:             %s (%s)\n\n", m$measure, m$unit))

  # Sensitivity level banner
  level_colors <- list(
    LOW = "LOW",
    MODERATE = "MODERATE",
    HIGH = "HIGH [!]",
    `VERY HIGH` = "VERY HIGH [!!]"
  )
  cat(sprintf("SENSITIVITY LEVEL: %s (CV = %.1f%%)\n\n",
              level_colors[[x$sensitivity_level]], m$cv_across_methods))

  # Method comparison table
  cat("METHOD COMPARISON:\n")
  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("%-12s %12s %12s %12s %8s\n",
              "Method", "Total (t)", "Mean/tree", "vs Ref (%)", "Rank"))
  cat("--------------------------------------------------------------------------------\n")

  ms <- x$method_summary
  for (i in seq_len(nrow(ms))) {
    ref_diff <- if ("pct_diff_vs_ref" %in% names(ms)) sprintf("%+.1f", ms$pct_diff_vs_ref[i]) else "-"
    cat(sprintf("%-12s %12.3f %12.3f %12s %8d\n",
                ms$method[i], ms$total[i], ms$mean_per_tree[i], ref_diff, ms$rank[i]))
  }
  cat("--------------------------------------------------------------------------------\n\n")

  # Key statistics
  cat("KEY STATISTICS:\n")
  cat(sprintf("  - CV across methods:  %.1f%%\n", m$cv_across_methods))
  cat(sprintf("  - Spread:             %.1f%%\n", m$spread_pct))
  cat(sprintf("  - Range ratio:        %.2fx (max/min)\n\n", m$range_ratio))

  # Interpretation
  cat("INTERPRETATION:\n")
  cat(strwrap(x$interpretation, width = 78, prefix = "  "), sep = "\n")
  cat("\n")

  # Recommendation
  cat("RECOMMENDATION:\n")
  cat(strwrap(x$recommendation, width = 78, prefix = "  "), sep = "\n")
  cat("\n")
  cat("================================================================================\n")

  invisible(x)
}


#' @export
summary.sensitivity_analysis <- function(object, ...) {
  print(object)
  invisible(object)
}


#' Plot Sensitivity Analysis Results
#'
#' @description Creates visualizations of sensitivity analysis results showing
#'   how carbon estimates vary across different allometric methods.
#'
#' @param x A sensitivity_analysis object from \code{sensitivity_analysis()}
#' @param type Character. Type of plot: "comparison" or "bar" (default) for
#'   bar chart of totals, "deviation" or "ref" for deviation from reference,
#'   "tree" for per-tree CV distribution (requires multiple trees).
#' @param ... Additional arguments (currently unused)
#'
#' @return A ggplot2 object (invisibly)
#'
#' @examples
#' \dontrun{
#' result <- sensitivity_analysis(tree_data, coords = c(-0.29, 51.48))
#' plot(result)  # Bar comparison
#' plot(result, type = "deviation")  # Deviation from reference
#' }
#'
#' @method plot sensitivity_analysis
#' @export
plot.sensitivity_analysis <- function(x, type = "comparison", ...) {

  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting. Install with: install.packages('ggplot2')")
  }

  # Validate input is a sensitivity_analysis object

  if (!inherits(x, "sensitivity_analysis")) {
    stop("'x' must be a sensitivity_analysis object. ",
         "Use sensitivity_analysis() to create one.")
  }

  # Extract method summary data
  ms <- x$method_summary

  if (type == "comparison" || type == "bar") {
    # Bar chart of totals by method
    p <- ggplot2::ggplot(ms, ggplot2::aes(x = stats::reorder(method, -total),
                                          y = total, fill = method)) +
      ggplot2::geom_col(alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", total)),
                         vjust = -0.5, size = 3.5) +
      ggplot2::labs(
        title = "Carbon Estimates by Allometric Method",
        subtitle = sprintf("Sensitivity: %s (CV = %.1f%%)",
                           x$sensitivity_level, x$sensitivity_metrics$cv_across_methods),
        x = "Method",
        y = sprintf("Total %s (tonnes)", x$sensitivity_metrics$measure),
        fill = "Method"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = "Set2")

  } else if (type == "deviation" || type == "ref") {
    # Deviation from reference
    if (!"pct_diff_vs_ref" %in% names(ms)) {
      stop("Reference comparison not available. ",
           "The reference method may have failed.")
    }

    ms$sign <- ifelse(ms$pct_diff_vs_ref >= 0, "Higher", "Lower")

    p <- ggplot2::ggplot(ms, ggplot2::aes(x = stats::reorder(method, pct_diff_vs_ref),
                                          y = pct_diff_vs_ref, fill = sign)) +
      ggplot2::geom_col(alpha = 0.8) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = sprintf("Deviation from %s Reference Method", x$sensitivity_metrics$reference),
        x = "Method",
        y = "% Difference from Reference",
        fill = ""
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_manual(values = c("Higher" = "#66c2a5", "Lower" = "#fc8d62"))

  } else if (type == "tree" && nrow(x$by_tree) > 1) {
    # Per-tree CV distribution
    tree_data <- x$by_tree

    p <- ggplot2::ggplot(tree_data, ggplot2::aes(x = cv_pct)) +
      ggplot2::geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
      ggplot2::geom_vline(xintercept = mean(tree_data$cv_pct, na.rm = TRUE),
                          color = "red", linetype = "dashed", linewidth = 1) +
      ggplot2::labs(
        title = "Distribution of Per-Tree Method Sensitivity",
        subtitle = sprintf("Mean CV = %.1f%%", mean(tree_data$cv_pct, na.rm = TRUE)),
        x = "CV across methods (%)",
        y = "Number of trees"
      ) +
      ggplot2::theme_minimal()

  } else if (type == "tree" && nrow(x$by_tree) == 1) {
    warning("'tree' plot type requires multiple trees. Using 'comparison' instead.")
    return(plot.sensitivity_analysis(x, type = "comparison", ...))

  } else {
    stop("Unknown plot type '", type, "'. Use 'comparison', 'deviation', or 'tree'")
  }

  print(p)
  invisible(p)
}


#' Convenience function for plotting sensitivity analysis
#'
#' @description Alternative function for plotting sensitivity analysis results
#'   that can be used when S3 method dispatch doesn't work (e.g., when sourcing
#'   files directly).
#'
#' @param x A sensitivity_analysis object
#' @param type Plot type (see \code{plot.sensitivity_analysis})
#' @param ... Additional arguments
#'
#' @return A ggplot2 object
#' @export
plot_sensitivity <- function(x, type = "comparison", ...) {
  plot.sensitivity_analysis(x, type = type, ...)
}


################## Bootstrap Sensitivity Analysis ##################
#' Bootstrap Sensitivity Analysis with Measurement Uncertainty
#'
#' @description
#' Quantifies the uncertainty around sensitivity metrics (CV, range ratio) by
#' propagating measurement errors through all allometric methods via bootstrapping.
#'
#' This answers the question: "Given my measurement uncertainty, how confident
#' am I in my conclusion about method sensitivity?"
#'
#' @param data A data frame with columns: dbh, height, genus, species.
#'   Optionally: type, name.
#' @param methods Character vector of methods to compare. Default:
#'   \code{c("WCC", "BIOMASS", "allodb", "Bunce")}
#' @param coords Coordinates for BIOMASS or allodb (longitude, latitude).
#' @param type Tree type: "broadleaf" or "conifer". Default "broadleaf".
#' @param n_boot Number of bootstrap iterations. Default 500.
#' @param dbh_cv Coefficient of variation for DBH measurement error (proportion).
#'   Default 0.02 (2 percent).
#' @param height_cv Coefficient of variation for height measurement error (proportion).
#'   Default 0.05 (5 percent).
#' @param method Carbon conversion method. Default "IPCC2".
#' @param biome Biome for carbon conversion. Default "temperate".
#' @param seed Random seed for reproducibility. Default NULL (no seed).
#' @param verbose Print progress messages. Default TRUE.
#'
#' @return A list of class "bootstrap_sensitivity" containing:
#' \itemize{
#'   \item cv_observed: The observed CV from point estimates
#'   \item cv_boot: Vector of bootstrapped CVs
#'   \item cv_ci: 95 percent confidence interval on CV
#'   \item cv_se: Standard error of CV
#'   \item range_ratio_observed: The observed range ratio
#'   \item range_ratio_boot: Vector of bootstrapped range ratios
#'   \item range_ratio_ci: 95 percent confidence interval on range ratio
#'   \item spread_pct_boot: Vector of bootstrapped spread percentages
#'   \item spread_pct_ci: 95 percent confidence interval on spread percentage
#'   \item n_boot: Number of bootstrap iterations completed
#'   \item n_failed: Number of failed iterations
#'   \item interpretation: Plain-language interpretation of results
#' }
#'
#' @details
#' \strong{Algorithm:}
#' For each bootstrap iteration:
#' \enumerate{
#'   \item Perturb DBH using normally-distributed relative error
#'   \item Perturb height using normally-distributed relative error
#'   \item Run all allometric methods with perturbed inputs
#'   \item Calculate between-method CV from the perturbed totals
#'   \item Store the CV
#' }
#' Then compute confidence intervals on the distribution of CVs.
#'
#' \strong{Interpretation:}
#' If the 95 percent CI on CV is narrow (around 5 percentage points),
#' your sensitivity conclusion is robust to measurement error. If wide
#' (more than 20 percentage points), measurement error significantly
#' affects your sensitivity assessment.
#'
#' \strong{Computational Note:}
#' This function is computationally intensive. For 100 trees and 500
#' bootstrap iterations, it runs 2000 allometry calculations. Consider
#' using fewer iterations (n_boot = 100) for initial exploration.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' tree_data <- data.frame(dbh = c(30, 45, 60), height = c(15, 20, 25),
#'   genus = c("Quercus", "Fagus", "Fraxinus"),
#'   species = c("robur", "sylvatica", "excelsior"))
#'
#' result <- bootstrap_sensitivity(tree_data, coords = c(-0.29, 51.48),
#'   n_boot = 100,  # Use more for final analysis
#'   seed = 42)
#'
#' print(result)
#' plot(result)
#' }
#'
#' @seealso \code{\link{sensitivity_analysis}} for point estimate sensitivity,
#'   \code{\link{mc_uncertainty}} for single-method uncertainty
#'
#' @export
bootstrap_sensitivity <- function(data,
                                  methods = c("WCC", "BIOMASS", "allodb", "Bunce"), coords = NULL,
                                  type = "broadleaf", n_boot = 500, dbh_cv = 0.02, height_cv = 0.05,
                                  method = "IPCC2", biome = "temperate", seed = NULL, verbose = TRUE) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  if (!all(c("dbh", "height", "genus", "species") %in% names(data))) {
    stop("'data' must contain columns: dbh, height, genus, species")
  }
  if (is.null(coords) && any(c("BIOMASS", "allodb") %in% methods)) {
    stop("'coords' required for BIOMASS and allodb methods")
  }

  if (!is.null(seed)) set.seed(seed)

  n_trees <- nrow(data)
  n_methods <- length(methods)

  if (verbose) { message(sprintf("Bootstrap sensitivity analysis: %d trees,
                    %d methods, %d iterations", n_trees, n_methods, n_boot))
    message(sprintf("Measurement error: DBH CV = %.1f%%, Height CV = %.1f%%",
                    100 * dbh_cv, 100 * height_cv))
  }

  # Storage for bootstrap results
  cv_boot <- numeric(n_boot)
  range_ratio_boot <- numeric(n_boot)
  spread_pct_boot <- numeric(n_boot)
  n_failed <- 0

  # Get type vector

  type_vec <- if ("type" %in% names(data)) data$type else rep(type, n_trees)

  # Progress tracking
  if (verbose) pb <- txtProgressBar(min = 0, max = n_boot, style = 3)

  for (i in seq_len(n_boot)) {

    # Perturb measurements
    dbh_perturbed <- data$dbh * (1 + stats::rnorm(n_trees, 0, dbh_cv))
    height_perturbed <- data$height * (1 + stats::rnorm(n_trees, 0, height_cv))

    # Ensure positive values
    dbh_perturbed <- pmax(dbh_perturbed, 1)  # Min 1 cm
    height_perturbed <- pmax(height_perturbed, 1)  # Min 1 m

    # Run allometries with perturbed inputs
    result <- tryCatch({
      allometries(
        genus = data$genus,
        species = data$species,
        dbh = dbh_perturbed,
        height = height_perturbed,
        type = type_vec,
        method = method,
        returnv = "AGC",
        coords = coords,
        biome = biome
      )
    }, error = function(e) NULL)

    if (is.null(result)) {
      n_failed <- n_failed + 1
      cv_boot[i] <- NA
      range_ratio_boot[i] <- NA
      spread_pct_boot[i] <- NA
      next
    }

    # Extract totals for each method
    suffix <- "_C_t"
    col_map <- c(
      "WCC" = paste0("WCC", suffix),
      "BIOMASS" = paste0("biomass", suffix),
      "allodb" = paste0("allodb", suffix),
      "Bunce" = paste0("Bunce", suffix)
    )

    totals <- sapply(methods, function(m) {
      col <- col_map[m]
      if (col %in% names(result)) sum(result[[col]], na.rm = TRUE) else NA
    })

    # Remove NAs and calculate metrics
    valid_totals <- totals[!is.na(totals)]

    if (length(valid_totals) >= 2) {
      cv_boot[i] <- 100 * stats::sd(valid_totals) / mean(valid_totals)
      range_ratio_boot[i] <- max(valid_totals) / min(valid_totals)
      spread_pct_boot[i] <- 100 * (max(valid_totals) - min(valid_totals)) / mean(valid_totals)
    } else {
      cv_boot[i] <- NA
      range_ratio_boot[i] <- NA
      spread_pct_boot[i] <- NA
      n_failed <- n_failed + 1
    }

    if (verbose) setTxtProgressBar(pb, i)
  }

  if (verbose) close(pb)

  # Calculate observed (point estimate) sensitivity
  observed <- tryCatch({
    allometries(
      genus = data$genus,
      species = data$species,
      dbh = data$dbh,
      height = data$height,
      type = type_vec,
      method = method,
      returnv = "AGC",
      coords = coords,
      biome = biome
    )
  }, error = function(e) NULL)

  if (!is.null(observed)) {
    obs_totals <- sapply(methods, function(m) {
      col <- col_map[m]
      if (col %in% names(observed)) sum(observed[[col]], na.rm = TRUE) else NA
    })
    valid_obs <- obs_totals[!is.na(obs_totals)]
    cv_observed <- 100 * stats::sd(valid_obs) / mean(valid_obs)
    range_ratio_observed <- max(valid_obs) / min(valid_obs)
    spread_pct_observed <- 100 * (max(valid_obs) - min(valid_obs)) / mean(valid_obs)
  } else {
    cv_observed <- NA
    range_ratio_observed <- NA
    spread_pct_observed <- NA
  }

  # Remove failed iterations for summary
  cv_valid <- cv_boot[!is.na(cv_boot)]
  rr_valid <- range_ratio_boot[!is.na(range_ratio_boot)]
  sp_valid <- spread_pct_boot[!is.na(spread_pct_boot)]

  # Calculate confidence intervals
  cv_ci <- stats::quantile(cv_valid, c(0.025, 0.975))
  range_ratio_ci <- stats::quantile(rr_valid, c(0.025, 0.975))
  spread_pct_ci <- stats::quantile(sp_valid, c(0.025, 0.975))

  # Generate interpretation
  ci_width <- cv_ci[2] - cv_ci[1]
  if (ci_width < 10) {
    stability <- "very stable"
    interpretation <- sprintf(
      "Your sensitivity estimate is %s (95%% CI width = %.1f%%). Measurement error has minimal impact on the conclusion that method sensitivity is %.0f%% (95%% CI: %.1f%% - %.1f%%).",
      stability, ci_width, cv_observed, cv_ci[1], cv_ci[2]
    )
  } else if (ci_width < 25) {
    stability <- "moderately stable"
    interpretation <- sprintf(
      "Your sensitivity estimate is %s (95%% CI width = %.1f%%). The true CV is likely between %.1f%% and %.1f%%, which spans the same sensitivity category.",
      stability, ci_width, cv_ci[1], cv_ci[2]
    )
  } else {
    stability <- "uncertain"
    interpretation <- sprintf(
      "Your sensitivity estimate is %s (95%% CI width = %.1f%%). Measurement error significantly affects the sensitivity conclusion. Consider improving measurement precision or using the range ratio (%.2fx, 95%% CI: %.2fx - %.2fx) which may be more stable.",
      stability, ci_width, range_ratio_observed, range_ratio_ci[1], range_ratio_ci[2]
    )
  }

  if (verbose) {
    message(sprintf("\nCompleted: %d successful, %d failed iterations",
                    n_boot - n_failed, n_failed))
    message(sprintf("CV: %.1f%% [95%% CI: %.1f%% - %.1f%%]",
                    cv_observed, cv_ci[1], cv_ci[2]))
  }

  # Build result object
  result <- list(
    cv_observed = cv_observed,
    cv_boot = cv_valid,
    cv_ci = cv_ci,
    cv_se = stats::sd(cv_valid),
    cv_mean_boot = mean(cv_valid),
    range_ratio_observed = range_ratio_observed,
    range_ratio_boot = rr_valid,
    range_ratio_ci = range_ratio_ci,
    spread_pct_observed = spread_pct_observed,
    spread_pct_boot = sp_valid,
    spread_pct_ci = spread_pct_ci,
    n_boot = n_boot,
    n_successful = n_boot - n_failed,
    n_failed = n_failed,
    n_trees = n_trees,
    methods = methods,
    dbh_cv = dbh_cv,
    height_cv = height_cv,
    interpretation = interpretation
  )

  class(result) <- c("bootstrap_sensitivity", "list")
  return(result)
}


#' @export
print.bootstrap_sensitivity <- function(x, ...) {

  cat("-------- BOOTSTRAP SENSITIVITY ANALYSIS: Measurement Uncertainty --------)\n")

  cat(sprintf("Trees analyzed:       %d\n", x$n_trees))
  cat(sprintf("Methods compared:     %s\n", paste(x$methods, collapse = ", ")))
  cat(sprintf("Bootstrap iterations: %d (%d successful)\n", x$n_boot, x$n_successful))
  cat(sprintf("Measurement error:    DBH CV = %.1f%%, Height CV = %.1f%%\n\n",
              100 * x$dbh_cv, 100 * x$height_cv))

  cat("--- SENSITIVITY METRICS WITH CONFIDENCE INTERVALS ---\n\n")

  cat(sprintf("CV across methods:\n"))
  cat(sprintf("  Point estimate:     %.1f%%\n", x$cv_observed))
  cat(sprintf("  Bootstrap mean:     %.1f%%\n", x$cv_mean_boot))
  cat(sprintf("  95%% CI:             [%.1f%%, %.1f%%]\n", x$cv_ci[1], x$cv_ci[2]))
  cat(sprintf("  Standard error:     %.2f%%\n\n", x$cv_se))

  cat(sprintf("Range ratio (max/min):\n"))
  cat(sprintf("  Point estimate:     %.2fx\n", x$range_ratio_observed))
  cat(sprintf("  95%% CI:             [%.2fx, %.2fx]\n\n", x$range_ratio_ci[1], x$range_ratio_ci[2]))

  cat(sprintf("Spread (%% of mean):\n"))
  cat(sprintf("  Point estimate:     %.1f%%\n", x$spread_pct_observed))
  cat(sprintf("  95%% CI:             [%.1f%%, %.1f%%]\n\n", x$spread_pct_ci[1], x$spread_pct_ci[2]))

  cat("--- INTERPRETATION ---\n")
  cat(strwrap(x$interpretation, width = 78, prefix = "  "), sep = "\n")
  cat("\n")

  invisible(x)
}


#' @export
plot.bootstrap_sensitivity <- function(x, type = "histogram", ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting")
  }

  if (type == "histogram" || type == "cv") {
    # Histogram of bootstrapped CVs
    df <- data.frame(cv = x$cv_boot)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = cv)) +
      ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
      ggplot2::geom_vline(xintercept = x$cv_observed, color = "red",
                          linetype = "dashed", linewidth = 1) +
      ggplot2::geom_vline(xintercept = x$cv_ci, color = "darkred",
                          linetype = "dotted", linewidth = 0.8) +
      ggplot2::annotate("text", x = x$cv_observed, y = Inf,
                        label = sprintf("Observed: %.1f%%", x$cv_observed),
                        vjust = 2, hjust = -0.1, color = "red", fontface = "bold") +
      ggplot2::labs(
        x = "CV across methods (%)",
        y = "Frequency",
        title = "Bootstrap Distribution of Method Sensitivity (CV)",
        subtitle = sprintf("n = %d iterations | 95%% CI: [%.1f%%, %.1f%%]",
                           x$n_successful, x$cv_ci[1], x$cv_ci[2])
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40"))

  } else if (type == "range_ratio") {
    # Histogram of range ratios
    df <- data.frame(rr = x$range_ratio_boot)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = rr)) +
      ggplot2::geom_histogram(bins = 30, fill = "purple", alpha = 0.7, color = "white") +
      ggplot2::geom_vline(xintercept = x$range_ratio_observed, color = "red",
                          linetype = "dashed", linewidth = 1) +
      ggplot2::geom_vline(xintercept = x$range_ratio_ci, color = "darkred",
                          linetype = "dotted", linewidth = 0.8) +
      ggplot2::labs(
        x = "Range Ratio (max/min)",
        y = "Frequency",
        title = "Bootstrap Distribution of Range Ratio",
        subtitle = sprintf("Observed: %.2fx | 95%% CI: [%.2fx, %.2fx]",
                           x$range_ratio_observed, x$range_ratio_ci[1], x$range_ratio_ci[2])
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))

  } else {
    stop("type must be 'histogram', 'cv', or 'range_ratio'")
  }

  print(p)
  invisible(p)
}

