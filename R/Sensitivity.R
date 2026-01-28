# ==============================================================================
# Method Sensitivity Analysis
# ==============================================================================

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
#'   \item CV < 10\%: Low sensitivity - method choice has minor impact
#'   \item CV 10-25\%: Moderate sensitivity - report range or use ensemble
#'   \item CV 25-50\%: High sensitivity - method choice significantly affects results
#'   \item CV > 50\%: Very high sensitivity - investigate cause, consider data quality
#' }
#'
#' @examples
#' \dontrun{
#' # RECOMMENDED: Use allometries() output for consistency
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

  # ============================================================================
  # DETECT INPUT TYPE: allometries() output OR raw tree data
  # ============================================================================

  # Check if this looks like allometries() output by looking for method columns
  suffix <- if (returnv == "AGC") "_C_t" else "_B_t"
  possible_cols <- paste0(c("WCC", "biomass", "allodb", "Bunce"), suffix)
  has_allometry_cols <- any(possible_cols %in% names(data))

  if (has_allometry_cols) {
    # =========================================================================
    # Option A: Data is from allometries() - extract directly (RECOMMENDED)
    # =========================================================================

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
    # =========================================================================
    # Option B: Raw tree data - calculate using allometries() internally
    # =========================================================================

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

  # ============================================================================
  # Calculate Sensitivity Metrics
  # ============================================================================
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

  cat("\n")
  cat("================================================================================\n")
  cat("           SENSITIVITY ANALYSIS: Method Choice Impact on Carbon\n")
  cat("================================================================================\n\n")

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
