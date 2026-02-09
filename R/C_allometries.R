# ==============================================================================
# TreeCarbon - Alternative Allometric Methods
# ==============================================================================
#
# This module provides wrappers and implementations for non-WCC allometric
# methods, enabling cross-method comparison and validation of carbon estimates.
#
# Methods included:
#   - Bunce (1968): UK deciduous woodland biomass equations
#   - BIOMASS: Pantropical allometry via the BIOMASS R package
#   - allodb: Global tree allometry database equations
#
# Authors: Justin Moat (J.Moat@kew.org), Isabel Openshaw (I.Openshaw@kew.org)
#
# References:
#   - Bunce, R.G.H. (1968). Biomass and Production of Trees in a Mixed
#     Deciduous Woodland: I. Girth and Height as Parameters for the
#     Estimation of Tree Dry Weight. Journal of Ecology, 56(3), 759-775.
#     https://doi.org/10.2307/2258105
#
#   - Réjou-Méchain, M., et al. (2017). biomass: An R package for estimating
#     above-ground biomass and its uncertainty in tropical forests.
#     Methods in Ecology and Evolution, 8(9), 1163-1167.
#     https://doi.org/10.1111/2041-210X.12753
#
#   - Gonzalez-Akre, E., et al. (2022). allodb: An R package for biomass
#     estimation at globally distributed extratropical forest plots.
#     Methods in Ecology and Evolution, 13(2), 330-338.
#     https://doi.org/10.1111/2041-210X.13756
#
############# Bunce Equation carbon calculation ########################
#'
#' @title Bunce biomass equation
#' @description Calculates dry weight based on species and dbh using the
#'   Bunce (1968) allometric equations for UK deciduous woodland.
#'
#'   When \code{rich_output = TRUE}, returns a comprehensive result object
#'   including method metadata, assumptions, validity warnings, and uncertainty
#'
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param name species name (common or binomial)
#' @param type 'broadleaf' or 'conifer' (optional)
#' @param dbh diameter at breast height (cm)
#' @param re_dbh relative error for DBH measurement (optional). Expressed as proportion (e.g., 0.025 = 2.5%). If provided, uncertainty will be calculated.
#' @param re relative error for model residual. Default = NULL (uses species-specific values from Table 6 in Bunce 1968, ranging 0.12-0.15). If provided, overrides species-specific defaults. Accounts for model prediction error. Based on Bunce (1968) R² values ~0.85-0.95 and Table 6 confidence limits.
#' @param rich_output Logical. If TRUE, returns a rich result object with
#'   metadata including: value, method, reference, assumptions, validity_warning,
#'   flags, uncertainty interval, region, and source type. Default FALSE for
#'   backwards compatibility.
#' @return If \code{rich_output = FALSE} (default): data.frame with biomass in kg.
#'   If \code{rich_output = TRUE}: an \code{allometry_result} object (for single
#'   trees) or a list of such objects (for multiple trees) containing:
#' \describe{
#'   \item{value}{Estimated biomass (kg)}
#'   \item{method}{"Bunce"}
#'   \item{reference}{Bunce (1968)}
#'   \item{assumptions}{Assumptions of the method}
#'   \item{validity_warning}{Warnings if inputs outside valid range}
#'   \item{flags}{Extrapolation or default flags}
#'   \item{uncertainty}{Standard deviation if re_dbh provided}
#'   \item{ci_low}{Lower 95 percent confidence interval bound}
#'   \item{ci_high}{Upper 95 percent confidence interval bound}
#'   \item{region}{"United Kingdom"}
#'   \item{source_type}{"peer-reviewed"}
#' }
#' @references Bunce, R. G. H. "Biomass and Production of Trees in a Mixed
#' Deciduous Woodland: I. Girth and height as Parameters for the Estimation of
#' Tree Dry Weight" (1968)
#' @importFrom utils data
#' @examples
#' # Standard output (backwards compatible)
#' Bunce("Oak", 24)
#' Bunce(c("Oak", "Beech"), c(23,23))
#'
#' # Rich output with metadata
#' result <- Bunce("Oak", 24, re_dbh = 0.05, rich_output = TRUE)
#' print(result)  # Shows assumptions prominently
#' summary(result)
#'
#' # Check for extrapolation warnings
#' result <- Bunce(c("Oak", "Beech"), c(25, 200), rich_output = TRUE)  # Large DBH
#' result$validity_warnings
#' @export
#' @aliases Bunce
#'
Bunce <- function(name, dbh, type = NULL, re_dbh = NULL, re = NULL,
                  rich_output = FALSE) {

  # ==== Input validation ====
  if (missing(name) || missing(dbh)) stop("Both 'name' and 'dbh' are required.")
  if (!is.character(name)) stop("'name' must be a character vector.")
  if (!is.numeric(dbh)) stop("'dbh' must be numeric.")
  if (length(name) != length(dbh)) stop("'name' and 'dbh' must have the same length.")

  # ==== Validate against method ranges ====
  validation <- validate_inputs_for_method("Bunce", dbh = dbh)
  flags <- if (validation$flags[1] != "None") validation$flags else character()
  warnings_list <- if (validation$validity_warnings[1] != "None") validation$validity_warnings else character()

  # ==== Species lookup ====
  lookup <- lookupcode(name, type = type, code = "short")

  r <- data.frame(species_name = name, dbh = as.numeric(dbh), biomass = NA,
                  spcode = lookup$code, a = NA, b = NA, stringsAsFactors = FALSE, row.names = NULL)

  r$spcode[r$spcode == "MX"] <- "XB"

  # ==== Find matching species coefficients ====
  matched_index <- match(r$spcode, buncedf$spcode)
  r$a <- buncedf$a[matched_index]
  r$b <- buncedf$b[matched_index]

  # Track which used fallback coefficients
  used_fallback <- is.na(r$a)
  if (any(used_fallback)) {
    flags <- c(flags, "Default_coefficients_used")
    n_fallback <- sum(used_fallback)
    warnings_list <- c(warnings_list,
                       sprintf("%d tree(s) used generic broadleaf coefficients (species not in Bunce database)", n_fallback))
  }

  # Fallback coefficients (generic broadleaf)
  r$a[is.na(r$a)] <- buncedf$a[6]
  r$b[is.na(r$b)] <- buncedf$b[6]

  # Update matched_index for fallback species to point to XB (row 6)
  matched_index[is.na(matched_index)] <- 6

  # ==== Calculate Biomass ====
  r$biomass <- exp(r$a + r$b * log(pi * r$dbh))

  # ==== Calculate uncertainty if error terms provided ====
  # Error propagation log-linear regression: ln(biomass) = a + b * ln(pi * d)
  #
  # For log-linear models, uncertainty is calculated in the log domain and
  # converted to the original scale. This properly accounts for:
  #   1. Parameter uncertainty in coefficients a and b (increases with DBH)
  #      - Uses fixed defaults: a_se = 0.05, b_se = 0.06 (typical allometric regression SEs)
  #   2. DBH measurement error (user-provided via re_dbh)
  #   3. Model residual error (species-specific defaults from Table 6, or user-provided)
  #
  # Uncertainty in log domain:
  #   Var(ln(biomass)) = Var(a) + [ln(pi*d)]^2 * Var(b) + b^2 * Var(ln(pi*d)) + residual_var
  #
  # Where:
  #   - Var(a) = a_se^2 (parameter uncertainty in intercept, fixed default)
  #   - Var(b) = b_se^2 (parameter uncertainty in slope, fixed default)
  #   - Var(ln(pi*d)) ≈ (re_dbh)^2 for small relative errors
  #   - residual_var = residual_cv^2 (model prediction error)
  #
  # Total CV in log domain:
  #   CV_log = sqrt(a_se^2 + [ln(pi*d)]^2 * b_se^2 + (b * re_dbh)^2 + residual_cv^2)
  #
  # This increases with DBH because ln(pi*d) increases, matching Monte Carlo results.
  # For small CVs, this approximates the CV in the original domain.

  if (!is.null(re_dbh)) {

    # === Get N and re from buncedf ===
    r$N <- buncedf$N[matched_index]
    r$N[is.na(r$N)] <- buncedf$N[6]  # Fallback to XB (generic broadleaf)

    r$re <- buncedf$re[matched_index]
    # For unknown species use highest re value found in Table 6, Bunce (at 0.15)
    r$re[is.na(r$re)] <- 0.15 # for conservative uncertainty

    # ==== Species-specific Confidence limits (CO) lookup using DBH ranges ====
    # Access lazy-loaded data directly (LazyData: true handles loading automatically)

    # Initialize result vectors
    r$co_lower_vec <- r$co_upper_vec <- numeric(length(r$dbh))

    # For each unique species, lookup CO values
    unique_spcodes <- unique(r$spcode)

    for (sp in unique_spcodes) {
      # Get CO lookup table for this species
      sp_co <- bunce_co_lookup[bunce_co_lookup$spcode == sp, ]

      # If species not found, use fallback (XB - Combined)
      if (nrow(sp_co) == 0) {
        sp_co <- bunce_co_lookup[bunce_co_lookup$spcode == "XB", ]
        if (nrow(sp_co) == 0) {
          # Ultimate fallback: use default values
          warning("CO lookup table not found for species ", sp, ". Using default values.")
          r$co_lower_vec[r$spcode == sp] <- 10.00 / 100  # Default 10%
          r$co_upper_vec[r$spcode == sp] <- 10.00 / 100
          next
        }
      }

      # Get trees of this species
      sp_mask <- r$spcode == sp
      sp_dbh <- r$dbh[sp_mask]

      # Find DBH range
      range_idx <- findInterval(sp_dbh, sp_co$lower_dbh, rightmost.closed = FALSE)

      # Values below first range: use first range
      range_idx[range_idx == 0] <- 1

      # Extract CO values for this species
      r$co_lower_vec[sp_mask] <- sp_co$co_lower_pct[range_idx] / 100
      r$co_upper_vec[sp_mask] <- sp_co$co_upper_pct[range_idx] / 100

      }

    # === Convert CO 95% CI to sd ===
    # CI = biomass ± (CO % * biomass)
    # Use average of lower and upper CO for symmetric approximation
    # sigma ≈ (average_CO * biomass) / 1.96
    r$co_avg <- (r$co_lower_vec + r$co_upper_vec) / 2

    # ==== N-based adjustment of uncertainty ====
    # To account for parameter uncertainty differences due to sample size
    N_ref <- 73 # Use max species-specific N as reference to ensure unknown
                #  species have higher uncertainty than well-sampled species

    # Adjust CO-based uncertainty for each tree based on its species' N value
    # If N < N_ref, uncertainty increases; if N > N_ref, uncertainty decreases
    # adjusted_CO = CO_base * sqrt(N_ref / N_actual)
    n_adjustment <- sqrt(N_ref / r$N)
    r$co_avg_adjusted <- r$co_avg * n_adjustment

    # Convert adjusted CO to standard deviation
    r$sigma <- r$biomass * r$co_avg_adjusted / 1.96

    # Calculate CI
    r$ci_low_val <- r$biomass - 1.96 * r$sigma
    r$ci_high_val <- r$biomass + 1.96 * r$sigma

    }

  # ==== Return rich output if requested ====
  if (rich_output) {

    # For single tree, return single allometry_result
    if (length(dbh) == 1) {
      # Check if sigma exists and has a valid value
      uncertainty_val <- NULL
      if (!is.null(re_dbh) && "sigma" %in% names(r) && length(r$sigma) > 0 && !is.na(r$sigma[1])) {
        uncertainty_val <- r$sigma[1]
      }
      
      return(single_tree_rich_output(value = r$biomass[1], method = "Bunce",
        measure = "AGB",                                   unit = "kg",
        uncertainty = uncertainty_val,
        validity_warnings = warnings_list,                 flags = flags,
        inputs = list(name = name,                         dbh = dbh,
                      spcode = r$spcode[1],                type = type)
        ))
    } else { # For multiple trees, return list of results with combined summary
      # Extract metadata from data tables (once, shared for all trees)
      bunce_meta <- method_metadata[method_metadata$method == "Bunce", ]
      bunce_assumptions <- method_assumptions$assumption[method_assumptions$method == "Bunce"]

      # Calculate totals first (vectorized)
      total_biomass_kg <- sum(r$biomass, na.rm = TRUE)
      total_uncertainty_kg <- if (!is.null(re_dbh) && !all(is.na(r$sigma))) {
        sqrt(sum(r$sigma^2, na.rm = TRUE))
      } else {
        NULL
      }

      # Collect all unique flags from all trees (vectorized)
      # Each tree may have different flags, so we collect from the flags vector
      # and from used_fallback if it exists
      all_flags_vec <- if (length(flags) > 0 && flags[1] != "None") flags else character()
      if (exists("used_fallback") && any(used_fallback)) {
        all_flags_vec <- unique(c(all_flags_vec, "Used_fallback_coefficients"))
      }
      all_flags <- unique(all_flags_vec)

      # Create summary table (vectorized)
      combined_df <- data.frame(
        tree_id = seq_len(nrow(r)),
        species_name = r$species_name,
        dbh = r$dbh,
        biomass_kg = r$biomass,
        uncertainty_kg = if (!is.null(re_dbh)) r$sigma else NA_real_,
        spcode = r$spcode,
        stringsAsFactors = FALSE
      )

      # Add CI columns if uncertainty was calculated
      if (!is.null(re_dbh) && !all(is.na(r$sigma))) {
        combined_df$ci_low_kg <- r$ci_low_val
        combined_df$ci_high_kg <- r$ci_high_val
      }

      # Create lightweight result objects (vectorized - no lapply)
      # Pre-calculate tree-specific values (vectorized)
      uncertainty_vals <- if (!is.null(re_dbh)) r$sigma else rep(NA_real_, nrow(r))
      ci_low_vals <- if (!is.null(re_dbh) && !all(is.na(r$sigma))) {
        r$biomass - 1.96 * r$sigma
      } else {
        rep(NA_real_, nrow(r))
      }
      ci_high_vals <- if (!is.null(re_dbh) && !all(is.na(r$sigma))) {
        r$biomass + 1.96 * r$sigma
      } else {
        rep(NA_real_, nrow(r))
      }
      
      # Tree-specific flags (each tree may have different flags)
      tree_flags_list <- if (exists("used_fallback")) {
        lapply(seq_len(nrow(r)), function(i) {
          tree_flags <- flags
          if (used_fallback[i]) {
            tree_flags <- c(tree_flags, "Used_fallback_coefficients")
          }
          if (length(tree_flags) > 0 && tree_flags[1] != "None") tree_flags else "None"
        })
      } else {
        rep(list(if (length(flags) > 0 && flags[1] != "None") flags else "None"), nrow(r))
      }

      # Create results_list (vectorized structure, but list of lists for compatibility)
      results_list <- lapply(seq_len(nrow(r)), function(i) {
        list(
          value = r$biomass[i],
          measure = "AGB",
          unit = "kg",
          method = "Bunce",
          method_full_name = bunce_meta$full_name,
          reference = bunce_meta$reference,
          reference_short = bunce_meta$reference_short,
          doi = bunce_meta$doi,
          source_type = bunce_meta$source_type,
          region = bunce_meta$region,
          biome = strsplit(bunce_meta$biome, "; ")[[1]],
          assumptions = bunce_assumptions,
          uncertainty = if (!is.na(uncertainty_vals[i])) uncertainty_vals[i] else NULL,
          uncertainty_method = bunce_meta$uncertainty_method,
          ci_low = if (!is.na(ci_low_vals[i])) ci_low_vals[i] else NULL,
          ci_high = if (!is.na(ci_high_vals[i])) ci_high_vals[i] else NULL,
          validity_warnings = if (length(warnings_list) > 0) warnings_list else "None",
          flags = tree_flags_list[[i]],
          valid_dbh_range = c(bunce_meta$dbh_min_cm, bunce_meta$dbh_max_cm),
          valid_height_range = c(bunce_meta$height_min_m, bunce_meta$height_max_m),
          height_required = bunce_meta$height_required,
          species_specific = bunce_meta$species_specific,
          wood_density_required = bunce_meta$wood_density_required
        )
      })

      # Return as a special multi-tree result
      result <- list(
        trees = results_list,
        summary_table = combined_df,
        n_trees = length(dbh),
        total_biomass_kg = total_biomass_kg,
        total_uncertainty_kg = total_uncertainty_kg,
        validation = validation
      )
      class(result) <- c("bunce_multi_result", "list")

      return(result)
    }
  }

  # ==== Standard return ====
  if(is.null(re_dbh)) {
    return(r[,c(1:6)])
  } else {
    return(r[,c(1:6,13)])
  }

}

#' @title Print method for multiple Bunce results
#' @description Formatted display for multi-tree Bunce results
#' @param x A bunce_multi_result object
#' @param ... Additional arguments (unused)
#' @export
print.bunce_multi_result <- function(x, ...) {

  cat("------------------- ALLOMETRY RESULT -------------------\n")
  cat(sprintf("Number of trees: %d\n", x$n_trees))
  cat(sprintf("Total AGB estimate: %.2f kg\n", x$total_biomass_kg))

  # Add uncertainty and CI if available
  if (!is.null(x$total_uncertainty_kg)) {
    cat(sprintf("Uncertainty: +/- %.2f kg (SD)\n", x$total_uncertainty_kg))
    ci_low <- x$total_biomass_kg - 1.96 * x$total_uncertainty_kg
    ci_high <- x$total_biomass_kg + 1.96 * x$total_uncertainty_kg
    cat(sprintf("95%% CI: [%.2f, %.2f] kg\n", ci_low, ci_high))
  }
  cat(" \n")

  # Get method metadata (from first tree)
  meta <- x$trees[[1]]

  cat("--- METHOD INFORMATION ---\n")
  cat(sprintf("Method: %s\n", meta$method_full_name))
  cat(sprintf("Source: %s\n", meta$source_type))
  cat(sprintf("Region: %s | Biome: %s\n", meta$region, paste(meta$biome, collapse = "; ")))
  cat(" \n")

  cat("--- ASSUMPTIONS ---\n")
  for (i in seq_along(meta$assumptions)) {
    cat(sprintf("  %d. %s\n", i, meta$assumptions[i]))
  }
  cat(" \n")

  cat("--- VALIDITY & FLAGS ---\n")
  cat(sprintf("Valid DBH range: %.0f - %.0f cm\n", meta$valid_dbh_range[1], meta$valid_dbh_range[2]))

  # Collect all unique flags from all trees
  all_flags <- unique(unlist(lapply(x$trees, function(tree) {
    if (is.character(tree$flags) && length(tree$flags) > 0 && tree$flags[1] != "None") {
      tree$flags
    } else {
      character()
    }
  })))

  if (length(all_flags) > 0) {
    cat("FLAGS:\n")
    for (f in all_flags) {
      cat(sprintf("  - %s\n", f))
    }
  }
  cat(" \n")

  # Collect all unique warnings
  all_warnings <- unique(c(
    if (any(x$validation$validity_warnings != "None")) x$validation$validity_warnings[x$validation$validity_warnings != "None"] else character(),
    unlist(lapply(x$trees, function(tree) {
      if (is.character(tree$validity_warnings) && length(tree$validity_warnings) > 0 && tree$validity_warnings[1] != "None") {
        tree$validity_warnings[tree$validity_warnings != "None"]
      } else {
        character()
      }
    }))
  ))

  if (length(all_warnings) > 0) {
    cat("--- WARNINGS ---\n")
    for (w in all_warnings) {
      cat(sprintf("  %s\n", w))
    }
    cat(" \n")
  }

  cat("--- REFERENCE ---\n")
  cat(sprintf("  %s\n", meta$reference))
  if (!is.na(meta$doi) && !is.null(meta$doi)) {
    cat(sprintf("  DOI: https://doi.org/%s\n", meta$doi))
  }

  invisible(x)
}

#' @title Print method for biomass_multi_result
#' @description Formatted display for BIOMASS multi-tree results
#' @param x A biomass_multi_result object
#' @param ... Additional arguments (unused)
#' @export
print.biomass_multi_result <- function(x, ...) {

  cat("------------------- ALLOMETRY RESULT -------------------\n")
  cat(sprintf("Number of trees: %d\n", x$n_trees))
  cat(sprintf("Total AGB estimate: %.2f kg\n", x$total_AGB_kg))

  # Add uncertainty and CI if available
  if (!is.null(x$total_AGB_sd_kg)) {
    cat(sprintf("Uncertainty: +/- %.2f kg (SD)\n", x$total_AGB_sd_kg))
    ci_low <- x$total_AGB_kg - 1.96 * x$total_AGB_sd_kg
    ci_high <- x$total_AGB_kg + 1.96 * x$total_AGB_sd_kg
    cat(sprintf("95%% CI: [%.2f, %.2f] kg\n", ci_low, ci_high))
  }
  cat(" \n")

  # Get method metadata (from first tree)
  meta <- x$trees[[1]]

  cat("--- METHOD INFORMATION ---\n")
  cat(sprintf("Method: %s\n", meta$method_full_name))
  cat(sprintf("Source: %s\n", meta$source_type))
  cat(sprintf("Region: %s | Biome: %s\n", meta$region, paste(meta$biome, collapse = "; ")))
  cat(" \n")

  cat("--- ASSUMPTIONS ---\n")
  for (i in seq_along(meta$assumptions)) {
    cat(sprintf("  %d. %s\n", i, meta$assumptions[i]))
  }
  cat(" \n")

  cat("--- VALIDITY & FLAGS ---\n")
  cat(sprintf("Valid DBH range: %.0f - %.0f cm\n", meta$valid_dbh_range[1], meta$valid_dbh_range[2]))

  # Collect all unique flags from all trees
  all_flags <- unique(unlist(lapply(x$trees, function(tree) {
    if (is.character(tree$flags) && length(tree$flags) > 0 && tree$flags[1] != "None") {
      tree$flags
    } else {
      character()
    }
  })))

  if (length(all_flags) > 0) {
    cat("FLAGS:\n")
    for (f in all_flags) {
      cat(sprintf("  - %s\n", f))
    }
  }
  cat(" \n")

  # Collect all unique warnings
  all_warnings <- unique(c(
    if (any(x$validation$validity_warnings != "None")) x$validation$validity_warnings[x$validation$validity_warnings != "None"] else character(),
    unlist(lapply(x$trees, function(tree) {
      if (is.character(tree$validity_warnings) && length(tree$validity_warnings) > 0 && tree$validity_warnings[1] != "None") {
        tree$validity_warnings[tree$validity_warnings != "None"]
      } else {
        character()
      }
    }))
  ))

  if (length(all_warnings) > 0) {
    cat("--- WARNINGS ---\n")
    for (w in all_warnings) {
      cat(sprintf("  %s\n", w))
    }
    cat(" \n")
  }

  cat("--- REFERENCE ---\n")
  cat(sprintf("  %s\n", meta$reference))
  if (!is.na(meta$doi) && !is.null(meta$doi)) {
    cat(sprintf("  DOI: https://doi.org/%s\n", meta$doi))
  }

  invisible(x)
}

#' @title Print method for allodb_multi_result
#' @description Formatted display for allodb multi-tree results
#' @param x An allodb_multi_result object
#' @param ... Additional arguments (unused)
#' @export
print.allodb_multi_result <- function(x, ...) {

  cat("------------------- ALLOMETRY RESULT -------------------\n")
  cat(sprintf("Number of trees: %d\n", x$n_trees))
  cat(sprintf("Total AGB estimate: %.2f kg\n", x$total_AGB_kg))

  # Add uncertainty and CI if available
  if (!is.null(x$total_uncertainty_kg)) {
    cat(sprintf("Uncertainty: +/- %.2f kg (SD)\n", x$total_uncertainty_kg))
    ci_low <- x$total_AGB_kg - 1.96 * x$total_uncertainty_kg
    ci_high <- x$total_AGB_kg + 1.96 * x$total_uncertainty_kg
    cat(sprintf("95%% CI: [%.2f, %.2f] kg\n", ci_low, ci_high))
  }
  cat(" \n")

  # Get method metadata (from first tree)
  meta <- x$trees[[1]]

  cat("--- METHOD INFORMATION ---\n")
  cat(sprintf("Method: %s\n", meta$method_full_name))
  cat(sprintf("Source: %s\n", meta$source_type))
  cat(sprintf("Region: %s | Biome: %s\n", meta$region, paste(meta$biome, collapse = "; ")))
  cat(" \n")

  cat("--- ASSUMPTIONS ---\n")
  for (i in seq_along(meta$assumptions)) {
    cat(sprintf("  %d. %s\n", i, meta$assumptions[i]))
  }
  cat(" \n")

  cat("--- VALIDITY & FLAGS ---\n")
  cat(sprintf("Valid DBH range: %.0f - %.0f cm\n", meta$valid_dbh_range[1], meta$valid_dbh_range[2]))

  # Collect all unique flags from all trees
  all_flags <- unique(unlist(lapply(x$trees, function(tree) {
    if (is.character(tree$flags) && length(tree$flags) > 0 && tree$flags[1] != "None") {
      tree$flags
    } else {
      character()
    }
  })))

  if (length(all_flags) > 0) {
    cat("FLAGS:\n")
    for (f in all_flags) {
      cat(sprintf("  - %s\n", f))
    }
  }
  cat(" \n")

  # Collect all unique warnings
  all_warnings <- unique(c(
    if (any(x$validation$validity_warnings != "None")) x$validation$validity_warnings[x$validation$validity_warnings != "None"] else character(),
    unlist(lapply(x$trees, function(tree) {
      if (is.character(tree$validity_warnings) && length(tree$validity_warnings) > 0 && tree$validity_warnings[1] != "None") {
        tree$validity_warnings[tree$validity_warnings != "None"]
      } else {
        character()
      }
    }))
  ))

  if (length(all_warnings) > 0) {
    cat("--- WARNINGS ---\n")
    for (w in all_warnings) {
      cat(sprintf("  %s\n", w))
    }
    cat(" \n")
  }

  cat("--- REFERENCE ---\n")
  cat(sprintf("  %s\n", meta$reference))
  if (!is.na(meta$doi) && !is.null(meta$doi)) {
    cat(sprintf("  DOI: https://doi.org/%s\n", meta$doi))
  }

  invisible(x)
}

############# BIOMASS Package carbon calculation ==========================
#'
#' @title Estimate Tree Biomass using BIOMASS R package
#' @description Using the BIOMASS package (Chave et al. pantropical equations)
#' to calculate above-ground biomass with optional Monte Carlo uncertainty
#' propagation.
#'
#'   **Required package**: This function requires the \code{BIOMASS} package,
#'   which is a dependency of TreeCarbon. If missing, install with:
#'   \code{install.packages("BIOMASS")}
#'
#'   When \code{rich_output = TRUE}, returns a comprehensive result object
#'   including method metadata, assumptions, validity warnings, and uncertainty.
#'
#'   When \code{uncertainty = TRUE}, uses \code{BIOMASS::AGBmonteCarlo} to
#'   propagate uncertainty from DBH measurement error, wood density uncertainty,
#'   and height estimation error, providing credible intervals and CV.
#'
#' @param dbh Diameter at breast height (cm)
#' @param height height of tree in metres (if not specified will estimate height)
#' @param genus First part of species binomial
#' @param species Second part of species binomial
#' @param coords either a vector of coordinates of the site or a matrix of
#'   coordinates for each tree of longitude and latitude
#' @param region of the World. See ?getWoodDensity for the list of regions
#' @param output.all if TRUE outputs all data from processing, else just outputs
#'   carbon figures
#' @param HD_method If there are missing height variables, the function will
#'   predict height using a height diameter model. This model is custom if there
#'   are over 15 height measurements inputted, in which case you can choose a
#'   model fit for this from, HD_method = c("log2","weibull","log1","exp","lin")
#'   See BIOMASS::modelHD for more.
#' @param rich_output Logical. If TRUE, returns a rich result object with
#'   metadata including: value, method, reference, assumptions, validity_warning,
#'   flags, region, and source type. Default FALSE for backwards compatibility.
#' @param uncertainty Logical. If TRUE, runs Monte Carlo uncertainty propagation
#'   using \code{BIOMASS::AGBmonteCarlo}. This propagates errors from:
#'   \itemize{
#'     \item DBH measurement error (via Dpropag)
#'     \item Wood density uncertainty (from species/genus matching)
#'     \item Height estimation error (if heights are predicted)
#'     \item Allometric model error
#'   }
#'   Returns SD, CV, and 5-95 percent credible intervals. Default FALSE.
#' @param n_mc Number of Monte Carlo iterations for uncertainty estimation.
#'   Default 1000. Higher values give more stable estimates but take longer.
#'   Recommended: 1000 for quick checks, 10000 for final results.
#' @param Dpropag Method for propagating DBH measurement error. Options:
#'   \itemize{
#'     \item "chave2004" (default): Uses error model from Chave et al. 2004
#'       where SD = 0.0062*DBH + 0.0904
#'     \item A numeric value: Fixed relative error applied to all trees
#'     \item A numeric vector: Per-tree absolute measurement errors
#'   }
#' @param errH Height measurement error. Required when \code{uncertainty = TRUE}
#'   and heights are provided directly (not estimated). Options:
#'   \itemize{
#'     \item NULL (default): If heights are estimated, error is propagated from
#'       the height-diameter model. If heights are provided directly, errH is
#'       set to 0 (no height error propagation).
#'     \item A numeric value: Fixed relative error (e.g., 0.05 for 5 percent)
#'       applied to all trees as SD = errH * height.
#'     \item A numeric vector: Per-tree absolute height errors (in metres).
#'   }
#' @param wd Wood density (g/cm3). Optional. If NULL (default), wood density
#'   is retrieved from the Global Wood Density Database via
#'   \code{BIOMASS::getWoodDensity()}. If provided, must be a numeric value or
#'   vector matching the length of \code{dbh}.
#' @param wd_sd Standard deviation of wood density. Optional. If NULL and
#'   \code{wd} is provided, a default SD of 10 percent of wd is used. If \code{wd}
#'   is NULL, this is retrieved from the database.
#'
#' @return If \code{rich_output = FALSE}: Above-ground Biomass in kg. If
#'   output.all = FALSE, returns columns 'genus_corrected','species', 'Family',
#'   'Latitude', 'Longitude', 'dbh', 'AGB_Biomass_kg'. If output.all = TRUE then
#'   additionally returns columns 'Wood_Density', 'Wood_Density_sd', 'height_est',
#'   'RSE'.
#'
#'   If \code{uncertainty = TRUE}, additional columns include:
#'   \itemize{
#'     \item AGB_mean_kg: Mean from Monte Carlo simulation
#'     \item AGB_sd_kg: Standard deviation
#'     \item AGB_CI_low_kg: 5th percentile (lower credible bound)
#'     \item AGB_CI_high_kg: 95th percentile (upper credible bound)
#'     \item AGB_median_kg: Median estimate
#'     \item AGB_CV_pct: Coefficient of variation (percent)
#'   }
#'
#'   If \code{rich_output = TRUE}: an \code{allometry_result} object with
#'   metadata including uncertainty estimates.
#'
#' @import remotes
#'
#' @references
#' Rejou-Mechain, M., Tanguy, A., Piponiot, C., Chave, J., & Herault, B. (2017).
#' BIOMASS: an R package for estimating above-ground biomass and its uncertainty
#' in tropical forests. Methods in Ecology and Evolution, 8(9), 1163-1167.
#' DOI: 10.1111/2041-210X.12753
#'
#' Chave, J. et al. (2014). Improved allometric models to estimate the
#' aboveground biomass of tropical trees. Global Change Biology, 20, 3177-3190.
#'
#' @examples
#' \dontrun{
#' # BIOMASS package is a required dependency of TreeCarbon
#' coords <- c(-0.088837, 51.071610)
#'
#' # Basic usage
#' BIOMASS(12, 12, 'Quercus', 'robur', coords)
#'
#' # With uncertainty estimation (default 5 percent height error)
#' result <- BIOMASS(30, 15, 'Quercus', 'robur', coords,
#'                   uncertainty = TRUE, n_mc = 1000)
#' result[, c("AGB_Biomass_kg", "AGB_sd_kg", "AGB_CV_pct")]
#'
#' # With specific height measurement error (10 percent relative error)
#' result <- BIOMASS(30, 15, 'Quercus', 'robur', coords,
#'                   uncertainty = TRUE, n_mc = 1000, errH = 0.10)
#' result[, c("AGB_Biomass_kg", "AGB_sd_kg", "AGB_CV_pct")]
#'
#' # Rich output with metadata and uncertainty
#' result <- BIOMASS(30, 15, 'Quercus', 'robur', coords,
#'                   rich_output = TRUE, uncertainty = TRUE)
#' print(result)
#' }
#'
#' @aliases biomass
#' @export
#'
BIOMASS <- function(dbh, height = NULL, genus, species, coords, region = "World",
                    output.all = TRUE, HD_method = "log2", rich_output = FALSE,
                    uncertainty = FALSE, n_mc = 1000, Dpropag = "chave2004",
                    errH = NULL, wd = NULL, wd_sd = NULL) {

  #### Input validation ####
  if(!is.numeric(dbh))stop("dbh must be numeric vector of tree diameters (cm).")

  if (!is.null(height) && !is.numeric(height))
    stop("`height` must be a numeric vector of tree heights (m), or NULL for
         height prediction.")

  if (!is.character(genus) || !is.character(species)) {
    stop("'genus' and 'species' must be character vectors.")
  }
  if (!is.numeric(coords) || length(coords) != 2) {
    stop("'coords' must be a numeric vector of latitude and longitude.")
  }
  if (!is.character(region) || length(region) != 1) {
    stop("'region' must be a single character string.")
  }
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }
  if (length(dbh) != length(genus) || length(dbh) != length(species)) {
    stop("Lengths of 'dbh', 'genus', and 'species' must be equal.")
  }

  # ==== Validate against BIOMASS method ranges ====
  validation <- validate_inputs_for_method("BIOMASS", dbh = dbh, height = height)
  flags <- if (validation$flags[1] != "None") validation$flags else character()
  warnings_list <- if (validation$validity_warnings[1] != "None") validation$validity_warnings else character()

  #### Build output dataframe ####
  df <- data.frame(dbh = dbh, height = height, genus = as.character(genus),
                   species = as.character(species), stringsAsFactors = FALSE)

  #### Wood density: use provided values or fetch from database ####
  wd_source <- "database"  # Track source for rich output

  if (!is.null(wd)) {
    # User-provided wood density
    wd_source <- "user_provided"
    flags <- c(flags, "Wood_density_user_provided")

    # Validate wd input
    if (!is.numeric(wd)) {
      stop("'wd' must be numeric (wood density in g/cm3)")
    }
    if (length(wd) == 1) {
      df$Wood_Density <- rep(wd, nrow(df))
    } else if (length(wd) == nrow(df)) {
      df$Wood_Density <- wd
    } else {
      stop("'wd' must be a single value or a vector matching the number of trees")
    }

    # Handle wd_sd
    if (!is.null(wd_sd)) {
      if (!is.numeric(wd_sd)) {
        stop("'wd_sd' must be numeric")
      }
      if (length(wd_sd) == 1) {
        df$Wood_Density_sd <- rep(wd_sd, nrow(df))
      } else if (length(wd_sd) == nrow(df)) {
        df$Wood_Density_sd <- wd_sd
      } else {
        stop("'wd_sd' must be a single value or a vector matching the number of trees")
      }
    } else {
      # Default: assume 10% uncertainty if not provided
      df$Wood_Density_sd <- df$Wood_Density * 0.10
      flags <- c(flags, "Wood_density_sd_default_10pct")
    }

    # Family not available when user-provided
    df$Family <- NA

  } else {
    # Fetch wood density from Global Wood Density Database via BIOMASS package
    # Suppress informational messages and warnings from BIOMASS package
    # (e.g., "The reference dataset contains...", "DRYAD data only stored...")
    wd_lookup <- suppressMessages(suppressWarnings(
      BIOMASS::getWoodDensity(df$genus, df$species, region = region)
    ))

    df$Wood_Density <- wd_lookup$meanWD
    df$Wood_Density_sd <- wd_lookup$sdWD
    df$Family <- wd_lookup$family
  }

  # Add wood density source to output
  df$Wood_Density_Source <- wd_source

  #### Estimate height ####
  height_estimated <- FALSE

  # If there are missing height entries
  if(anyNA(height) || is.null(height)){
    height_estimated <- TRUE
    flags <- c(flags, "Height_estimated")

    # All tree heights are NA or there are <= 15 height measurements
    if (is.null(height) || all(is.na(height)) || sum(!is.na(df$height)) <= 15) {
      # Use global model
      h <- BIOMASS::retrieveH(D = df$dbh, coord = coords)
      df$height_pred <- h$H

      # Else if tree heights > 15, generate dbh x height model
    } else {

      # Use linear modelling
      HDmodel <- try(BIOMASS::modelHD(D = df$dbh, H = df$height,
                                      method = HD_method), silent = TRUE)

      if (inherits(HDmodel, "try-error")) {
        # Fallback to global if local model fails
        h <- BIOMASS::retrieveH(D = df$dbh, coord = coords)
        df$height_pred <- h$H

      } else {
        df$height_pred <- predict(HDmodel$model, newdata = data.frame(D = dbh))
      }
    }

    # Combine height data with height estimates
    df$height_input <- df$height
    df$height <- ifelse(is.na(df$height), df$height_pred, df$height)
  }

  #### Calculate Above-Ground Biomass (AGB) ####
  df$AGB_Biomass_kg <- BIOMASS::computeAGB(D = as.numeric(df$dbh),
                                           WD = as.numeric(df$Wood_Density), H = df$height) * 1000

  #### Monte Carlo Uncertainty Estimation ####
  # Uses AGBmonteCarlo from BIOMASS package (Rejou-Mechain et al. 2017)
  # Propagates uncertainty from: DBH measurement, wood density, height model
  mc_result <- NULL
  agb_sd <- NULL
  agb_credible <- NULL

  if (uncertainty) {
    # Build HD model if heights were estimated
    HDmodel_mc <- NULL
    if (height_estimated) {
      if (!is.null(height) && sum(!is.na(height)) > 15) {
        # Local HD model exists
        HDmodel_mc <- try(BIOMASS::modelHD(D = df$dbh, H = df$height,
                                           method = HD_method), silent = TRUE)
        if (inherits(HDmodel_mc, "try-error")) HDmodel_mc <- NULL
      }
    }

    # Run Monte Carlo simulation
    # AGBmonteCarlo propagates uncertainty from:
    #   - Diameter measurement error (Dpropag)
    #   - Wood density uncertainty (errWD from getWoodDensity)
    #   - Height estimation error (if using HDmodel or coord, or errH if H provided)
    #   - Allometric model error

    # Determine height error for Monte Carlo
    # When H is provided directly, errH must be set (AGBmonteCarlo requirement)
    mc_errH <- NULL
    if (!height_estimated) {
      # Heights provided directly - need errH
      if (is.null(errH)) {
        # Default: assume small measurement error (5% of height)
        # This is reasonable for field-measured heights
        mc_errH <- 0.05 * df$height
        flags <- c(flags, "errH_default_5pct")
      } else if (length(errH) == 1 && errH < 1) {
        # Relative error provided (e.g., 0.05 for 5%)
        mc_errH <- errH * df$height
        flags <- c(flags, paste0("errH_relative_", errH * 100, "pct"))
      } else {
        # Absolute errors provided
        mc_errH <- errH
        flags <- c(flags, "errH_absolute")
      }
    }

    mc_result <- tryCatch({
      BIOMASS::AGBmonteCarlo(D = df$dbh, WD = df$Wood_Density,
        errWD = df$Wood_Density_sd, H = if (!height_estimated) df$height else NULL,
        errH = mc_errH, HDmodel = HDmodel_mc,
        coord = if (height_estimated && is.null(HDmodel_mc)) coords else NULL,
        Dpropag = Dpropag,               n = n_mc)
    }, error = function(e) {
      warning("AGBmonteCarlo failed: ", e$message, ". Returning point estimate only.")
      NULL
    })

    if (!is.null(mc_result)) {
      # Extract per-tree uncertainty from Monte Carlo simulations
      # AGBmonteCarlo returns AGB in Mg (tonnes), convert to kg
      #
      # Note: AGBmonteCarlo return structure:
      #   - mc_result$AGB_simu: simulation matrix where each row is a simulation
      #     and each column is a tree (n_mc rows x n_trees cols)
      #   - For single tree, this might be a vector of length n_mc

      n_trees <- nrow(df)

      # Handle AGB_simu which contains all Monte Carlo simulations
      if (!is.null(mc_result$AGB_simu)) {
        agb_simu_kg <- mc_result$AGB_simu * 1000

        # Ensure matrix format for consistent handling
        if (is.null(dim(agb_simu_kg))) {
          # Single tree: convert vector to matrix (n_mc rows x 1 column)
          agb_simu_kg <- matrix(agb_simu_kg, ncol = 1)
        } else {
          # Check if matrix is transposed (trees in rows, simulations in columns)
          # The correct format is: rows = simulations, columns = trees
          # If ncol equals n_mc and nrow equals n_trees, we need to transpose
          if (ncol(agb_simu_kg) == n_mc && nrow(agb_simu_kg) == n_trees) {
            agb_simu_kg <- t(agb_simu_kg)
          }
        }

        # Calculate summary statistics from simulations (apply over columns = trees)
        df$AGB_mean_kg <- apply(agb_simu_kg, 2, mean, na.rm = TRUE)
        df$AGB_sd_kg <- apply(agb_simu_kg, 2, stats::sd, na.rm = TRUE)
        df$AGB_CI_low_kg <- apply(agb_simu_kg, 2, stats::quantile, probs = 0.05, na.rm = TRUE)
        df$AGB_CI_high_kg <- apply(agb_simu_kg, 2, stats::quantile, probs = 0.95, na.rm = TRUE)
        df$AGB_median_kg <- apply(agb_simu_kg, 2, stats::median, na.rm = TRUE)
        df$AGB_CV_pct <- 100 * df$AGB_sd_kg / df$AGB_mean_kg

      } else if (!is.null(mc_result$AGB)) {
        # Fallback: use AGB field if AGB_simu not available
        # Check if AGB is simulations (length > n_trees) or point estimates
        if (length(mc_result$AGB) == n_trees) {
          df$AGB_mean_kg <- mc_result$AGB * 1000
        } else if (n_trees == 1 && length(mc_result$AGB) == n_mc) {
          # Single tree with all simulations in AGB
          sims_kg <- mc_result$AGB * 1000
          df$AGB_mean_kg <- mean(sims_kg, na.rm = TRUE)
          df$AGB_sd_kg <- stats::sd(sims_kg, na.rm = TRUE)
          df$AGB_CI_low_kg <- stats::quantile(sims_kg, probs = 0.05, na.rm = TRUE)
          df$AGB_CI_high_kg <- stats::quantile(sims_kg, probs = 0.95, na.rm = TRUE)
          df$AGB_median_kg <- stats::median(sims_kg, na.rm = TRUE)
          df$AGB_CV_pct <- 100 * df$AGB_sd_kg / df$AGB_mean_kg
        }
      }

      # Store summary statistics for rich output
      agb_sd <- df$AGB_sd_kg
      agb_credible <- data.frame(
        CI_5 = df$AGB_CI_low_kg,
        CI_95 = df$AGB_CI_high_kg,
        median = df$AGB_median_kg
      )

      flags <- c(flags, paste0("MC_uncertainty_N=", n_mc))
    }
  }

  # ==== Return rich output if requested ====
  if (rich_output) {
    if (length(dbh) == 1) {
      return(single_tree_rich_output(value = df$AGB_Biomass_kg[1],
        method = "BIOMASS",          measure = "AGB",          unit = "kg",
        uncertainty = if (!is.null(agb_sd)) agb_sd[1] else NULL,
        validity_warnings = warnings_list,                     flags = flags,
        inputs = list(genus = genus, species = species,        dbh = dbh,
          height = if (height_estimated) df$height[1] else height,
          height_estimated = height_estimated, wood_density = df$Wood_Density[1],
          wood_density_sd = df$Wood_Density_sd[1], wood_density_source = wd_source,
          region = region,                        coords = coords,
          uncertainty_method = if (uncertainty) "AGBmonteCarlo" else NULL,
          n_mc = if (uncertainty) n_mc else NULL,
          CI_5_pct = if (!is.null(agb_credible)) agb_credible$CI_5[1] else NULL,
          CI_95_pct = if (!is.null(agb_credible)) agb_credible$CI_95[1] else NULL,
          median = if (!is.null(agb_credible)) agb_credible$median[1] else NULL,
          CV_pct = if (!is.null(df$AGB_CV_pct)) df$AGB_CV_pct[1] else NULL
        )
      ))
    }

    # Multiple trees
    # Get shared method metadata once (same for all trees) - direct access
    biomass_meta <- method_metadata[method_metadata$method == "BIOMASS", ]
    biomass_assumptions <- method_assumptions$assumption[method_assumptions$method == "BIOMASS"]

    # Calculate totals first (vectorized)
    total_AGB_kg <- sum(df$AGB_Biomass_kg, na.rm = TRUE)
    total_AGB_sd_kg <- if (!is.null(agb_sd) && !all(is.na(agb_sd))) {
      sqrt(sum(agb_sd^2, na.rm = TRUE))
    } else {
      NULL
    }

    # Collect all unique flags from all trees (vectorized)
    all_flags <- if (length(flags) > 0 && flags[1] != "None") unique(flags) else character()

    # Pre-calculate tree-specific values (vectorized)
    uncertainty_vals <- if (!is.null(agb_sd)) {
      ifelse(!is.na(agb_sd), agb_sd, NA_real_)
    } else {
      rep(NA_real_, nrow(df))
    }
    
    ci_low_vals <- if (!is.null(agb_credible)) {
      agb_credible$CI_5
    } else if (!is.null(agb_sd) && !all(is.na(agb_sd))) {
      df$AGB_Biomass_kg - 1.96 * agb_sd
    } else {
      rep(NA_real_, nrow(df))
    }
    
    ci_high_vals <- if (!is.null(agb_credible)) {
      agb_credible$CI_95
    } else if (!is.null(agb_sd) && !all(is.na(agb_sd))) {
      df$AGB_Biomass_kg + 1.96 * agb_sd
    } else {
      rep(NA_real_, nrow(df))
    }

    # Create summary table directly from df + agb_sd + agb_credible (vectorized)
    combined_df <- data.frame(
      tree_id = seq_len(nrow(df)),
      genus = df$genus,
      species = df$species,
      dbh = df$dbh,
      height = df$height,
      biomass_kg = df$AGB_Biomass_kg,
      uncertainty_kg = uncertainty_vals,
      stringsAsFactors = FALSE
    )

    # Add Monte Carlo CI columns if available
    if (!is.null(agb_credible)) {
      combined_df$ci_low_kg <- agb_credible$CI_5
      combined_df$ci_high_kg <- agb_credible$CI_95
      combined_df$median_kg <- agb_credible$median
      if (!is.null(df$AGB_CV_pct)) {
        combined_df$cv_pct <- df$AGB_CV_pct
      }
    } else if (!is.null(agb_sd) && !all(is.na(agb_sd))) {
      # Use symmetric CI approximation if only SD available
      combined_df$ci_low_kg <- ci_low_vals
      combined_df$ci_high_kg <- ci_high_vals
    }

    # Create result objects (vectorized structure, but list of lists for compatibility)
    results_list <- lapply(seq_len(nrow(df)), function(i) {
      # Create lightweight result object with shared metadata
      result <- list(
        # === Core estimate (tree-specific) ===
        value = if (!is.na(df$AGB_Biomass_kg[i])) df$AGB_Biomass_kg[i] else NA_real_,
        measure = "AGB",
        unit = "kg",

        # === Method information (shared - from metadata) ===
        method = "BIOMASS",
        method_full_name = biomass_meta$full_name,
        reference = biomass_meta$reference,
        reference_short = biomass_meta$reference_short,
        doi = biomass_meta$doi,
        source_type = biomass_meta$source_type,
        region = biomass_meta$region,
        biome = strsplit(biomass_meta$biome, "; ")[[1]],

        # === Assumptions (shared - from metadata) ===
        assumptions = biomass_assumptions,

        # === Uncertainty (tree-specific) ===
        uncertainty = if (!is.na(uncertainty_vals[i])) uncertainty_vals[i] else NULL,
        uncertainty_method = biomass_meta$uncertainty_method,
        ci_low = if (!is.na(ci_low_vals[i])) ci_low_vals[i] else NULL,
        ci_high = if (!is.na(ci_high_vals[i])) ci_high_vals[i] else NULL,
        ci_level = if (!is.na(ci_low_vals[i])) 0.95 else NA,

        # === Validity and flags (tree-specific) ===
        validity_warnings = if (length(warnings_list) > 0) warnings_list else "None",
        flags = if (length(flags) > 0) flags else "None",

        # === Valid ranges (shared - from metadata) ===
        valid_dbh_range = c(biomass_meta$dbh_min_cm, biomass_meta$dbh_max_cm),
        valid_height_range = c(biomass_meta$height_min_m, biomass_meta$height_max_m),
        height_required = biomass_meta$height_required,
        species_specific = biomass_meta$species_specific,
        wood_density_required = biomass_meta$wood_density_required,

        # === Inputs (minimal - only additional fields, since genus/species/dbh/height are in df) ===
        inputs = list(
          height_estimated = height_estimated,
          wood_density = df$Wood_Density[i],
          wood_density_sd = df$Wood_Density_sd[i],
          wood_density_source = wd_source,
          region = region,
          coords = coords,
          uncertainty_method = if (uncertainty) "AGBmonteCarlo" else NULL,
          n_mc = if (uncertainty) n_mc else NULL,
          CI_5_pct = if (!is.null(agb_credible)) agb_credible$CI_5[i] else NULL,
          CI_95_pct = if (!is.null(agb_credible)) agb_credible$CI_95[i] else NULL,
          median = if (!is.null(agb_credible)) agb_credible$median[i] else NULL,
          CV_pct = if (!is.null(df$AGB_CV_pct)) df$AGB_CV_pct[i] else NULL
        )
      )
      class(result) <- "allometry_result"
      return(result)
    })

    result <- list(
      trees = results_list,
      summary_table = combined_df,
      n_trees = length(dbh),
      total_AGB_kg = total_AGB_kg,
      mean_AGB_kg = mean(df$AGB_Biomass_kg, na.rm = TRUE),
      total_AGB_sd_kg = total_AGB_sd_kg,
      mc_result = mc_result,
      validation = validation,
      detailed_output = if (output.all) df else NULL
    )
    class(result) <- c("biomass_multi_result", "list")
    return(result)
  }

  # Output results
  if (output.all) {
    return(df)
  } else {
    return(df$AGB_Biomass_kg)
  }
}

############# allodb Package carbon calculation ==========================
#' @title Estimate Tree Biomass using allodb R package
#' @description Use the allodb R package to calculate above-ground biomass
#' using a weighted combination of multiple published allometric equations.
#'
#'   **Required package**: This function requires the \code{allodb} package,
#'   which is a dependency of TreeCarbon. If missing, install from GitHub with:
#'   \code{remotes::install_github("ropensci/allodb")}
#'
#'   When \code{rich_output = TRUE}, returns a comprehensive result object
#'   including method metadata, assumptions, validity warnings, and uncertainty
#'   - making limitations and assumptions "impossible to miss".
#'
#' @param dbh Diameter at breast height (cm)
#' @param genus First part of species binomial
#' @param species Second part of species binomial
#' @param coords either a vector of coordinates of the site or a matrix of
#' coordinates for each tree of longitude and latitude
#' @param output.all if TRUE outputs all data from processing, else just outputs biomass
#' @param new.eqtable a subset or extension of the allometric equation table. Create with allodb::new_equations
#' @param rich_output Logical. If TRUE, returns a rich result object with
#'   metadata including: value, method, reference, assumptions, validity_warning,
#'   flags, uncertainty interval, region, and source type. Default FALSE.
#' @references Gonzalez-Akre, E., Piponiot, C., Lepore, M., & Anderson-Teixeira,
#' K. (2020). allodb: An R package for biomass estimation at globally
#' distributed extratropical forest plots. Methods in Ecology and Evolution,
#' 11(10), 1273-1280
#' @return If \code{rich_output = FALSE}: Above-ground Biomass in kg. If output.all = TRUE
#'   then returns columns 'dbh', 'genus', 'species', 'AGB_allodb_kg', 'allodb_a',
#'   'allodb_b', 'allodb_sigma' (where AGB = a*dbh^b+e, e ~ N(0,sigma^2)).
#'   If \code{rich_output = TRUE}: an \code{allometry_result} object with metadata.
#' @examples
#' \dontrun{
#' # allodb package is a required dependency of TreeCarbon
#' coords <- c(-0.088837,51.071610)
#' allodb(81.887, "Pinus", "nigra", coords, output.all = FALSE)
#' allodb(c(76, 76), c("Pinus","Pinus"), c("nigra", "abies"), coords)
#'
#' # Rich output with metadata
#' result <- allodb(50, "Quercus", "robur", coords, rich_output = TRUE)
#' print(result)
#' }
#' @import remotes
#' @export
#' @aliases allodb
#'
allodb <- function(dbh, genus, species, coords, output.all = TRUE,
                   new.eqtable = NULL, rich_output = FALSE){

  # ==== Check Inputs ====
  if (!is.numeric(dbh)) stop("'dbh' must be numeric.")
  if (!is.character(genus)) stop("'genus' must be a character vector.")
  if (!is.character(species)) stop("'species' must be a character vector.")

  if (length(dbh) != length(genus) || length(dbh) != length(species)) {
    stop("Lengths of 'dbh', 'genus', and 'species' must be equal.")
  }

  if (!(is.numeric(coords) && length(coords) == 2) &&
      !(is.matrix(coords)  && ncol(coords) == 2)) {
    stop("'coords' must be either a numeric vector of length 2
         (longitude and latitude) or a matrix with two columns.")
  }
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.null(new.eqtable) && !is.data.frame(new.eqtable)) {
    stop("'new.eqtable' must be a data frame or NULL.")
  }

  # ==== Validate against allodb method ranges ====
  validation <- validate_inputs_for_method("allodb", dbh = dbh)
  flags <- if (validation$flags[1] != "None") validation$flags else character()
  warnings_list <- if (validation$validity_warnings[1] != "None") validation$validity_warnings else character()

  if(output.all == TRUE && is.null(new.eqtable)){
    # Output dataframe
    df <- data.frame(dbh = dbh, genus = genus, species = species,
                     AGB_allodb_kg = NA, allodb_a = NA, allodb_b = NA,
                     allodb_sigma = NA, stringsAsFactors = FALSE)

    Names <- as.factor(paste(df$genus, df$species))
    for(i in seq_along(levels(Names))){
      name <- levels(Names)[i]
      treeID <- which(Names == name)

      # Get parameters and sigma: AGB = a*dbh^b+e {e ~ N(0,sigma^2}
      params <- allodb::est_params(genus = as.character(df$genus[treeID][1]),
                                   species = as.character(df$species[treeID][1]),
                                   coords = coords)
      df$allodb_a[treeID] <- params$a
      df$allodb_b[treeID] <- params$b
      df$allodb_sigma[treeID] <- params$sigma
    }

    # Calculate Biomass
    df$AGB_allodb_kg <- df$allodb_a * df$dbh ^ df$allodb_b

    # Clean df
    NAs <- which(is.na(df$AGB_allodb_kg))
    df$allodb_a[NAs] <- df$allodb_b[NAs] <- df$allodb_sigma[NAs] <- NA

    # ==== Return rich output if requested ====
    if (rich_output) {
      if (length(dbh) == 1) {
        return(single_tree_rich_output(
          value = df$AGB_allodb_kg[1],
          method = "allodb",
          measure = "AGB",
          unit = "kg",
          uncertainty = df$allodb_sigma[1],
          validity_warnings = warnings_list,
          flags = flags,
          inputs = list(
            genus = genus,
            species = species,
            dbh = dbh,
            coords = coords,
            allodb_a = df$allodb_a[1],
            allodb_b = df$allodb_b[1]
          )
        ))
      }

      # Multiple trees
      # Get shared method metadata once (same for all trees) - direct access
      allodb_meta <- method_metadata[method_metadata$method == "allodb", ]
      allodb_assumptions <- method_assumptions$assumption[method_assumptions$method == "allodb"]

      # Calculate totals first (vectorized)
      total_AGB_kg <- sum(df$AGB_allodb_kg, na.rm = TRUE)
      total_uncertainty_kg <- if (!all(is.na(df$allodb_sigma))) {
        sqrt(sum(df$allodb_sigma^2, na.rm = TRUE))
      } else {
        NULL
      }

      # Collect all unique flags from all trees (vectorized)
      all_flags <- if (length(flags) > 0 && flags[1] != "None") unique(flags) else character()

      # Pre-calculate tree-specific values (vectorized)
      uncertainty_vals <- ifelse(!is.na(df$allodb_sigma), df$allodb_sigma, NA_real_)
      ci_low_vals <- if (!all(is.na(df$allodb_sigma))) {
        df$AGB_allodb_kg - 1.96 * df$allodb_sigma
      } else {
        rep(NA_real_, nrow(df))
      }
      ci_high_vals <- if (!all(is.na(df$allodb_sigma))) {
        df$AGB_allodb_kg + 1.96 * df$allodb_sigma
      } else {
        rep(NA_real_, nrow(df))
      }

      # Create summary table directly from df (vectorized)
      combined_df <- data.frame(
        tree_id = seq_len(nrow(df)),
        genus = df$genus,
        species = df$species,
        dbh = df$dbh,
        biomass_kg = df$AGB_allodb_kg,
        uncertainty_kg = uncertainty_vals,
        stringsAsFactors = FALSE
      )

      # Add CI columns if uncertainty was calculated
      if (!all(is.na(df$allodb_sigma))) {
        combined_df$ci_low_kg <- ci_low_vals
        combined_df$ci_high_kg <- ci_high_vals
      }

      # Create result objects (vectorized structure, but list of lists for compatibility)
      results_list <- lapply(seq_len(nrow(df)), function(i) {
        # Create lightweight result object with shared metadata
        result <- list(
          # === Core estimate (tree-specific) ===
          value = if (!is.na(df$AGB_allodb_kg[i])) df$AGB_allodb_kg[i] else NA_real_,
          measure = "AGB",
          unit = "kg",

          # === Method information (shared - from metadata) ===
          method = "allodb",
          method_full_name = allodb_meta$full_name,
          reference = allodb_meta$reference,
          reference_short = allodb_meta$reference_short,
          doi = allodb_meta$doi,
          source_type = allodb_meta$source_type,
          region = allodb_meta$region,
          biome = strsplit(allodb_meta$biome, "; ")[[1]],

          # === Assumptions (shared - from metadata) ===
          assumptions = allodb_assumptions,

          # === Uncertainty ===
          uncertainty = if (!is.na(uncertainty_vals[i])) uncertainty_vals[i] else NULL,
          uncertainty_method = allodb_meta$uncertainty_method,
          ci_low = if (!is.na(ci_low_vals[i])) ci_low_vals[i] else NULL,
          ci_high = if (!is.na(ci_high_vals[i])) ci_high_vals[i] else NULL,

          # === Validity and flags (tree-specific) ===
          validity_warnings = if (length(warnings_list) > 0) warnings_list else "None",
          flags = if (length(flags) > 0) flags else "None",

          # === Valid ranges (shared - from metadata) ===
          valid_dbh_range = c(allodb_meta$dbh_min_cm, allodb_meta$dbh_max_cm),
          valid_height_range = c(allodb_meta$height_min_m, allodb_meta$height_max_m),
          height_required = allodb_meta$height_required,
          species_specific = allodb_meta$species_specific,
          wood_density_required = allodb_meta$wood_density_required,

          # === Inputs (minimal - only coords and coefficients, since genus/species/dbh are in df) ===
          inputs = list(
            coords = coords,
            allodb_a = df$allodb_a[i],
            allodb_b = df$allodb_b[i]
          )
        )
        class(result) <- "allometry_result"
        return(result)
      })

      result <- list(
        trees = results_list,
        summary_table = combined_df,
        n_trees = length(dbh),
        total_AGB_kg = total_AGB_kg,
        mean_AGB_kg = mean(df$AGB_allodb_kg, na.rm = TRUE),
        total_uncertainty_kg = total_uncertainty_kg,
        validation = validation,
        detailed_output = df
      )
      class(result) <- c("allodb_multi_result", "list")
      return(result)
    }

    return(df)

  } else {
    biomass <- allodb::get_biomass(dbh = as.numeric(dbh),
                                   genus = as.character(genus),
                                   species = as.character(species),
                                   coords = coords,
                                   new_eqtable = new.eqtable)

    if (rich_output && length(dbh) == 1) {
      return(single_tree_rich_output(
        value = biomass,
        method = "allodb",
        measure = "AGB",
        unit = "kg",
        uncertainty = NULL,
        validity_warnings = warnings_list,
        flags = flags,
        inputs = list(genus = genus, species = species, dbh = dbh, coords = coords)
      ))
    }

    return(biomass)
  }
}

