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
# ==============================================================================
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
#' @param re_dbh relative measurement error for diameter at breast height (optional)
#' @param re  relative error of coefficients (default = 0.025)
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
Bunce <- function(name, dbh, type = NULL, re_dbh = NULL, re = 0.025,
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

  # ==== Calculate Biomass ====
  r$biomass <- exp(r$a + r$b * log(pi * r$dbh))

  # ==== Calculate uncertainty if error terms provided ====
  # Error propagation for Bunce equation: biomass = exp(a + b * ln(pi * d))
  #
  # The Bunce equation is a log-linear regression:
  #   ln(biomass) = a + b * ln(pi * d)
  #
  # For log-linear models, we use the residual standard error (RSE) approach:
  # The RSE in the log domain translates to a coefficient of variation (CV)
  # in the original domain due to properties of log-normal distributions.
  #
  # Based on typical allometric regression statistics:
  #   - Model RSE in log domain: ~0.15-0.25 (corresponds to CV of ~15-25%)
  #   - This is combined with DBH measurement error
  #
  # Using partial derivative for DBH only (measurable uncertainty):
  #   d(biomass)/d(d) = biomass * b/d
  #   sigma_dbh = biomass * (b/d) * sigma_d = biomass * b * re_dbh
  #
  # Combined uncertainty (model + measurement):
  #   sigma = biomass * sqrt(model_cv^2 + (b * re_dbh)^2)
  #
  # Where model_cv accounts for:
  #   - Regression residual error
  #   - Parameter uncertainty in a and b
  #   - Model structural uncertainty

  r$sigma <- NA
  if (!is.null(re_dbh)) {
    # Model CV based on typical allometric regression uncertainty
    # This accounts for parameter uncertainty and residual error
    # Bunce (1968) reported R² values ~0.85-0.95, suggesting RSE ~15-20%
    model_cv <- re  # Use the 're' parameter as model CV (default 2.5% may be too low)

    # For more realistic uncertainty, use a minimum model CV of 10%
    # This is conservative for allometric equations without published SEs
    effective_model_cv <- max(model_cv, 0.10)

    # DBH measurement error contribution
    dbh_error_contribution <- (r$b * re_dbh)^2

    # Combined uncertainty
    r$sigma <- r$biomass * sqrt(effective_model_cv^2 + dbh_error_contribution)
  }

  # ==== Return rich output if requested ====
  if (rich_output) {

    # For single tree, return single allometry_result
    if (length(dbh) == 1) {
      return(create_allometry_result(
        value = r$biomass[1],
        method = "Bunce",
        measure = "AGB",
        unit = "kg",
        uncertainty = if (!is.na(r$sigma[1])) r$sigma[1] else NULL,
        validity_warnings = warnings_list,
        flags = flags,
        inputs = list(
          name = name,
          dbh = dbh,
          spcode = r$spcode[1],
          type = type
        )
      ))
    }

    # For multiple trees, return list of results with combined summary
    results_list <- lapply(seq_len(nrow(r)), function(i) {
      # Individual tree flags
      tree_flags <- flags
      if (used_fallback[i]) {
        tree_flags <- c(tree_flags, "Used_fallback_coefficients")
      }

      create_allometry_result(
        value = r$biomass[i],
        method = "Bunce",
        measure = "AGB",
        unit = "kg",
        uncertainty = if (!is.na(r$sigma[i])) r$sigma[i] else NULL,
        validity_warnings = warnings_list,
        flags = tree_flags,
        inputs = list(
          name = name[i],
          dbh = dbh[i],
          spcode = r$spcode[i],
          type = if (!is.null(type)) type[min(i, length(type))] else NULL
        )
      )
    })

    # Create combined data frame
    combined_df <- do.call(rbind, lapply(results_list, as.data.frame.allometry_result))
    combined_df$tree_id <- seq_len(nrow(combined_df))
    combined_df$species_name <- name
    combined_df$spcode <- r$spcode

    # Return as a special multi-tree result
    result <- list(
      trees = results_list,
      summary_table = combined_df,
      n_trees = length(dbh),
      total_biomass_kg = sum(r$biomass, na.rm = TRUE),
      mean_biomass_kg = mean(r$biomass, na.rm = TRUE),
      validation = validation
    )
    class(result) <- c("bunce_multi_result", "list")

    return(result)
  }

  # ==== Standard return (backwards compatible) ====
  r <- r[, !(names(r) %in% c("a", "b"))]
  return(r)
}

#' @title Print method for multiple Bunce results
#' @description Formatted display for multi-tree Bunce results
#' @param x A bunce_multi_result object
#' @param ... Additional arguments (unused)
#' @export
print.bunce_multi_result <- function(x, ...) {

  cat("---------- BUNCE BIOMASS ESTIMATES ----------\n")
  cat("             Multiple trees \n\n")

  cat(sprintf("Number of trees: %d\n", x$n_trees))
  cat(sprintf("Total biomass: %.2f kg (%.4f t)\n", x$total_biomass_kg, x$total_biomass_kg / 1000))
  cat(sprintf("Mean biomass per tree: %.2f kg\n", x$mean_biomass_kg))

  # Get method metadata (from first tree)
  meta <- x$trees[[1]]

  cat("METHOD INFORMATION\n")
  cat(sprintf("Method: %s\n", meta$method_full_name))
  cat(sprintf("Reference: %s\n", meta$reference_short))
  cat(sprintf("Source: %s | Region: %s\n", meta$source_type, meta$region))

  cat(" ASSUMPTIONS \n")
  for (i in seq_along(meta$assumptions)) {
    cat(sprintf("  %d. %s\n", i, meta$assumptions[i]))
  }

  cat("VALIDITY\n")
  cat(sprintf("Valid DBH range: %.0f - %.0f cm\n", meta$valid_dbh_range[1], meta$valid_dbh_range[2]))

  if (any(x$validation$validity_warnings != "None")) {
    cat("\n WARNINGS \n")
    for (w in x$validation$validity_warnings) {
      cat(sprintf("  ! %s\n", w))
    }
  }

  if (any(x$validation$flags != "None")) {
    cat("\nFLAGS:\n")
    for (f in x$validation$flags) {
      cat(sprintf("  - %s\n", f))
    }
  }

  cat("SUMMARY TABLE (first 10 rows)\n")
  print(utils::head(x$summary_table[, c("tree_id", "species_name", "value", "uncertainty", "flags")], 10))

  if (x$n_trees > 10) {
    cat(sprintf("... and %d more trees\n", x$n_trees - 10))
  }

  cat("Reference:\n")
  cat(sprintf("  %s\n", meta$reference))
  if (!is.na(meta$doi)) {
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
      BIOMASS::AGBmonteCarlo(
        D = df$dbh,
        WD = df$Wood_Density,
        errWD = df$Wood_Density_sd,
        H = if (!height_estimated) df$height else NULL,
        errH = mc_errH,
        HDmodel = HDmodel_mc,
        coord = if (height_estimated && is.null(HDmodel_mc)) coords else NULL,
        Dpropag = Dpropag,
        n = n_mc
      )
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
      return(create_allometry_result(
        value = df$AGB_Biomass_kg[1],
        method = "BIOMASS",
        measure = "AGB",
        unit = "kg",
        uncertainty = if (!is.null(agb_sd)) agb_sd[1] else NULL,
        validity_warnings = warnings_list,
        flags = flags,
        inputs = list(
          genus = genus,
          species = species,
          dbh = dbh,
          height = if (height_estimated) df$height[1] else height,
          height_estimated = height_estimated,
          wood_density = df$Wood_Density[1],
          wood_density_sd = df$Wood_Density_sd[1],
          wood_density_source = wd_source,
          region = region,
          coords = coords,
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
    results_list <- lapply(seq_len(nrow(df)), function(i) {
      create_allometry_result(
        value = df$AGB_Biomass_kg[i],
        method = "BIOMASS",
        measure = "AGB",
        unit = "kg",
        uncertainty = if (!is.null(agb_sd)) agb_sd[i] else NULL,
        validity_warnings = warnings_list,
        flags = flags,
        inputs = list(
          genus = genus[i],
          species = species[i],
          dbh = dbh[i],
          height = df$height[i],
          wood_density = df$Wood_Density[i],
          wood_density_sd = df$Wood_Density_sd[i],
          wood_density_source = wd_source,
          CI_5_pct = if (!is.null(agb_credible)) agb_credible$CI_5[i] else NULL,
          CI_95_pct = if (!is.null(agb_credible)) agb_credible$CI_95[i] else NULL,
          CV_pct = if (!is.null(df$AGB_CV_pct)) df$AGB_CV_pct[i] else NULL
        )
      )
    })

    combined_df <- do.call(rbind, lapply(results_list, as.data.frame.allometry_result))
    combined_df$tree_id <- seq_len(nrow(combined_df))
    combined_df$genus <- genus
    combined_df$species <- species

    result <- list(
      trees = results_list,
      summary_table = combined_df,
      n_trees = length(dbh),
      total_AGB_kg = sum(df$AGB_Biomass_kg, na.rm = TRUE),
      mean_AGB_kg = mean(df$AGB_Biomass_kg, na.rm = TRUE),
      total_AGB_sd_kg = if (!is.null(mc_result)) sqrt(sum(agb_sd^2, na.rm = TRUE)) else NULL,
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
#' remotes::install_github('ropensci/allodb')
#' coords <- c(-0.088837,51.071610)
#' allodb(81.887, "Pinus", "nigra", coords, output.all = FALSE)
#' allodb(c(76, 76), c("Pinus","Pinus"), c("nigra", "abies"), coords)
#'
#' # Rich output with metadata
#' result <- allodb(50, "Quercus", "robur", coords, rich_output = TRUE)
#' print(result)
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

  # Ensure the allodb package is installed
  if (nchar(system.file(package = 'allodb')) == 0) {
    warning("The 'allodb' package is not installed. Installing it now...")
    remotes::install_github("ropensci/allodb")
  }

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
        return(create_allometry_result(
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
      results_list <- lapply(seq_len(nrow(df)), function(i) {
        create_allometry_result(
          value = df$AGB_allodb_kg[i],
          method = "allodb",
          measure = "AGB",
          unit = "kg",
          uncertainty = df$allodb_sigma[i],
          validity_warnings = warnings_list,
          flags = flags,
          inputs = list(
            genus = genus[i],
            species = species[i],
            dbh = dbh[i]
          )
        )
      })

      combined_df <- do.call(rbind, lapply(results_list, as.data.frame.allometry_result))
      combined_df$tree_id <- seq_len(nrow(combined_df))
      combined_df$genus <- genus
      combined_df$species <- species

      result <- list(
        trees = results_list,
        summary_table = combined_df,
        n_trees = length(dbh),
        total_AGB_kg = sum(df$AGB_allodb_kg, na.rm = TRUE),
        mean_AGB_kg = mean(df$AGB_allodb_kg, na.rm = TRUE),
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
      return(create_allometry_result(
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

