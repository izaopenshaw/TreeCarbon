# ==============================================================================
# TreeCarbon - Method Metadata Registry and Input Validation
# ==============================================================================
#
# This module provides:
#   - Method metadata registry (get_method_metadata)
#   - Method metadata display functions (print_method_info, method_comparison_table)
#   - Input validation for allometric methods (validate_allometry_inputs,
#     validate_inputs_for_method)
#
# Authors: Justin Moat (J.Moat@kew.org), Isabel Openshaw (I.Openshaw@kew.org)
#
# ==============================================================================

get_method_metadata <- function(method = "all") {

 # Use internal package data (method_metadata and method_assumptions)
  # These are loaded automatically with LazyData: true
  # Fallback to inline definition if data not available

  # Access lazy-loaded data directly (LazyData: true handles loading automatically)
  # Check if data exists (should always be available in installed package)
  if (!exists("method_metadata") || !exists("method_assumptions")) {
    stop("Package data 'method_metadata' or 'method_assumptions' not found. ",
         "This indicates a package installation problem.")
  }

  meta_df <- method_metadata
  assump_df <- method_assumptions

  # If data tables are available, use them
  if (!is.null(meta_df) && !is.null(assump_df)) {
    return(.build_metadata_from_tables(meta_df, assump_df, method))
  }

  # Fallback to inline definition (used during package build or if data missing)
  .get_method_metadata_inline(method)
}

#' @title Build metadata list structure from data tables
#' @description Internal helper that converts metadata and assumptions data frames
#'   into the nested list structure expected by get_method_metadata().
#' @param meta_df Data frame containing method metadata (one row per method).
#' @param assump_df Data frame containing method assumptions (multiple rows per method).
#' @param method Character string specifying which method to return, or "all".
#' @return A list containing method metadata, or a nested list of all methods.
#' @keywords internal
.build_metadata_from_tables <- function(meta_df, assump_df, method) {
  # Helper function to build metadata for a single method
  .build_single_method <- function(m) {
    # Extract single row for this method from metadata table
    row <- meta_df[meta_df$method == m, ]
    if (nrow(row) == 0) {
      stop("Method '", m, "' not found in metadata table")
    }
    # Extract all assumptions for this method (may be multiple rows)
    assumptions <- assump_df$assumption[assump_df$method == m]

    # Construct metadata list with all required fields
    list(
      full_name = row$full_name,
      reference = row$citation,
      reference_short = row$citation_short,
      doi = row$doi,
      assumptions = assumptions,
      region = row$region,
      biome = strsplit(row$biome, "; ")[[1]],  # Split semicolon-separated biomes
      source_type = row$source_type,
      dbh_range = c(row$dbh_min_cm, row$dbh_max_cm),
      height_range = c(row$height_min_m, row$height_max_m),
      height_required = row$height_required,
      species_specific = row$species_specific,
      wood_density_required = row$wood_density_required,
      uncertainty_method = row$uncertainty_method
    )
  }

  # ==== Return requested method(s) ====
  if (method == "all") {
    # Build metadata for all methods
    methods <- meta_df$method
    metadata <- lapply(methods, .build_single_method)
    names(metadata) <- methods
    return(metadata)
  } else {
    # Only build metadata for the requested method (more efficient)
    if (!method %in% meta_df$method) {
      available <- paste(meta_df$method, collapse = ", ")
      stop("Unknown method: ", method, ". Choose from: ", available, ", or 'all'")
    }
    return(.build_single_method(method))
  }
}

#' @title Get method metadata from inline definitions
#' @description Internal fallback function providing method metadata when package
#'   data tables are not available (e.g., during package build or testing).
#'   This ensures get_method_metadata() always works even without loaded data.
#' @param method Character string specifying the method, or "all" for all methods.
#' @return A list containing method metadata, matching the structure from data tables.
#' @keywords internal
.get_method_metadata_inline <- function(method) {
  # ==== Inline fallback definitions (should match data/method_metadata.rda) ====
  # These definitions are used when the package data tables are not available
  metadata <- list(
    WCC = list(
      full_name = "UK Woodland Carbon Code",
      reference = "Jenkins, T.A.R., et al. (2018). FC Woodland Carbon Code: Carbon Assessment Protocol (v2.0). Forestry Commission.",
      reference_short = "Jenkins et al. (2018)",
      doi = NA,
      assumptions = c(
        "Tree form follows UK forestry commission models",
        "Species-specific tariff numbers apply",
        "Nominal specific gravity from UK timber data",
        "Crown and root biomass estimated from DBH relationships",
        "Carbon fraction varies by tree type and biome"
      ),
      region = "United Kingdom",
      biome = c("temperate"),
      source_type = "government protocol",
      dbh_range = c(7, 200),
      height_range = c(1, 50),
      height_required = TRUE,
      species_specific = TRUE,
      wood_density_required = FALSE,
      uncertainty_method = "Error propagation through tariff equations"
    ),

    BIOMASS = list(
      full_name = "BIOMASS R Package (Chave et al. pantropical equations)",
      reference = "Rejou-Mechain, M., et al. (2017). BIOMASS: an R package for estimating above-ground biomass and its uncertainty in tropical forests. Methods in Ecology and Evolution, 8(9), 1163-1167.",
      reference_short = "Rejou-Mechain et al. (2017)",
      doi = "10.1111/2041-210X.12753",
      assumptions = c(
        "Chave et al. (2014) pantropical allometric equation applies",
        "Wood density from Global Wood Density Database",
        "Height can be estimated from DBH if not provided",
        "Environmental stress factor (E) derived from coordinates",
        "Designed for tropical forests but applicable elsewhere"
      ),
      region = "Pantropical (global)",
      biome = c("tropical", "subtropical", "temperate"),
      source_type = "peer-reviewed",
      dbh_range = c(5, 200),
      height_range = c(1, 60),
      height_required = FALSE,
      species_specific = FALSE,
      wood_density_required = TRUE,
      uncertainty_method = "Monte Carlo propagation of wood density and model error"
    ),

    allodb = list(
      full_name = "allodb R Package (multi-source allometric database)",
      reference = "Gonzalez-Akre, E., et al. (2020). allodb: An R package for biomass estimation at globally distributed extratropical forest plots. Methods in Ecology and Evolution, 11(10), 1273-1280.",
      reference_short = "Gonzalez-Akre et al. (2020)",
      doi = "10.1111/2041-210X.13452",
      assumptions = c(
        "Weighted combination of multiple published allometries",
        "Equations selected based on taxonomic and geographic similarity",
        "DBH is primary predictor variable",
        "Regional weighting based on coordinates",
        "Designed for extratropical forests"
      ),
      region = "Global (extratropical focus)",
      biome = c("temperate", "boreal", "mediterranean"),
      source_type = "peer-reviewed",
      dbh_range = c(1, 200),
      height_range = c(NA, NA),
      height_required = FALSE,
      species_specific = TRUE,
      wood_density_required = FALSE,
      uncertainty_method = "Model averaging with propagated parameter uncertainty"
    ),

    Bunce = list(
      full_name = "Bunce (1968) UK Deciduous Woodland Equations",
      reference = "Bunce, R.G.H. (1968). Biomass and Production of Trees in a Mixed Deciduous Woodland: I. Girth and Height as Parameters for the Estimation of Tree Dry Weight. Journal of Ecology, 56(3), 759-775.",
      reference_short = "Bunce (1968)",
      doi = "10.2307/2258105",
      assumptions = c(
        "Based on UK mixed deciduous woodland",
        "Dry weight estimated from girth (DBH) only",
        "Limited species coverage (mainly UK broadleaves)",
        "Historic dataset - calibration trees from 1960s",
        "Simple power-law relationship",
        "Confidence intervals are symmetric approximations (biomass ± 1.96 * sigma)",
        "Table 6 shows some asymmetric limits at large DBH, but symmetric intervals are reasonable for CV < 30%"
      ),
      region = "United Kingdom",
      biome = c("temperate"),
      source_type = "peer-reviewed",
      dbh_range = c(7, 150),
      height_range = c(NA, NA),
      height_required = FALSE,
      species_specific = TRUE,
      wood_density_required = FALSE,
      uncertainty_method = "Regression standard error"
    )
  )

  if (method == "all") {
    return(metadata)
  } else if (method %in% names(metadata)) {
    return(metadata[[method]])
  } else {
    stop("Unknown method: ", method, ". Choose from: ",
         paste(names(metadata), collapse = ", "), ", or 'all'")
  }
}

############# Method Metadata Display Functions ################
#' @title Print method metadata in formatted display
#' @description Displays comprehensive metadata for allometric methods in a
#'   readable, formatted output suitable for reports or quick reference.
#' @param method Character string specifying the method, or "all" for all methods.
#' @param include_assumptions Logical. Include detailed assumptions? Default TRUE.
#' @param include_ranges Logical. Include valid input ranges? Default TRUE.
#' @return Invisibly returns the metadata list. Called for its side effect of printing.
#' @examples
#' print_method_info("WCC")
#' print_method_info("all", include_assumptions = FALSE)
#' @export
print_method_info <- function(method = "all", include_assumptions = TRUE,
                              include_ranges = TRUE) {

  if (method == "all") {
    methods <- c("WCC", "BIOMASS", "allodb", "Bunce")
  } else {
    methods <- method
  }

  cat("------------------- ALLOMETRIC METHOD REFERENCE GUIDE -------------------\n")

  for (m in methods) {
    meta <- get_method_metadata(m)

    cat("\n")
    cat(sprintf("  %s: %s\n", m, meta$full_name))
    cat(" \n")

    # Citation
    cat("--- REFERENCE ---\n")
    cat(sprintf("  %s\n", meta$reference))
    if (!is.na(meta$doi)) {
      cat(sprintf("  DOI: https://doi.org/%s\n", meta$doi))
    }
    cat(" \n")

    # Source type
    cat("--- SOURCE TYPE ---\n")
    cat(sprintf("%s\n", meta$source_type))
    cat(" \n")

    # Regional applicability
    cat(sprintf("REGION: %s\n", meta$region))
    cat(sprintf("BIOME(S): %s\n", paste(meta$biome, collapse = ", ")))
    cat(" \n")
    # Valid ranges
    if (include_ranges) {
      cat("--- VALID INPUT RANGES ---\n")
      cat(sprintf("  DBH: %.0f - %.0f cm\n", meta$dbh_range[1], meta$dbh_range[2]))
      if (!all(is.na(meta$height_range))) {
        cat(sprintf("  Height: %.0f - %.0f m\n", meta$height_range[1], meta$height_range[2]))
      } else {
        cat("  Height: Not used as input\n")
      }
      cat(sprintf("  Height required: %s\n", ifelse(meta$height_required, "YES", "No")))
      cat(sprintf("  Species-specific: %s\n", ifelse(meta$species_specific, "YES", "No")))
      cat(sprintf("  Wood density required: %s\n", ifelse(meta$wood_density_required, "YES", "No")))
      cat(" \n")
    }

    # Uncertainty
    cat("--- UNCERTAINTY METHOD ---\n")
    cat(sprintf("  %s\n", meta$uncertainty_method))
    cat(" \n")

    # Assumptions
    if (include_assumptions) {
      cat("--- ASSUMPTIONS ---\n")
      for (i in seq_along(meta$assumptions)) {
        cat(sprintf("  %d. %s\n", i, meta$assumptions[i]))
      }
      cat(" \n")
    }
  }

  invisible(get_method_metadata(method))
}


########### Method Comparison Table ###########
#' @title Get method comparison summary as a data frame
#' @description Returns a data frame comparing key attributes across all allometric
#'   methods, useful for quick comparison and method selection. Provides a
#'   tabular summary of references, valid ranges, and data requirements.
#' @return A data frame with one row per method and columns for key attributes:
#' \describe{
#'   \item{method}{Method identifier (WCC, BIOMASS, allodb, Bunce)}
#'   \item{full_name}{Name of the method}
#'   \item{reference_short}{Short reference (Author, Year)}
#'   \item{source_type}{Type of source (peer-reviewed, government, etc.)}
#'   \item{region}{Geographic region of applicability}
#'   \item{dbh_min, dbh_max}{Valid DBH range in cm}
#'   \item{height_required}{Whether height input is required}
#'   \item{species_specific}{Whether method uses species-specific parameters}
#'   \item{wood_density_required}{Whether wood density is needed}
#' }
#' @examples
#' method_comparison_table()
#' @export
method_comparison_table <- function() {
  # Retrieve all method metadata
  all_meta <- get_method_metadata("all")

  # Build comparison table by extracting key attributes from each method
  data.frame(
    method = names(all_meta),
    full_name = sapply(all_meta, function(x) x$full_name),
    reference_short = sapply(all_meta, function(x) x$reference_short),
    source_type = sapply(all_meta, function(x) x$source_type),
    region = sapply(all_meta, function(x) x$region),
    dbh_min = sapply(all_meta, function(x) x$dbh_range[1]),
    dbh_max = sapply(all_meta, function(x) x$dbh_range[2]),
    height_required = sapply(all_meta, function(x) x$height_required),
    species_specific = sapply(all_meta, function(x) x$species_specific),
    wood_density_required = sapply(all_meta, function(x) x$wood_density_required),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#############  Input Validation and Flag Generation ################
#' @title Validate inputs and generate flags for allometric methods
#' @description Checks input values against valid ranges for each method and
#'   generates appropriate warnings and flags.
#' @param method Character string specifying the method
#' @param dbh Numeric vector of DBH values (cm)
#' @param height Numeric vector of height values (m), can be NULL
#' @param genus Character vector of genus names, can be NULL
#' @param species Character vector of species names, can be NULL
#' @param type Character vector of tree types ("broadleaf" or "conifer"), can be NULL
#' @return A list containing:
#' \describe{
#'   \item{validity_warnings}{Character vector of warning messages}
#'   \item{flags}{Named list of logical flags}
#'   \item{extrapolation}{Logical indicating if any extrapolation occurred}
#'   \item{defaults_used}{Character vector of defaults that were applied}
#' }
#' @examples
#' validate_allometry_inputs("WCC", dbh = c(5, 50, 250), height = c(10, 20, 30))
#' @export
validate_allometry_inputs <- function(method, dbh, height = NULL,
                                       genus = NULL, species = NULL,
                                       type = NULL) {

  # Retrieve method-specific metadata for range checking
  meta <- get_method_metadata(method)

  # Initialize tracking variables for warnings, flags, and defaults
  warnings_list <- character()
  flags <- list()
  defaults_used <- character()

  # ==== DBH Range Check ====
  dbh_range <- meta$dbh_range
  if (!is.null(dbh) && !all(is.na(dbh))) {
    below_min <- sum(dbh < dbh_range[1], na.rm = TRUE)
    above_max <- sum(dbh > dbh_range[2], na.rm = TRUE)

    flags$dbh_below_range <- below_min > 0
    flags$dbh_above_range <- above_max > 0

    if (below_min > 0) {
      warnings_list <- c(warnings_list,
        sprintf("%d tree(s) have DBH < %.1f cm (method minimum)", below_min, dbh_range[1]))
    }
    if (above_max > 0) {
      warnings_list <- c(warnings_list,
        sprintf("%d tree(s) have DBH > %.1f cm (method maximum)", above_max, dbh_range[2]))
    }
  }

  # ==== Height Checks ====
  height_range <- meta$height_range
  flags$height_required <- meta$height_required
  flags$height_missing <- FALSE
  flags$height_estimated <- FALSE

  if (meta$height_required) {
    if (is.null(height) || all(is.na(height))) {
      flags$height_missing <- TRUE
      warnings_list <- c(warnings_list,
        sprintf("%s requires height input - results may be unreliable", method))
    }
  }

  if (!is.null(height) && !all(is.na(height)) && !all(is.na(height_range))) {
    below_min <- sum(height < height_range[1], na.rm = TRUE)
    above_max <- sum(height > height_range[2], na.rm = TRUE)

    flags$height_below_range <- below_min > 0
    flags$height_above_range <- above_max > 0

    if (below_min > 0) {
      warnings_list <- c(warnings_list,
        sprintf("%d tree(s) have height < %.1f m (method minimum)", below_min, height_range[1]))
    }
    if (above_max > 0) {
      warnings_list <- c(warnings_list,
        sprintf("%d tree(s) have height > %.1f m (method maximum)", above_max, height_range[2]))
    }
  }

  # ==== Species/Type Checks ====
  flags$species_missing <- FALSE
  flags$type_missing <- FALSE

  if (meta$species_specific) {
    if ((is.null(genus) || all(is.na(genus))) && (is.null(species) || all(is.na(species)))) {
      flags$species_missing <- TRUE
      defaults_used <- c(defaults_used, "Generic/default species parameters used")
    }
  }

  if (method == "WCC") {
    if (is.null(type) || all(is.na(type)) || all(type == "")) {
      flags$type_missing <- TRUE
      defaults_used <- c(defaults_used, "Tree type inferred from species lookup")
    }
  }

  # ==== Determine Overall Extrapolation Flag ====
  extrapolation_flags <- c(
    flags$dbh_below_range %||% FALSE,
    flags$dbh_above_range %||% FALSE,
    flags$height_below_range %||% FALSE,
    flags$height_above_range %||% FALSE,
    flags$height_missing %||% FALSE,
    flags$species_missing %||% FALSE
  )
  flags$extrapolation <- any(extrapolation_flags, na.rm = TRUE)

  return(list(
    validity_warnings = warnings_list,
    flags = flags,
    extrapolation = flags$extrapolation,
    defaults_used = defaults_used
  ))
}

############### Helper: Validate and Flag inputs for any method #############
#' @title Validate inputs for any allometric method
#' @description Validates input values against method-specific ranges and
#'   generates warnings and flags. Use this before calculations to check validity.
#' @param method Character. Method identifier ("WCC", "BIOMASS", "allodb", "Bunce")
#' @param dbh Numeric. DBH value(s) in cm
#' @param height Numeric or NULL. Height value(s) in m
#' @param genus Character or NULL. Genus name(s)
#' @param species Character or NULL. Species name(s)
#' @return A list with validity_warnings, flags, and extrapolation indicator
#' @examples
#' # Check if inputs are valid for WCC
#' check <- validate_inputs_for_method("WCC", dbh = 250, height = 10)
#' check$validity_warnings
#' check$flags
#' @export
validate_inputs_for_method <- function(method, dbh, height = NULL,
                                        genus = NULL, species = NULL) {

  meta <- get_method_metadata(method)
  warnings <- character()
  flags <- character()

  # DBH range check
  if (!is.null(dbh) && !all(is.na(dbh))) {
    if (any(dbh < meta$dbh_range[1], na.rm = TRUE)) {
      n <- sum(dbh < meta$dbh_range[1], na.rm = TRUE)
      warnings <- c(warnings, sprintf("%d value(s) below minimum DBH (%.0f cm)", n, meta$dbh_range[1]))
      flags <- c(flags, "DBH_below_range")
    }
    if (any(dbh > meta$dbh_range[2], na.rm = TRUE)) {
      n <- sum(dbh > meta$dbh_range[2], na.rm = TRUE)
      warnings <- c(warnings, sprintf("%d value(s) above maximum DBH (%.0f cm)", n, meta$dbh_range[2]))
      flags <- c(flags, "DBH_above_range")
    }
  }

  # Height checks
  if (meta$height_required) {
    if (is.null(height) || all(is.na(height))) {
      warnings <- c(warnings, sprintf("%s requires height but none provided", method))
      flags <- c(flags, "Height_missing")
    }
  }

  if (!is.null(height) && !all(is.na(height)) && !all(is.na(meta$height_range))) {
    if (any(height < meta$height_range[1], na.rm = TRUE)) {
      n <- sum(height < meta$height_range[1], na.rm = TRUE)
      warnings <- c(warnings, sprintf("%d value(s) below minimum height (%.0f m)", n, meta$height_range[1]))
      flags <- c(flags, "Height_below_range")
    }
    if (any(height > meta$height_range[2], na.rm = TRUE)) {
      n <- sum(height > meta$height_range[2], na.rm = TRUE)
      warnings <- c(warnings, sprintf("%d value(s) above maximum height (%.0f m)", n, meta$height_range[2]))
      flags <- c(flags, "Height_above_range")
    }
  }

  extrapolation <- length(flags) > 0 && any(grepl("range", flags))

  list(
    validity_warnings = if (length(warnings) > 0) warnings else "None",
    flags = if (length(flags) > 0) flags else "None",
    extrapolation = extrapolation,
    method_meta = meta
  )
}

############# Helper: null coalescing operator ################
#' @title NULL-coalescing operator
#' @description Returns the left-hand side if not NULL, otherwise returns right-hand side.
#' @param x Value to check for NULL.
#' @param y Default value to return if x is NULL.
#' @return x if x is not NULL, otherwise y.
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x


#' @title Convert allometry_result to data frame
#' @description Converts the result object to a single-row data frame for easy
#'   combination with other results
#' @param x An allometry_result object
#' @param ... Additional arguments (unused)
#' @return A data frame with one row
#' @export
as.data.frame.allometry_result <- function(x, ...) {
  # Helper function to safely extract scalar values (always returns length 1)
  get_scalar <- function(field, default) {
    if (is.null(field) || length(field) == 0) return(default)
    if (length(field) == 1) return(field)
    return(field[1])  # Take first element if vector
  }

  # Helper function to safely paste character vectors (always returns length 1 string)
  paste_safe <- function(field, collapse = "; ", default = "None") {
    if (is.null(field) || length(field) == 0) return(default)
    result <- paste(field, collapse = collapse)
    if (length(result) == 0 || result == "") return(default)
    return(result)
  }

  # Extract all fields with safe defaults - ensure all are length 1
  value_scalar <- get_scalar(x$value, NA_real_)
  measure_scalar <- get_scalar(x$measure, "")
  unit_scalar <- get_scalar(x$unit, "")
  method_scalar <- get_scalar(x$method, "")
  method_full_name_scalar <- get_scalar(x$method_full_name, "")
  reference_short_scalar <- get_scalar(x$reference_short, "")
  source_type_scalar <- get_scalar(x$source_type, "")
  region_scalar <- get_scalar(x$region, "")
  uncertainty_scalar <- get_scalar(x$uncertainty, NA_real_)
  ci_low_scalar <- get_scalar(x$ci_low, NA_real_)
  ci_high_scalar <- get_scalar(x$ci_high, NA_real_)
  height_required_scalar <- get_scalar(x$height_required, NA)

  validity_warnings_str <- paste_safe(x$validity_warnings, collapse = "; ", default = "None")
  flags_str <- paste_safe(x$flags, collapse = "; ", default = "None")
  assumptions_str <- paste_safe(x$assumptions, collapse = " | ", default = "")

  # Create data frame - all fields are guaranteed to be length 1
  data.frame(
    value = value_scalar,
    measure = measure_scalar,
    unit = unit_scalar,
    method = method_scalar,
    method_full_name = method_full_name_scalar,
    reference_short = reference_short_scalar,
    source_type = source_type_scalar,
    region = region_scalar,
    uncertainty = uncertainty_scalar,
    ci_low = ci_low_scalar,
    ci_high = ci_high_scalar,
    validity_warnings = validity_warnings_str,
    flags = flags_str,
    assumptions = assumptions_str,
    height_required = height_required_scalar,
    stringsAsFactors = FALSE
  )
}

#' @title Summary method for allometry_result
#' @description Quick summary of the result
#' @param object An allometry_result object
#' @param ... Additional arguments (unused)
#' @export
summary.allometry_result <- function(object, ...) {
  cat(sprintf("%s estimate using %s: %.4f %s",
              object$measure, object$method, object$value, object$unit))
  if (!is.null(object$uncertainty)) {
    cat(sprintf(" (+/- %.4f)", object$uncertainty))
  }
  cat("\n")
  cat(sprintf("Source: %s (%s)\n", object$reference_short, object$source_type))
  if (any(object$validity_warnings != "None")) {
    cat(sprintf("WARNINGS: %s\n", paste(object$validity_warnings, collapse = "; ")))
  }
  invisible(object)
}

#' @title Print method for allometry_result
#' @description Displays the result with assumptions prominently shown
#' @param x An allometry_result object
#' @param ... Additional arguments (unused)
#' @export
print.allometry_result <- function(x, ...) {

  cat("------------------- ALLOMETRY RESULT ------------------- \n")

  # Main estimate
  cat(sprintf("ESTIMATE: %.4f %s (%s)\n", x$value, x$unit, x$measure))
  if (!is.null(x$uncertainty) && !is.na(x$uncertainty)) {
    cat(sprintf("UNCERTAINTY: +/- %.4f %s (SD)\n", x$uncertainty, x$unit))
  }
  if (!is.null(x$ci_low) && !is.na(x$ci_low)) {
    cat(sprintf("95%% CI: [%.4f, %.4f] %s\n", x$ci_low, x$ci_high, x$unit))
  }
  cat(" \n")
  cat("--- METHOD INFORMATION ---\n")
  cat(sprintf("Method: %s\n", x$method_full_name))
  cat(sprintf("Reference: %s\n", x$reference_short))
  cat(sprintf("Source: %s\n", x$source_type))
  cat(sprintf("Region: %s | Biome: %s\n", x$region, x$biome))
  cat(" \n")
  cat("--- ASSUMPTIONS ---\n")
  for (i in seq_along(x$assumptions)) {
    cat(sprintf("  %d. %s\n", i, x$assumptions[i]))
  }
  cat(" \n")
  cat("--- VALIDITY & FLAGS ---\n")
  cat(sprintf("Valid DBH range: %.0f - %.0f cm\n", x$valid_dbh_range[1], x$valid_dbh_range[2]))
  if (!all(is.na(x$valid_height_range))) {
    cat(sprintf("Valid height range: %.0f - %.0f m\n", x$valid_height_range[1], x$valid_height_range[2]))
  }
  cat(sprintf("Height required: %s\n", ifelse(x$height_required, "YES", "No")))

  if (any(x$validity_warnings != "None")) {
    cat("\n*** WARNINGS ***\n")
    for (w in x$validity_warnings) {
      cat(sprintf("  ! %s\n", w))
    }
  }
  if (any(x$flags != "None")) {
    cat("\nFLAGS:\n")
    for (f in x$flags) {
      cat(sprintf("  - %s\n", f))
    }
  }
  cat(" \n")
  cat("--- REFERENCE ---\n")
  cat(sprintf("  %s\n", x$reference))
  if (!is.na(x$doi)) {
    cat(sprintf("  DOI: https://doi.org/%s\n", x$doi))
  }
  invisible(x)
}

#################### Single Tree Rich Return ######################

#' @title Create a rich allometry result object for a single tree entry
#' @description Creates a standardized result object that includes the estimate,
#'   method metadata, validity warnings, flags, and uncertainty information.
#' @param value Numeric. The estimated value (biomass, carbon, etc.)
#' @param method Character. The method identifier ("WCC", "BIOMASS", "allodb", "Bunce")
#' @param measure Character. What was measured ("AGB", "AGC", "CO2e", "volume", etc.)
#' @param unit Character. Unit of measurement ("t", "kg", "m3", etc.)
#' @param uncertainty Numeric or NULL. Standard deviation/error of the estimate
#' @param ci_low Numeric or NULL. Lower confidence interval bound
#' @param ci_high Numeric or NULL. Upper confidence interval bound
#' @param validity_warnings Character vector. Any validity warnings generated
#' @param flags Character vector. Flags for extrapolation, defaults used, etc.
#' @param inputs Named list. The input values used (dbh, height, species, etc.)
#' @return An object of class "allometry_result" with standardized structure
#' @examples
#' # Internal use - creates rich result for downstream functions
#' result <- single_tree_rich_output(value = 1.5, method = "WCC",
#'   measure = "AGC", unit = "t", uncertainty = 0.15,
#'   inputs = list(dbh = 30, height = 15, species = "Quercus robur"))
#' print(result)
#' @export
single_tree_rich_output <- function(value, method, measure, unit,
                                    uncertainty = NULL,
                                    ci_low = NULL, ci_high = NULL,
                                    validity_warnings = character(),
                                    flags = character(),
                                    inputs = list()) {

  # Get method metadata

  meta <- get_method_metadata(method)

  # Calculate CI if we have uncertainty but not explicit CI
  if (!is.null(uncertainty) && is.null(ci_low)) {
    ci_low <- value - 1.96 * uncertainty
    ci_high <- value + 1.96 * uncertainty
  }

  # Build the result object
  result <- list(
    # === Core estimate ===
    value = value,
    measure = measure,
    unit = unit,

    # === Method information ===
    method = method,
    method_full_name = meta$full_name,
    reference = meta$reference,
    reference_short = meta$reference_short,
    doi = meta$doi,
    source_type = meta$source_type,
    region = meta$region,
    biome = paste(meta$biome, collapse = "; "),

    # === Assumptions (critical!) ===
    assumptions = meta$assumptions,

    # === Uncertainty ===
    uncertainty = uncertainty,
    uncertainty_method = meta$uncertainty_method,
    ci_low = ci_low,
    ci_high = ci_high,
    ci_level = if (!is.null(ci_low)) 0.95 else NA,

    # === Validity and flags ===
    validity_warnings = if (length(validity_warnings) > 0) validity_warnings else "None",
    flags = if (length(flags) > 0) flags else "None",

    # === Valid ranges ===
    valid_dbh_range = meta$dbh_range,
    valid_height_range = meta$height_range,
    height_required = meta$height_required,
    species_specific = meta$species_specific,
    wood_density_required = meta$wood_density_required,

    # === Inputs used ===
    inputs = inputs
  )

  class(result) <- c("allometry_result", "list")
  return(result)
}


#################### Multiple Tree Rich Return ######################

#' @title Create a rich allometry result object for multiple trees
#' @description Creates a standardized result object that includes the estimate,
#'   method metadata, validity warnings, flags, and uncertainty information.
#' @param value Numeric. The total estimated value (biomass, carbon, etc.)
#' @param method Character. The method identifier ("WCC", "BIOMASS", "allodb", "Bunce")
#' @param measure Character. What was measured ("AGB", "AGC", "CO2e", "volume", etc.)
#' @param unit Character. Unit of measurement ("t", "kg", "m3", etc.)
#' @param sigma Numeric or NULL. Total sigma
#' @param validity_warnings Character vector. Any validity warnings generated
#' @param flags Character vector. Flags for extrapolation, defaults used, etc.
#' @param inputs Named list. The input values used (dbh, height, species, etc.)
#' @return An object of class "allometry_result" with standardized structure
#' @examples
#' # Internal use - creates rich result for downstream functions
#' result <- multi_tree_rich_output(value = 1.5, method = "WCC",
#'   measure = "AGC", unit = "t", uncertainty = 0.15,
#'   inputs = list(dbh = 30, height = 15, species = "Quercus robur"))
#' print(result)
#' @export
#'
multi_tree_rich_output <- function(value, method, measure, unit, sigma = NULL,
                                   validity_warnings = character(),
                                   flags = character(), inputs = list())
  {

  # Input check
  methods <- c("WCC", "BIOMASS", "allodb", "Bunce")
  if (!missing(method) && !(method %in% methods)) {
    stop("Invalid method. Choose from: ", paste(method, collapse = ", "))
  }

  # Get method metadata
  meta_df <- method_metadata[method_metadata$method == method, ]
  assump_df <- method_assumptions[method_assumptions$method == method, ]

  # Calculate CI if we have uncertainty but not explicit CI
  if (!is.null(uncertainty) && is.null(ci_low)) {
    ci_low <- value - 1.96 * uncertainty
    ci_high <- value + 1.96 * uncertainty
  }

  # Build the result object
  result <- list(
    # === Core estimate ===
    value = value,
    measure = measure,
    unit = unit,

    # === Method information ===
    method = method,
    method_full_name = meta$full_name,
    reference = meta$reference,
    reference_short = meta$reference_short,
    doi = meta$doi,
    source_type = meta$source_type,
    region = meta$region,
    biome = paste(meta$biome, collapse = "; "),

    # === Assumptions (critical!) ===
    assumptions = meta$assumptions,

    # === Uncertainty ===
    uncertainty = uncertainty,
    uncertainty_method = meta$uncertainty_method,
    ci_low = ci_low,
    ci_high = ci_high,
    ci_level = if (!is.null(ci_low)) 0.95 else NA,

    # === Validity and flags ===
    validity_warnings = if (length(validity_warnings) > 0) validity_warnings else "None",
    flags = if (length(flags) > 0) flags else "None",

    # === Valid ranges ===
    valid_dbh_range = meta$dbh_range,
    valid_height_range = meta$height_range,
    height_required = meta$height_required,
    species_specific = meta$species_specific,
    wood_density_required = meta$wood_density_required,

    # === Inputs used ===
    inputs = inputs
  )

  class(result) <- c("allometry_result", "list")
  return(result)
}
