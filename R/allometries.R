# ==============================================================================
# TreeCarbon - Multi-Method Allometric Calculations
# ==============================================================================
#
# This module provides the main user-facing interface for calculating tree
# carbon/biomass using multiple allometric methods with consistent output.
#
# Functions included:
#   - Method metadata registry (get_method_metadata)
#   - Input validation for allometric methods
#   - Multi-method allometry calculations (allometries)
#   - Method comparison utilities (compare_allometries)
#   - Sensitivity analysis for method choice
#   - Plotting functions for method evaluation
#
# Authors: Justin Moat (J.Moat@kew.org), Isabel Openshaw (I.Openshaw@kew.org)
#
# ==========================================================================
#
# Suppress R CMD check notes for ggplot2 aes() bindings
# (suppresses "no visible binding for global variable x" warnings)
utils::globalVariables(c("method", "value", "DBH", "Height", "tree", "ymin", "ymax",
                         "total", "pct_diff_vs_ref", "cv_pct", "sign"))

############# Method Metadata Registry ################
#' @title Get allometric method metadata
#' @description Returns comprehensive metadata for each allometric method including
#'   citations, assumptions, valid ranges, and regional applicability.
#'   Metadata is loaded from the package data tables \code{method_metadata} and
#'   \code{method_assumptions}.
#' @param method Character string specifying the method. Options: "WCC", "BIOMASS",
#'   "allodb", "Bunce", or "all" to return all methods.
#' @return A list containing method metadata with components:
#' \describe{
#'   \item{full_name}{Full descriptive name of the method}
#'   \item{citation}{Full citation for the method}
#'   \item{citation_short}{Short citation (Author, Year)}
#'   \item{assumptions}{Character vector of key assumptions}
#'   \item{region}{Geographic region(s) where method is applicable}
#'   \item{biome}{Biome(s) where method is applicable}
#'   \item{source_type}{Type of source (e.g., "peer-reviewed", "government")}
#'   \item{dbh_range}{Valid DBH range in cm (min, max)}
#'   \item{height_range}{Valid height range in m (min, max), NA if not required}
#'   \item{height_required}{Logical, whether height is required}
#'   \item{species_specific}{Logical, whether method is species-specific}
#'   \item{wood_density_required}{Logical, whether wood density is needed}
#'   \item{uncertainty_method}{How uncertainty is calculated}
#' }
#' @examples
#' get_method_metadata("WCC")
#' get_method_metadata("all")
#' @seealso \code{\link{method_metadata}}, \code{\link{method_assumptions}}
#' @export
get_method_metadata <- function(method = "all") {

 # Use internal package data (method_metadata and method_assumptions)
  # These are loaded automatically with LazyData: true
  # Fallback to inline definition if data not available

  meta_df <- .get_package_data("method_metadata")
  assump_df <- .get_package_data("method_assumptions")

  # If data tables are available, use them
  if (!is.null(meta_df) && !is.null(assump_df)) {
    return(.build_metadata_from_tables(meta_df, assump_df, method))
  }

  # Fallback to inline definition (used during package build or if data missing)
  .get_method_metadata_inline(method)
}

#' @title Retrieve lazy-loaded package data
#' @description Internal helper function to retrieve lazy-loaded package data tables.
#'   Handles both library() and devtools::load_all() scenarios gracefully.
#' @param name Character string. Name of the data object to retrieve.
#' @return The requested data object, or NULL if not found.
#' @keywords internal
.get_package_data <- function(name) {
  # Initialize result as NULL (will remain NULL if data not found)
  result <- NULL

  # ==== First attempt: Get from package namespace ====
  # This is the most reliable method when package is properly loaded
  tryCatch({
    ns <- getNamespace("TreeCarbon")
    # Check if data object exists in namespace (LazyData objects stored here)
    if (exists(name, envir = ns, inherits = FALSE)) {
      result <- get(name, envir = ns)
    }
  }, error = function(e) NULL)  # Silently fail if namespace not available

  # ==== Second attempt: Use data() function ====
  # Fallback for when package is not fully loaded (e.g., during development)
  if (is.null(result)) {
    tryCatch({
      # Create isolated environment to avoid polluting global namespace
      env <- new.env()
      utils::data(list = name, package = "TreeCarbon", envir = env)
      # Check if data was successfully loaded into environment
      if (exists(name, envir = env)) {
        result <- get(name, envir = env)
      }
    }, error = function(e) NULL)  # Silently fail if data() fails
  }

  return(result)
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
  # Get list of all available methods from metadata table

  methods <- meta_df$method

  # ==== Build nested list structure for each method ====
  metadata <- lapply(methods, function(m) {
    # Extract single row for this method from metadata table
    row <- meta_df[meta_df$method == m, ]
    # Extract all assumptions for this method (may be multiple rows)
    assumptions <- assump_df$assumption[assump_df$method == m]

    # Construct metadata list with all required fields
    list(
      full_name = row$full_name,
      citation = row$citation,
      citation_short = row$citation_short,
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
  })
  # Name list elements by method for easy lookup
  names(metadata) <- methods

  # ==== Return requested method(s) ====
  if (method == "all") {
    return(metadata)
  } else if (method %in% names(metadata)) {
    return(metadata[[method]])
  } else {
    stop("Unknown method: ", method, ". Choose from: ",
         paste(names(metadata), collapse = ", "), ", or 'all'")
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
      citation = "Jenkins, T.A.R., et al. (2018). FC Woodland Carbon Code: Carbon Assessment Protocol (v2.0). Forestry Commission.",
      citation_short = "Jenkins et al. (2018)",
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
      citation = "Rejou-Mechain, M., et al. (2017). BIOMASS: an R package for estimating above-ground biomass and its uncertainty in tropical forests. Methods in Ecology and Evolution, 8(9), 1163-1167.",
      citation_short = "Rejou-Mechain et al. (2017)",
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
      citation = "Gonzalez-Akre, E., et al. (2020). allodb: An R package for biomass estimation at globally distributed extratropical forest plots. Methods in Ecology and Evolution, 11(10), 1273-1280.",
      citation_short = "Gonzalez-Akre et al. (2020)",
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
      citation = "Bunce, R.G.H. (1968). Biomass and Production of Trees in a Mixed Deciduous Woodland: I. Girth and Height as Parameters for the Estimation of Tree Dry Weight. Journal of Ecology, 56(3), 759-775.",
      citation_short = "Bunce (1968)",
      doi = "10.2307/2258105",
      assumptions = c(
        "Based on UK mixed deciduous woodland",
        "Dry weight estimated from girth (DBH) only",
        "Limited species coverage (mainly UK broadleaves)",
        "Historic dataset - calibration trees from 1960s",
        "Simple power-law relationship"
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

  cat("\n")
  cat("================================================================================\n")
  cat("                    ALLOMETRIC METHOD REFERENCE GUIDE                           \n")
  cat("================================================================================\n")

  for (m in methods) {
    meta <- get_method_metadata(m)

    cat("\n")
    cat("--------------------------------------------------------------------------------\n")
    cat(sprintf("  %s: %s\n", m, meta$full_name))
    cat("--------------------------------------------------------------------------------\n")

    # Citation
    cat("\nCITATION:\n")
    cat(sprintf("  %s\n", meta$citation))
    if (!is.na(meta$doi)) {
      cat(sprintf("  DOI: https://doi.org/%s\n", meta$doi))
    }

    # Source type
    cat(sprintf("\nSOURCE TYPE: %s\n", meta$source_type))

    # Regional applicability
    cat(sprintf("REGION: %s\n", meta$region))
    cat(sprintf("BIOME(S): %s\n", paste(meta$biome, collapse = ", ")))

    # Valid ranges
    if (include_ranges) {
      cat("\nVALID INPUT RANGES:\n")
      cat(sprintf("  DBH: %.0f - %.0f cm\n", meta$dbh_range[1], meta$dbh_range[2]))
      if (!all(is.na(meta$height_range))) {
        cat(sprintf("  Height: %.0f - %.0f m\n", meta$height_range[1], meta$height_range[2]))
      } else {
        cat("  Height: Not used as input\n")
      }
      cat(sprintf("  Height required: %s\n", ifelse(meta$height_required, "YES", "No")))
      cat(sprintf("  Species-specific: %s\n", ifelse(meta$species_specific, "YES", "No")))
      cat(sprintf("  Wood density required: %s\n", ifelse(meta$wood_density_required, "YES", "No")))
    }

    # Uncertainty
    cat(sprintf("\nUNCERTAINTY METHOD:\n  %s\n", meta$uncertainty_method))

    # Assumptions
    if (include_assumptions) {
      cat("\nKEY ASSUMPTIONS:\n")
      for (i in seq_along(meta$assumptions)) {
        cat(sprintf("  %d. %s\n", i, meta$assumptions[i]))
      }
    }
  }

  cat("\n================================================================================\n")
  cat("                                END OF REFERENCE                                \n")
  cat("================================================================================\n\n")

  invisible(get_method_metadata(method))
}


#' @title Get method comparison summary as a data frame
#' @description Returns a data frame comparing key attributes across all allometric
#'   methods, useful for quick comparison and method selection. Provides a
#'   tabular summary of citations, valid ranges, and data requirements.
#' @return A data frame with one row per method and columns for key attributes:
#' \describe{
#'   \item{method}{Method identifier (WCC, BIOMASS, allodb, Bunce)}
#'   \item{full_name}{Full descriptive name of the method}
#'   \item{citation_short}{Short citation (Author, Year)}
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
    citation_short = sapply(all_meta, function(x) x$citation_short),
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

############# Helper Functions ################
# NULL-coalescing operator (internal utility)
# Returns the left-hand side if not NULL, otherwise returns right-hand side
# @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x


############# Multi-Method Allometry Calculations ################
#' @title Calculate above ground carbon/biomass using multiple allometric methods
#' @description  Function that inputs tree species, dbh, height and calculates
#' carbon/biomass estimates using four different allometric methods: WCC, BIOMASS,
#' allodb, and Bunce. Enables comparison across methods.
#'
#'   When \code{rich_output = TRUE}, returns a comprehensive result object
#'   including method metadata, assumptions, validity warnings for each method,
#'   and uncertainty
#'
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param genus First part of species binomial
#' @param species Second part of species binomial
#' @param dbh diameter at breast height in centimetres
#' @param height in metres
#' @param type either 'broadleaf' or 'conifer'
#' @param method method of converting biomass to carbon. Either 'Thomas' or
#' 'IPCC2' has the error associated and "Matthews1", "Matthews2" or "IPCC1" do not.
#' @param returnv either 'AGC' or 'AGB' for above ground carbon or biomass
#' @param region of the World. See ?getWoodDensity for the list of regions
#' @param biome temperate, boreal, mediterranean, tropical, subtropical or all
#' @param coords either a vector of coordinates of the site or a matrix of
#' coordinates for each tree of longitude and latitude
#' @param re_dbh relative measurement error for diameter at breast height
#' @param re_h relative error of height measurement
#' @param re relative error of coefficients (default = 2.5\%)
#' @param nsg nominal specific gravity (optional)
#' @param sig_nsg sigma for nominal specific gravity
#' @param checkTaxo If TRUE then BIOMASS::correctTaxo will check spelling of
#' species name. This is included in output if there were modifications made.
#' @param rich_output Logical. If TRUE, returns a rich result object with full
#'   metadata for ALL methods including: values, citations, assumptions,
#'   validity warnings, flags, and uncertainties. Default FALSE.
#' @returns If \code{rich_output = FALSE}: data.frame with estimates from all methods.
#'   If \code{rich_output = TRUE}: a list containing method-specific results with
#'   full metadata for WCC, BIOMASS, allodb, and Bunce.
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#'  Carbon Assessment Protocol (v2. 0)." (2018).
#' Rejou-Mechain, M., Tanguy, A., Piponiot, C., Chave, J., & Herault, B. (2017).
#'  BIOMASS: an R package for estimating above-ground biomass and its
#'  uncertainty in tropical forests. Methods in Ecology and Evolution, 8(9),
#'  1163-1167
#' Gonzalez-Akre, E., Piponiot, C., Lepore, M., & Anderson-Teixeira, K. (2020).
#'  allodb: An R package for biomass estimation at globally distributed
#'  extratropical forest plots. Methods in Ecology and Evolution, 11(10),
#'  1273-1280
#' Bunce, R. G. H. "Biomass and Production of Trees in a Mixed Deciduous
#'  Woodland: I. Girth and height as Parameters for the Estimation of Tree Dry
#'  Weight" (1968)
#' @importFrom utils data
#' @examples
#' # Standard output
#' allometries("Quercus", "robur", 20, 10)
#'
#' # Rich output with full metadata for all methods
#' result <- allometries("Quercus", "robur", 30, 15, rich_output = TRUE)
#' print(result)
#' @export
#'
allometries <- function(genus, species, dbh, height, type = NULL, method ="IPCC2",
                        returnv = "AGC", region = "Europe", biome = "temperate",
                        coords = c(-0.088837,51.071610), re_dbh = 0.05,
                        re_h = 0.1, re = 0.025, nsg = NULL, sig_nsg = 0.09413391,
                        checkTaxo = FALSE, rich_output = FALSE){

  # ==== Input checks ====
  if (!is.character(genus) || !is.character(species))
    stop("genus and species must be character strings.")

  if (!is.numeric(dbh) || any(dbh < 0, na.rm = TRUE))
    stop("dbh must be numeric and positive")

  if (!is.numeric(height) || any(height < 0, na.rm = TRUE))
    stop("height must be numeric and positive")

  if (!is.numeric(coords) || length(coords) != 2) {
    stop("coords must be a numeric vector of length 2 (longitude, latitude).")}

  error_terms <- c(re_dbh, re_h, re, sig_nsg)
  if(anyNA(error_terms) || !is.numeric(error_terms) || any(error_terms < 0)){
    stop("Error terms (re_dbh, re_h, re, sig_nsg) must be postive and numeric.")}

  if (!is.character(region) || !is.character(biome)){
    stop("region and biome must be character strings.")}

  # ==== Validate all methods ====
  validations <- list(
    WCC = validate_inputs_for_method("WCC", dbh = dbh, height = height),
    BIOMASS = validate_inputs_for_method("BIOMASS", dbh = dbh, height = height),
    allodb = validate_inputs_for_method("allodb", dbh = dbh),
    Bunce = validate_inputs_for_method("Bunce", dbh = dbh)
  )

  #### Check spelling ####
  modified_names <- FALSE
  if(checkTaxo){
    correct <- BIOMASS::correctTaxo(genus = genus, species = species, useCache = TRUE)
    modified_names <- any(correct$nameModified == "TRUE", na.rm = TRUE)
    if (modified_names) {
      genus_input   <- genus
      species_input <- species
      genus         <- correct$genusCorrected
      species       <- correct$speciesCorrected
      name_modified <- correct$nameModified
    }
  }

  # ==== Calculate AGB ====
  #### BIOMASS package ####
  bio <- suppressMessages(suppressWarnings(
    BIOMASS(dbh, height, genus, species, coords, region, output.all = TRUE)))

  #### Woodland Carbon Code ####
  name <- paste(genus, species)

  if(method %in% c("Thomas", "IPCC2")){ # These methods contain errors for biomass conversion
    WCC <- suppressWarnings(fc_agc_error(name, dbh, height, type, method,
                                         biome, TRUE, re_dbh, re_h, re, nsg, sig_nsg))
  } else if((method %in% c("Matthews1", "Matthews2", "IPCC1"))) {
    WCC <- suppressWarnings(fc_agc(name, dbh, height, type,
                                   method, output.all = TRUE))
  } else {
    stop("Invalid method specified. Choose from 'Thomas', 'IPCC2', 'Matthews1',
         'Matthews2', or 'IPCC1'.")
  }

  # New type variable: either looked up type from species name or type inputted
  type0 <- ifelse(is.na(WCC$type) | WCC$type == "any",
                  as.character(type), as.character(WCC$type))
  WCC$AGB_WCC_t <- WCC$crownbiomass_t + WCC$stembiomass_t

  #### allodb package ####
  allo <- allodb(dbh, genus, species, coords, TRUE)

  #### Bunce ####
  AGB_Bunce_kg <- suppressWarnings(Bunce(name, dbh, type, re_dbh, re))

  # ==== Convert Biomass to Carbon ====
  if(returnv == "AGC"){

    # Output one warning message for missing entries that will be skipped
    if (any(is.na(type0) | !type0 %in% c("broadleaf", "conifer"))) {
      warning("Skipping entries with undefined 'type', see biomass2c function.")
    }

    # Calculate carbon in tonnes
    # Note: Bunce sigma must also be converted from kg to tonnes (multiply by 0.001)
    suppressWarnings({
      bio_AGC <- biomass2c(bio$AGB_Biomass_kg*0.001, method, type0, biome)
      allo_AGC <- biomass2c(allo$AGB_allodb_kg*0.001, method, type0, biome, allo$allodb_sigma*0.001)
      bunce_AGC <- biomass2c(AGB_Bunce_kg$biomass*0.001, method, type0, biome, AGB_Bunce_kg$sigma*0.001)
    })

    # Output data frame
    df <- data.frame(genus, species, family = bio$Family, dbh, height,
                     allodb_C_t = allo_AGC$AGC,  allodb_C_sig = allo_AGC$sig_AGC,
                     biomass_C_t = bio_AGC,      biomass_C_sig = bio_AGC,
                     WCC_C_t = WCC$AGB_WCC_t,    WCC_C_sig = WCC$sig_AGC,
                     Bunce_C_t = bunce_AGC$AGC,  Bunce_C_sig = bunce_AGC$sig_AGC)

  } else {

    df <- data.frame(genus, species, family = bio$Family, dbh, height,
                     allodb_B_t = allo$AGB_allodb_kg/1000,  allodb_B_sig = allo$allodb_sigma/1000,
                     biomass_B_t = bio$AGB_Biomass_kg/1000, biomass_B_sig=bio$AGB_Biomass_kg/1000,
                     WCC_B_t = WCC$AGB_WCC_t,               WCC_B_sig = WCC$sig_AGC,
                     Bunce_B_t = AGB_Bunce_kg$biomass/1000, Bunce_B_sig = AGB_Bunce_kg$sigma/1000)

  }

  # If height was predicted add these columns
  if('height_pred' %in% names(bio)){
    df$input_height <- bio$height_input
    df$pred_height <- bio$height_pred
  }

  # Add taxonomy corrections if checked
  if(checkTaxo && modified_names){
    df$input_genus <- genus_input
    df$input_species <- species_input
    df$name_modified <- name_modified
  }

  # If type not entered then provide WCC calculations (used in biomass2c fn)
  if(is.null(type) || anyNA(type)){
    df$type <- type0
    df$input_type <- type
  }

  # ==== Return rich output if requested ====
  if (rich_output) {
    # Get metadata for all methods
    all_meta <- get_method_metadata("all")

    result <- list(
      # Summary table
      estimates = df,

      # Method-specific results with full metadata
      methods = list(
        WCC = list(
          value = if(returnv == "AGC") WCC$AGC_WCC_t else WCC$AGB_WCC_t,
          uncertainty = WCC$sig_AGC,
          unit = "t",
          measure = returnv,
          metadata = all_meta$WCC,
          validation = validations$WCC
        ),
        BIOMASS = list(
          value = if(returnv == "AGC") bio_AGC else bio$AGB_Biomass_kg/1000,
          uncertainty = NULL,
          unit = "t",
          measure = returnv,
          metadata = all_meta$BIOMASS,
          validation = validations$BIOMASS
        ),
        allodb = list(
          value = if(returnv == "AGC") allo_AGC$AGC else allo$AGB_allodb_kg/1000,
          uncertainty = if(returnv == "AGC") allo_AGC$sig_AGC else allo$allodb_sigma/1000,
          unit = "t",
          measure = returnv,
          metadata = all_meta$allodb,
          validation = validations$allodb
        ),
        Bunce = list(
          value = if(returnv == "AGC") bunce_AGC$AGC else AGB_Bunce_kg$biomass/1000,
          uncertainty = if(returnv == "AGC") bunce_AGC$sig_AGC else AGB_Bunce_kg$sigma/1000,
          unit = "t",
          measure = returnv,
          metadata = all_meta$Bunce,
          validation = validations$Bunce
        )
      ),

      # Inputs
      inputs = list(
        genus = genus,
        species = species,
        dbh = dbh,
        height = height,
        type = type0,
        biome = biome,
        region = region,
        coords = coords
      ),

      # Summary stats
      n_trees = length(dbh),
      return_type = returnv
    )

    class(result) <- c("allometries_rich_result", "list")
    return(result)
  }

  return(df)
}

#' @title Print method for allometries rich result
#' @description Formatted display for multi-method allometry results
#' @param x An allometries_rich_result object
#' @param ... Additional arguments (unused)
#' @export
print.allometries_rich_result <- function(x, ...) {
  cat("\n")
  cat("================================================================================\n")
  cat("              MULTI-METHOD ALLOMETRY COMPARISON RESULTS                         \n")
  cat("================================================================================\n\n")

  cat(sprintf("Trees analysed: %d\n", x$n_trees))
  cat(sprintf("Output type: %s\n", x$return_type))
  cat(sprintf("Species: %s %s\n", x$inputs$genus[1], x$inputs$species[1]))

  cat("\n--------------------------------------------------------------------------------\n")
  cat("ESTIMATES BY METHOD\n")
  cat("--------------------------------------------------------------------------------\n")

  for (m_name in names(x$methods)) {
    m <- x$methods[[m_name]]
    val <- if (length(m$value) == 1) m$value else mean(m$value, na.rm = TRUE)
    cat(sprintf("\n%s:\n", m_name))
    cat(sprintf("  Value: %.4f %s | %s\n", val, m$unit, m$metadata$citation_short))
    cat(sprintf("  Region: %s | Source: %s\n", m$metadata$region, m$metadata$source_type))

    if (!is.null(m$uncertainty) && !all(is.na(m$uncertainty))) {
      unc <- if (length(m$uncertainty) == 1) m$uncertainty else mean(m$uncertainty, na.rm = TRUE)
      cat(sprintf("  Uncertainty: +/- %.4f\n", unc))
    }

    if (m$validation$validity_warnings[1] != "None") {
      cat("  *** WARNINGS: ")
      cat(paste(m$validation$validity_warnings, collapse = "; "))
      cat("\n")
    }
  }

  cat("\n--------------------------------------------------------------------------------\n")
  cat("*** KEY ASSUMPTIONS BY METHOD ***\n")
  cat("--------------------------------------------------------------------------------\n")

  for (m_name in names(x$methods)) {
    m <- x$methods[[m_name]]
    cat(sprintf("\n%s (%s):\n", m_name, m$metadata$citation_short))
    for (i in seq_along(m$metadata$assumptions)) {
      cat(sprintf("  %d. %s\n", i, m$metadata$assumptions[i]))
    }
  }

  cat("\n================================================================================\n")
  cat("Use compare_allometries() for detailed statistical comparison\n")
  cat("================================================================================\n\n")

  invisible(x)
}

############# Plotting Helper Functions ################

#' @title Calculate jitter width based on data range
#' @description Calculates an appropriate jitter width for scatter plots based on the range of the data.
#' This prevents overplotting when multiple methods have similar values by adding small random offsets.
#' @param data_range Numeric value representing the range of the data (max - min)
#' @param default_width Default width to use if range is 0 or invalid (default: 0.1)
#' @param proportion Proportion of the data range to use for jitter width (default: 0.02, i.e., 2%)
#' @returns Numeric value representing the jitter width to use
#' @noRd
calculate_jitter_width <- function(data_range, default_width = 0.1, proportion = 0.02) {
  if (length(data_range) != 1 || is.na(data_range) || data_range <= 0) {
    return(default_width)
  }
  return(max(data_range * proportion, default_width))
}

#' @title Set up jitter positions and error bar width
#' @description Sets up position adjustments for points and error bars based on whether jitter is enabled.
#' If jitter is enabled, calculates jitter width from data range and creates position_jitter objects.
#' If jitter is disabled, returns identity positioning and default error bar width.
#' @param data_range Numeric value representing the range of the data (max - min)
#' @param jitter Logical, whether to apply jitter (default: TRUE)
#' @param seed Random seed for jitter reproducibility (default: 123)
#' @returns List with three elements: point_position (position adjustment for points),
#'   error_position (position adjustment for error bars, or NULL if no jitter),
#'   and error_width (width of error bar caps)
#' @noRd
setup_jitter_positions <- function(data_range, jitter = TRUE, seed = 123) {
  if (jitter) {
    jitter_width <- calculate_jitter_width(data_range)
    return(list(
      point_position = position_jitter(width = jitter_width, seed = seed),
      error_position = position_jitter(width = jitter_width, seed = seed),
      error_width = jitter_width * 2
    ))
  } else {
    return(list(
      point_position = "identity",
      error_position = NULL,
      error_width = 0.1
    ))
  }
}

#' @title Add error bars to a ggplot
#' @description Adds error bars to a ggplot object if error data is available and show_errors is TRUE.
#' Checks for the presence of standard error (se) and ymin/ymax columns before adding error bars.
#' @param p ggplot object to which error bars will be added
#' @param plot_df Data frame containing the plotting data, must have columns: se (standard error), ymin, and ymax
#' @param show_errors Logical, whether to show error bars (if FALSE, returns plot unchanged)
#' @param position Position adjustment for error bars (e.g., position_jitter, position_dodge). If NULL, uses default positioning
#' @param width Width of error bar caps in plot units (default: 0.1)
#' @returns ggplot object with error bars added (if applicable), or the original plot if error bars cannot be added
#' @noRd
add_error_bars <- function(p, plot_df, show_errors, position = NULL, width = 0.1) {
  if (!show_errors) {
    return(p)
  }

  # Check if se column exists and has valid values
  if (!"se" %in% colnames(plot_df) || all(is.na(plot_df$se))) {
    return(p)
  }

  # Check if ymin/ymax exist and have valid values
  if (!"ymin" %in% colnames(plot_df) || !"ymax" %in% colnames(plot_df)) {
    return(p)
  }

  if (all(is.na(plot_df$ymin)) && all(is.na(plot_df$ymax))) {
    return(p)
  }

  # Build errorbar call - use linewidth (not size) for ggplot2 3.4.0+
  # Use inherit.aes = FALSE to prevent inheriting size aesthetic from points
  # Then explicitly specify x and y aesthetics from the plot data

  if (is.null(position)) {
    p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax),
                           inherit.aes = TRUE,
                           width = width, linewidth = 0.5, size = NULL, na.rm = TRUE)
  } else {
    p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax),
                           inherit.aes = TRUE,
                           position = position, width = width, linewidth = 0.5, size = NULL, na.rm = TRUE)
  }

  return(p)
}

############# Visualisation Functions ################
#' @title Create comparison plots for allometry methods
#' @description Creates ggplot2 or plotly plots comparing different allometric methods (WCC, BIOMASS, allodb, Bunce).
#' Supports multiple plot types including bar charts, scatter plots, density plots, and residual plots.
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param results_df Data frame with results from allometries function. Must contain columns for method estimates
#'   (e.g., WCC_C_t, biomass_C_t, allodb_C_t, Bunce_C_t for AGC, or corresponding _B_t columns for AGB)
#'   and optionally error columns (e.g., WCC_C_sig, biomass_C_sig, etc.)
#' @param plot_type Character string specifying the type of plot. Options: "bar" (bar chart comparing methods),
#'   "dbh" (scatter plot against DBH), "height" (scatter plot against height), "tree_index" (scatter plot against tree index),
#'   "density" (density distribution plot), or "residual" (residual plot showing differences from mean)
#' @param returnv Character string, either "AGC" for above ground carbon or "AGB" for above ground biomass.
#'   The function will auto-detect if the specified type doesn't match available columns
#' @param show_errors Logical, whether to show error bars representing uncertainty estimates (default: TRUE)
#' @param interactive Logical, whether to return an interactive plotly plot instead of a static ggplot (default: FALSE)
#' @param log_scale Logical, whether to use logarithmic scale on y-axis. Only applies to scatter plots (dbh, height),
#'   not bar plots, density plots, or residual plots (default: FALSE)
#' @param color_scheme Character string specifying the color scheme. Options: "default" (ggplot2 default colors),
#'   "viridis" (viridis color palette), or "colorblind" (Okabe-Ito colorblind-friendly palette) (default: "default")
#' @param font_size Numeric value specifying the base font size for plot text in points (default: 12)
#' @param size_scale Character string or NULL. For scatter plots, specifies variable to map point size to:
#'   "Height" maps size to height values, "DBH" maps size to DBH values, or NULL/"none" for fixed size points (default: NULL)
#' @param theme ggplot2 theme function to apply to the plot. Can be any ggplot2 theme function
#'   (e.g., theme_classic, theme_gray, theme_light, theme_dark) or NULL to use default ggplot2 theme.
#'   If NULL, defaults to theme_minimal with the specified font_size (default: NULL)
#' @param x_min Numeric value, minimum x-axis value for axis limits (optional, default: NULL)
#' @param x_max Numeric value, maximum x-axis value for axis limits (optional, default: NULL)
#' @param y_min Numeric value, minimum y-axis value for axis limits (optional, default: NULL)
#' @param y_max Numeric value, maximum y-axis value for axis limits (optional, default: NULL)
#' @param jitter Logical, whether to apply jitter to points and error bars in scatter plots (dbh, height).
#'   Jitter helps prevent overplotting when multiple methods have similar values. The jitter width is calculated
#'   as 2% of the data range (x-axis), with a minimum of 0.1 units (default: TRUE)
#' @param point_size Numeric value specifying the size of points in scatter plots (default: 2)
#' @details
#' This function creates visualizations comparing estimates from different allometric methods. For scatter plots
#' (dbh, height), jitter can be applied to points and error bars to prevent overplotting when multiple
#' methods have similar values. The jitter width is calculated as 2% of the data range (x-axis), with a minimum
#' of 0.1 units. This helps distinguish overlapping points while maintaining the overall pattern of the data.
#'
#' Interactive plots (when interactive = TRUE) display hover tooltips with formatted values: species name (if available),
#' method name, value (3 decimal places), and additional context-specific information (DBH, Height, Tree index, etc.).
#'
#' The function will warn if log scale is selected but data contains values less than or equal to zero, as log scale
#' cannot display such values. It will also warn if some methods have no data available.
#' @returns ggplot or plotly object depending on the interactive parameter. Returns NULL with a warning if no data
#'   is available to plot or if no matching method columns are found in the results data frame.
#' @import ggplot2
#' @importFrom scales viridis_pal
#' @examples
#' results <- allometries(c("Quercus","Quercus"), c("robur","robur"), c(20, 30), c(10, 15))
#' plot_allometries(results, "bar", "AGC", interactive = FALSE)
#' @export
#'
plot_allometries <- function(results_df, plot_type = "bar", returnv = "AGC",
                             show_errors = TRUE, interactive = FALSE,
                             log_scale = FALSE, color_scheme = "default",
                             font_size = 12, size_scale = NULL, theme = NULL,
                             jitter = TRUE, point_size = 2, x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL) {

  # ==== Input validation ====
  # Check results_df
  if (missing(results_df) || is.null(results_df)) {
    stop("'results_df' is required and cannot be NULL")
  }
  if (!is.data.frame(results_df)) {
    stop("'results_df' must be a data frame")
  }
  if (nrow(results_df) == 0) {
    stop("'results_df' must contain at least one row")
  }

  # Check plot_type
  valid_plot_types <- c("bar", "boxplot", "dbh", "height", "tree_index", "density", "residual")
  if (!is.character(plot_type) || length(plot_type) != 1 || !plot_type %in% valid_plot_types) {
    stop("'plot_type' must be one of: ", paste(valid_plot_types, collapse = ", "))
  }

  # Check returnv
  valid_returnv <- c("AGC", "AGB")
  if (!is.character(returnv) || length(returnv) != 1 || !returnv %in% valid_returnv) {
    stop("'returnv' must be either 'AGC' or 'AGB'")
  }

  # Check logical inputs
  if (!is.logical(show_errors) || length(show_errors) != 1) {
    stop("'show_errors' must be a single logical value (TRUE or FALSE)")
  }
  if (!is.logical(interactive) || length(interactive) != 1) {
    stop("'interactive' must be a single logical value (TRUE or FALSE)")
  }
  if (!is.logical(log_scale) || length(log_scale) != 1) {
    stop("'log_scale' must be a single logical value (TRUE or FALSE)")
  }
  if (!is.logical(jitter) || length(jitter) != 1) {
    stop("'jitter' must be a single logical value (TRUE or FALSE)")
  }
  if (!is.numeric(point_size) || length(point_size) != 1 || point_size <= 0) {
    stop("'point_size' must be a single positive numeric value")
  }

  # Check color_scheme
  valid_color_schemes <- c("default", "viridis", "colorblind")
  if (!is.character(color_scheme) || length(color_scheme) != 1 || !color_scheme %in% valid_color_schemes) {
    stop("'color_scheme' must be one of: ", paste(valid_color_schemes, collapse = ", "))
  }

  # Check font_size
  if (!is.numeric(font_size) || length(font_size) != 1 || font_size <= 0) {
    stop("'font_size' must be a single positive numeric value")
  }

  # Check size_scale (optional) - convert NA/empty to NULL
  if (is.null(size_scale) || (length(size_scale) == 1 && (is.na(size_scale) || size_scale == "" || tolower(size_scale) == "none"))) {
    size_scale <- NULL
  } else {
    if (!is.character(size_scale) || length(size_scale) != 1) {
      stop("'size_scale' must be NULL, 'none', 'Height', or 'DBH'")
    }
    valid_size_scales <- c("height", "dbh")
    if (!tolower(size_scale) %in% valid_size_scales) {
      stop("'size_scale' must be NULL, 'none', 'Height', or 'DBH'")
    }
    # Normalize to title case
    size_scale <- if (tolower(size_scale) == "height") "Height" else "DBH"
  }

  # Check axis limits (optional) - convert NA/empty to NULL
  if (is.null(x_min) || (length(x_min) == 1 && (is.na(x_min) || x_min == ""))) {
    x_min <- NULL
  } else {
    if (!is.numeric(x_min) || length(x_min) != 1) {
      stop("'x_min' must be a single numeric value or NULL")
    }
  }

  if (is.null(x_max) || (length(x_max) == 1 && (is.na(x_max) || x_max == ""))) {
    x_max <- NULL
  } else {
    if (!is.numeric(x_max) || length(x_max) != 1) {
      stop("'x_max' must be a single numeric value or NULL")
    }
  }

  if (is.null(y_min) || (length(y_min) == 1 && (is.na(y_min) || y_min == ""))) {
    y_min <- NULL
  } else {
    if (!is.numeric(y_min) || length(y_min) != 1) {
      stop("'y_min' must be a single numeric value or NULL")
    }
  }

  if (is.null(y_max) || (length(y_max) == 1 && (is.na(y_max) || y_max == ""))) {
    y_max <- NULL
  } else {
    if (!is.numeric(y_max) || length(y_max) != 1) {
      stop("'y_max' must be a single numeric value or NULL")
    }
  }

  # Check that min < max if both are provided
  if (!is.null(x_min) && !is.null(x_max) && x_min >= x_max) {
    stop("'x_min' must be less than 'x_max'")
  }
  if (!is.null(y_min) && !is.null(y_max) && y_min >= y_max) {
    stop("'y_min' must be less than 'y_max'")
  }

  # Auto-detect data type if returnv doesn't match available columns
  has_AGC <- any(grepl("_C_t$", colnames(results_df)))
  has_AGB <- any(grepl("_B_t$", colnames(results_df)))

  # If returnv doesn't match data, auto-detect
  if (returnv == "AGC" && !has_AGC && has_AGB) {
    returnv <- "AGB"
    warning("Data contains AGB columns but returnv='AGC' was specified. Using AGB instead.")
  } else if (returnv == "AGB" && !has_AGB && has_AGC) {
    returnv <- "AGC"
    warning("Data contains AGC columns but returnv='AGB' was specified. Using AGC instead.")
  }

  # Determine columns
  if (returnv == "AGC") {
    methods_cols <- c("WCC" = "WCC_C_t", "Biomass" = "biomass_C_t",
                      "Allodb" = "allodb_C_t", "Bunce" = "Bunce_C_t")
    err_cols <- c("WCC" = "WCC_C_sig", "Biomass" = "biomass_C_sig",
                  "Allodb" = "allodb_C_sig", "Bunce" = "Bunce_C_sig")
    y_label <- "Carbon (t)"
  } else {
    methods_cols <- c("WCC" = "WCC_B_t", "Biomass" = "biomass_B_t",
                      "Allodb" = "allodb_B_t", "Bunce" = "Bunce_B_t")
    err_cols <- c("WCC" = "WCC_B_sig", "Biomass" = "biomass_B_sig",
                  "Allodb" = "allodb_B_sig", "Bunce" = "Bunce_B_sig")
    y_label <- "Biomass (t)"
  }

  # Check which columns actually exist in the data
  available_method_cols <- methods_cols[methods_cols %in% colnames(results_df)]

  # Identify which methods have no data
  missing_methods <- setdiff(names(methods_cols), names(available_method_cols))

  if (length(available_method_cols) == 0) {
    # Try to provide helpful error message
    if (!has_AGC && !has_AGB) {
      warning("No method columns found in results_df (neither _C_t nor _B_t columns). Available columns: ",
              paste(colnames(results_df), collapse = ", "))
    } else {
      warning("No matching method columns found for returnv='", returnv, "'. Available columns: ",
              paste(colnames(results_df), collapse = ", "),
              ". Try using returnv='", ifelse(has_AGC, "AGC", "AGB"), "' instead.")
    }
    return(NULL)
  }

  # Build long dataframe
  n <- nrow(results_df)
  plot_df_list <- lapply(names(available_method_cols), function(m) {
    col_name <- available_method_cols[m]
    if (col_name %in% colnames(results_df)) {
      data.frame(
        tree = seq_len(n),
        DBH = results_df$dbh,
        Height = results_df$height,
        method = m,
        value = results_df[[col_name]],
        genus = if ("genus" %in% colnames(results_df)) results_df$genus else NA_character_,
        species = if ("species" %in% colnames(results_df)) results_df$species else NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  plot_df <- do.call(rbind, Filter(Negate(is.null), plot_df_list))

  if (is.null(plot_df) || nrow(plot_df) == 0) {
    warning("No data to plot after filtering. Methods with no data: ",
            paste(missing_methods, collapse = ", "))
    return(NULL)
  }

  # Remove rows with all NA values and check which methods have no valid data
  plot_df_before_na <- plot_df
  plot_df <- plot_df[!is.na(plot_df$value), ]

  # Check which methods have no non-NA values
  methods_with_data <- unique(plot_df$method)
  methods_with_no_data <- setdiff(unique(plot_df_before_na$method), methods_with_data)
  all_missing_methods <- c(missing_methods, methods_with_no_data)

  if (nrow(plot_df) == 0) {
    warning("No non-NA values to plot. Methods with no data: ",
            paste(all_missing_methods, collapse = ", "))
    return(NULL)
  }

  # Warn about methods with no data (if any)
  if (length(all_missing_methods) > 0) {
    warning("Some methods have no data and will not be plotted: ",
            paste(all_missing_methods, collapse = ", "))
  }

  # Warn if log scale is selected but data contains values <= 0
  if (log_scale && plot_type != "bar" && plot_type != "boxplot" && plot_type != "tree_index" && plot_type != "density" && plot_type != "residual") {
    if (any(plot_df$value <= 0, na.rm = TRUE)) {
      n_zero_or_neg <- sum(plot_df$value <= 0, na.rm = TRUE)
      warning("Log scale selected but data contains ", n_zero_or_neg,
              " value(s) less than or equal to zero. ",
              "Log scale cannot display zero or negative values. ",
              "Consider using linear scale or filtering out zero/negative values.")
    }
  }

  # Add error bars if available
  # Always create se column (even if show_errors is FALSE) for aggregation purposes
  plot_df$se <- NA_real_
  if (show_errors) {
    available_err_cols <- err_cols[err_cols %in% colnames(results_df)]
    for (m in names(available_err_cols)) {
      if (m %in% plot_df$method) {
        err_vals <- results_df[[available_err_cols[m]]]
        if (!all(is.na(err_vals))) {
          plot_df$se[plot_df$method == m] <- err_vals
        }
      }
    }
    # For log scale, allow ymin to go below 0 (will be handled by scale)
    # For linear scale, ensure ymin >= small positive value
    if (log_scale) {
      plot_df$ymin <- plot_df$value - plot_df$se
      plot_df$ymax <- plot_df$value + plot_df$se
    } else {
      plot_df$ymin <- pmax(plot_df$value - plot_df$se, 1e-6, na.rm = TRUE)
      plot_df$ymax <- plot_df$value + plot_df$se
    }
  } else {
    # Initialize ymin/ymax even when not showing errors (needed for aggregation)
    plot_df$ymin <- NA_real_
    plot_df$ymax <- NA_real_
  }

  # Define color schemes
  if (color_scheme == "viridis") {
    method_colors <- scales::viridis_pal()(length(unique(plot_df$method)))
  } else if (color_scheme == "colorblind") {
    # Okabe-Ito colorblind-friendly palette (no package required)
    okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
    n_methods <- length(unique(plot_df$method))
    method_colors <- okabe_ito[seq_len(min(n_methods, length(okabe_ito)))]
  } else {
    # Default colors
    method_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
  }
  names(method_colors) <- unique(plot_df$method)

  # Create plots
  if (plot_type == "bar") {
    if (length(unique(plot_df$tree)) == 1) {
      p <- ggplot(plot_df, aes(method, value, fill = method)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = method_colors) +
        geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.4, size = font_size * 0.3) +
        labs(x = "Method", y = y_label, title = paste("Above Ground", returnv, "Estimates for Single Tree")) +
        ggplot2::theme(legend.position = "none")
      p <- add_error_bars(p, plot_df, show_errors, width = 0.1)
    } else {
      # Aggregate values
      sum_df <- aggregate(value ~ method, plot_df, sum, na.rm = TRUE)

      # Aggregate errors if se column exists and has values
      if ("se" %in% colnames(plot_df) && !all(is.na(plot_df$se))) {
        se_agg <- aggregate(se ~ method, plot_df, function(x) sqrt(sum(x^2, na.rm = TRUE)), na.action = na.pass)
        sum_df <- merge(sum_df, se_agg, by = "method", all.x = TRUE)
        sum_df$se[is.na(sum_df$se)] <- 0
      } else {
        sum_df$se <- 0
      }

      # For log scale, allow ymin to go below 0
      if (log_scale) {
        sum_df$ymin <- sum_df$value - sum_df$se
        sum_df$ymax <- sum_df$value + sum_df$se
      } else {
        sum_df$ymin <- pmax(sum_df$value - sum_df$se, 1e-6, na.rm = TRUE)
        sum_df$ymax <- sum_df$value + sum_df$se
      }

      p <- ggplot(sum_df, aes(method, value, fill = method)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = method_colors[names(method_colors) %in% sum_df$method]) +
        geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.4, size = font_size * 0.3) +
        labs(x = "Method", y = y_label, title = paste("Total Above Ground", returnv, "Estimates per Method")) +
        ggplot2::theme(legend.position = "none")
      p <- add_error_bars(p, sum_df, show_errors, width = 0.1)
    }
  } else if (plot_type == "boxplot") {
    # Boxplot of per-tree estimates by method
    p <- ggplot(plot_df, aes(x = method, y = value, fill = method)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.alpha = 0.5) +
      geom_jitter(width = 0.2, alpha = 0.3, size = point_size * 0.7) +
      scale_fill_manual(values = method_colors[names(method_colors) %in% unique(plot_df$method)]) +
      labs(x = "Method", y = y_label,
           title = paste("Distribution of Per-Tree", returnv, "Estimates by Method"),
           subtitle = paste0("n = ", length(unique(plot_df$tree)), " trees")) +
      ggplot2::theme(legend.position = "none")
  } else if (plot_type == "dbh") {
    # Set up jitter positions based on data range
    dbh_range <- diff(range(plot_df$DBH, na.rm = TRUE))
    jitter_setup <- setup_jitter_positions(dbh_range, jitter)
    point_position <- jitter_setup$point_position
    error_position <- jitter_setup$error_position
    error_width <- jitter_setup$error_width

    # Build geom_point based on size_scale
    if (is.null(size_scale)) {
      p <- ggplot(plot_df, aes(DBH, value, colour = method)) +
        geom_point(position = point_position, size = point_size)
    } else if (size_scale == "Height") {
      p <- ggplot(plot_df, aes(DBH, value, colour = method, size = Height)) +
        geom_point(position = point_position)
    } else {
      p <- ggplot(plot_df, aes(DBH, value, colour = method, size = DBH)) +
        geom_point(position = point_position)
    }

    p <- p +
      scale_color_manual(values = method_colors) +
      labs(x = "DBH (cm)", y = y_label, title = paste("Above Ground", returnv, "Estimates by DBH"))
    p <- add_error_bars(p, plot_df, show_errors,
                        position = error_position,
                        width = error_width)
  } else if (plot_type == "height") {
    # Set up jitter positions based on data range
    height_range <- diff(range(plot_df$Height, na.rm = TRUE))
    jitter_setup <- setup_jitter_positions(height_range, jitter)
    point_position <- jitter_setup$point_position
    error_position <- jitter_setup$error_position
    error_width <- jitter_setup$error_width

    # Build geom_point based on size_scale
    if (is.null(size_scale)) {
      p <- ggplot(plot_df, aes(Height, value, colour = method)) +
        geom_point(position = point_position, size = point_size)
    } else if (size_scale == "Height") {
      p <- ggplot(plot_df, aes(Height, value, colour = method, size = Height)) +
        geom_point(position = point_position)
    } else {
      p <- ggplot(plot_df, aes(Height, value, colour = method, size = DBH)) +
        geom_point(position = point_position)
    }

    p <- p +
      scale_color_manual(values = method_colors) +
      labs(x = "Height (m)", y = y_label, title = paste("Above Ground", returnv, "Estimates by Height"))
    p <- add_error_bars(p, plot_df, show_errors,
                        position = error_position,
                        width = error_width)
  } else if (plot_type == "tree_index") {
    # Use position_dodge (or jitterdodge if jitter enabled) to separate methods at each tree index
    # Calculate dodge width based on number of methods
    n_methods <- length(unique(plot_df$method))
    dodge_width <- min(0.3, 0.5 / n_methods)

    # Set up position based on jitter parameter
    if (jitter) {
      point_position <- position_jitterdodge(jitter.width = 0.1, dodge.width = dodge_width, seed = 123)
      error_position <- position_dodge(width = dodge_width)
    } else {
      point_position <- position_dodge(width = dodge_width)
      error_position <- position_dodge(width = dodge_width)
    }

    # Build geom_point based on size_scale
    if (is.null(size_scale)) {
      p <- ggplot(plot_df, aes(as.factor(tree), value, colour = method)) +
        geom_point(position = point_position, size = point_size)
    } else if (size_scale == "Height") {
      p <- ggplot(plot_df, aes(as.factor(tree), value, colour = method, size = Height)) +
        geom_point(position = point_position)
    } else {
      p <- ggplot(plot_df, aes(as.factor(tree), value, colour = method, size = DBH)) +
        geom_point(position = point_position)
    }

    p <- p +
      scale_color_manual(values = method_colors) +
      scale_x_discrete(name = "Tree Index") +
      labs(y = y_label, title = paste("Above Ground", returnv, "Estimates Across Allometric Methods by Tree Index"))
    p <- add_error_bars(p, plot_df, show_errors,
                        position = error_position,
                        width = 0.2)
  } else if (plot_type == "density") {
    # Density plot showing distribution of estimates for each method
    p <- ggplot(plot_df, aes(value, colour = method, fill = method)) +
      geom_density(alpha = 0.3) +
      scale_color_manual(values = method_colors) +
      scale_fill_manual(values = method_colors) +
      labs(x = y_label, y = "Density",
           title = paste("Distribution of Above Ground", returnv, "Estimates by Method"))
  } else if (plot_type == "residual") {
    # Residual plot showing differences between methods
    # Calculate mean value across all methods for each tree as reference
    if (length(unique(plot_df$tree)) > 1) {
      # For multiple trees, calculate mean per tree
      tree_means <- aggregate(value ~ tree, plot_df, mean, na.rm = TRUE)
      names(tree_means)[2] <- "mean_value"
      plot_df <- merge(plot_df, tree_means, by = "tree", all.x = TRUE)
      plot_df$residual <- plot_df$value - plot_df$mean_value
      x_var <- "mean_value"
      x_label <- paste("Mean", y_label, "across methods")
    } else {
      # For single tree, use method mean as reference
      method_mean <- mean(plot_df$value, na.rm = TRUE)
      plot_df$residual <- plot_df$value - method_mean
      plot_df$mean_value <- method_mean
      x_var <- "method"
      x_label <- "Method"
    }

    if (x_var == "mean_value") {
      # Scatter plot of residuals vs mean value
      # Build plot based on size_scale - must not pass size=NULL when using size aesthetic
      if (is.null(size_scale)) {
        p <- ggplot(plot_df, aes(x = .data[["mean_value"]], y = .data[["residual"]], colour = method)) +
          geom_point(size = point_size)
      } else if (size_scale == "Height") {
        p <- ggplot(plot_df, aes(x = .data[["mean_value"]], y = .data[["residual"]], colour = method, size = Height)) +
          geom_point()
      } else {
        p <- ggplot(plot_df, aes(x = .data[["mean_value"]], y = .data[["residual"]], colour = method, size = DBH)) +
          geom_point()
      }
      p <- p +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        scale_color_manual(values = method_colors) +
        labs(x = x_label, y = paste("Residual (", y_label, ")"),
             title = paste("Residuals from Mean Above Ground", returnv, "by Method")) +
        theme_minimal(base_size = font_size)
    } else {
      # Bar plot of residuals for single tree
      p <- ggplot(plot_df, aes(x = method, y = .data[["residual"]], fill = method)) +
        geom_col(width = 0.6, position = position_dodge()) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        scale_fill_manual(values = method_colors) +
        labs(x = x_label, y = paste("Residual (", y_label, ")"),
             title = paste("Residuals from Mean Above Ground", returnv, "by Method")) +
        ggplot2::theme(legend.position = "none")
    }
  } else {
    return(NULL)
  }

  # Apply axis limits if specified
  x_limits <- NULL
  if ((!is.null(x_min) && !is.na(x_min)) || (!is.null(x_max) && !is.na(x_max))) {
    x_limits <- c(if (!is.null(x_min) && !is.na(x_min)) x_min else -Inf,
                  if (!is.null(x_max) && !is.na(x_max)) x_max else Inf)
    p <- p + xlim(x_limits)
  }

  y_limits <- NULL
  if ((!is.null(y_min) && !is.na(y_min)) || (!is.null(y_max) && !is.na(y_max))) {
    y_limits <- c(if (!is.null(y_min) && !is.na(y_min)) y_min else -Inf,
                  if (!is.null(y_max) && !is.na(y_max)) y_max else Inf)
    p <- p + ylim(y_limits)
  }

  # Apply log scale if requested (only for scatter plots, not bar/boxplot/density/residual)
  # Bar plots, boxplots, density, and residual plots should always use linear scale
  if (log_scale && plot_type != "bar" && plot_type != "boxplot" && plot_type != "tree_index" && plot_type != "density" && plot_type != "residual") {
    if (!interactive) {
      p <- p + scale_y_log10()
    }
  }

  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p_plotly <- plotly::ggplotly(p, tooltip = "text")

    # Helper function to get species name from plot_df
    get_species_name <- function(method_name, tree_idx = NULL, x_val = NULL, y_val = NULL) {
      if (!"genus" %in% colnames(plot_df) || !"species" %in% colnames(plot_df)) {
        return(NULL)
      }

      # Try to match based on method and optionally tree/x/y values
      if (!is.null(tree_idx) && tree_idx %in% plot_df$tree) {
        # For tree_index plots
        matching_rows <- plot_df[plot_df$tree == tree_idx & plot_df$method == method_name, ]
      } else if (!is.null(x_val) && !is.null(y_val)) {
        # For scatter plots, try to match by method and approximate values
        matching_rows <- plot_df[plot_df$method == method_name, ]
        if (nrow(matching_rows) > 0) {
          # Get first matching row
          matching_rows <- matching_rows[1, ]
        }
      } else {
        # For bar/density plots, just match by method
        matching_rows <- plot_df[plot_df$method == method_name, ]
        if (nrow(matching_rows) > 0) {
          matching_rows <- matching_rows[1, ]
        }
      }

      if (nrow(matching_rows) > 0 && !is.na(matching_rows$genus[1]) && !is.na(matching_rows$species[1])) {
        return(paste0(matching_rows$genus[1], " ", matching_rows$species[1]))
      }
      return(NULL)
    }

    # Helper function to create consistent hover text
    # Format: Species (if available), Method, Value, and optionally additional fields
    create_hover_text <- function(species_name = NULL, method_name = "", value = NULL,
                                  value_label = "", additional_fields = NULL) {
      parts <- character()

      # Add species if available
      if (!is.null(species_name) && !is.na(species_name) && species_name != "") {
        parts <- c(parts, paste0("Species: ", species_name))
      }

      # Add method
      if (!is.null(method_name) && method_name != "") {
        parts <- c(parts, paste0("Method: ", method_name))
      }

      # Add value
      if (!is.null(value) && !is.na(value)) {
        parts <- c(parts, paste0("Value: ", sprintf("%.3f", value), " ", value_label))
      }

      # Add additional fields (list of character vectors)
      if (!is.null(additional_fields)) {
        parts <- c(parts, additional_fields)
      }

      paste(parts, collapse = "<br>")
    }

    # Customize hover text to use sprintf for all numeric values
    # Format: Value with 3 dp, DBH/Height with 2 dp
    for (i in seq_along(p_plotly$x$data)) {
      if (!is.null(p_plotly$x$data[[i]]$y) && length(p_plotly$x$data[[i]]$y) > 0) {
        # Get original text if it exists
        original_text <- p_plotly$x$data[[i]]$text
        method_name <- ifelse(is.null(p_plotly$x$data[[i]]$name), "", p_plotly$x$data[[i]]$name)

        # Format numeric values in hover text using sprintf
        if (!is.null(original_text) && length(original_text) > 0) {
          # Format each text element - need to distinguish between Value, DBH, and Height
          formatted_text <- sapply(seq_along(original_text), function(idx) {
            txt <- original_text[idx]
            if (any(is.na(txt)) || txt == "") return(txt)

            # Check if this is a value (y-axis) or DBH/Height (x-axis)
            # For bar plots, we only have y values
            # For scatter plots, we have both x and y

            # Extract value (y) - always 3 dp
            y_val <- if (!is.null(p_plotly$x$data[[i]]$y) && length(p_plotly$x$data[[i]]$y) >= idx) {
              p_plotly$x$data[[i]]$y[idx]
            } else NULL

            # Extract x value (DBH or Height) - 2 dp
            x_val <- if (!is.null(p_plotly$x$data[[i]]$x) && length(p_plotly$x$data[[i]]$x) >= idx) {
              p_plotly$x$data[[i]]$x[idx]
            } else NULL

            # Get species name
            species_name <- get_species_name(method_name, tree_idx = x_val, x_val = x_val, y_val = y_val)

            # Build formatted text based on plot type using helper function
            if (plot_type == "dbh" && !is.null(x_val) && !is.null(y_val)) {
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label),
                additional_fields = c(paste0("DBH: ", sprintf("%.2f", x_val), " cm"))
              )
            } else if (plot_type == "height" && !is.null(x_val) && !is.null(y_val)) {
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label),
                additional_fields = c(paste0("Height: ", sprintf("%.2f", x_val), " m"))
              )
            } else if (plot_type == "tree_index" && !is.null(y_val)) {
              # For tree_index, we need to get DBH from the data if available
              tree_idx <- if (!is.null(x_val)) x_val else idx
              species_name <- get_species_name(method_name, tree_idx = tree_idx)

              dbh_val <- if (tree_idx %in% plot_df$tree && length(plot_df$DBH[plot_df$tree == tree_idx]) > 0) {
                plot_df$DBH[plot_df$tree == tree_idx][1]
              } else NULL
              height_val <- if (tree_idx %in% plot_df$tree && length(plot_df$Height[plot_df$tree == tree_idx]) > 0) {
                plot_df$Height[plot_df$tree == tree_idx][1]
              } else NULL

              additional_fields <- c(
                paste0("Tree: ", tree_idx),
                if (!is.null(dbh_val)) paste0("DBH: ", sprintf("%.2f", dbh_val), " cm") else NULL,
                if (!is.null(height_val)) paste0("Height: ", sprintf("%.2f", height_val), " m") else NULL
              )

              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label),
                additional_fields = additional_fields
              )
            } else if (plot_type == "bar" && !is.null(y_val)) {
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label)
              )
            } else if (plot_type == "density" && !is.null(y_val)) {
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label)
              )
            } else if (plot_type == "residual" && !is.null(y_val)) {
              additional_fields <- if (!is.null(x_val)) {
                c(paste0("Mean Value: ", sprintf("%.3f", x_val), " ", tolower(y_label)),
                  paste0("Residual: ", sprintf("%.3f", y_val), " ", tolower(y_label)))
              } else {
                c(paste0("Residual: ", sprintf("%.3f", y_val), " ", tolower(y_label)))
              }
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                additional_fields = additional_fields
              )
            } else {
              # Fallback: try to parse and format existing text
              txt
            }
          })
          p_plotly$x$data[[i]]$text <- formatted_text
        } else {
          # Create hover text if it doesn't exist using helper function
          method_name <- ifelse(is.null(p_plotly$x$data[[i]]$name), "", p_plotly$x$data[[i]]$name)

          if (plot_type == "dbh" && !is.null(p_plotly$x$data[[i]]$x) && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name, x_val = p_plotly$x$data[[i]]$x[1], y_val = p_plotly$x$data[[i]]$y[1])
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              value = p_plotly$x$data[[i]]$y[1],
              value_label = tolower(y_label),
              additional_fields = c(paste0("DBH: ", sprintf("%.2f", p_plotly$x$data[[i]]$x[1]), " cm"))
            )
          } else if (plot_type == "height" && !is.null(p_plotly$x$data[[i]]$x) && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name, x_val = p_plotly$x$data[[i]]$x[1], y_val = p_plotly$x$data[[i]]$y[1])
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              value = p_plotly$x$data[[i]]$y[1],
              value_label = tolower(y_label),
              additional_fields = c(paste0("Height: ", sprintf("%.2f", p_plotly$x$data[[i]]$x[1]), " m"))
            )
          } else if (plot_type == "tree_index" && !is.null(p_plotly$x$data[[i]]$y)) {
            # For tree_index, get DBH and Height from plot_df
            tree_vals <- unique(plot_df$tree)
            p_plotly$x$data[[i]]$text <- sapply(seq_along(p_plotly$x$data[[i]]$y), function(idx) {
              tree_idx <- if (!is.null(p_plotly$x$data[[i]]$x) && length(p_plotly$x$data[[i]]$x) >= idx) {
                p_plotly$x$data[[i]]$x[idx]
              } else if (idx <= length(tree_vals)) tree_vals[idx] else idx

              species_name <- get_species_name(method_name, tree_idx = tree_idx)

              dbh_val <- if (tree_idx %in% plot_df$tree && length(plot_df$DBH[plot_df$tree == tree_idx]) > 0) {
                plot_df$DBH[plot_df$tree == tree_idx][1]
              } else NULL
              height_val <- if (tree_idx %in% plot_df$tree && length(plot_df$Height[plot_df$tree == tree_idx]) > 0) {
                plot_df$Height[plot_df$tree == tree_idx][1]
              } else NULL

              additional_fields <- c(
                paste0("Tree: ", tree_idx),
                if (!is.null(dbh_val)) paste0("DBH: ", sprintf("%.2f", dbh_val), " cm") else NULL,
                if (!is.null(height_val)) paste0("Height: ", sprintf("%.2f", height_val), " m") else NULL
              )

              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = p_plotly$x$data[[i]]$y[idx],
                value_label = tolower(y_label),
                additional_fields = additional_fields
              )
            })
          } else if (plot_type == "density" && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name)
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              value = p_plotly$x$data[[i]]$y[1],
              value_label = tolower(y_label)
            )
          } else if (plot_type == "residual" && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name)
            additional_fields <- if (!is.null(p_plotly$x$data[[i]]$x)) {
              c(paste0("Mean Value: ", sprintf("%.3f", p_plotly$x$data[[i]]$x[1]), " ", tolower(y_label)),
                paste0("Residual: ", sprintf("%.3f", p_plotly$x$data[[i]]$y[1]), " ", tolower(y_label)))
            } else {
              c(paste0("Residual: ", sprintf("%.3f", p_plotly$x$data[[i]]$y[1]), " ", tolower(y_label)))
            }
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              additional_fields = additional_fields
            )
          } else if (plot_type == "bar" && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name)
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              value = p_plotly$x$data[[i]]$y[1],
              value_label = tolower(y_label)
            )
          }
        }
      }
    }

    # Build axis configurations
    xaxis_config <- list()
    yaxis_config <- list()

    # Format hover text: x-axis (DBH/Height) with 2 dp, y-axis (Value) with 3 dp
    xaxis_config$hoverformat <- ".2f"
    yaxis_config$hoverformat <- ".3f"

    # Only apply log scale to scatter plots, not bar plots, tree_index, density, or residual
    if (log_scale && plot_type != "bar" && plot_type != "tree_index" && plot_type != "density" && plot_type != "residual") {
      yaxis_config$type <- "log"
    }

    # Apply axis limits for plotly (if not already applied via xlim/ylim)
    if (!is.null(x_limits)) {
      xaxis_config$range <- x_limits
    }
    if (!is.null(y_limits)) {
      yaxis_config$range <- y_limits
    }

    # Apply all axis configurations
    p_plotly <- plotly::layout(p_plotly, xaxis = xaxis_config, yaxis = yaxis_config)

    return(p_plotly)
  } else {
    # Apply theme if specified
    if (!is.null(theme)) {
      p <- p + theme(base_size = font_size)
    } else {
      p <- p + ggplot2::theme_minimal(base_size = font_size)
    }
    return(p)
  }
}


############# Method Comparison Functions ################
#'
#' @title Calculate comparison statistics for allometry methods
#' @description Calculates summary statistics comparing different allometric methods
#' including mean differences, coefficient of variation, and method agreement metrics
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param results_df Data frame with results from allometries function
#' @param returnv Either "AGC" for carbon or "AGB" for biomass
#' @return List with comparison statistics
#' @examples
#' results <- allometries(c("Quercus","Quercus"), c("robur","robur"), c(20, 30), c(10, 15))
#' compare_methods(results, "AGC")
#' @export
#'
compare_methods <- function(results_df, returnv = "AGC") {
  if (returnv == "AGC") {
    method_cols <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")
    method_names <- c("WCC", "Biomass", "Allodb", "Bunce")
  } else {
    method_cols <- c("WCC_B_t", "biomass_B_t", "allodb_B_t", "Bunce_B_t")
    method_names <- c("WCC", "Biomass", "Allodb", "Bunce")
  }

  # Extract method values
  method_data <- results_df[, intersect(method_cols, colnames(results_df)), drop = FALSE]
  if (ncol(method_data) == 0) {
    return(list(summary_stats = data.frame(Message = "No method data available")))
  }
  colnames(method_data) <- method_names[method_cols %in% colnames(results_df)]

  # Remove rows with all NAs
  valid_rows <- rowSums(!is.na(method_data)) > 0
  method_data <- method_data[valid_rows, , drop = FALSE]

  if (nrow(method_data) == 0) {
    return(list(summary_stats = data.frame(Statistic = "No valid data", Value = NA)))
  }

  # Calculate statistics
  stats_list <- list()

  # Mean values per method
  means <- colMeans(method_data, na.rm = TRUE)
  stats_list$mean <- data.frame(
    Method = names(means),
    Value = means,
    Statistic = "Mean"
  )

  # Standard deviation per method
  sds <- apply(method_data, 2, function(x) sd(x, na.rm = TRUE))
  stats_list$sd <- data.frame(
    Method = names(sds),
    Value = sds,
    Statistic = "SD"
  )

  # Coefficient of variation
  cv <- (sds / means) * 100
  stats_list$cv <- data.frame(
    Method = names(cv),
    Value = cv,
    Statistic = "CV (%)"
  )

  # Mean difference matrix
  n_methods <- length(colnames(method_data))
  method_names_actual <- colnames(method_data)
  diff_matrix <- matrix(NA, nrow = n_methods, ncol = n_methods)
  rownames(diff_matrix) <- colnames(diff_matrix) <- method_names_actual

  for (i in 1:n_methods) {
    for (j in 1:n_methods) {
      if (i != j) {
        diff_vals <- method_data[, i] - method_data[, j]
        diff_matrix[i, j] <- mean(diff_vals, na.rm = TRUE)
      }
    }
  }

  # Correlation matrix
  cor_matrix <- cor(method_data, use = "pairwise.complete.obs")

  # Method agreement (mean absolute difference)
  agreement <- data.frame(
    Method1 = character(),
    Method2 = character(),
    Mean_Abs_Diff = numeric(),
    Correlation = numeric()
  )

  for (i in 1:(n_methods - 1)) {
    for (j in (i + 1):n_methods) {
      diff_vals <- abs(method_data[, i] - method_data[, j])
      agreement <- rbind(agreement, data.frame(
        Method1 = method_names_actual[i],
        Method2 = method_names_actual[j],
        Mean_Abs_Diff = mean(diff_vals, na.rm = TRUE),
        Correlation = cor_matrix[i, j]
      ))
    }
  }

  # Round all statistics to 4 decimal places
  stats_list$mean$Value <- round(stats_list$mean$Value, 4)
  stats_list$sd$Value <- round(stats_list$sd$Value, 4)
  stats_list$cv$Value <- round(stats_list$cv$Value, 4)
  diff_matrix <- round(diff_matrix, 4)
  agreement$Mean_Abs_Diff <- round(agreement$Mean_Abs_Diff, 4)
  agreement$Correlation <- round(agreement$Correlation, 4)

  return(list(
    summary_stats = rbind(stats_list$mean, stats_list$sd, stats_list$cv),
    mean_differences = diff_matrix,
    correlation_matrix = cor_matrix,  # Note: correlation often = 1 for single tree, less useful
    method_agreement = agreement
  ))
}


#'
#' @title Comprehensive comparison of allometric methods
#' @description Compares different allometric methods (WCC, BIOMASS, allodb, Bunce)
#' providing a detailed side-by-side comparison table with estimates, uncertainty,
#' percent differences, extrapolation flags, and method assumptions.
#'
#' @author Isabel Openshaw. I.Openshaw@kew.org
#'
#' @param df Data frame with results from the `allometries()` function, containing
#'   columns for each method's estimates and uncertainties (e.g., WCC_C_t, WCC_C_sig,
#'   biomass_C_t, etc.).
#' @param methods Character vector specifying which methods to compare.
#'   Options: "WCC", "BIOMASS", "allodb", "Bunce". Default is all four methods.
#' @param reference Character string specifying the reference method for computing
#'   percent differences. Default is "WCC".
#' @param returnv Character string, either "AGC" for above-ground carbon or "AGB"
#'   for above-ground biomass. Default is "AGC".
#' @param ci_level Numeric value between 0 and 1 specifying the confidence interval
#'   level. Default is 0.95 (95% CI).
#' @param aggregate Logical. If TRUE (default), returns aggregated totals across
#'   all trees. If FALSE, returns per-tree comparison.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{method}{Name of the allometric method}
#'   \item{agb_t}{Above-ground biomass in tonnes (or carbon if returnv="AGC")}
#'   \item{co2e_t}{CO2 equivalent in tonnes}
#'   \item{ci_low}{Lower bound of confidence interval}
#'   \item{ci_high}{Upper bound of confidence interval}
#'   \item{pct_diff_vs_ref}{Percent difference compared to reference method}
#'   \item{abs_diff_vs_ref}{Absolute difference compared to reference method}
#'   \item{cv_pct}{Coefficient of variation as percentage}
#'   \item{extrapolation}{Logical flag indicating if extrapolation may have occurred}
#'   \item{ci_overlap}{Logical flag indicating if CI overlaps with reference method}
#'   \item{assumptions_summary}{Human-readable summary of method assumptions}
#'   \item{rank}{Ranking from lowest to highest estimate}
#' }
#'
#' Additionally, attributes are attached with summary statistics:
#' \describe{
#'   \item{cv_across_methods}{Coefficient of variation across all methods}
#'   \item{range_min}{Minimum estimate across methods}
#'   \item{range_max}{Maximum estimate across methods}
#'   \item{spread}{Difference between max and min estimates}
#'   \item{agreement_summary}{Summary of method agreement/disagreement}
#' }
#'
#' @details
#' **Method Assumptions:**
#' - **WCC (Woodland Carbon Code)**: UK-specific tariff-based allometry; assumes UK
#'   species parameters; best for UK temperate woodlands.
#' - **BIOMASS**: Pantropical Chave et al. equations; uses wood density lookup;
#'   designed for tropical forests but can estimate temperate.
#' - **allodb**: Multi-source allometric database; region-weighted equations;
#'   extratropical focus; may lack coverage for some species.
#' - **Bunce**: Historic UK deciduous woodland equation; simple DBH-based model;
#'   limited to specific UK broadleaf species.
#'
#' **Extrapolation Flags:**
#' Extrapolation is flagged when:
#' - DBH or height values fall outside the calibration range of the method
#' - Species/genus is not found in the method's reference database
#' - Geographic region differs significantly from the method's development region
#'
#' @references
#' Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment
#'   Protocol (v2.0)." (2018).
#' Rejou-Mechain, M., et al. (2017). BIOMASS: an R package for estimating
#'   above-ground biomass. Methods in Ecology and Evolution.
#' Gonzalez-Akre, E., et al. (2020). allodb: An R package for biomass
#'   estimation at globally distributed extratropical forest plots.
#' Bunce, R.G.H. (1968). Biomass and Production of Trees in a Mixed
#'   Deciduous Woodland.
#'
#' @examples
#' # Run allometries for sample trees
#' results <- allometries(genus = c("Quercus", "Fagus", "Pinus"),
#'   species = c("robur", "sylvatica", "sylvestris"),
#'   dbh = c(45, 38, 52),
#'   height = c(18, 15, 22)
#' )
#'
#' # Compare all methods with WCC as reference
#' comparison <- compare_allometries(results)
#' print(comparison)
#'
#' # Save comparison table to CSV
#' # write.csv(comparison, "comparison_output.csv", row.names = FALSE)
#'
#' # Save as RDS to preserve attributes
#' # saveRDS(comparison, "comparison_output.rds")
#'
#' # Compare only WCC and BIOMASS methods
#' comparison_subset <- compare_allometries(results, methods = c("WCC", "BIOMASS"))
#'
#' # Per-tree comparison
#' per_tree <- compare_allometries(results, aggregate = FALSE)
#'
#' @export
#'
compare_allometries <- function(df,
                                methods = c("WCC", "BIOMASS", "allodb", "Bunce"),
                                reference = "WCC",
                                returnv = "AGC",
                                ci_level = 0.95,
                                aggregate = TRUE) {

  # ==== Input Validation ====
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame from the allometries() function")
  }

  valid_methods <- c("WCC", "BIOMASS", "allodb", "Bunce")
  methods <- match.arg(methods, valid_methods, several.ok = TRUE)

  if (!reference %in% methods) {
    stop("'reference' must be one of the selected methods: ",
         paste(methods, collapse = ", "))
  }

  if (!returnv %in% c("AGC", "AGB")) {
    stop("'returnv' must be either 'AGC' or 'AGB'")
  }

  if (!is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("'ci_level' must be a numeric value between 0 and 1")
  }

  # ==== Define Column Mappings ====
  if (returnv == "AGC") {
    method_cols <- c(
      "WCC" = "WCC_C_t",
      "BIOMASS" = "biomass_C_t",
      "allodb" = "allodb_C_t",
      "Bunce" = "Bunce_C_t"
    )
    sig_cols <- c(
      "WCC" = "WCC_C_sig",
      "BIOMASS" = "biomass_C_sig",
      "allodb" = "allodb_C_sig",
      "Bunce" = "Bunce_C_sig"
    )
  } else {
    method_cols <- c(
      "WCC" = "WCC_B_t",
      "BIOMASS" = "biomass_B_t",
      "allodb" = "allodb_B_t",
      "Bunce" = "Bunce_B_t"
    )
    sig_cols <- c(
      "WCC" = "WCC_B_sig",
      "BIOMASS" = "biomass_B_sig",
      "allodb" = "allodb_B_sig",
      "Bunce" = "Bunce_B_sig"
    )
  }

  # Filter to selected methods
  method_cols <- method_cols[methods]
  sig_cols <- sig_cols[methods]

  # Check which columns exist
  available_methods <- names(method_cols)[method_cols %in% colnames(df)]
  if (length(available_methods) == 0) {
    stop("No matching method columns found in 'df'. Expected columns like: ",
         paste(method_cols, collapse = ", "))
  }

  missing_methods <- setdiff(methods, available_methods)
  if (length(missing_methods) > 0) {
    warning("Methods not found in data and will be skipped: ",
            paste(missing_methods, collapse = ", "))
  }

  methods <- available_methods
  method_cols <- method_cols[methods]
  sig_cols <- sig_cols[methods]

  # ==== Get Method Metadata ====
  all_metadata <- get_method_metadata("all")

  # ==== Validate Inputs for Each Method ====
  dbh_vals <- if ("dbh" %in% colnames(df)) df$dbh else NULL
  height_vals <- if ("height" %in% colnames(df)) df$height else NULL
  genus_vals <- if ("genus" %in% colnames(df)) df$genus else NULL
  species_vals <- if ("species" %in% colnames(df)) df$species else NULL
  type_vals <- if ("type" %in% colnames(df)) df$type else NULL

  # ==== Compute Per-Tree or Aggregated Results ====
  z_score <- qnorm(1 - (1 - ci_level) / 2)

  if (aggregate) {
    # Aggregate across all trees
    result_list <- list()

    for (m in methods) {
      col <- method_cols[m]
      sig_col <- sig_cols[m]
      meta <- all_metadata[[m]]

      # Validate inputs for this method
      validation <- validate_allometry_inputs(m, dbh_vals, height_vals,
                                               genus_vals, species_vals, type_vals)

      # Sum values and propagate uncertainty (sqrt of sum of squared sigmas)
      values <- df[[col]]
      total <- sum(values, na.rm = TRUE)

      if (sig_col %in% colnames(df)) {
        sigmas <- df[[sig_col]]
        sigma_total <- sqrt(sum(sigmas^2, na.rm = TRUE))
      } else {
        sigma_total <- NA
      }

      # CO2 equivalent (carbon * 44/12)
      co2e <- ifelse(returnv == "AGC", total * (44 / 12), total * 0.5 * (44 / 12))

      # Confidence intervals
      ci_low <- ifelse(!is.na(sigma_total), total - z_score * sigma_total, NA)
      ci_high <- ifelse(!is.na(sigma_total), total + z_score * sigma_total, NA)

      # CV within method
      cv_pct <- ifelse(!is.na(sigma_total) && total > 0,
                       (sigma_total / total) * 100, NA)

      # Flag if many NAs (possible species mismatch)
      na_ratio <- sum(is.na(values)) / length(values)
      defaults_flag <- if (na_ratio > 0.2) "High NA ratio - species may not be covered" else ""

      # Combine defaults used
      defaults_combined <- paste(c(validation$defaults_used,
                                   if (defaults_flag != "") defaults_flag else NULL),
                                 collapse = "; ")
      if (defaults_combined == "") defaults_combined <- "None"

      # Combine validity warnings
      warnings_combined <- paste(validation$validity_warnings, collapse = "; ")
      if (warnings_combined == "") warnings_combined <- "None"

      result_list[[m]] <- data.frame(
        method = m,
        value = total,
        co2e_t = co2e,
        ci_low = ci_low,
        ci_high = ci_high,
        sigma = sigma_total,
        cv_pct = cv_pct,
        pct_diff_vs_ref = NA_real_,
        abs_diff_vs_ref = NA_real_,
        citation = meta$citation_short,
        assumptions = paste(meta$assumptions[1:2], collapse = "; "),
        region = meta$region,
        source = meta$source_type,
        height_required = meta$height_required,
        extrapolation = validation$extrapolation || (na_ratio > 0.2),
        defaults_used = defaults_combined,
        validity_warning = warnings_combined,
        ci_overlap = NA,
        stringsAsFactors = FALSE
      )
    }

    result <- do.call(rbind, result_list)

    # Compute differences vs reference
    ref_idx <- which(result$method == reference)
    if (length(ref_idx) == 1) {
      ref_value <- result$value[ref_idx]
      ref_ci_low <- result$ci_low[ref_idx]
      ref_ci_high <- result$ci_high[ref_idx]

      for (i in seq_len(nrow(result))) {
        if (ref_value != 0 && !is.na(ref_value)) {
          result$pct_diff_vs_ref[i] <- ((result$value[i] - ref_value) / ref_value) * 100
        }
        result$abs_diff_vs_ref[i] <- result$value[i] - ref_value

        # Check CI overlap
        if (!is.na(result$ci_low[i]) && !is.na(ref_ci_low)) {
          result$ci_overlap[i] <- !(result$ci_high[i] < ref_ci_low ||
                                      result$ci_low[i] > ref_ci_high)
        }
      }
    }

  } else {
    # Per-tree comparison
    n_trees <- nrow(df)
    result_list <- list()

    for (tree_idx in seq_len(n_trees)) {
      # Build results for this tree
      tree_rows <- list()

      # Get single-tree values for validation
      tree_dbh <- if ("dbh" %in% colnames(df)) df$dbh[tree_idx] else NULL
      tree_height <- if ("height" %in% colnames(df)) df$height[tree_idx] else NULL
      tree_genus <- if ("genus" %in% colnames(df)) df$genus[tree_idx] else NULL
      tree_species <- if ("species" %in% colnames(df)) df$species[tree_idx] else NULL
      tree_type <- if ("type" %in% colnames(df)) df$type[tree_idx] else NULL

      for (m in methods) {
        col <- method_cols[m]
        sig_col <- sig_cols[m]
        meta <- all_metadata[[m]]

        # Validate inputs for this method and tree
        validation <- validate_allometry_inputs(m, tree_dbh, tree_height,
                                                 tree_genus, tree_species, tree_type)

        value <- df[[col]][tree_idx]
        sigma <- if (sig_col %in% colnames(df)) df[[sig_col]][tree_idx] else NA

        co2e <- ifelse(returnv == "AGC", value * (44 / 12), value * 0.5 * (44 / 12))
        ci_low <- ifelse(!is.na(sigma), value - z_score * sigma, NA)
        ci_high <- ifelse(!is.na(sigma), value + z_score * sigma, NA)
        cv_pct <- ifelse(!is.na(sigma) && !is.na(value) && value > 0, (sigma / value) * 100, NA)

        # Combine extrapolation flag
        extrapolation <- validation$extrapolation || is.na(value)

        # Combine defaults and warnings
        defaults_combined <- paste(validation$defaults_used, collapse = "; ")
        if (defaults_combined == "") defaults_combined <- "None"
        warnings_combined <- paste(validation$validity_warnings, collapse = "; ")
        if (warnings_combined == "") warnings_combined <- "None"

        tree_rows[[m]] <- data.frame(
          tree_id = tree_idx,
          method = m,
          value = value,
          co2e_t = co2e,
          ci_low = ci_low,
          ci_high = ci_high,
          sigma = sigma,
          cv_pct = cv_pct,
          pct_diff_vs_ref = NA_real_,
          abs_diff_vs_ref = NA_real_,
          citation = meta$citation_short,
          assumptions = paste(meta$assumptions[1:2], collapse = "; "),
          region = meta$region,
          source = meta$source_type,
          height_required = meta$height_required,
          extrapolation = extrapolation,
          defaults_used = defaults_combined,
          validity_warning = warnings_combined,
          ci_overlap = NA,
          stringsAsFactors = FALSE
        )
      }

      # Combine rows for this tree
      tree_result <- do.call(rbind, tree_rows)

      # Compute differences vs reference for this tree
      ref_idx <- which(tree_result$method == reference)
      if (length(ref_idx) == 1) {
        ref_value <- tree_result$value[ref_idx]
        ref_ci_low <- tree_result$ci_low[ref_idx]
        ref_ci_high <- tree_result$ci_high[ref_idx]

        for (i in seq_len(nrow(tree_result))) {
          if (!is.na(ref_value) && ref_value != 0) {
            tree_result$pct_diff_vs_ref[i] <- ((tree_result$value[i] - ref_value) / ref_value) * 100
          }
          tree_result$abs_diff_vs_ref[i] <- tree_result$value[i] - ref_value

          if (!is.na(tree_result$ci_low[i]) && !is.na(ref_ci_low)) {
            tree_result$ci_overlap[i] <- !(tree_result$ci_high[i] < ref_ci_low ||
                                             tree_result$ci_low[i] > ref_ci_high)
          }
        }
      }

      result_list[[tree_idx]] <- tree_result
    }

    result <- do.call(rbind, result_list)
  }

  # ==== Add Ranking ====
  if (aggregate) {
    result$rank <- rank(result$value, ties.method = "min", na.last = "keep")
  } else {
    # Rank within each tree
    result$rank <- ave(result$value, result$tree_id,
                       FUN = function(x) rank(x, ties.method = "min", na.last = "keep"))
  }

  # ==== Compute Summary Attributes ====
  if (aggregate) {
    values <- result$value
  } else {
    # Aggregate for summary stats
    values <- tapply(result$value, result$method, sum, na.rm = TRUE)
  }

  valid_values <- values[!is.na(values)]
  if (length(valid_values) > 1) {
    cv_across <- (sd(valid_values) / mean(valid_values)) * 100
    range_min <- min(valid_values)
    range_max <- max(valid_values)
    spread <- range_max - range_min

    # Agreement summary
    n_overlap <- if (aggregate) sum(result$ci_overlap, na.rm = TRUE) else NA
    n_methods <- length(valid_values)

    if (!is.na(n_overlap)) {
      if (n_overlap == n_methods) {
        agreement <- "All methods show overlapping uncertainty intervals"
      } else if (n_overlap >= n_methods / 2) {
        agreement <- "Partial agreement: majority of CIs overlap with reference"
      } else {
        agreement <- "Disagreement: most CIs do not overlap with reference"
      }
    } else {
      agreement <- "Unable to assess CI overlap"
    }
  } else {
    cv_across <- NA
    range_min <- NA
    range_max <- NA
    spread <- NA
    agreement <- "Insufficient methods for comparison"
  }

  # Attach summary as attributes
  attr(result, "cv_across_methods") <- round(cv_across, 2)
  attr(result, "range_min") <- round(range_min, 4)
  attr(result, "range_max") <- round(range_max, 4)
  attr(result, "spread") <- round(spread, 4)
  attr(result, "agreement_summary") <- agreement
  attr(result, "reference_method") <- reference
  attr(result, "return_type") <- returnv
  attr(result, "ci_level") <- ci_level

  # Round numeric columns
  numeric_cols <- c("value", "co2e_t", "ci_low", "ci_high", "sigma",
                    "pct_diff_vs_ref", "abs_diff_vs_ref", "cv_pct")
  for (col in numeric_cols) {
    if (col %in% colnames(result)) {
      result[[col]] <- round(result[[col]], 4)
    }
  }

  class(result) <- c("allometry_comparison", "data.frame")

  return(result)
}


#' @title Print method for allometry comparison
#' @description Custom print method for compare_allometries output
#' @param x An allometry_comparison object
#' @param ... Additional arguments (unused)
#' @export
print.allometry_comparison <- function(x, ...) {

  cat("\n=== Allometric Method Comparison ===\n\n")

  ref <- attr(x, "reference_method")
  ret <- attr(x, "return_type")
  ci <- attr(x, "ci_level")

  cat(sprintf("Reference method: %s\n", ref))
  cat(sprintf("Return type: %s (%s)\n", ret, ifelse(ret == "AGC", "Carbon (t)", "Biomass (t)")))
  cat(sprintf("Confidence level: %.0f%%\n\n", ci * 100))

  # Select key columns for display
  display_cols <- c("method", "value", "co2e_t", "ci_low", "ci_high",
                    "pct_diff_vs_ref", "extrapolation", "source", "rank")
  if ("tree_id" %in% names(x)) {
    display_cols <- c("tree_id", display_cols)
  }
  display_cols <- display_cols[display_cols %in% names(x)]

  print.data.frame(x[, display_cols], row.names = FALSE)

  cat("\n--- Summary Statistics ---\n")
  cat(sprintf("CV across methods: %.2f%%\n", attr(x, "cv_across_methods")))
  cat(sprintf("Range: %.4f to %.4f (spread: %.4f)\n",
              attr(x, "range_min"), attr(x, "range_max"), attr(x, "spread")))
  cat(sprintf("Agreement: %s\n", attr(x, "agreement_summary")))

  cat("\n--- Citations ---\n")
  unique_methods <- unique(x$method)
  for (m in unique_methods) {
    citation <- x$citation[x$method == m][1]
    cat(sprintf("  %s: %s\n", m, citation))
  }

  cat("\n--- Validity Warnings ---\n")
  for (m in unique_methods) {
    warning_msg <- x$validity_warning[x$method == m][1]
    if (warning_msg != "None") {
      cat(sprintf("  %s: %s\n", m, warning_msg))
    }
  }
  if (all(x$validity_warning == "None")) {
    cat("  None - all inputs within valid ranges\n")
  }

  invisible(x)
}


#' @title Summary method for allometry comparison
#' @description Provides a detailed summary of the comparison results
#' @param object An allometry_comparison object
#' @param ... Additional arguments (unused)
#' @export
summary.allometry_comparison <- function(object, ...) {

  cat("\n====== ALLOMETRIC COMPARISON SUMMARY ======\n\n")

  ref <- attr(object, "reference_method")
  ret <- attr(object, "return_type")
  value_type <- ifelse(ret == "AGC", "Carbon", "Biomass")

  cat(sprintf("Return type: %s (t)\n", value_type))
  cat(sprintf("Reference method: %s\n\n", ref))

  # Basic stats
  cat("METHODS COMPARED:\n")
  methods <- unique(object$method)
  for (m in methods) {
    row <- object[object$method == m, ][1, ]
    marker <- ifelse(m == ref, " [REFERENCE]", "")
    cat(sprintf("  - %s%s: %.4f t (rank %d) | Source: %s\n",
                m, marker, row$value, row$rank, row$source))
  }

  cat("\nCITATIONS:\n")
  for (m in methods) {
    row <- object[object$method == m, ][1, ]
    cat(sprintf("  - %s: %s\n", m, row$citation))
  }

  cat("\nUNCERTAINTY:\n")
  for (m in methods) {
    row <- object[object$method == m, ][1, ]
    if (!is.na(row$cv_pct)) {
      cat(sprintf("  - %s: CV = %.2f%%, CI = [%.4f, %.4f]\n",
                  m, row$cv_pct, row$ci_low, row$ci_high))
    } else {
      cat(sprintf("  - %s: Uncertainty not available\n", m))
    }
  }

  cat("\nDIFFERENCES VS REFERENCE:\n")
  for (m in methods) {
    if (m != ref) {
      row <- object[object$method == m, ][1, ]
      sign <- ifelse(row$pct_diff_vs_ref >= 0, "+", "")
      cat(sprintf("  - %s: %s%.2f%% (%s%.4f t)\n",
                  m, sign, row$pct_diff_vs_ref, sign, row$abs_diff_vs_ref))
    }
  }

  cat("\nMETHOD REQUIREMENTS:\n")
  for (m in methods) {
    row <- object[object$method == m, ][1, ]
    height_req <- ifelse(row$height_required, "Yes", "No")
    cat(sprintf("  - %s: Height required = %s | Region = %s\n",
                m, height_req, row$region))
  }

  cat("\nFLAGS & WARNINGS:\n")
  extrap <- object$method[object$extrapolation == TRUE]
  no_overlap <- object$method[object$ci_overlap == FALSE & !is.na(object$ci_overlap)]

  if (length(extrap) > 0) {
    cat(sprintf("  - Extrapolation warnings: %s\n", paste(unique(extrap), collapse = ", ")))
  } else {
    cat("  - No extrapolation warnings\n")
  }

  if (length(no_overlap) > 0) {
    cat(sprintf("  - Non-overlapping CIs with reference: %s\n",
                paste(unique(no_overlap), collapse = ", ")))
  } else {
    cat("  - All CIs overlap with reference\n")
  }

  # Show validity warnings
  for (m in methods) {
    row <- object[object$method == m, ][1, ]
    if (row$validity_warning != "None") {
      cat(sprintf("  - %s validity: %s\n", m, row$validity_warning))
    }
    if (row$defaults_used != "None") {
      cat(sprintf("  - %s defaults: %s\n", m, row$defaults_used))
    }
  }

  cat("\nOVERALL ASSESSMENT:\n")
  cat(sprintf("  %s\n", attr(object, "agreement_summary")))
  cat(sprintf("  Spread across methods: %.4f t (CV = %.2f%%)\n",
              attr(object, "spread"), attr(object, "cv_across_methods")))

  invisible(object)
}


#############################################################################
#                   STANDARDIZED RICH RETURN STRUCTURE                      #
#############################################################################

#' @title Create a rich allometry result object
#' @description Creates a standardized result object that includes the estimate,
#'   method metadata, validity warnings, flags, and uncertainty information.
#'   This structure ensures assumptions and limitations are "impossible to miss".
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
#' result <- create_allometry_result(
#'   value = 1.5,
#'   method = "WCC",
#'   measure = "AGC",
#'   unit = "t",
#'   uncertainty = 0.15,
#'   inputs = list(dbh = 30, height = 15, species = "Quercus robur")
#' )
#' print(result)
#' @export
create_allometry_result <- function(value, method, measure, unit,
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
    citation = meta$citation,
    citation_short = meta$citation_short,
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

#' @title Print method for allometry_result
#' @description Displays the result with assumptions prominently shown
#' @param x An allometry_result object
#' @param ... Additional arguments (unused)
#' @export
print.allometry_result <- function(x, ...) {

  cat("\n")
  cat("================================================================================\n")
  cat("                         ALLOMETRY RESULT                                       \n")
  cat("================================================================================\n\n")

  # Main estimate
  cat(sprintf("ESTIMATE: %.4f %s (%s)\n", x$value, x$unit, x$measure))
  if (!is.null(x$uncertainty) && !is.na(x$uncertainty)) {
    cat(sprintf("UNCERTAINTY: +/- %.4f %s (SD)\n", x$uncertainty, x$unit))
  }
  if (!is.null(x$ci_low) && !is.na(x$ci_low)) {
    cat(sprintf("95%% CI: [%.4f, %.4f] %s\n", x$ci_low, x$ci_high, x$unit))
  }

  cat("\n--------------------------------------------------------------------------------\n")
  cat("METHOD INFORMATION\n")
  cat("--------------------------------------------------------------------------------\n")
  cat(sprintf("Method: %s\n", x$method_full_name))
  cat(sprintf("Citation: %s\n", x$citation_short))
  cat(sprintf("Source: %s\n", x$source_type))
  cat(sprintf("Region: %s | Biome: %s\n", x$region, x$biome))

  cat("\n--------------------------------------------------------------------------------\n")
  cat("*** KEY ASSUMPTIONS ***\n")
  cat("--------------------------------------------------------------------------------\n")
  for (i in seq_along(x$assumptions)) {
    cat(sprintf("  %d. %s\n", i, x$assumptions[i]))
  }

  cat("\n--------------------------------------------------------------------------------\n")
  cat("VALIDITY & FLAGS\n")
  cat("--------------------------------------------------------------------------------\n")
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

  cat("\n================================================================================\n")
  cat("Full citation:\n")
  cat(sprintf("  %s\n", x$citation))
  if (!is.na(x$doi)) {
    cat(sprintf("  DOI: https://doi.org/%s\n", x$doi))
  }
  cat("================================================================================\n\n")

  invisible(x)
}

#' @title Convert allometry_result to data frame
#' @description Converts the result object to a single-row data frame for easy
#'   combination with other results
#' @param x An allometry_result object
#' @param ... Additional arguments (unused)
#' @return A data frame with one row
#' @export
as.data.frame.allometry_result <- function(x, ...) {
  data.frame(
    value = x$value,
    measure = x$measure,
    unit = x$unit,
    method = x$method,
    method_full_name = x$method_full_name,
    citation_short = x$citation_short,
    source_type = x$source_type,
    region = x$region,
    uncertainty = ifelse(is.null(x$uncertainty), NA, x$uncertainty),
    ci_low = ifelse(is.null(x$ci_low), NA, x$ci_low),
    ci_high = ifelse(is.null(x$ci_high), NA, x$ci_high),
    validity_warnings = paste(x$validity_warnings, collapse = "; "),
    flags = paste(x$flags, collapse = "; "),
    assumptions = paste(x$assumptions, collapse = " | "),
    height_required = x$height_required,
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
  cat(sprintf("Source: %s (%s)\n", object$citation_short, object$source_type))
  if (any(object$validity_warnings != "None")) {
    cat(sprintf("WARNINGS: %s\n", paste(object$validity_warnings, collapse = "; ")))
  }
  invisible(object)
}


#############################################################################
#              HELPER: VALIDATE AND FLAG INPUTS FOR ANY METHOD              #
#############################################################################

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


