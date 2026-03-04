# ==============================================================================
# TreeCarbon - Forestry Commission Woodland Carbon Code: Carbon Estimation
# ==============================================================================
#
# This module implements the Forestry Commission's Woodland Carbon Code (WCC)
# methodology for biomass and carbon estimation. It builds on tariff/volume
# calculations to estimate above-ground, below-ground, and total carbon.
#
# Functions included:
#   - Species code lookup and matching
#   - Crown biomass estimation (Equations 6-7)
#   - Root biomass estimation (Equations 8-9)
#   - Stem wood biomass (using NSG and CVF)
#   - Biomass to carbon conversion
#   - WCC carbon pipeline (fc_agc)
#
# Authors: Justin Moat (J.Moat@kew.org), Isabel Openshaw (I.Openshaw@kew.org)
#
# References:
#   - Jenkins, T.A.R., et al. (2018). FC Woodland Carbon Code: Carbon
#     Assessment Protocol (v2.0). Forestry Commission, Edinburgh.
#     https://www.woodlandcarboncode.org.uk/
#
# ==============================================================================
#
############# Lookup species Code ################
#'
#' @title Lookup species code
#' @description  Function that looks up species codes for Woodland Carbon Code
#' @author Isabel Openshaw I.Openshaw@kew.org, Justin Moat J.Moat@kew.org
#' @param name name of species (common or botanical). See lookup_df.Rda
#' @param type either 'broadleaf' or 'conifer'
#' @param code either 'short', 'single', 'stand', 'Root' or 'Crown' Or other
#' column names from lookup_df.Rda
#' @param returnv either 'code' for just the code output or 'all' (default) with
#'  spname and matchtype for checking
#' @return species code
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom stringr word str_trim
#' @import remotes
#' @examples
#' lookupcode(name="Pine")
#' lookupcode(name='Quercus robur', code='single')
#' lookupcode(name=c('Oak', 'Quercus', 'Quercus rubra', 'Hawthorn', NA) ,
#'  type = c(rep("broadleaf", 4), NA), code='short')
#' @export
#' @aliases lookupcode
#'
lookupcode <- function(name, type = NULL, code = "short", returnv = "all") {
  if (!is.character(name)) stop("'name' must be a character vector.")

  # Identify the column index for the selected code
  col_index <- which(names(lookup_df) == code)
  if (length(col_index) == 0) stop("Invalid 'code' column selected.")

  # Make sure name is lower case
  clean_name <- tolower(stringr::str_trim(name))

  # Vectorised matching
  match_binomial <- match(clean_name, tolower(lookup_df$latin_name))
  match_common <- match(clean_name, tolower(lookup_df$common_name))
  match_genus  <- match(stringr::word(clean_name, 1), tolower(lookup_df$General.genus))
  match_genus2 <- match(stringr::word(clean_name, 1), tolower(lookup_df$genus))

  # Create result dataframe
  result_df <- data.frame(spname = name, code = NA,
                          matchtype = NA, stringsAsFactors = FALSE)
  unmatched <- c()

  # species Binomial Matches
  valid_binomial <- !is.na(match_binomial)
  if (any(valid_binomial, na.rm = TRUE)) {
    result_df$code[valid_binomial] <- lookup_df[match_binomial[valid_binomial], col_index]
    result_df$matchtype[valid_binomial] <- "binomial"
  }

  # Common Name Matches
  valid_common <- !is.na(match_common) & is.na(result_df$code)
  if (any(valid_common, na.rm = TRUE)) {
    result_df$code[valid_common] <- lookup_df[match_common[valid_common], col_index]
    result_df$matchtype[valid_common] <- "common"
  }

  # genus Matches
  valid_genus <- !is.na(match_genus) & is.na(result_df$code)
  if (any(valid_genus, na.rm = TRUE)) {
    result_df$code[valid_genus] <- lookup_df[match_genus[valid_genus], col_index]
    result_df$matchtype[valid_genus] <- "genus"
  }
  valid_genus2 <- !is.na(match_genus2) & is.na(result_df$code)
  if (any(valid_genus2, na.rm = TRUE)) {
    result_df$code[valid_genus2] <- lookup_df[match_genus2[valid_genus2], col_index]
    result_df$matchtype[valid_genus2] <- "genus"
  }

  # Type Matches
  valid_type <- is.na(result_df$code) & type %in% c('broadleaf', 'conifer')

  if (any(valid_type, na.rm = TRUE)) {
    unmatched <- unique(result_df$spname[valid_type])

    match_type <- match(type[valid_type], tolower(lookup_df$General.type))

    result_df$code[valid_type] <- lookup_df[match_type, col_index]
    result_df$matchtype[valid_type] <- type[valid_type]

  }

  # Fallback to Mixed species
  valid_mixed <- is.na(result_df$code)
  if (any(valid_mixed, na.rm = TRUE)) {
    unmatched <- c(unmatched, unique(result_df$spname[valid_mixed]))

    result_df$code[valid_mixed] <- lookup_df[33, col_index]
    result_df$matchtype[valid_mixed] <- "mixed"

  }

  if (length(unmatched) > 0) {warning("The following species were not found: ",
                                      paste(unmatched, collapse = ", "))}

  if(returnv == "all"){
    return(result_df)
  } else {
    return(result_df$code)
  }

}


############# FC Above Ground Carbon ################
#'
#' @title Calculate above ground carbon using Woodland Carbon Code
#' @description  Function that inputs tree species code, dbh, height and method
#' for converting biomass to carbon, and returns the carbon estimate using
#' the UK Woodland Carbon Code methodology.
#'
#'   When \code{rich_output = TRUE}, returns a comprehensive result object
#'   including method metadata, assumptions, validity warnings, and uncertainty
#'   - making limitations and assumptions "impossible to miss".
#'
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param name species name, either binomial or common
#' @param type either 'broadleaf' or 'conifer'
#' @param dbh diameter at breast height in centimetres
#' @param height tree height in metres (total height). For broadleaves, provide \code{height_timber}
#'   for WCC protocol compliance.
#' @param height_timber timber height in metres (height to first branch). Required for WCC protocol
#'   for broadleaf species. If not provided, will be estimated using \code{timber_ratio}.
#' @param timber_ratio ratio for estimating timber height from total height when \code{height_timber}
#'   is not provided. Can be either a numeric value (0-1, default = 0.85) or the string "estimate"
#'   to use species-specific ratios via \code{estimate_timber_height()}. Only used for broadleaf species.
#' @param method method of converting biomass to carbon. Default \code{"Matthews2"}
#'   (recommended for WCC; Matthews 1993). Also: \code{"Thomas"}, \code{"IPCC2"}.
#' @param biome temperate, boreal, mediterranean, tropical, subtropical or all
#' @param output.all if TRUE (default) outputs all data from processing, else outputs carbon estimate
#' @param nsg nominal specific gravity. Optionally specified, else will use that
#'  given by the WCC
#' @param rich_output Logical. If TRUE, returns a rich result object with
#'   metadata including: value, method, reference, assumptions, validity_warning,
#'   flags, region, and source type. Default FALSE for backwards compatibility.
#' @return If \code{rich_output = FALSE}: either Above ground carbon (AGC) in tonnes,
#'   or a data.frame with tariff number, merchantable volume, stem volume,
#'   stem biomass, crown biomass, root biomass, and AGC.
#'   If \code{rich_output = TRUE}: an \code{allometry_result} object (for single
#'   trees) or a list of such objects (for multiple trees).
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @import remotes
#' @examples
#' fc_agc('Quercus robur', dbh=74, height=24, output.all = FALSE)
#' # Input wood density and sd from BIOMASS package
#' wd <- BIOMASS::getWoodDensity('Quercus', 'robur', region='Europe')
#' fc_agc('beech', 72, 24, nsg = wd$meanWD)
#'
#' # Rich output with metadata
#' result <- fc_agc('Oak', dbh=50, height=20, rich_output = TRUE)
#' print(result)
#' @export
#' @aliases fc_agc
#'
fc_agc <- function(name, dbh, height, type = NULL, method = "Matthews2", biome =
                     "temperate", output.all = TRUE, nsg = NULL,
                   height_timber = NULL, timber_ratio = 0.85, rich_output = FALSE){

  # ==== Check arguments ====
  if(!is.character(name)) stop ("name must be a character")
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }
  class <- c("broadleaf", "conifer")
  if(any(!type %in% c(class, NA, "NA"))){
    warning("type must equal either conifer, broadleaf or NA")
  }
  if (!(method %in% c("Matthews2", "IPCC2", "Thomas"))) {
    stop("Invalid method. Choose from: 'Matthews2', 'IPCC2', 'Thomas'")
  }
  biomes <- c("tropical", "subtropical", "mediterranean", "temperate", "boreal")
  if (!missing(biome) && !(biome %in% biomes)) {
    stop("Invalid biome. Choose from: ", paste(biomes, collapse = ", "))
  }
  if (any(dbh < 0, na.rm = TRUE) || !is.numeric(dbh) || anyNA(dbh)) {
    warning("dbh must be numeric and positive")}

  # ==== Validate against WCC method ranges ====
  validation <- validate_inputs_for_method("WCC", dbh = dbh, height = height)
  flags <- if (validation$flags[1] != "None") validation$flags else character()
  warnings_list <- if (validation$validity_warnings[1] != "None") validation$validity_warnings else character()

  # ==== Lookup species codes and type ====
  spcodes <- lookupcode(name, type, code = 'short')
  rec <- lookup_df[match(spcodes$code, lookup_df$short), ]
  type <- ifelse(is.na(rec$type) | rec$type == "MX", type, rec$type)

  # Handle height_timber for all trees
  # WCC protocol requires timber height for broadleaves
  # For conifers, timber height = total height (ratio = 1.0)
  n <- length(height)

  # Initialize height_timber as a vector of length n
  # For conifers, timber height = total height
  if (is.null(height_timber)) {
    height_timber <- rep(NA_real_, n)
    # For conifers, timber height equals total height
    conifer_idx <- type == "conifer" & !is.na(type)
    if (any(conifer_idx, na.rm = TRUE)) {
      height_timber[conifer_idx] <- height[conifer_idx]
    }
  } else {
    # Ensure height_timber is a vector of length n
    if (length(height_timber) == 1) {
      height_timber <- rep(height_timber, n)
    } else if (length(height_timber) != n) {
      stop("height_timber must be NULL, a single value, or a vector of length equal to the number of trees")
    }
    # For conifers, ensure timber height = total height
    conifer_idx <- type == "conifer" & !is.na(type)
    if (any(conifer_idx, na.rm = TRUE)) {
      height_timber[conifer_idx] <- height[conifer_idx]
    }
  }

  # Handle broadleaves
  if (any(type == "broadleaf", na.rm = TRUE)) {
    broadleaf_idx <- type == "broadleaf" & !is.na(type)
    broadleaf_missing <- broadleaf_idx & is.na(height_timber)

    if (any(broadleaf_missing, na.rm = TRUE)) {
      # Need to estimate timber height for broadleaves
      mature_tall <- broadleaf_missing & !is.na(height) & height > 20
      if (is.character(timber_ratio) && timber_ratio == "estimate") {
        # Use estimate_timber_height function with species-specific ratios
        estimated_timber <- estimate_timber_height(
          height = height[broadleaf_missing],
          spcode = spcodes$code[broadleaf_missing],
          type = type[broadleaf_missing],
          default_ratio = 0.85
        )
        height_timber[broadleaf_missing] <- estimated_timber
        flags <- c(flags, "Broadleaf_timber_height_estimated_via_species_ratios")
        warnings_list <- c(warnings_list,
          "For broadleaf species, WCC protocol requires timber height (height to first branch). Timber height was estimated from total height using species-specific ratios (or default 0.85 if not available). Timber height estimation is crude; measured values are required for WCC certification.")
        if (any(mature_tall, na.rm = TRUE)) {
          flags <- c(flags, "Timber_height_estimated_for_mature_trees_over_20m")
          warnings_list <- c(warnings_list,
            "Estimated timber height may be less accurate for large, old trees (>20 m height). Measured timber height is strongly recommended for WCC certification.")
          warning("Timber height was estimated for mature trees (>20 m). Estimated timber height may be less accurate for large, old trees. Measured values are required for WCC certification.")
        }
      } else if (is.numeric(timber_ratio) && timber_ratio > 0 && timber_ratio <= 1) {
        # Use provided numeric ratio
        height_timber[broadleaf_missing] <- height[broadleaf_missing] * timber_ratio
        flags <- c(flags, paste0("Broadleaf_timber_height_estimated_via_fixed_ratio_", timber_ratio))
        warnings_list <- c(warnings_list,
          paste0("For broadleaf species, WCC protocol requires timber height (height to first branch). Timber height was estimated from total height using ratio of ", timber_ratio, ". Timber height estimation is crude; measured values are required for WCC certification."))
        if (any(mature_tall, na.rm = TRUE)) {
          flags <- c(flags, "Timber_height_estimated_for_mature_trees_over_20m")
          warnings_list <- c(warnings_list,
            "Estimated timber height may be less accurate for large, old trees (>20 m height). Measured timber height is strongly recommended for WCC certification.")
          warning("Timber height was estimated for mature trees (>20 m). Estimated timber height may be less accurate for large, old trees. Measured values are required for WCC certification.")
        }
      } else {
        stop("timber_ratio must be either a numeric value between 0 and 1, or the string 'estimate'")
      }
    } else {
      flags <- c(flags, "Broadleaf_timber_height_provided")
    }
  }

  # ==== Create results table ====
  if(output.all){
    r <- data.frame(name=name, type=type, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, dbh=dbh, height=height,
                    height_timber = height_timber,
                    NSG=rec$NSG, tariff=NA, mercvol_m.3=NA, stemvol_m.3=NA,
                    stembiomass_t=NA, crownbiomass_t=NA, rootbiomass_t=NA,
                    AGC_WCC_t=NA, stringsAsFactors=FALSE)
  } else {
    r <- data.frame(name=name, AGC_WCC_t=NA, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, stringsAsFactors=FALSE)
  }

  # ==== Small Trees ====
  # Get indicies for trees with height < 10 or dbh < 7m
  small_id <- type %in% class & !is.na(height) & (!is.na(dbh) & dbh < 7 |
                                                    is.na(dbh) & height < 10)
  # For small trees, calculate carbon using sapling function
  if (any(small_id, na.rm = TRUE)) {
    carbon <- sap_seedling2C(height = height[small_id], type[small_id])
    # sap_seedling2C returns a vector when re_h is NULL, list otherwise
    if (is.list(carbon)) {
      r$AGC_WCC_t[small_id] <- carbon$carbon
    } else {
      r$AGC_WCC_t[small_id] <- carbon
    }
  }

  # ==== Tall Trees ====
  # Get indices for trees with dbh >= 7 (same logic as fc_agc_error)
  tall_id <- dbh >= 7 & !is.na(dbh) & !is.na(height)

  if (any(tall_id, na.rm = TRUE)) {
    # Tariff number (without error params, returns vector)
    # For broadleaves, use height_timber if provided
    height_timber_tall <- if (!is.null(height_timber)) height_timber[tall_id] else NULL
    tariff <- tariffs(spcodes$code[tall_id], height[tall_id],
                      dbh[tall_id], type[tall_id], height_timber = height_timber_tall)

    # Volume & Biomass (without error params, returns vectors)
    mercvol <- merchtreevol(dbh[tall_id], tariff)
    stemvol <- treevol(mercvol, dbh = dbh[tall_id])

    woodbio <- woodbiomass(stemvol, rec$NSG[tall_id])
    crownbio <- crownbiomass(rec$Crown[tall_id], dbh[tall_id])

    # Above Ground Biomass
    AGB <- woodbio + crownbio

    # Carbon Conversion
    convert <- type[tall_id] %in% class
    if (any(convert, na.rm = TRUE)) {
      # biomass2c without sig_AGB returns vector, not list
      AGC <- biomass2c(AGB[convert], method, type[tall_id][convert], biome)
      # Handle both return types (vector or list with $AGC)
      if (is.list(AGC) && !is.null(AGC$AGC)) {
        r$AGC_WCC_t[tall_id][convert] <- AGC$AGC
      } else {
        r$AGC_WCC_t[tall_id][convert] <- AGC
      }
    } else {
      r$AGC_WCC_t[tall_id] <- AGB * 0.47
      warning("Type must be specified as 'broadleaf' or 'conifer' for carbon
              conversion.")
    }

    if(output.all){
      # Root Biomass (without error params, returns vector)
      rootbio <- rootbiomass(rec$Root[tall_id], dbh[tall_id])

      r[tall_id, c("tariff", "mercvol_m.3", "stemvol_m.3",
                   "stembiomass_t", "crownbiomass_t", "rootbiomass_t")] <-
        list(tariff, mercvol, stemvol,
             woodbio, crownbio, rootbio)
    }
  }

  # ==== Return rich output if requested ====
  if (rich_output) {
    # For single tree
    if (length(dbh) == 1) {
      return(single_tree_rich_output(value = r$AGC_WCC_t[1], method = "WCC",
        measure = "AGC",             unit = "t",             uncertainty = NULL,
        validity_warnings = warnings_list,                   flags = flags,
        inputs = list(name = name,   dbh = dbh,              height = height,
                      height_timber = height_timber, timber_ratio = timber_ratio,
                      type = type,   biome = biome)
      ))
    }

    # For multiple trees
    # Get shared method metadata
    wcc_meta <- method_metadata[method_metadata$method == "WCC", ]
    wcc_assumptions <- method_assumptions$assumption[method_assumptions$method == "WCC"]

    # Calculate totals first
    total_AGC_t <- sum(r$AGC_WCC_t, na.rm = TRUE)
    total_uncertainty_t <- NULL  # fc_agc doesn't calculate uncertainty

    # Collect all unique flags from all trees
    all_flags <- if (length(flags) > 0 && flags[1] != "None") unique(flags) else character()

    # Create summary table directly from r
    combined_df <- data.frame(
      tree_id = seq_len(nrow(r)),
      name = r$name,
      dbh = r$dbh,
      height = r$height,
      carbon_t = r$AGC_WCC_t,
      stringsAsFactors = FALSE
    )

    # Create result objects (vectorized structure, but list of lists for compatibility)
    results_list <- lapply(seq_len(nrow(r)), function(i) {
      # Create lightweight result object with shared metadata
      result <- list(
        # === Core estimate (tree-specific) ===
        value = if (!is.na(r$AGC_WCC_t[i])) r$AGC_WCC_t[i] else NA_real_,
        measure = "AGC",
        unit = "t",

        # === Method information (shared - from metadata) ===
        method = "WCC",
        method_full_name = wcc_meta$full_name,
        reference = wcc_meta$reference,
        reference_short = wcc_meta$reference_short,
        doi = wcc_meta$doi,
        source_type = wcc_meta$source_type,
        region = wcc_meta$region,
        biome = strsplit(wcc_meta$biome, "; ")[[1]],

        # === Assumptions (shared - from metadata) ===
        assumptions = wcc_assumptions,

        # === Uncertainty (tree-specific - NULL for fc_agc) ===
        uncertainty = NULL,
        uncertainty_method = wcc_meta$uncertainty_method,
        ci_low = NULL,
        ci_high = NULL,
        ci_level = NA,

        # === Validity and flags (tree-specific) ===
        validity_warnings = if (length(warnings_list) > 0) warnings_list else "None",
        flags = if (length(flags) > 0) flags else "None",

        # === Valid ranges (shared - from metadata) ===
        valid_dbh_range = c(wcc_meta$dbh_min_cm, wcc_meta$dbh_max_cm),
        valid_height_range = c(wcc_meta$height_min_m, wcc_meta$height_max_m),
        height_required = wcc_meta$height_required,
        species_specific = wcc_meta$species_specific,
        wood_density_required = wcc_meta$wood_density_required,

        # === Inputs (minimal - only type and biome, since name/dbh/height are in r) ===
        inputs = list(
          type = if (!is.null(type)) type[min(i, length(type))] else NULL,
          biome = biome
        )
      )
      class(result) <- "allometry_result"
      return(result)
    })

    result <- list(
      trees = results_list,
      summary_table = combined_df,
      n_trees = length(dbh),
      total_AGC_t = total_AGC_t,
      mean_AGC_t = mean(r$AGC_WCC_t, na.rm = TRUE),
      total_uncertainty_t = total_uncertainty_t,
      validation = validation,
      detailed_output = if (output.all) r else NULL
    )
    class(result) <- c("wcc_multi_result", "list")
    return(result)
  }

  return(r)
}

############# FC Above Ground Carbon with error ################
#'
#' @title Calculate above ground carbon with error propagation (WCC)
#' @description  Function that inputs tree species code, dbh, height and method
#' for converting biomass to carbon, and returns the carbon estimate with
#' propagated uncertainty using Woodland Carbon Code methodology.
#'
#'   When \code{rich_output = TRUE}, returns a comprehensive result object
#'   including method metadata, assumptions, validity warnings, and uncertainty
#'
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param name species name, either binomial or common
#' @param type either 'broadleaf' or 'conifer'
#' @param dbh diameter at breast height in centimetres
#' @param height tree height in metres (total height). For broadleaves, provide \code{height_timber}
#'   for WCC protocol compliance.
#' @param height_timber timber height in metres (height to first branch). Required for WCC protocol
#'   for broadleaf species. If not provided, total height will be used with a warning.
#' @param method method of converting biomass to carbon. Default \code{"Matthews2"}
#'   (recommended for WCC; Matthews 1993). Use \code{"IPCC2"} or \code{"Thomas"}
#'   for full uncertainty propagation through carbon conversion.
#' @param biome temperate, boreal, mediterranean, tropical, subtropical or all
#' @param output.all if TRUE outputs all data from processing, else just outputs carbon estimates
#' @param re_dbh relative measurement error for diameter at breast height, single value
#' @param re_h relative error of height measurement, single value
#' @param sig_nsg sigma for nominal specific gravity (NSG) or wood density
#' @param re relative error of coefficients (default = 0.025)
#' @param nsg nominal specific gravity. Optionally specified, else will use that
#'  given by the WCC
#' @param rich_output Logical. If TRUE, returns a rich result object with
#'   metadata including: value, method, reference, assumptions, validity_warning,
#'   flags, uncertainty interval, region, and source type. Default FALSE.
#' @return If \code{rich_output = FALSE}: either Above ground carbon (AGC) in tonnes,
#'   or if output.all = TRUE, a data.frame with tariff number, volumes, biomass,
#'   carbon estimates and associated uncertainties (sig_*).
#'   If \code{rich_output = TRUE}: an \code{allometry_result} object with metadata.
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @seealso \code{\link{wcc_per_hectare}}, \code{\link{wcc_stratify}}, \code{\link{error_product}};
#'   vignette("uncertainty-and-comparison", package = "TreeCarbon") for error propagation rules.
#' @examples
#' fc_agc_error(name='Quercus robur', dbh=74, height=24, output.all = FALSE)
#' fc_agc_error('Oak', dbh=74, height=24, method="IPCC2",
#' biome="temperate", output.all = FALSE, re_dbh=1, re_h=1)
#' # Input wood density and sd from BIOMASS package
#' wd <- BIOMASS::getWoodDensity('Quercus', 'robur', region='Europe')
#' fc_agc_error('Oak', 72, 24, nsg = wd$meanWD, sig_nsg = wd$sdWD)
#'
#' # Rich output with metadata
#' result <- fc_agc_error('Oak', 50, 20, rich_output = TRUE)
#' print(result)
#' @export
#' @aliases fc_agc_error
#'
fc_agc_error <- function(name, dbh, height, type = NULL, method = "Matthews2", biome =
                           "temperate", output.all = TRUE, re_dbh = 0.05, re_h =
                           0.1, re = 0.025, nsg = NULL, sig_nsg = 0.09413391,
                         height_timber = NULL, timber_ratio = NULL, rich_output = FALSE){

  # ==== Check arguments ====
  if(!is.character(name)) stop ("name must be a character")
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }
  class <- c("broadleaf", "conifer")
  if(any(!type %in% c(class, NA, "NA"))){
    warning("type must equal either conifer, broadleaf or NA")
  }
  if(!is.numeric(re_dbh) || any(re_dbh<=0)){
    stop("re_dbh must be numeric & positive")
  }
  if(!is.numeric(re_h) || any(re_h<0)) stop ("re_h must be numeric & positive")

  if (!(method %in% c("Matthews2", "IPCC2", "Thomas"))) {
    stop("Invalid method. Choose from: 'Matthews2', 'IPCC2', 'Thomas'")
  }
  biomes <- c("tropical", "subtropical", "mediterranean", "temperate", "boreal")
  if (!missing(biome) && !(biome %in% biomes)) {
    stop("Invalid biome. Choose from: ", paste(biomes, collapse = ", "))
  }
  if (any(dbh < 0, na.rm = TRUE) || !is.numeric(dbh) || anyNA(dbh)) {
    warning("dbh must be numeric and positive")}

  # ==== Validate against WCC method ranges ====
  validation <- validate_inputs_for_method("WCC", dbh = dbh, height = height)
  flags <- if (validation$flags[1] != "None") validation$flags else character()
  warnings_list <- if (validation$validity_warnings[1] != "None") validation$validity_warnings else character()

  # Lookup species codes and type
  spcodes <- lookupcode(name, type, code = 'short')
  rec <- lookup_df[match(spcodes$code, lookup_df$short), ]
  type <- ifelse(is.na(rec$type) | rec$type == "MX", type, rec$type)

  # Handle height_timber for all trees
  # WCC protocol requires timber height for broadleaves
  # For conifers, timber height = total height (ratio = 1.0)
  n <- length(height)

  # Initialize height_timber as a vector of length n
  # For conifers, timber height = total height
  if (is.null(height_timber)) {
    height_timber <- rep(NA_real_, n)
    # For conifers, timber height equals total height
    conifer_idx <- type == "conifer" & !is.na(type)
    if (any(conifer_idx, na.rm = TRUE)) {
      height_timber[conifer_idx] <- height[conifer_idx]
    }
  } else {
    # Ensure height_timber is a vector of length n
    if (length(height_timber) == 1) {
      height_timber <- rep(height_timber, n)
    } else if (length(height_timber) != n) {
      stop("height_timber must be NULL, a single value, or a vector of length equal to the number of trees")
    }
    # For conifers, ensure timber height = total height
    conifer_idx <- type == "conifer" & !is.na(type)
    if (any(conifer_idx, na.rm = TRUE)) {
      height_timber[conifer_idx] <- height[conifer_idx]
    }
  }

  # Handle broadleaves
  if (any(type == "broadleaf", na.rm = TRUE)) {
    broadleaf_idx <- type == "broadleaf" & !is.na(type)
    broadleaf_missing <- broadleaf_idx & is.na(height_timber)

    if (any(broadleaf_missing, na.rm = TRUE)) {
      # Need to estimate timber height for broadleaves
      mature_tall <- broadleaf_missing & !is.na(height) & height > 20
      if (is.character(timber_ratio) && timber_ratio == "estimate") {
        # Use estimate_timber_height function with species-specific ratios
        estimated_timber <- estimate_timber_height(
          height = height[broadleaf_missing],
          spcode = spcodes$code[broadleaf_missing],
          type = type[broadleaf_missing],
          default_ratio = 0.85
        )
        height_timber[broadleaf_missing] <- estimated_timber
        flags <- c(flags, "Broadleaf_timber_height_estimated_via_species_ratios")
        warnings_list <- c(warnings_list,
          "For broadleaf species, WCC protocol requires timber height (height to first branch). Timber height was estimated from total height using species-specific ratios (or default 0.85 if not available). Timber height estimation is crude; measured values are required for WCC certification.")
        if (any(mature_tall, na.rm = TRUE)) {
          flags <- c(flags, "Timber_height_estimated_for_mature_trees_over_20m")
          warnings_list <- c(warnings_list,
            "Estimated timber height may be less accurate for large, old trees (>20 m height). Measured timber height is strongly recommended for WCC certification.")
          warning("Timber height was estimated for mature trees (>20 m). Estimated timber height may be less accurate for large, old trees. Measured values are required for WCC certification.")
        }
      } else if (is.numeric(timber_ratio) && timber_ratio > 0 && timber_ratio <= 1) {
        # Use provided numeric ratio
        height_timber[broadleaf_missing] <- height[broadleaf_missing] * timber_ratio
        flags <- c(flags, paste0("Broadleaf_timber_height_estimated_via_fixed_ratio_", timber_ratio))
        warnings_list <- c(warnings_list,
          paste0("For broadleaf species, WCC protocol requires timber height (height to first branch). Timber height was estimated from total height using ratio of ", timber_ratio, ". Timber height estimation is crude; measured values are required for WCC certification."))
        if (any(mature_tall, na.rm = TRUE)) {
          flags <- c(flags, "Timber_height_estimated_for_mature_trees_over_20m")
          warnings_list <- c(warnings_list,
            "Estimated timber height may be less accurate for large, old trees (>20 m height). Measured timber height is strongly recommended for WCC certification.")
          warning("Timber height was estimated for mature trees (>20 m). Estimated timber height may be less accurate for large, old trees. Measured values are required for WCC certification.")
        }
      } else {
        stop("timber_ratio must be either a numeric value between 0 and 1, or the string 'estimate'")
      }
    } else {
      flags <- c(flags, "Broadleaf_timber_height_provided")
    }
  }

  # Create results table
  if(output.all){
    r <- data.frame(name=name, type=type, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, dbh=dbh, height=height,
                    height_timber = height_timber,
                    NSG=rec$NSG, tariff=NA, sig_tariff=NA, mercvol_m.3=NA,
                    sig_mercvol=NA, stemvol_m.3=NA, sig_stemvol=NA,
                    stembiomass_t=NA, sig_stembiomass=NA, crownbiomass_t=NA,
                    sig_crownbiomass=NA, rootbiomass_t=NA, sig_rootbiomass=NA,
                    AGC_WCC_t=NA, sig_AGC=NA, stringsAsFactors=FALSE)
  } else {
    r <- data.frame(name=name, AGC_WCC_t=NA, sig_AGC=NA, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, stringsAsFactors=FALSE)
  }

  # Trees with dbh < 7m
  small_id <- type %in% class & !is.na(height) & (!is.na(dbh) & dbh < 7 | is.na(dbh) & height < 10)

  if (any(small_id, na.rm = TRUE)) {
    carbon <- sap_seedling2C(height = height[small_id],
                             type[small_id], re_h, re)
    r$AGC_WCC_t[small_id] <- carbon$carbon
    r$sig_AGC[small_id] <- carbon$sd
  }

  # Trees with dbh >= 7 m
  tall_id <- dbh >= 7 & !is.na(dbh) & !is.na(height)

  if (any(tall_id, na.rm = TRUE)) {
    # For broadleaves, use height_timber if provided
    height_timber_tall <- height_timber[tall_id]
    tariff <- tariffs(spcodes$code[tall_id], height[tall_id],
                      dbh[tall_id], type[tall_id], height_timber = height_timber_tall,
                      re_h, re_dbh, re = re)

    # Volume & Biomass
    mercvol <- merchtreevol(dbh[tall_id], tariff$tariff,
                            re_dbh, tariff$sigma, re)
    stemvol <- treevol(mercvol$volume, dbh = dbh[tall_id], mercvol$sigma, re)

    woodbio <- woodbiomass(stemvol$stemvolume, rec$NSG[tall_id],
                           stemvol$sigma, sig_nsg)
    crownbio <- crownbiomass(rec$Crown[tall_id], dbh[tall_id], re_dbh, re)

    # Above Ground Biomass
    AGB <- woodbio$woodbiomass + crownbio$biomass
    sig_AGB <- sqrt(woodbio$sigma^2 + as.numeric(crownbio$sigma)^2)

    # Carbon Conversion
    convert <- type[tall_id] %in% class
    if (any(convert, na.rm = TRUE)) {
      AGC <- biomass2c(AGB[convert], method, type[tall_id][convert],
                       biome, sig_AGB[convert])
      if (is.data.frame(AGC) && "AGC" %in% names(AGC)) {
        r$AGC_WCC_t[tall_id][convert] <- AGC$AGC
        r$sig_AGC[tall_id][convert] <- AGC$sig_AGC
      } else {
        r$AGC_WCC_t[tall_id][convert] <- AGC
        r$sig_AGC[tall_id][convert] <- NA_real_
      }
    } else {
      r$AGC_WCC_t[tall_id] <- AGB * 0.4885
      warning("Type must be specified as 'broadleaf' or 'conifer' for carbon
              conversion.")
    }

    if(output.all){
      # Root Biomass
      rootbio <- rootbiomass(rec$Root[tall_id], dbh[tall_id], re_dbh)

      r[tall_id, c("tariff", "sig_tariff",
                   "mercvol_m.3", "sig_mercvol", "stemvol_m.3", "sig_stemvol",
                   "stembiomass_t", "sig_stembiomass", "crownbiomass_t", "sig_crownbiomass",
                   "rootbiomass_t", "sig_rootbiomass")] <-
        list(tariff$tariff, tariff$sigma,
             mercvol$volume, mercvol$sigma, stemvol$stemvolume, stemvol$sigma,
             woodbio$woodbiomass, woodbio$sigma, crownbio$biomass,
             crownbio$sigma, rootbio$rootbiomass, rootbio$sigma)
    }
  }

  # ==== Return rich output if requested ====
  if (rich_output) {
    if (length(dbh) == 1) {
      return(single_tree_rich_output(
        value = r$AGC_WCC_t[1],
        method = "WCC",
        measure = "AGC",
        unit = "t",
        uncertainty = r$sig_AGC[1],
        validity_warnings = warnings_list,
        flags = flags,
        inputs = list(
          name = name,
          dbh = dbh,
          height = height,
          height_timber = height_timber,
          timber_ratio = timber_ratio,
          type = type,
          biome = biome,
          re_dbh = re_dbh,
          re_h = re_h
        )
      ))
    }

    # Multiple trees
    # Get shared method metadata once (same for all trees) - direct access
    wcc_meta <- method_metadata[method_metadata$method == "WCC", ]
    wcc_assumptions <- method_assumptions$assumption[method_assumptions$method == "WCC"]

    # Calculate totals first (vectorized)
    total_AGC_t <- sum(r$AGC_WCC_t, na.rm = TRUE)
    total_uncertainty_t <- if (!all(is.na(r$sig_AGC))) {
      sqrt(sum(r$sig_AGC^2, na.rm = TRUE))
    } else {
      NULL
    }

    # Collect all unique flags from all trees (vectorized)
    all_flags <- if (length(flags) > 0 && flags[1] != "None") unique(flags) else character()

    # Pre-calculate tree-specific values (vectorized)
    uncertainty_vals <- ifelse(!is.na(r$sig_AGC), r$sig_AGC, NA_real_)
    ci_low_vals <- if (!all(is.na(r$sig_AGC))) {
      r$AGC_WCC_t - 1.96 * r$sig_AGC
    } else {
      rep(NA_real_, nrow(r))
    }
    ci_high_vals <- if (!all(is.na(r$sig_AGC))) {
      r$AGC_WCC_t + 1.96 * r$sig_AGC
    } else {
      rep(NA_real_, nrow(r))
    }

    # Create summary table directly from r (vectorized)
    combined_df <- data.frame(
      tree_id = seq_len(nrow(r)),
      name = r$name,
      dbh = r$dbh,
      height = r$height,
      carbon_t = r$AGC_WCC_t,
      uncertainty_t = uncertainty_vals,
      stringsAsFactors = FALSE
    )

    # Add CI columns if uncertainty was calculated
    if (!all(is.na(r$sig_AGC))) {
      combined_df$ci_low_t <- ci_low_vals
      combined_df$ci_high_t <- ci_high_vals
    }

    # Create result objects (vectorized structure, but list of lists for compatibility)
    results_list <- lapply(seq_len(nrow(r)), function(i) {
      # Create lightweight result object with shared metadata
      result <- list(
        # === Core estimate (tree-specific) ===
        value = if (!is.na(r$AGC_WCC_t[i])) r$AGC_WCC_t[i] else NA_real_,
        measure = "AGC",
        unit = "t",

        # === Method information (shared - from metadata) ===
        method = "WCC",
        method_full_name = wcc_meta$full_name,
        reference = wcc_meta$reference,
        reference_short = wcc_meta$reference_short,
        doi = wcc_meta$doi,
        source_type = wcc_meta$source_type,
        region = wcc_meta$region,
        biome = strsplit(wcc_meta$biome, "; ")[[1]],

        # === Assumptions (shared - from metadata) ===
        assumptions = wcc_assumptions,

        # === Uncertainty (tree-specific) ===
        uncertainty = if (!is.na(uncertainty_vals[i])) uncertainty_vals[i] else NULL,
        uncertainty_method = wcc_meta$uncertainty_method,
        ci_low = if (!is.na(ci_low_vals[i])) ci_low_vals[i] else NULL,
        ci_high = if (!is.na(ci_high_vals[i])) ci_high_vals[i] else NULL,
        ci_level = if (!is.na(ci_low_vals[i])) 0.95 else NA,

        # === Validity and flags (tree-specific) ===
        validity_warnings = if (length(warnings_list) > 0) warnings_list else "None",
        flags = if (length(flags) > 0) flags else "None",

        # === Valid ranges (shared - from metadata) ===
        valid_dbh_range = c(wcc_meta$dbh_min_cm, wcc_meta$dbh_max_cm),
        valid_height_range = c(wcc_meta$height_min_m, wcc_meta$height_max_m),
        height_required = wcc_meta$height_required,
        species_specific = wcc_meta$species_specific,
        wood_density_required = wcc_meta$wood_density_required,

        # === Inputs (minimal - only type, biome, and error params, since name/dbh/height are in r) ===
        inputs = list(
          type = if (!is.null(type)) type[min(i, length(type))] else NULL,
          biome = biome,
          re_dbh = re_dbh,
          re_h = re_h
        )
      )
      class(result) <- "allometry_result"
      return(result)
    })

    result <- list(
      trees = results_list,
      summary_table = combined_df,
      n_trees = length(dbh),
      total_AGC_t = total_AGC_t,
      mean_AGC_t = mean(r$AGC_WCC_t, na.rm = TRUE),
      total_uncertainty_t = total_uncertainty_t,
      validation = validation,
      detailed_output = if (output.all) r else NULL
    )
    class(result) <- c("wcc_multi_result", "list")
    return(result)
  }

  return(r)
}

#' @title Print method for wcc_multi_result
#' @description Formatted display for WCC multi-tree results
#' @param x A wcc_multi_result object
#' @param ... Additional arguments (unused)
#' @export
print.wcc_multi_result <- function(x, ...) {

  cat("------------------- ALLOMETRY RESULT -------------------\n")
  cat(sprintf("Number of trees: %d\n", x$n_trees))
  cat(sprintf("Total AGC estimate: %.2f t\n", x$total_AGC_t))

  # Add uncertainty and CI if available
  if (!is.null(x$total_uncertainty_t)) {
    cat(sprintf("Uncertainty: +/- %.2f t (SD)\n", x$total_uncertainty_t))
    ci_low <- x$total_AGC_t - 1.96 * x$total_uncertainty_t
    ci_high <- x$total_AGC_t + 1.96 * x$total_uncertainty_t
    cat(sprintf("95%% CI: [%.2f, %.2f] t\n", ci_low, ci_high))
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

############# Seedlings and saplings to Carbon ################
#'
#' @title Seedlings and saplings to carbon
#' @description Calculate the carbon content of tree seedlings and saplings
#'   (below and above ground) from height. Uses WCC lookup tables for heights
#'   from 0.01 to 10 m. Both seedlings and saplings use the same lookup tables;
#'   classification is for field sampling (see Details).
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param type 'conifer' or 'broadleaf'
#' @param re relative error of estimates (default = 0.025)
#' @param re_h relative error of height measurement (optional)
#' @return Carbon in tonnes **per individual** seedling/sapling. If \code{re_h}
#'   provided, returns a list with \code{carbon} and \code{sd}.
#' @note Just uses simple linear interpolation between lookup values.
#' @details
#'   **Per-individual result:** The return value is carbon **per seedling/sapling**.
#'   For stratum or project totals, multiply by the total number of individuals
#'   in the stratum (WCC protocol: plot-based counts scaled to per-hectare, then
#'   multiplied by stratum area).
#'
#'   **WCC category definitions (for field sampling):**
#'   - **Seedlings**: Height < 1 m (100 cm) OR DBH < 7 cm
#'   - **Saplings**: Height 1–10 m (100–1000 cm) OR DBH 7–10 cm
#'   - **Trees**: DBH >= 7 cm AND height >= 10 m (use tariff-based methods instead)
#'
#'   This function handles both seedlings and saplings using the same lookup
#'   tables (height range 0.01–10 m). Use \code{fc_agc} or \code{fc_agc_error}
#'   for trees; they route small stems (height < 10 m or DBH < 7 cm) to
#'   \code{sap_seedling2C} internally.
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018)
#' @importFrom utils head tail
#' @import remotes
#' @examples
#' # Seedling (height 0.5 m)
#' sap_seedling2C(0.5, 'conifer')
#'
#' # fx (height 9 m) with uncertainty
#' sap_seedling2C(height = 9, type = 'broadleaf', re_h = 0.05)
#'
#' # Transition examples: seedling vs sapling vs tree
#' sap_seedling2C(0.3, 'broadleaf')   # seedling (< 1 m)
#' sap_seedling2C(5, 'conifer')       # sapling (1–10 m)
#' # For trees (DBH >= 7 cm, height >= 10 m): use fc_agc() or tariffs()
#' @aliases sap_seedling2C
#' @export
#'
sap_seedling2C <- function(height, type, re_h = NULL, re = 0.025) {
  if (!is.numeric(height)){
    stop("Argument 'height' must be numeric.")
  }
  if (any(height < 0.01 | height > 10, na.rm=TRUE)){
    warning("height is only defined for values between 0.01 and 10 metres.")
  }
  if (!any(type %in% c("broadleaf", "conifer"))){
    warning("type must be defined as 'broadleaf', 'conifer' for each tree")
  }
  if (length(height) != length(type)) stop("'height' and 'type' must have the same length")

  # Convert height from metres to centimetres for internal lookup
  heightincm <- height * 100

  sapling_carbon <- function(h, t) {

    if (is.na(h) | is.na(t) | h < 1 | h > 1000) return(NA)

    if          (t == "broadleaf") {   data <- seedlings_broad
    } else if (t == "conifer") {     data <- seedlings_conifer
    } else {                         return(NA) }

    lower_bound <- utils::tail(data[data$height.cm <= h, ], 1)
    upper_bound <- utils::head(data[data$height.cm >= h, ], 1)

    if (nrow(lower_bound) == 0 || nrow(upper_bound) == 0) return(NA)

    if (lower_bound$height.cm == upper_bound$height.cm) {
      carbon_value <- lower_bound$Carbon.kg
    } else {
      h_diff <- upper_bound$height.cm - lower_bound$height.cm
      carbon_diff <- upper_bound$Carbon.kg - lower_bound$Carbon.kg
      proportion <- (h - lower_bound$height.cm) / h_diff
      carbon_value <- lower_bound$Carbon.kg + proportion * carbon_diff
    }
    return(carbon_value)
  }

  results <- mapply(sapling_carbon, heightincm, type, SIMPLIFY = FALSE)

  df <- do.call(rbind, lapply(results, as.data.frame))
  colnames(df) <- c("carbon")

  if (!is.null(re_h)) {
    if (!is.numeric(re_h) | re_h < 0 | !is.numeric(re) | re < 0) {
      stop("'re' and 're_h' must be numeric and positive")
    }

    carbon_sd <- 10 * re * df$carbon

    return(list(carbon = df$carbon, sd = carbon_sd))
  } else {
    return(df$carbon)
  }
}

############# FC Stand-Level Carbon (WCC Method D) ################
#'
#' @title Calculate stand-level carbon using WCC Method D
#' @description Calculate stand-level above-ground carbon using WCC Method D
#'   (stand-level tariff). This function takes all tree DBH and height
#'   measurements from a stand, calculates top height and mean DBH internally,
#'   and returns stand-level carbon estimates with optional error propagation.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param name species name, either binomial or common (single value for stand)
#' @param dbh diameter at breast height in centimetres (vector of all tree measurements)
#' @param height tree height in metres (vector of all tree measurements, same length as dbh)
#' @param area_ha area of the stand/plot in hectares
#' @param type either 'broadleaf' or 'conifer' (optional, will be looked up if not provided)
#' @param method method of converting biomass to carbon. Default \code{"Matthews2"}
#'   (recommended for WCC). Also: \code{"Thomas"}, \code{"IPCC2"}.
#' @param biome temperate, boreal, mediterranean, tropical, subtropical
#' @param output.all if TRUE outputs all intermediate values, else outputs carbon estimate only
#' @param re_dbh relative measurement error for diameter at breast height (optional)
#' @param re_h relative error of height measurement (optional)
#' @param re relative error of coefficients (default = 0.025)
#' @param nsg nominal specific gravity (optional, will use WCC default if not provided)
#' @param sig_nsg sigma for nominal specific gravity (default = 0.094)
#' @param n_trees number of largest trees per hectare to use for top height (default = 100, as per WCC)
#' @param rich_output Logical. If TRUE, returns a rich result object with metadata
#' @return If \code{rich_output = FALSE}: stand-level carbon in tonnes, or if
#'   output.all = TRUE, a data.frame with all intermediate values and uncertainties.
#'   If \code{rich_output = TRUE}: an \code{allometry_result} object with metadata.
#' @details
#'   This function implements WCC Method D (stand-level assessment) which is used
#'   when you have tree measurements from a stand but want to calculate carbon at
#'   the stand level rather than for individual trees.
#'
#'   **Method D Workflow:**
#'   1. Calculate top height (mean height of 100 largest trees/ha by DBH)
#'   2. Calculate stand tariff from top height
#'   3. Calculate mean DBH from all trees
#'   4. Calculate mean merchantable volume using mean DBH and stand tariff
#'   5. Calculate mean stem volume, biomass, and carbon
#'   6. Scale to stand level (multiply by trees per hectare and stand area)
#'
#'   **When to use:**
#'   - You have DBH and height measurements for trees in a stand/plot
#'   - You want stand-level carbon estimates (not individual tree carbon)
#'   - You're doing Method D assessment per WCC protocol
#'
#'   **When NOT to use:**
#'   - You want individual tree carbon estimates → Use \code{fc_agc()} or \code{fc_agc_error()}
#'   - You already have top height and mean DBH measured directly → Use lower-level functions
#'   - You have all trees measured and want full inventory → Use \code{fc_agc_error()} (Method E)
#'
#'   **Note:** If you already have top height and mean DBH measured directly in the field,
#'   you can use the lower-level functions directly: \code{stand_tariff()} → \code{merchtreevol()}
#'   → \code{treevol()} → \code{woodbiomass()} → \code{crownbiomass()} → \code{biomass2c()}
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 4.1.4, Method D.
#' @examples
#' # Example 1: Stand with known area
#' dbh <- c(45, 52, 38, 60, 42, 55, 48, 35, 58, 40)  # 10 trees
#' height <- c(18, 22, 15, 24, 17, 21, 19, 14, 23, 16)
#' area_ha <- 0.1  # 0.1 hectare plot
#'
#' # Stand carbon without uncertainty
#' stand_carbon <- fc_stand_carbon(name = "Oak", dbh = dbh,
#'   height = height,
#'   area_ha = area_ha,
#'   type = "broadleaf",
#'   output.all = FALSE
#' )
#'
#' # Stand carbon with uncertainty
#' stand_carbon_error <- fc_stand_carbon(
#'   name = "Oak",
#'   dbh = dbh,
#'   height = height,
#'   area_ha = area_ha,
#'   type = "broadleaf",
#'   re_dbh = 0.05,
#'   re_h = 0.05,
#'   output.all = TRUE
#' )
#'
#' # Example 2: Larger stand (1 hectare)
#' # Simulate 200 trees
#' dbh_large <- runif(200, 20, 60)
#' height_large <- runif(200, 10, 25)
#'
#' stand_carbon_large <- fc_stand_carbon("Sitka Spruce", dbh_large, height_large,
#'   area_ha = 1.0, type = "conifer", re_dbh = 0.05, re_h = 0.05)
#'
#' # Rich output
#' fc_stand_carbon("Sitka Spruce", dbh_large, height_large,
#'   area_ha = 1.0, type = "conifer", re_dbh = 0.05, re_h = 0.05,
#'   rich_output = TRUE)
#' @export
#' @aliases fc_stand_carbon
#'
fc_stand_carbon <- function(name, dbh, height, area_ha, type = NULL,
                            method = "Matthews2", biome = "temperate",
                            output.all = TRUE, re_dbh = NULL, re_h = NULL,
                            re = 0.025, nsg = NULL, sig_nsg = 0.09413391,
                            n_trees = 100, rich_output = FALSE) {

  # ==== Check arguments ====
  if (!is.character(name) || length(name) != 1) {
    stop("name must be a single character value (species name for the stand)")
  }
  if (!is.numeric(dbh) || any(dbh < 0, na.rm = TRUE)) {
    stop("dbh must be numeric and non-negative")
  }
  if (!is.numeric(height) || any(height < 0, na.rm = TRUE)) {
    stop("height must be numeric and non-negative")
  }
  if (length(dbh) != length(height)) {
    stop("dbh and height must have the same length")
  }
  if (!is.numeric(area_ha) || length(area_ha) != 1 || area_ha <= 0) {
    stop("area_ha must be a single positive numeric value")
  }
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE)")
  }
  if (!(method %in% c("Matthews2", "IPCC2", "Thomas"))) {
    stop("Invalid method. Choose from: 'Matthews2', 'IPCC2', 'Thomas'")
  }
  biomes <- c("tropical", "subtropical", "mediterranean", "temperate", "boreal")
  if (!(biome %in% biomes)) {
    stop("Invalid biome. Choose from: ", paste(biomes, collapse = ", "))
  }
  if (!is.null(re_dbh) && (!is.numeric(re_dbh) || re_dbh < 0)) {
    stop("re_dbh must be numeric and non-negative")
  }
  if (!is.null(re_h) && (!is.numeric(re_h) || re_h < 0)) {
    stop("re_h must be numeric and non-negative")
  }
  if (!is.numeric(n_trees) || n_trees <= 0) {
    stop("n_trees must be numeric and positive")
  }

  # ==== Validate against WCC method ranges ====
  validation <- validate_inputs_for_method("WCC", dbh = dbh, height = height)
  flags <- if (validation$flags[1] != "None") validation$flags else character()
  warnings_list <- if (validation$validity_warnings[1] != "None") validation$validity_warnings else character()

  # ==== Lookup species code and type ====
  spcodes <- lookupcode(name, type, code = 'short')
  spcode <- spcodes$code[1]  # Single species for stand
  rec <- lookup_df[match(spcode, lookup_df$short), ]
  type <- ifelse(is.na(rec$type) | rec$type == "MX", type, rec$type)
  if (is.na(type)) {
    stop("type must be specified as 'broadleaf' or 'conifer'")
  }

  # ==== Helper: extract value and sigma from function results ====
  extract_val_sigma <- function(res, val_name) {
    if (is.null(res)) return(list(val = NULL, sig = NULL))
    if (is.atomic(res)) return(list(val = res, sig = NULL))
    sig <- res$sigma
    if (is.null(sig) && "sig_AGC" %in% names(res)) sig <- res$sig_AGC
    val <- if (val_name %in% names(res)) res[[val_name]] else res[[1L]]
    list(val = val, sig = sig)
  }
  null_or <- function(x, y) if (is.null(x)) y else x

  # ==== Calculate trees per hectare ====
  n_measured <- length(dbh)
  trees_per_ha <- n_measured / area_ha

  # ==== Calculate top height (mean of 100 largest trees/ha by DBH) ====
  top_h_result <- calculate_top_height(dbh, height, area_ha = area_ha,
                                       re_h = re_h, n_trees = n_trees)
  th <- extract_val_sigma(top_h_result, "top_height")
  top_height <- null_or(th$val, top_h_result)
  top_h_sigma <- th$sig
  top_h_re_h <- if (is.data.frame(top_h_result) && "re_h" %in% names(top_h_result))
    top_h_result$re_h else re_h

  # ==== Calculate mean DBH ====
  valid_dbh <- dbh[!is.na(dbh) & dbh > 0]
  if (length(valid_dbh) == 0) stop("No valid DBH measurements found")
  mean_dbh <- mean(valid_dbh, na.rm = TRUE)
  mean_dbh_sigma <- if (!is.null(re_dbh)) (re_dbh * mean_dbh) / sqrt(length(valid_dbh)) else NULL

  # ==== Calculate stand tariff from top height ====
  tariff_result <- stand_tariff(spcode, top_height, re_h = top_h_re_h, re = re)
  tr <- extract_val_sigma(tariff_result, "tariff")
  stand_tariff_val <- null_or(tr$val, tariff_result)
  tariff_sigma <- tr$sig

  # ==== Calculate mean merchantable volume ====
  mercvol_result <- merchtreevol(mean_dbh, stand_tariff_val,
                                 re_dbh = re_dbh, sig_tariff = tariff_sigma, re = re)
  mv <- extract_val_sigma(mercvol_result, "volume")
  mean_mercvol <- null_or(mv$val, mercvol_result)
  mercvol_sigma <- mv$sig

  # ==== Calculate mean stem volume ====
  stemvol_result <- treevol(mean_mercvol, dbh = mean_dbh,
                            sig_mtreevol = mercvol_sigma, re = re)
  sv <- extract_val_sigma(stemvol_result, "stemvolume")
  mean_stemvol <- null_or(sv$val, stemvol_result)
  stemvol_sigma <- sv$sig

  # ==== Calculate mean stem biomass ====
  if (is.null(nsg)) {
    nsg <- rec$NSG[1]
    if (is.na(nsg)) stop("NSG not found for species. Please provide nsg parameter.")
  }
  woodbio_result <- woodbiomass(mean_stemvol, nsg,
                                sig_treevol = stemvol_sigma, sig_nsg = sig_nsg)
  wb <- extract_val_sigma(woodbio_result, "woodbiomass")
  mean_woodbio <- null_or(wb$val, woodbio_result)
  woodbio_sigma <- wb$sig

  # ==== Calculate mean crown biomass ====
  crownbio_result <- crownbiomass(spcode, mean_dbh, re_d = re_dbh, re = re)
  cb <- extract_val_sigma(crownbio_result, "biomass")
  mean_crownbio <- null_or(cb$val, crownbio_result)
  crownbio_sigma <- cb$sig

  # ==== Calculate mean above-ground biomass and convert to carbon ====
  mean_AGB <- mean_woodbio + mean_crownbio
  AGB_sigma <- if (!is.null(woodbio_sigma) && !is.null(crownbio_sigma))
    sqrt(woodbio_sigma^2 + crownbio_sigma^2) else NULL

  carbon_result <- biomass2c(mean_AGB, method, type, biome, sig_biomass = AGB_sigma)
  cr <- extract_val_sigma(carbon_result, "AGC")
  mean_AGC <- null_or(cr$val, carbon_result)
  AGC_sigma <- cr$sig

  # ==== Scale to stand level ====
  stand_AGC <- mean_AGC * trees_per_ha * area_ha
  stand_AGC_sigma <- if (!is.null(AGC_sigma)) AGC_sigma * trees_per_ha * area_ha else NULL

  # ==== Create results table ====
  has_uncertainty <- !is.null(stand_AGC_sigma)
  r <- if (output.all) {
    out <- data.frame(
      name = name, spcode = spcode, type = type, area_ha = area_ha,
      trees_per_ha = trees_per_ha, n_trees_measured = n_measured,
      top_height_m = top_height, mean_dbh_cm = mean_dbh,
      stand_tariff = stand_tariff_val, mean_mercvol_m3 = mean_mercvol,
      mean_stemvol_m3 = mean_stemvol, mean_woodbiomass_t = mean_woodbio,
      mean_crownbiomass_t = mean_crownbio, mean_AGB_t = mean_AGB,
      mean_AGC_t = mean_AGC, stand_AGC_t = stand_AGC,
      stringsAsFactors = FALSE
    )
    if (has_uncertainty) {
      out$sig_top_height <- null_or(top_h_sigma, NA_real_)
      out$sig_mean_dbh <- null_or(mean_dbh_sigma, NA_real_)
      out$sig_tariff <- null_or(tariff_sigma, NA_real_)
      out$sig_mercvol <- null_or(mercvol_sigma, NA_real_)
      out$sig_stemvol <- null_or(stemvol_sigma, NA_real_)
      out$sig_woodbiomass <- null_or(woodbio_sigma, NA_real_)
      out$sig_crownbiomass <- null_or(crownbio_sigma, NA_real_)
      out$sig_AGB <- null_or(AGB_sigma, NA_real_)
      out$sig_AGC <- null_or(AGC_sigma, NA_real_)
      out$sig_stand_AGC <- stand_AGC_sigma
    }
    out
  } else {
    out <- data.frame(name = name, spcode = spcode, stand_AGC_t = stand_AGC,
                      stringsAsFactors = FALSE)
    if (has_uncertainty) out$sig_stand_AGC <- stand_AGC_sigma
    out
  }

  # ==== Return rich output if requested ====
  if (rich_output) {
    # Get method metadata (use get_method_metadata for correct reference/citation mapping)
    wcc_meta <- get_method_metadata("WCC")

    result <- list(
      # === Core estimate ===
      value = stand_AGC,
      measure = "AGC",
      unit = "t",

      # === Method information ===
      method = "WCC",
      method_full_name = wcc_meta$full_name,
      reference = wcc_meta$reference,
      reference_short = wcc_meta$reference_short,
      doi = wcc_meta$doi,
      source_type = wcc_meta$source_type,
      region = wcc_meta$region,
      biome = paste(wcc_meta$biome, collapse = "; "),

      # === Assumptions ===
      assumptions = wcc_meta$assumptions,

      # === Uncertainty ===
      uncertainty = stand_AGC_sigma,
      uncertainty_method = wcc_meta$uncertainty_method,
      ci_low = if (!is.null(stand_AGC_sigma)) stand_AGC - 1.96 * stand_AGC_sigma else NULL,
      ci_high = if (!is.null(stand_AGC_sigma)) stand_AGC + 1.96 * stand_AGC_sigma else NULL,
      ci_level = if (!is.null(stand_AGC_sigma)) 0.95 else NA,

      # === Validity and flags ===
      validity_warnings = if (length(warnings_list) > 0) warnings_list else "None",
      flags = if (length(flags) > 0) flags else "None",

      # === Valid ranges ===
      valid_dbh_range = wcc_meta$dbh_range,
      valid_height_range = wcc_meta$height_range,
      height_required = wcc_meta$height_required,
      species_specific = wcc_meta$species_specific,
      wood_density_required = wcc_meta$wood_density_required,

      # === Inputs ===
      inputs = list(
        name = name,
        area_ha = area_ha,
        n_trees_measured = n_measured,
        type = type,
        biome = biome,
        method = method,
        re_dbh = re_dbh,
        re_h = re_h
      ),

      # === Stand-level details ===
      stand_details = list(
        top_height = top_height,
        mean_dbh = mean_dbh,
        trees_per_ha = trees_per_ha,
        mean_AGC_per_tree = mean_AGC
      ),

      # === Detailed output ===
      detailed_output = if (output.all) r else NULL
    )
    class(result) <- "allometry_result"
    return(result)
  }

  return(r)
}


############# WCC Stand-Level Helper Functions ################
#'
#' @title Mean tariff from sample trees (WCC)
#' @description Calculate mean stand tariff from sample tree measurements with
#'   proper uncertainty propagation. Useful for scaling up from sample plots.
#' @param spcode Species code(s) for sample trees
#' @param height Tree height(s) in metres
#' @param dbh Diameter at breast height in cm
#' @param type Tree type: "broadleaf" or "conifer"
#' @param height_timber Timber height for broadleaves (optional)
#' @param re_h Relative error of height (optional, for uncertainty)
#' @param re_dbh Relative error of DBH (optional, for uncertainty)
#' @param re Relative error of coefficients (default 0.025)
#' @return Mean tariff value, or if re_h/re_dbh provided a list with mean_tariff
#'   and sigma (standard error of the mean)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @export
wcc_mean_tariff <- function(spcode, height, dbh, type = NULL,
                            height_timber = NULL, re_h = NULL, re_dbh = 0.05,
                            re = 0.025) {
  tariff_out <- tariffs(spcode, height, dbh, type = type,
                        height_timber = height_timber,
                        re_h = re_h, re_dbh = re_dbh, re = re)
  if (is.data.frame(tariff_out)) {
    valid <- !is.na(tariff_out$tariff)
    if (!any(valid)) stop("No valid tariff values")
    mean_tariff <- mean(tariff_out$tariff[valid])
    sigma <- if ("sigma" %in% names(tariff_out) && any(!is.na(tariff_out$sigma[valid])))
      sqrt(sum(tariff_out$sigma[valid]^2, na.rm = TRUE)) / sum(valid) else NULL
    if (!is.null(sigma)) {
      return(list(mean_tariff = mean_tariff, sigma = sigma))
    }
    return(mean_tariff)
  }
  valid <- !is.na(tariff_out)
  if (!any(valid)) stop("No valid tariff values")
  mean(tariff_out[valid], na.rm = TRUE)
}

#'
#' @title Scale carbon to per-hectare values (WCC)
#' @description Scale per-tree carbon or stand carbon to per-hectare values.
#'   For project-level scaling from sample plots. Propagates uncertainty when
#'   \code{sig_carbon_per_tree} or \code{sig_carbon_per_ha} is provided.
#' @param carbon_per_tree Numeric. Carbon per tree in tonnes (or vector of per-tree values;
#'   vectors are averaged before scaling)
#' @param trees_per_ha Numeric. Number of trees per hectare
#' @param area_ha Numeric. Stand area in hectares (optional, for total carbon)
#' @param sig_carbon_per_tree Numeric. Uncertainty (SD) in carbon per tree in tonnes.
#'   If \code{carbon_per_tree} is a vector, provide the SE of the mean. Optional.
#' @param sig_carbon_per_ha Numeric. Uncertainty (SD) in carbon per hectare (t/ha).
#'   Used instead of \code{sig_carbon_per_tree} if both provided. Optional.
#' @return If \code{area_ha} is NULL: carbon per hectare (t/ha), or if sigma provided,
#'   list with \code{carbon_per_ha}, \code{sig_carbon_per_ha}. If \code{area_ha} given:
#'   list with \code{carbon_per_ha}, \code{total_carbon}, and when sigma provided
#'   \code{sig_carbon_per_ha}, \code{sig_total_carbon}.
#' @details Uncertainty propagation: linear scaling. For \code{Y = k * X},
#'   \eqn{\sigma_Y = |k| \sigma_X}. So \code{sig_carbon_per_ha = sig_carbon_per_tree * trees_per_ha}
#'   and \code{sig_total_carbon = sig_carbon_per_ha * area_ha}.
#' @seealso \code{\link{wcc_stratify}}, \code{\link{fc_agc_error}}; vignette("uncertainty-and-comparison",
#'   package = "TreeCarbon") for error propagation rules.
#' @export
wcc_per_hectare <- function(carbon_per_tree, trees_per_ha, area_ha = NULL,
                            sig_carbon_per_tree = NULL, sig_carbon_per_ha = NULL) {
  if (length(carbon_per_tree) > 1) {
    carbon_per_tree <- mean(carbon_per_tree, na.rm = TRUE)
  }
  carbon_per_ha <- carbon_per_tree * trees_per_ha
  sig_per_ha <- NULL
  if (!is.null(sig_carbon_per_ha)) {
    sig_per_ha <- sig_carbon_per_ha
  } else if (!is.null(sig_carbon_per_tree)) {
    sig_per_ha <- sig_carbon_per_tree * trees_per_ha
  }
  if (is.null(area_ha)) {
    if (!is.null(sig_per_ha)) {
      return(list(carbon_per_ha = carbon_per_ha, sig_carbon_per_ha = sig_per_ha))
    }
    return(carbon_per_ha)
  }
  total_carbon <- carbon_per_ha * area_ha
  out <- list(carbon_per_ha = carbon_per_ha, total_carbon = total_carbon)
  if (!is.null(sig_per_ha)) {
    out$sig_carbon_per_ha <- sig_per_ha
    out$sig_total_carbon <- sig_per_ha * area_ha
  }
  out
}

#'
#' @title Combine carbon estimates from stratified sampling (WCC)
#' @description Aggregate carbon across strata using area-weighted averaging.
#'   For projects with multiple stands or strata. Propagates uncertainty when
#'   sigma columns are provided.
#' @param strata_data Data.frame with columns area_ha and either total_carbon_t
#'   or carbon_per_ha_t. Or a list of such data.frames. For uncertainty: include
#'   total_sig_carbon_t (when using total_carbon_t) or carbon_per_ha_sig_t
#'   (when using carbon_per_ha_t).
#' @param area_col Character. Name of area column (default "area_ha")
#' @param carbon_col Character. Name of carbon column - "total_carbon_t" or
#'   "carbon_per_ha_t" (default auto-detected)
#' @param sigma_col Character. Name of sigma column - "total_sig_carbon_t" or
#'   "carbon_per_ha_sig_t". Default auto-detected from strata_data.
#' @return List with total_area_ha, total_carbon_t, weighted_mean_carbon_per_ha_t.
#'   When sigma provided: total_sig_carbon_t, weighted_mean_sig.
#' @details Uncertainty propagation (assuming independent strata):
#'   \itemize{
#'     \item When using total_carbon_t: Var(total) = sum(sigma_total_i^2)
#'     \item When using carbon_per_ha_t: Var(total) = sum(area_i^2 * sigma_per_ha_i^2)
#'     \item sigma_weighted_mean = total_sig_carbon_t / total_area_ha
#'   }
#' @seealso \code{\link{wcc_per_hectare}}, \code{\link{fc_agc_error}}; vignette("uncertainty-and-comparison",
#'   package = "TreeCarbon") for error propagation rules.
#' @export
wcc_stratify <- function(strata_data, area_col = "area_ha",
                         carbon_col = NULL, sigma_col = NULL) {
  if (is.list(strata_data) && !is.data.frame(strata_data)) {
    strata_data <- do.call(rbind, strata_data)
  }
  if (!area_col %in% names(strata_data))
    stop("area_col '", area_col, "' not found in strata_data")
  area <- strata_data[[area_col]]
  total_area <- sum(area, na.rm = TRUE)
  if (total_area <= 0) stop("Total area must be positive")
  if (is.null(carbon_col)) {
    carbon_col <- if ("total_carbon_t" %in% names(strata_data)) "total_carbon_t"
      else if ("carbon_per_ha_t" %in% names(strata_data)) "carbon_per_ha_t"
      else stop("carbon column not found. Use total_carbon_t or carbon_per_ha_t")
  }
  carbon <- strata_data[[carbon_col]]
  if (carbon_col == "carbon_per_ha_t") {
    total_carbon <- sum(carbon * area, na.rm = TRUE)
  } else {
    total_carbon <- sum(carbon, na.rm = TRUE)
  }
  out <- list(
    total_area_ha = total_area,
    total_carbon_t = total_carbon,
    weighted_mean_carbon_per_ha_t = total_carbon / total_area
  )
  if (is.null(sigma_col)) {
    sigma_col <- if ("total_sig_carbon_t" %in% names(strata_data)) "total_sig_carbon_t"
      else if ("carbon_per_ha_sig_t" %in% names(strata_data)) "carbon_per_ha_sig_t"
      else NULL
  }
  if (!is.null(sigma_col) && sigma_col %in% names(strata_data)) {
    sig <- strata_data[[sigma_col]]
    if (sigma_col == "carbon_per_ha_sig_t") {
      total_sig <- sqrt(sum((area * sig)^2, na.rm = TRUE))
    } else {
      total_sig <- sqrt(sum(sig^2, na.rm = TRUE))
    }
    out$total_sig_carbon_t <- total_sig
    out$weighted_mean_sig <- total_sig / total_area
  }
  out
}

#'
#' @title Stand-level carbon (WCC Method D) - alias
#' @description Alias for \code{fc_stand_carbon}. Calculate stand-level above-ground
#'   carbon using WCC Method D (top height and mean DBH from sample trees).
#' @param ... Arguments passed to \code{fc_stand_carbon}
#' @return Same as \code{fc_stand_carbon}
#' @seealso \code{\link{fc_stand_carbon}}
#' @export
wcc_stand_carbon <- function(...) {
  fc_stand_carbon(...)
}


############# WCC Process DBH Class Tally ################
#'
#' @title Process DBH class tallies for WCC Method E
#' @description Helper function to process DBH class tallies (counts of trees in
#'   each DBH class) for use in WCC Method E (Full Tree Count). Converts DBH
#'   class tallies into a format suitable for carbon calculation, with
#'   representative DBH and height values for each class.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh_class DBH class values. Can be either:
#'   - Midpoints of DBH classes (e.g., c(5, 15, 25, 35) for 0-10, 10-20, 20-30, 30-40 cm classes)
#'   - Lower bounds of DBH classes (e.g., c(0, 10, 20, 30) for 0-10, 10-20, 20-30, 30-40 cm classes)
#' @param count Number of trees in each DBH class (vector, same length as dbh_class)
#' @param height Optional: Representative height(s) for each DBH class. Can be:
#'   - A single value (applied to all classes)
#'   - A vector of values (one per class)
#'   - NULL (heights will need to be estimated or provided later)
#' @param class_width Width of DBH classes in cm (default = 10). Used when
#'   dbh_class represents lower bounds to calculate midpoints.
#' @param use_midpoints Logical. If TRUE, treats dbh_class as midpoints.
#'   If FALSE, treats as lower bounds and calculates midpoints (default = TRUE).
#' @return A data.frame with columns:
#'   - dbh_class: DBH class identifier
#'   - dbh_midpoint: Representative DBH (midpoint) for the class in cm
#'   - count: Number of trees in the class
#'   - height: Representative height for the class in m (if provided)
#'   - dbh_lower: Lower bound of DBH class (if calculated)
#'   - dbh_upper: Upper bound of DBH class (if calculated)
#' @details
#'   This function is designed for WCC Method E (Full Tree Count) where trees
#'   are tallied by DBH class rather than measured individually. It processes
#'   the tally data to create representative trees for each class.
#'
#'   **DBH Class Format:**
#'   - If using midpoints: dbh_class = c(5, 15, 25, 35) represents classes
#'     0-10, 10-20, 20-30, 30-40 cm
#'   - If using lower bounds: dbh_class = c(0, 10, 20, 30) with class_width = 10
#'     will calculate midpoints as 5, 15, 25, 35 cm
#'
#'   **Height Handling:**
#'   - If height is provided, it will be used for carbon calculations
#'   - If height is NULL, it should be estimated using height-DBH relationships
#'     or measured for representative trees in each class
#'   - Heights can be provided per class or as a single value for all classes
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Method E.
#' @examples
#' # Example 1: DBH classes as midpoints
#' dbh_midpoints <- c(5, 15, 25, 35, 45)  # 0-10, 10-20, 20-30, 30-40, 40-50 cm
#' counts <- c(150, 200, 180, 120, 50)    # Trees in each class
#' heights <- c(3, 8, 15, 20, 22)         # Representative heights
#'
#' wcc_process_tally(dbh_class = dbh_midpoints, count = counts,
#'                   height = heights, use_midpoints = TRUE)
#'
#' # Example 2: DBH classes as lower bounds
#' dbh_lower <- c(0, 10, 20, 30, 40)
#' counts <- c(150, 200, 180, 120, 50)
#'
#' wcc_process_tally(dbh_class = dbh_lower, count = counts,
#'                   class_width = 10, use_midpoints = FALSE)
#'
#' # Example 3: Without heights (to be estimated later)
#' wcc_process_tally(dbh_class = c(5, 15, 25, 35), count = c(100, 150, 120, 80))
#'
#' @export
#' @aliases wcc_process_tally
#'
wcc_process_tally <- function(dbh_class, count, height = NULL,
                              class_width = 10, use_midpoints = TRUE) {

  # ==== Input validation ====
  if (!is.numeric(dbh_class) || any(dbh_class < 0, na.rm = TRUE)) {
    stop("dbh_class must be numeric and non-negative")
  }
  if (!is.numeric(count) || any(count < 0, na.rm = TRUE)) {
    stop("count must be numeric and non-negative")
  }
  if (length(dbh_class) != length(count)) {
    stop("dbh_class and count must have the same length")
  }
  if (!is.numeric(class_width) || class_width <= 0) {
    stop("class_width must be numeric and positive")
  }
  if (!is.logical(use_midpoints) || length(use_midpoints) != 1) {
    stop("use_midpoints must be a single logical value")
  }

  # ==== Calculate DBH midpoints ====
  if (use_midpoints) {
    dbh_midpoint <- dbh_class
    dbh_lower <- dbh_class - class_width / 2
    dbh_upper <- dbh_class + class_width / 2
  } else {
    # Treat as lower bounds
    dbh_lower <- dbh_class
    dbh_upper <- dbh_class + class_width
    dbh_midpoint <- dbh_class + class_width / 2
  }

  # ==== Handle height input ====
  if (!is.null(height)) {
    if (!is.numeric(height) || any(height < 0, na.rm = TRUE)) {
      stop("height must be numeric and non-negative")
    }
    if (length(height) == 1) {
      # Single value - apply to all classes
      height_vec <- rep(height, length(dbh_class))
    } else if (length(height) == length(dbh_class)) {
      # One value per class
      height_vec <- height
    } else {
      stop("height must be NULL, a single value, or a vector of length equal to dbh_class")
    }
  } else {
    height_vec <- rep(NA_real_, length(dbh_class))
  }

  # ==== Create output data frame ====
  result <- data.frame(
    dbh_class = seq_along(dbh_class),
    dbh_midpoint = dbh_midpoint,
    count = count,
    height = height_vec,
    dbh_lower = dbh_lower,
    dbh_upper = dbh_upper,
    stringsAsFactors = FALSE
  )

  return(result)
}

############# WCC DBH Class Carbon Calculation ################
#'
#' @title Calculate carbon from DBH class distribution (WCC Method E)
#' @description Calculates total stand carbon from DBH class tallies using WCC
#'   Method E (Full Tree Count). This function takes processed DBH class tallies
#'   and calculates carbon for each class, then sums to get total stand carbon.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param name Species name(s), either binomial or common. Can be:
#'   - Single value (applied to all classes)
#'   - Vector (one per class, same length as tally data)
#' @param tally_data Data frame from \code{wcc_process_tally()} containing DBH
#'   class information, or a data frame with columns: dbh_midpoint, count, height
#' @param type Either 'broadleaf' or 'conifer' (single value or vector, same length as tally_data)
#' @param area_ha Area of the stand/plot in hectares. If NULL, assumes data
#'   represents trees per hectare (i.e., counts are already per hectare).
#' @param method Method of converting biomass to carbon. Default \code{"Matthews2"}
#'   (recommended for WCC). Also: \code{"Thomas"}, \code{"IPCC2"}.
#' @param biome Temperate, boreal, mediterranean, tropical, subtropical
#' @param output.all If TRUE, outputs carbon per class and total. If FALSE, outputs total only.
#' @param re_dbh Relative measurement error for diameter at breast height (single value).
#' @param re_h Relative error of height measurement (single value).
#' @param re Relative error of coefficients (default = 0.025).
#' @param nsg Nominal specific gravity. Optionally specified, else will use that
#'   given by the WCC.
#' @param sig_nsg Sigma for nominal specific gravity (NSG) or wood density.
#' @param height_timber Optional: Timber height(s) for broadleaf species. Can be
#'   a single value or vector matching tally_data rows.
#' @param timber_ratio Ratio for estimating timber height from total height when
#'   height_timber is not provided (default = 0.85).
#' @param rich_output Logical. If TRUE, returns a rich result object with
#'   metadata. Default FALSE.
#' @return If \code{rich_output = FALSE}: either total stand AGC in tonnes,
#'   or if output.all = TRUE, a data.frame with carbon per class and total.
#'   If \code{rich_output = TRUE}: an \code{allometry_result} object with metadata.
#' @details
#'   This function implements WCC Method E (Full Tree Count) for stands where
#'   trees are tallied by DBH class rather than measured individually. It:
#'   1. Calculates carbon for each DBH class using representative DBH and height
#'   2. Multiplies by the number of trees in each class
#'   3. Sums across all classes to get total stand carbon
#'
#'   **Input Requirements:**
#'   - DBH class midpoints (representative DBH for each class)
#'   - Counts of trees in each class
#'   - Heights (either provided or will be estimated)
#'   - Species information
#'
#'   **Height Handling:**
#'   - If heights are missing, they should be estimated using height-DBH
#'     relationships or measured for representative trees
#'   - For broadleaves, timber height is required (or will be estimated)
#'   - For conifers, total height is used
#'
#'   **Scaling:**
#'   - If area_ha is provided, counts are assumed to be for that area
#'   - If area_ha is NULL, counts are assumed to already represent trees per hectare
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Method E.
#' @examples
#' # Example 1: Process tally and calculate carbon
#' dbh_midpoints <- c(5, 15, 25, 35, 45)
#' counts <- c(150, 200, 180, 120, 50)
#' heights <- c(3, 8, 15, 20, 22)
#'
#' # Process tally
#' tally_data <- wcc_process_tally(
#'   dbh_class = dbh_midpoints,
#'   count = counts,
#'   height = heights
#' )
#'
#' # Calculate carbon
#' carbon_result <- wcc_dbh_class_carbon(
#'   name = "Oak",
#'   tally_data = tally_data,
#'   type = "broadleaf",
#'   area_ha = 0.1,
#'   output.all = TRUE
#' )
#' print(carbon_result)
#'
#' # Example 2: With uncertainty propagation
#' carbon_with_error <- wcc_dbh_class_carbon(
#'   name = "Sitka Spruce",
#'   tally_data = tally_data,
#'   type = "conifer",
#'   area_ha = 0.1,
#'   re_dbh = 0.05,
#'   re_h = 0.1,
#'   output.all = TRUE
#' )
#' print(carbon_with_error)
#' @export
#' @aliases wcc_dbh_class_carbon
#'
wcc_dbh_class_carbon <- function(name, tally_data, type = NULL, area_ha = NULL,
                                 method = "Matthews2", biome = "temperate",
                                 output.all = TRUE, re_dbh = NULL, re_h = NULL,
                                 re = 0.025, nsg = NULL, sig_nsg = 0.09413391,
                                 height_timber = NULL, timber_ratio = 0.85,
                                 rich_output = FALSE) {

  # ==== Input validation ====
  if (!is.character(name)) {
    stop("name must be a character vector")
  }
  if (!is.data.frame(tally_data)) {
    stop("tally_data must be a data frame (use wcc_process_tally() to create)")
  }
  required_cols <- c("dbh_midpoint", "count")
  if (!all(required_cols %in% colnames(tally_data))) {
    stop("tally_data must contain columns: ", paste(required_cols, collapse = ", "))
  }

  n_classes <- nrow(tally_data)

  # Handle name input (single value or vector)
  if (length(name) == 1) {
    name_vec <- rep(name, n_classes)
  } else if (length(name) == n_classes) {
    name_vec <- name
  } else {
    stop("name must be a single value or a vector of length equal to number of classes in tally_data")
  }

  # Handle type input
  if (!is.null(type)) {
    if (length(type) == 1) {
      type_vec <- rep(type, n_classes)
    } else if (length(type) == n_classes) {
      type_vec <- type
    } else {
      stop("type must be NULL, a single value, or a vector of length equal to number of classes")
    }
  } else {
    type_vec <- rep(NA_character_, n_classes)
  }

  # Extract data from tally_data
  dbh_rep <- tally_data$dbh_midpoint
  counts <- tally_data$count
  heights <- if ("height" %in% colnames(tally_data)) tally_data$height else rep(NA_real_, n_classes)

  # Check for missing heights
  if (any(is.na(heights))) {
    warning("Some DBH classes have missing heights. Heights should be measured for representative trees or estimated using height-DBH relationships.")
  }

  # ==== Calculate carbon for each class ====
  # Use fc_agc_error if uncertainty is requested, otherwise fc_agc
  calc_uncertainty <- !is.null(re_dbh) || !is.null(re_h)

  if (calc_uncertainty) {
    # Calculate with uncertainty
    carbon_per_class <- fc_agc_error(
      name = name_vec,
      dbh = dbh_rep,
      height = heights,
      type = type_vec,
      method = method,
      biome = biome,
      output.all = TRUE,
      re_dbh = re_dbh,
      re_h = re_h,
      re = re,
      nsg = nsg,
      sig_nsg = sig_nsg,
      height_timber = height_timber,
      timber_ratio = timber_ratio,
      rich_output = FALSE
    )
  } else {
    # Calculate without uncertainty
    carbon_per_class <- fc_agc(
      name = name_vec,
      dbh = dbh_rep,
      height = heights,
      type = type_vec,
      method = method,
      biome = biome,
      output.all = TRUE,
      nsg = nsg,
      height_timber = height_timber,
      timber_ratio = timber_ratio,
      rich_output = FALSE
    )
  }

  # ==== Scale by tree counts and sum ====
  # Carbon per tree * number of trees in class
  if (calc_uncertainty && "sig_AGC" %in% colnames(carbon_per_class)) {
    carbon_per_class$class_carbon_t <- carbon_per_class$AGC_WCC_t * counts
    carbon_per_class$class_sig_carbon_t <- carbon_per_class$sig_AGC * counts
  } else {
    carbon_per_class$class_carbon_t <- carbon_per_class$AGC_WCC_t * counts
    carbon_per_class$class_sig_carbon_t <- NA_real_
  }

  # Calculate total carbon
  total_carbon_t <- sum(carbon_per_class$class_carbon_t, na.rm = TRUE)

  # Calculate total uncertainty (sum of variances)
  if (calc_uncertainty && !all(is.na(carbon_per_class$class_sig_carbon_t))) {
    total_sig_carbon_t <- sqrt(sum(carbon_per_class$class_sig_carbon_t^2, na.rm = TRUE))
  } else {
    total_sig_carbon_t <- NA_real_
  }

  # ==== Scale by area if provided ====
  if (!is.null(area_ha)) {
    if (!is.numeric(area_ha) || length(area_ha) != 1 || area_ha <= 0) {
      stop("area_ha must be a single positive numeric value")
    }
    # If counts are for the entire area, they're already correct
    # If counts are per hectare, multiply by area
    # We assume counts are for the given area_ha
    total_carbon_t <- total_carbon_t  # Already scaled by counts
    if (!is.na(total_sig_carbon_t)) {
      total_sig_carbon_t <- total_sig_carbon_t
    }
  }

  # ==== Prepare output ====
  if (output.all) {
    result <- data.frame(
      dbh_class = tally_data$dbh_class,
      dbh_midpoint = dbh_rep,
      count = counts,
      height = heights,
      carbon_per_tree_t = carbon_per_class$AGC_WCC_t,
      class_carbon_t = carbon_per_class$class_carbon_t,
      stringsAsFactors = FALSE
    )

    if (calc_uncertainty && !all(is.na(carbon_per_class$class_sig_carbon_t))) {
      result$sig_carbon_per_tree_t <- carbon_per_class$sig_AGC
      result$sig_class_carbon_t <- carbon_per_class$class_sig_carbon_t
    }

    result$total_carbon_t <- total_carbon_t
    if (!is.na(total_sig_carbon_t)) {
      result$total_sig_carbon_t <- total_sig_carbon_t
    } else {
      result$total_sig_carbon_t <- NA_real_
    }
  } else {
    result <- data.frame(
      total_carbon_t = total_carbon_t,
      stringsAsFactors = FALSE
    )
    if (!is.na(total_sig_carbon_t)) {
      result$total_sig_carbon_t <- total_sig_carbon_t
    }
  }

  # ==== Return rich output if requested ====
  if (rich_output) {
    wcc_meta <- method_metadata[method_metadata$method == "WCC", ]
    wcc_assumptions <- method_assumptions$assumption[method_assumptions$method == "WCC"]

    rich_result <- list(
      value = total_carbon_t,
      measure = "Total Stand AGC",
      unit = "t",
      method = "WCC",
      method_full_name = wcc_meta$full_name,
      reference = wcc_meta$reference,
      reference_short = wcc_meta$reference_short,
      doi = wcc_meta$doi,
      source_type = wcc_meta$source_type,
      region = wcc_meta$region,
      biome = strsplit(wcc_meta$biome, "; ")[[1]],
      assumptions = wcc_assumptions,
      uncertainty = if (!is.na(total_sig_carbon_t)) total_sig_carbon_t else NULL,
      uncertainty_method = wcc_meta$uncertainty_method,
      ci_low = if (!is.na(total_sig_carbon_t)) total_carbon_t - 1.96 * total_sig_carbon_t else NULL,
      ci_high = if (!is.na(total_sig_carbon_t)) total_carbon_t + 1.96 * total_sig_carbon_t else NULL,
      ci_level = if (!is.na(total_sig_carbon_t)) 0.95 else NA,
      validity_warnings = "None",
      flags = "None",
      valid_dbh_range = c(wcc_meta$dbh_min_cm, wcc_meta$dbh_max_cm),
      valid_height_range = c(wcc_meta$height_min_m, wcc_meta$height_max_m),
      height_required = wcc_meta$height_required,
      species_specific = wcc_meta$species_specific,
      wood_density_required = wcc_meta$wood_density_required,
      inputs = list(
        name = unique(name_vec),
        n_classes = n_classes,
        area_ha = area_ha,
        type = unique(type_vec),
        biome = biome,
        method = method
      ),
      detailed_output = if (output.all) result else NULL
    )
    class(rich_result) <- "allometry_result"
    return(rich_result)
  }

  return(result)
}
