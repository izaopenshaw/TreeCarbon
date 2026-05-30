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
#' @param name species name, either binomial or common. A single value is recycled when
#'   \code{dbh} and \code{height} are vectors (same species, one row per tree).
#' @param type either 'broadleaf' or 'conifer'. A single value is recycled when
#'   \code{dbh} and \code{height} are vectors.
#' @param dbh diameter at breast height in centimetres
#' @param height tree height in metres (total height). For broadleaves, provide \code{height_timber}
#'   for WCC protocol compliance. Not required when \code{tariff} is provided (e.g. for QMD + mean
#'   tariff stand-level use).
#' @param height_timber timber height in metres (height to first branch). Required for WCC protocol
#'   for broadleaf species. If not provided, will be estimated using \code{timber_ratio}.
#' @param timber_ratio ratio for estimating timber height from total height when \code{height_timber}
#'   is not provided. Can be either a numeric value (0-1, default = 0.85) or the string "estimate"
#'   to use species-specific ratios via \code{estimate_timber_height()}. Only used for broadleaf species.
#' @param tariff optional pre-computed tariff number. When provided, skips internal tariff
#'   calculation from height. Use for stand-level estimates with QMD + mean tariff from height
#'   sample trees. Scalar or same length as \code{dbh}. When provided, \code{height} is not
#'   required for trees with dbh >= 7 cm.
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
#' @details
#'   **When to use \code{fc_agc}:**
#'   - Individual tree carbon estimates (one or more DBH/height pairs)
#'   - Full tree inventory with per-tree carbon
#'   - QMD + mean tariff from height sample trees (provide \code{tariff})
#'
#'   **When NOT to use \code{fc_agc}:**
#'   - Stand-level carbon from plot/stand DBH and height samples without
#'     pre-computed tariff \eqn{\rightarrow} use \code{\link{fc_stand_carbon}} or
#'     \code{\link{fc_method_e_carbon}}
#'   - You have top height and mean DBH and want Method D workflow \eqn{\rightarrow}
#'     use \code{\link{fc_stand_carbon}}
#'
#' @seealso \code{\link{fc_stand_carbon}} (Methods A-D), \code{\link{fc_method_e_carbon}} (Method E),
#'   \code{\link{fc_agc_error}} (with uncertainty propagation).
#' @import remotes
#' @examples
#' # Single tree
#' fc_agc("Oak", dbh = 45, height = 18, type = "broadleaf", output.all = FALSE)
#'
#' fc_agc('Quercus robur', dbh=74, height=24, output.all = FALSE)
#' # Input wood density and sd from BIOMASS package
#' wd <- BIOMASS::getWoodDensity('Quercus', 'robur', region='Europe')
#' fc_agc('beech', 72, 24, nsg = wd$meanWD)
#'
#' # QMD + mean tariff (stand-level approximation)
#' fc_agc("Sitka Spruce", dbh = 28, tariff = 22, type = "conifer", output.all = FALSE)
#'
#' # Rich output with metadata
#' result <- fc_agc('Oak', dbh=50, height=20, rich_output = TRUE)
#' print(result)
#' @export
#' @aliases fc_agc
#'
fc_agc <- function(name, dbh, height, height_timber = NULL, timber_ratio = 0.85,
                   tariff = NULL, type = NULL, method = "Matthews2", biome = "temperate",
                   output.all = TRUE, nsg = NULL, rich_output = FALSE){

  # ==== Check arguments ====
  if(!is.character(name)) stop ("name must be a character")
  if (!is.null(tariff)) {
    if (!is.numeric(tariff) || any(tariff < 0, na.rm = TRUE)) {
      stop("tariff must be numeric and non-negative when provided")
    }
    if (length(tariff) != 1L && length(tariff) != length(dbh)) {
      stop("tariff must be scalar or same length as dbh")
    }
  }
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
  dbh <- floor(dbh)

  n <- length(dbh)
  if (length(height) != n) {
    stop("'dbh' and 'height' must have the same length (one row per tree).")
  }
  if (length(name) == 1L && n > 1L) {
    name <- rep(name, n)
  } else if (length(name) != n) {
    stop("'name' must have length 1 or the same length as 'dbh' and 'height'.")
  }
  if (!is.null(type)) {
    if (length(type) == 1L && n > 1L) {
      type <- rep(type, n)
    } else if (length(type) != n) {
      stop("'type' must have length 1, be NULL, or the same length as 'dbh' and 'height'.")
    }
  }

  # ==== QMD-like usage warning (single dbh/height in typical stand range) ====
  if (length(dbh) == 1L && length(height) == 1L && is.null(tariff) &&
      !is.na(dbh) && !is.na(height) && dbh >= 20 && dbh <= 80 &&
      height >= 12 && height <= 35) {
    warning("Single DBH and height in typical stand range (QMD/top height). ",
            "For stand-level estimates, consider fc_stand_carbon() or fc_method_e_carbon().")
  }

  # ==== Validate against WCC method ranges ====
  validation <- validate_inputs_for_method("WCC", dbh = dbh, height = height)
  flags <- if (validation$flags[1] != "None") validation$flags else character()
  warnings_list <- if (validation$validity_warnings[1] != "None") validation$validity_warnings else character()

  # ==== Lookup species codes and type ====
  spcodes <- lookupcode(name, type, code = 'short')
  rec <- lookup_df[match(spcodes$code, lookup_df$short), ]
  type <- ifelse(is.na(rec$type) | rec$type == "MX", type, rec$type)

  n <- length(height)

  # ==== Create results table ====
  if(output.all){
    r <- data.frame(name=name, type=type, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, dbh=dbh, height=height,
                    height_timber = NA_real_,
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
  # Get indices for trees with dbh >= 7; height or tariff required
  use_provided_tariff <- !is.null(tariff)
  tall_id <- dbh >= 7 & !is.na(dbh) & (use_provided_tariff | !is.na(height))

  # Resolve tariff: use provided value or compute from height
  if (use_provided_tariff) {
    tariff_full <- rep_len(tariff, n)
    attr(tariff_full, "timber_height_used") <- rep(NA_real_, n)
    attr(tariff_full, "notes") <- character()
  } else {
    tariff_full <- tariffs(spcodes$code, height, dbh, type,
                           height_timber = height_timber, timber_ratio = timber_ratio)
  }
  tariff <- tariff_full[tall_id]
  if (output.all) {
    r$height_timber <- attr(tariff_full, "timber_height_used") %||% rep(NA_real_, n)
    notes <- attr(tariff_full, "notes") %||% character()
    if (length(notes) > 0L) {
      flags <- c(flags, notes)
      if (any(grepl("species_ratios", notes))) {
        warnings_list <- c(warnings_list,
          "For broadleaf species, WCC protocol requires timber height (height to first branch). Timber height was estimated from total height using species-specific ratios (or default 0.85 if not available). Timber height estimation is crude; measured values are required for WCC certification.")
      }
      if (any(grepl("fixed_ratio", notes))) {
        rat <- regmatches(notes, regexpr("fixed_ratio_[0-9.]+", notes))
        if (length(rat) > 0L) {
          rr <- sub("fixed_ratio_", "", rat[1L])
          warnings_list <- c(warnings_list,
            paste0("For broadleaf species, WCC protocol requires timber height (height to first branch). Timber height was estimated from total height using ratio of ", rr, ". Timber height estimation is crude; measured values are required for WCC certification."))
        }
      }
      if (any(type == "broadleaf" & !is.na(height) & height > 20, na.rm = TRUE)) {
        flags <- c(flags, "Timber_height_estimated_for_mature_trees_over_20m")
        warnings_list <- c(warnings_list,
          "Estimated timber height may be less accurate for large, old trees (>20 m height). Measured timber height is strongly recommended for WCC certification.")
      }
    }
  }
  tariff <- tariff_full[tall_id]

  if (any(tall_id, na.rm = TRUE)) {

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
                      height_timber = attr(tariff_full, "timber_height_used"),
                      timber_ratio = timber_ratio,
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
#' @param name species name, either binomial or common. A single value is recycled when
#'   \code{dbh} and \code{height} are vectors (same species, one row per tree).
#' @param type either 'broadleaf' or 'conifer'. A single value is recycled when
#'   \code{dbh} and \code{height} are vectors.
#' @param dbh diameter at breast height in centimetres
#' @param height tree height in metres (total height). For broadleaves, provide \code{height_timber}
#'   for WCC protocol compliance. Not required when \code{tariff} is provided (e.g. for QMD + mean
#'   tariff stand-level use).
#' @param height_timber timber height in metres (height to first branch). Required for WCC protocol
#'   for broadleaf species. If not provided, total height will be used with a warning.
#' @param timber_ratio ratio for estimating timber height from total height when \code{height_timber}
#'   is not provided. Can be either a numeric value (0-1, default = 0.85) or the string "estimate"
#'   to use species-specific ratios via \code{estimate_timber_height()}. Only used for broadleaf species.
#' @param tariff optional pre-computed tariff number. When provided, skips internal tariff
#'   calculation from height. Use for stand-level estimates with QMD + mean tariff from height
#'   sample trees. Scalar or same length as \code{dbh}. When provided, \code{height} is not
#'   required for trees with dbh >= 7 cm.
#' @param sig_tariff optional sigma for \code{tariff} when \code{tariff} is provided. Used for
#'   uncertainty propagation. Scalar or same length as \code{tariff}. If \code{tariff} provided
#'   without \code{sig_tariff}, tariff uncertainty is assumed zero.
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
#' @details
#'   **When to use \code{fc_agc_error}:**
#'   - Individual tree carbon estimates with uncertainty (one or more DBH/height pairs)
#'   - Full tree inventory with per-tree carbon and propagated uncertainty
#'   - QMD + mean tariff from height sample trees (provide \code{tariff})
#'
#'   **When NOT to use \code{fc_agc_error}:**
#'   - Stand-level carbon from plot/stand DBH and height samples without
#'     pre-computed tariff \eqn{\rightarrow} use \code{\link{fc_stand_carbon}} or
#'     \code{\link{fc_method_e_carbon}}
#'   - You have top height and mean DBH and want Method D workflow \eqn{\rightarrow}
#'     use \code{\link{fc_stand_carbon}}
#'
#' @seealso \code{\link{fc_agc}}, \code{\link{fc_stand_carbon}} (Methods A-D),
#'   \code{\link{fc_method_e_carbon}} (Method E), \code{\link{wcc_per_hectare}},
#'   \code{\link{wcc_stratify}}, \code{\link{error_product}};
#'   vignette("uncertainty-and-comparison", package = "TreeCarbon") for error propagation rules.
#' @examples
#' # Single tree
#' fc_agc_error("Oak", dbh = 45, height = 18, type = "broadleaf", output.all = FALSE)
#'
#' fc_agc_error(name='Quercus robur', dbh=74, height=24, output.all = FALSE)
#' fc_agc_error('Oak', dbh=74, height=24, method="IPCC2",
#' biome="temperate", output.all = FALSE, re_dbh=1, re_h=1)
#' # Input wood density and sd from BIOMASS package
#' wd <- BIOMASS::getWoodDensity('Quercus', 'robur', region='Europe')
#' fc_agc_error('Oak', 72, 24, nsg = wd$meanWD, sig_nsg = wd$sdWD)
#'
#' # QMD + mean tariff with uncertainty
#' fc_agc_error("Sitka Spruce", dbh = 28, tariff = 22, sig_tariff = 1.5,
#'   type = "conifer", output.all = FALSE)
#'
#' # Rich output with metadata
#' result <- fc_agc_error('Oak', 50, 20, rich_output = TRUE)
#' print(result)
#' @export
#' @aliases fc_agc_error
#'
fc_agc_error <- function(name, dbh, height, height_timber = NULL, timber_ratio = 0.85,
                         tariff = NULL, sig_tariff = NULL, type = NULL, method = "Matthews2",
                         biome = "temperate", output.all = TRUE, re_dbh = 0.05, re_h = 0.1,
                         re = 0.025, nsg = NULL, sig_nsg = 0.09413391, rich_output = FALSE){

  # ==== Check arguments ====
  if(!is.character(name)) stop ("name must be a character")
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.null(tariff)) {
    if (!is.numeric(tariff) || any(tariff < 0, na.rm = TRUE)) {
      stop("tariff must be numeric and non-negative when provided")
    }
    if (length(tariff) != 1L && length(tariff) != length(dbh)) {
      stop("tariff must be scalar or same length as dbh")
    }
    if (!is.null(sig_tariff) && length(sig_tariff) != 1L && length(sig_tariff) != length(tariff)) {
      stop("sig_tariff must be scalar or same length as tariff when provided")
    }
  }
  if (!is.null(sig_tariff) && is.null(tariff)) {
    stop("sig_tariff requires tariff to be provided")
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
  dbh <- floor(dbh)

  n <- length(dbh)
  if (length(height) != n) {
    stop("'dbh' and 'height' must have the same length (one row per tree).")
  }
  if (length(name) == 1L && n > 1L) {
    name <- rep(name, n)
  } else if (length(name) != n) {
    stop("'name' must have length 1 or the same length as 'dbh' and 'height'.")
  }
  if (!is.null(type)) {
    if (length(type) == 1L && n > 1L) {
      type <- rep(type, n)
    } else if (length(type) != n) {
      stop("'type' must have length 1, be NULL, or the same length as 'dbh' and 'height'.")
    }
  }

  # ==== QMD-like usage warning (single dbh/height in typical stand range) ====
  if (length(dbh) == 1L && length(height) == 1L && is.null(tariff) &&
      !is.na(dbh) && !is.na(height) && dbh >= 20 && dbh <= 80 &&
      height >= 12 && height <= 35) {
    warning("Single DBH and height in typical stand range (QMD/top height). ",
            "For stand-level estimates, consider fc_stand_carbon() or fc_method_e_carbon().")
  }

  # ==== Validate against WCC method ranges ====
  validation <- validate_inputs_for_method("WCC", dbh = dbh, height = height)
  flags <- if (validation$flags[1] != "None") validation$flags else character()
  warnings_list <- if (validation$validity_warnings[1] != "None") validation$validity_warnings else character()

  # Lookup species codes and type
  spcodes <- lookupcode(name, type, code = 'short')
  rec <- lookup_df[match(spcodes$code, lookup_df$short), ]
  type <- ifelse(is.na(rec$type) | rec$type == "MX", type, rec$type)

  n <- length(height)

  # Create results table
  if(output.all){
    r <- data.frame(name=name, type=type, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, dbh=dbh, height=height,
                    height_timber = NA_real_,
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

  # Trees with dbh >= 7 m; height or tariff required
  use_provided_tariff <- !is.null(tariff)
  tall_id <- dbh >= 7 & !is.na(dbh) & (use_provided_tariff | !is.na(height))

  # Resolve tariff: use provided value or compute from height
  if (use_provided_tariff) {
    n_tariff <- length(dbh)
    tariff_full <- data.frame(
      tariff = rep_len(tariff, n_tariff),
      sigma = if (!is.null(sig_tariff)) rep_len(sig_tariff, n_tariff) else rep(0, n_tariff),
      stringsAsFactors = FALSE
    )
    attr(tariff_full, "timber_height_used") <- rep(NA_real_, n_tariff)
    attr(tariff_full, "notes") <- character()
  } else {
    tariff_full <- tariffs(spcodes$code, height, dbh, type,
                           height_timber = height_timber, timber_ratio = timber_ratio,
                           re_h = re_h, re_dbh = re_dbh, re = re)
  }
  tariff <- tariff_full[tall_id, , drop = FALSE]
  if (output.all) {
    r$height_timber <- attr(tariff_full, "timber_height_used") %||% rep(NA_real_, n)
    notes <- attr(tariff_full, "notes") %||% character()
    if (length(notes) > 0L) {
      flags <- c(flags, notes)
      if (any(grepl("species_ratios", notes))) {
        warnings_list <- c(warnings_list,
          "For broadleaf species, WCC protocol requires timber height (height to first branch). Timber height was estimated from total height using species-specific ratios (or default 0.85 if not available). Timber height estimation is crude; measured values are required for WCC certification.")
      }
      if (any(grepl("fixed_ratio", notes))) {
        rat <- regmatches(notes, regexpr("fixed_ratio_[0-9.]+", notes))
        if (length(rat) > 0L) {
          rr <- sub("fixed_ratio_", "", rat[1L])
          warnings_list <- c(warnings_list,
            paste0("For broadleaf species, WCC protocol requires timber height (height to first branch). Timber height was estimated from total height using ratio of ", rr, ". Timber height estimation is crude; measured values are required for WCC certification."))
        }
      }
      if (any(type == "broadleaf" & !is.na(height) & height > 20, na.rm = TRUE)) {
        flags <- c(flags, "Timber_height_estimated_for_mature_trees_over_20m")
        warnings_list <- c(warnings_list,
          "Estimated timber height may be less accurate for large, old trees (>20 m height). Measured timber height is strongly recommended for WCC certification.")
      }
    }
  }
  tariff <- tariff_full[tall_id, , drop = FALSE]

  if (any(tall_id, na.rm = TRUE)) {

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
          height_timber = attr(tariff_full, "timber_height_used"),
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
  dbh <- floor(dbh)

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

  # ==== Calculate quadratic mean DBH (FC_WCC Section 4.1.5) ====
  valid_dbh <- dbh[!is.na(dbh) & dbh > 0]
  if (length(valid_dbh) == 0) stop("No valid DBH measurements found")
  qmd <- sqrt(mean(valid_dbh^2, na.rm = TRUE))
  mean_dbh <- round(qmd, 1)
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


############# FC Method E: Full tree count (FC_WCC Section 4.2) ################
#'
#' @title Carbon from full tree count (WCC Method E)
#' @description Estimate above-ground carbon using FC Woodland Carbon Code Method E
#'   (full tree count and DBH assessment). Applicable for woodlands up to 1.25 ha
#'   or up to 3000 tree stems (FC_WCC 4.2.1).
#'
#'   Method E uses a 100\% DBH census and systematic height sampling per Table 4.2.2.
#'   For conifers: total height and Equation 3 (conifer_tariff). For broadleaves:
#'   timber height and Equation 2 (broadleaf_tariff).
#'
#' @param name Species name(s), vector matching \code{dbh} and \code{height}.
#'   For mixed stands, provide one name per tree.
#' @param dbh Diameter at breast height in cm (vector). All trees measured (census).
#' @param height Tree height in metres (vector). For conifers: total height.
#'   For broadleaves: provide \code{height_timber} or it is estimated from height.
#' @param area_ha Net stratum area in hectares.
#' @param type \code{"conifer"} or \code{"broadleaf"}. Stratum type; used when
#'   \code{classification} is not provided. For mixed stands, provide
#'   \code{classification} per tree.
#' @param classification Optional vector of \code{"conifer"} or \code{"broadleaf"}
#'   per tree (same length as \code{name}). If provided, overrides \code{type}
#'   for tariff and biomass lookup per species. FC_WCC: minor species (<10\%)
#'   are grouped with the dominant species of the same type.
#' @param height_timber Timber height for broadleaves (optional). If NULL, estimated
#'   from \code{height} using \code{timber_ratio}.
#' @param timber_ratio Ratio for estimating timber height when not provided (default 0.85).
#' @param method Carbon conversion: \code{"Matthews2"} (default), \code{"IPCC2"}, \code{"Thomas"}.
#' @param biome \code{"temperate"} (default), \code{"boreal"}, etc.
#' @param re_dbh Relative error for DBH (optional, for uncertainty propagation).
#' @param re_h Relative error for height (optional).
#' @param re Relative error of coefficients (default 0.025).
#' @param output.all If TRUE (default), return data.frame with by-species breakdown.
#'
#' @return If \code{output.all = FALSE}: stand AGC in tonnes. If \code{output.all = TRUE}:
#'   data.frame with \code{stand_AGC_t}, \code{by_species} (list), \code{n_trees}.
#'
#' @references Jenkins, T.A.R., et al. (2018). FC Woodland Carbon Code: Carbon
#'   Assessment Protocol (v2.0). Forestry Commission, Edinburgh. Section 4.2.
#'   \url{https://www.woodlandcarboncode.org.uk/}
#'
#' @seealso \code{\link{fc_stand_carbon}} (Methods A-D), \code{\link{fc_agc_error}},
#'   \code{\link{conifer_tariff}}, \code{\link{broadleaf_tariff}}.
#'
#' @examples
#' # Conifer stratum, 25 trees (height sample 1:2 per Table 4.2.2)
#' dbh <- c(12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 15, 17, 19, 21, 23,
#'          25, 27, 29, 31, 13, 15, 17, 19, 21)
#' height <- c(8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 9, 11, 13, 15, 17,
#'             19, 21, 23, 25, 8.5, 10.5, 12.5, 14.5, 16.5)
#' fc_method_e(name = "Norway spruce", dbh = dbh, height = height,
#'                   area_ha = 0.4, type = "conifer")
#'
#' @export
#'
fc_method_e <- function(name, dbh, height, area_ha, type = "conifer",
                               classification = NULL, height_timber = NULL,
                               timber_ratio = 0.85, method = "Matthews2",
                               biome = "temperate", re_dbh = NULL, re_h = NULL,
                               re = 0.025, output.all = TRUE) {

  # ==== Check arguments ====
  if (!is.character(name) && !is.factor(name)) stop("name must be character or factor")
  name <- as.character(name)
  if (length(name) != length(dbh) || length(name) != length(height)) {
    stop("name, dbh and height must have the same length")
  }
  if (!is.null(classification) && length(classification) != length(name)) {
    stop("classification must have the same length as name")
  }
  if (!is.numeric(area_ha) || length(area_ha) != 1 || area_ha <= 0) {
    stop("area_ha must be a single positive numeric value")
  }
  if (!(type %in% c("conifer", "broadleaf"))) {
    stop("type must be 'conifer' or 'broadleaf'")
  }
  if (!(method %in% c("Matthews2", "IPCC2", "Thomas"))) {
    stop("method must be 'Matthews2', 'IPCC2', or 'Thomas'")
  }

  # FC_WCC 4.2.1: Method E for area <= 1.25 ha or <= 3000 trees
  n_total <- sum(!is.na(dbh) & dbh >= 7)
  if (n_total < 1) stop("No trees with DBH >= 7 cm")
  if (area_ha > 1.25 || n_total > 3000) {
    warning("Method E applies to area <= 1.25 ha and <= 3000 trees (FC_WCC 4.2.1). ",
            "Consider Methods A-D for larger stands.")
  }

  # FC_WCC Appendix 1 A1.2: DBH rounded down to nearest whole cm
  dbh_floor <- floor(dbh)
  valid <- !is.na(dbh_floor) & dbh_floor >= 7 & !is.na(name) & name != ""
  if (!any(valid)) stop("No valid trees (DBH >= 7 cm, non-NA name)")

  name <- name[valid]
  dbh_floor <- dbh_floor[valid]
  height <- height[valid]
  if (!is.null(height_timber)) height_timber <- height_timber[valid]

  # FC_WCC Table 4.2.2: height sampling fraction (1:n = measure every nth tree)
  height_frac <- function(n) {
    if (n <= 20) 1L else if (n <= 30) 2L else if (n <= 40) 3L else if (n <= 50) 4L else
    if (n <= 60) 5L else if (n <= 70) 6L else if (n <= 80) 7L else if (n <= 90) 8L else
    if (n <= 100) 9L else if (n <= 200) 10L else if (n <= 400) 10L else if (n <= 600) 10L else
    if (n <= 800) 10L else if (n <= 1000) 10L else if (n <= 1200) 10L else if (n <= 1500) 10L else
    if (n <= 2000) 10L else if (n <= 2500) 10L else if (n <= 3000) 10L else 10L
  }

  # Per-species type: from classification if provided, else from lookup
  n_total_valid <- length(name)
  sp_to_type <- character(0)
  sp_to_spcode <- character(0)

  for (sp in unique(name)) {
    lk <- suppressWarnings(lookupcode(sp, type = NULL, code = "short", returnv = "all"))
    sc <- if (is.data.frame(lk)) lk$code[1] else lk[1]
    if (is.na(sc)) {
      lk <- suppressWarnings(lookupcode(sp, type = type, code = "short", returnv = "all"))
      sc <- if (is.data.frame(lk)) lk$code[1] else lk[1]
    }
    if (is.na(sc)) next
    sp_type <- lookup_df$type[match(sc, lookup_df$short)]
    if (is.na(sp_type) || sp_type == "any" || sp_type == "NA") {
      if (!is.null(classification)) {
        sp_type <- classification[which(name == sp)[1]]
      } else {
        sp_type <- type
      }
    }
    sp_to_type[sp] <- sp_type
    sp_to_spcode[sp] <- sc
  }

  # FC_WCC: minor species (<10%) grouped with dominant of same type
  sp_counts <- table(name)
  for (sp in names(sp_counts)) {
    if (sp_counts[sp] < 0.1 * n_total_valid && sp %in% names(sp_to_spcode)) {
      st <- sp_to_type[sp]
      same_type <- names(sp_counts)[!is.na(sp_to_type[names(sp_counts)]) &
                                    sp_to_type[names(sp_counts)] == st]
      if (length(same_type) > 1) {
        dominant <- same_type[which.max(sp_counts[same_type])]
        if (dominant != sp && dominant %in% names(sp_to_spcode)) {
          sp_to_spcode[sp] <- sp_to_spcode[dominant]
        }
      }
    }
  }

  sp_groups <- unique(name)
  carbon_total <- 0
  by_species_list <- list()

  for (sp in sp_groups) {
    idx_sp <- which(name == sp)
    n_sp <- length(idx_sp)
    if (n_sp < 1) next

    spcode <- sp_to_spcode[sp]
    sp_type <- if (sp %in% names(sp_to_type)) sp_to_type[sp] else type
    if (is.na(spcode) || spcode == "") {
      warning("Species '", sp, "' not found in lookup; skipping.")
      next
    }

    dbh_sp <- dbh_floor[idx_sp]
    height_sp <- height[idx_sp]
    h_timber_sp <- if (!is.null(height_timber)) height_timber[idx_sp] else NULL

    # FC_WCC 4.2.3 c: QMD from all trees (rounded-down DBH)
    qmd <- sqrt(mean(dbh_sp^2, na.rm = TRUE))
    if (is.na(qmd) || qmd <= 0) next

    # FC_WCC 4.2.2, Table 4.2.2: tariff from height sample trees (every nth)
    frac <- height_frac(n_sp)
    idx_h <- seq(1, n_sp, by = frac)
    idx_h <- idx_h[!is.na(height_sp[idx_h]) & height_sp[idx_h] > 0]
    if (length(idx_h) < 1) {
      idx_h <- which(!is.na(height_sp) & height_sp > 0)
      if (length(idx_h) < 1) {
        warning("No valid height for species '", sp, "'; skipping.")
        next
      }
    }

    h_sample <- height_sp[idx_h]
    dbh_sample <- dbh_sp[idx_h]

    # Use tariff by actual species type (conifer vs broadleaf)
    if (sp_type == "conifer") {
      t_out <- suppressWarnings(conifer_tariff(rep(spcode, length(idx_h)), h_sample, dbh_sample,
                              re_h = re_h, re_dbh = re_dbh, re = re))
    } else {
      h_t <- if (!is.null(h_timber_sp)) h_timber_sp[idx_h] else h_sample * timber_ratio
      h_t[is.na(h_t)] <- h_sample[is.na(h_t)] * timber_ratio
      t_out <- suppressWarnings(broadleaf_tariff(rep(spcode, length(idx_h)), height_timber = h_t,
                                dbh = dbh_sample, re_h = re_h, re_dbh = re_dbh, re = re))
    }
    tariff_vals <- if (is.data.frame(t_out)) t_out$tariff else t_out
    mean_tariff <- floor(mean(tariff_vals, na.rm = TRUE))

    # FC_WCC 4.2.3 d-g: volume, biomass, carbon; N = actual count (census)
    mercvol <- merchtreevol(dbh = qmd, tariff = mean_tariff, re_dbh = re_dbh, re = re)
    mercvol_val <- if (is.list(mercvol)) mercvol$volume else mercvol
    stemvol <- treevol(mtreevol = mercvol_val, dbh = qmd, re = re)
    stemvol_val <- if (is.data.frame(stemvol)) stemvol$stemvolume else stemvol

    # Use spcode (may be grouped for minor species) for NSG and Crown
    rec <- lookup_df[match(spcode, lookup_df$short), ]
    nsg <- as.numeric(rec$NSG[1])
    crown_code <- rec$Crown[1]
    if (is.na(nsg) || is.na(crown_code)) {
      lk <- lookupcode(sp, type = sp_type, code = "NSG")
      nsg <- as.numeric(if (is.data.frame(lk)) lk$code[1] else lk[1])
      lk <- lookupcode(sp, type = sp_type, code = "Crown")
      crown_code <- if (is.data.frame(lk)) lk$code[1] else lk[1]
    }

    woodbio <- woodbiomass(treevol = stemvol_val, nsg = nsg)
    woodbio_val <- if (is.list(woodbio)) woodbio$woodbiomass else woodbio
    crownbio <- crownbiomass(spcode = crown_code, dbh = qmd, re_d = re_dbh, re = re)
    crownbio_val <- if (is.list(crownbio)) crownbio$biomass else crownbio

    agb <- woodbio_val + crownbio_val
    carb <- biomass2c(agb, method = method, type = sp_type, biome = biome)
    carbon_tree <- if (is.data.frame(carb)) carb$AGC[1] else carb[1]

    sp_carbon <- carbon_tree * n_sp
    carbon_total <- carbon_total + sp_carbon
    by_species_list[[sp]] <- data.frame(
      species = sp, n_trees = n_sp, qmd_cm = round(qmd, 1),
      mean_tariff = mean_tariff, carbon_t = sp_carbon,
      stringsAsFactors = FALSE
    )
  }

  if (output.all) {
    out <- data.frame(
      stand_AGC_t = carbon_total,
      n_trees = n_total,
      area_ha = area_ha,
      type = type,
      method = method,
      stringsAsFactors = FALSE
    )
    out$by_species <- list(do.call(rbind, by_species_list))
    return(out)
  }
  carbon_total
}

#' @rdname fc_method_e
#' @export
fc_method_e_carbon <- fc_method_e


#' Plot-stratum carbon (FC_WCC Methods B/C office work, SP 9.2)
#'
#' Estimates above-ground carbon for a stratum from one or more sample plots:
#' trees per ha from plot totals, quadratic mean DBH per species, tariff from
#' height-sample trees, then volume, biomass and carbon (FC_WCC 4.1.5).
#'
#' @param plot_list Named list of data.frames, one per plot. Each must contain
#'   columns `Genus`, `Species`, `DBH` (cm), `Height` (m), `Classification`
#'   (`"broadleaf"` or `"conifer"`). Optional: `timber_height`, `x_m`, `y_m`.
#' @param plot_areas_ha Named numeric vector of plot areas (ha); names must match
#'   `names(plot_list)`.
#' @param stratum_area_ha Net stratum area (ha) for scaling tree numbers.
#' @param type Default `"broadleaf"` or `"conifer"` for tariff/volume when the
#'   stratum is single-type; mixed conifer/broadleaf uses per-tree classification.
#' @param re_dbh,re_h Relative measurement errors (passed to tariff/volume functions).
#' @param method,biome Passed to [biomass2c()] (default `Matthews2`, `temperate`).
#' @param timber_ratio Used to impute timber height for broadleaves when
#'   `timber_height` is NA: `Height * timber_ratio`.
#' @param seed If not `NULL`, used only around random selection of height-sample
#'   trees when plot coordinates are missing; global RNG state is restored after
#'   each draw. If `NULL`, [sample.int()] is used without resetting the global seed.
#' @return A list with `carbon_t` (t C), `n_stratum` (estimated trees in stratum),
#'   `trees_per_ha`, and `by_species` (a `data.frame` with species-level columns).
#' @references Jenkins et al. FC Woodland Carbon Code Carbon Assessment Protocol
#'   (2018); WCC Survey Protocol for projects (2024).
#' @seealso [fc_stand_carbon()], [fc_method_e()] (same help as `fc_method_e_carbon`),
#'   [fc_agc_error()]
#' @export
fc_plot_stratum_carbon <- function(plot_list, plot_areas_ha, stratum_area_ha,
                                   type = "broadleaf", re_dbh = 0.05, re_h = 0.1,
                                   method = "Matthews2", biome = "temperate",
                                   timber_ratio = 0.85, seed = NULL) {
  if (!type %in% c("broadleaf", "conifer"))
    stop("type must be 'broadleaf' or 'conifer'.")
  if (!is.list(plot_list) || length(plot_list) == 0L)
    return(list(carbon_t = 0, n_stratum = 0, trees_per_ha = 0, by_species =
                  data.frame(Species = character(), N_sample = numeric(),
                             QMD_cm = numeric(), Mean_tariff = numeric(),
                             Carbon_t = numeric(), stringsAsFactors = FALSE)))
  pn <- names(plot_list)
  if (is.null(pn) || any(pn == ""))
    stop("plot_list must be a named list (one component per plot).")
  if (is.null(names(plot_areas_ha)))
    stop("plot_areas_ha must be a named vector (ha), names = plot IDs.")
  miss <- setdiff(pn, names(plot_areas_ha))
  if (length(miss))
    stop("plot_areas_ha missing entries for plot(s): ", paste(miss, collapse = ", "))
  if (!is.numeric(stratum_area_ha) || length(stratum_area_ha) != 1L ||
      !is.finite(stratum_area_ha) || stratum_area_ha <= 0)
    stop("stratum_area_ha must be a single positive finite number (ha).")
  req <- c("Genus", "Species", "DBH", "Height", "Classification")
  for (nm in pn) {
    missc <- setdiff(req, names(plot_list[[nm]]))
    if (length(missc))
      stop("Plot ", nm, " data.frame missing column(s): ", paste(missc, collapse = ", "))
  }

  if (sum(plot_areas_ha[pn], na.rm = TRUE) <= 0)
    return(list(carbon_t = 0, n_stratum = 0, trees_per_ha = 0, by_species =
                  data.frame(Species = character(), N_sample = numeric(),
                             QMD_cm = numeric(), Mean_tariff = numeric(),
                             Carbon_t = numeric(), stringsAsFactors = FALSE)))

  total_trees <- sum(vapply(plot_list, nrow, integer(1)))
  total_area <- sum(plot_areas_ha[pn], na.rm = TRUE)
  trees_per_ha <- total_trees / total_area
  n_stratum <- trees_per_ha * stratum_area_ha

  all_trees <- do.call(rbind, plot_list)
  all_trees$Plot_id <- rep(pn, vapply(plot_list, nrow, integer(1)))
  all_trees$DBH_floor <- floor(all_trees$DBH)
  if (!"timber_height" %in% names(all_trees))
    all_trees$timber_height <- NA_real_
  sp_groups <- unique(paste(all_trees$Genus, all_trees$Species))

  sp_counts <- table(paste(all_trees$Genus, all_trees$Species))
  sp_to_spcode <- character()
  sp_to_type <- character()
  sp_to_dominant <- character()
  for (sp in names(sp_counts)) {
    sp_tmp <- all_trees[paste(all_trees$Genus, all_trees$Species) == sp, ][1L, ]
    st <- as.character(sp_tmp$Classification[1])
    sp_to_type[sp] <- st
    sp_to_dominant[sp] <- sp
    lk <- suppressWarnings(lookupcode(sp, type = st, code = "short", returnv = "all"))
    sp_to_spcode[sp] <- if (is.data.frame(lk)) lk$code[1] else lk[1]
  }
  for (sp in names(sp_counts)) {
    if (sp_counts[sp] < 0.1 * total_trees && sp %in% names(sp_to_spcode)) {
      st <- sp_to_type[sp]
      same_type <- names(sp_counts)[sp_to_type[names(sp_counts)] == st]
      if (length(same_type) > 1L) {
        dominant <- same_type[which.max(sp_counts[same_type])]
        if (dominant != sp && dominant %in% names(sp_to_spcode)) {
          sp_to_spcode[sp] <- sp_to_spcode[dominant]
          sp_to_dominant[sp] <- dominant
        }
      }
    }
  }

  .sample_idx <- function(n, size, replace, seed_local) {
    if (size < 1L || n < 1L) return(integer(0))
    if (is.null(seed_local)) return(sample.int(n, size = size, replace = replace))
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      os <- .GlobalEnv$.Random.seed
      on.exit(assign(".Random.seed", os, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit(rm(list = ".Random.seed", envir = .GlobalEnv), add = TRUE)
    }
    set.seed(seed_local)
    sample.int(n, size = size, replace = replace)
  }

  carbon_total <- 0
  by_rows <- list()

  for (sp in sp_groups) {
    sp_trees <- all_trees[paste(all_trees$Genus, all_trees$Species) == sp, ]
    if (nrow(sp_trees) < 1L) next

    sp_type <- as.character(sp_trees$Classification[1])
    spcode <- sp_to_spcode[sp]
    if (is.na(spcode) || spcode == "") spcode <- lookupcode(sp, type = sp_type)$code

    sp_idx <- which(paste(all_trees$Genus, all_trees$Species) == sp)
    qmd <- sqrt(mean(all_trees$DBH_floor[sp_idx]^2, na.rm = TRUE))
    if (is.na(qmd) || qmd <= 0) next

    tariff_vals <- numeric(0)
    has_coords <- all(c("x_m", "y_m") %in% names(sp_trees))
    for (pid in pn) {
      pl_trees <- sp_trees[sp_trees$Plot_id == pid, ]
      n_samp <- min(2L, nrow(pl_trees))
      if (n_samp > 0L) {
        if (has_coords && all(c("x_m", "y_m") %in% names(pl_trees)) &&
            !any(is.na(pl_trees$x_m)) && !any(is.na(pl_trees$y_m))) {
          pl_all <- plot_list[[pid]]
          cx <- mean(pl_all$x_m, na.rm = TRUE)
          cy <- mean(pl_all$y_m, na.rm = TRUE)
          d <- sqrt((pl_trees$x_m - cx)^2 + (pl_trees$y_m - cy)^2)
          samp_idx <- order(d)[seq_len(n_samp)]
        } else {
          samp_idx <- .sample_idx(nrow(pl_trees), n_samp, n_samp > nrow(pl_trees), seed)
        }
        h_t <- pl_trees$timber_height[samp_idx]
        h_t[is.na(h_t)] <- pl_trees$Height[samp_idx][is.na(h_t)] * timber_ratio
        if (sp_type == "conifer") {
          t_out <- conifer_tariff(rep(spcode, n_samp), pl_trees$Height[samp_idx],
                                  pl_trees$DBH[samp_idx], re_h = re_h, re_dbh = re_dbh)
        } else {
          t_out <- broadleaf_tariff(rep(spcode, n_samp), height_timber = h_t,
                                    dbh = pl_trees$DBH[samp_idx], re_h = re_h, re_dbh = re_dbh)
        }
        t_vals <- if (is.data.frame(t_out)) t_out$tariff else t_out
        tariff_vals <- c(tariff_vals, t_vals)
      }
    }
    if (length(tariff_vals) == 0L) {
      h_t <- sp_trees$timber_height
      h_t[is.na(h_t)] <- sp_trees$Height[is.na(h_t)] * timber_ratio
      if (sp_type == "conifer") {
        t_out <- conifer_tariff(rep(spcode, nrow(sp_trees)), sp_trees$Height, sp_trees$DBH,
                                re_h = re_h, re_dbh = re_dbh)
      } else {
        t_out <- broadleaf_tariff(rep(spcode, nrow(sp_trees)), height_timber = h_t,
                                  dbh = sp_trees$DBH, re_h = re_h, re_dbh = re_dbh)
      }
      tariff_vals <- if (is.data.frame(t_out)) t_out$tariff else t_out
    }
    mean_tariff <- floor(mean(tariff_vals, na.rm = TRUE))

    mercvol <- merchtreevol(dbh = qmd, tariff = mean_tariff, re_dbh = re_dbh)
    mercvol_val <- if (is.list(mercvol)) mercvol$volume else mercvol
    stemvol <- treevol(mtreevol = mercvol_val, dbh = qmd)
    stemvol_val <- if (is.data.frame(stemvol)) stemvol$stemvolume else stemvol
    dom_sp <- if (sp %in% names(sp_to_dominant)) sp_to_dominant[sp] else sp
    nsg <- as.numeric(lookupcode(dom_sp, type = sp_type, code = "NSG")$code)
    crown_code <- lookupcode(dom_sp, type = sp_type, code = "Crown")$code
    woodbio <- woodbiomass(treevol = stemvol_val, nsg = nsg)
    crownbio <- crownbiomass(spcode = crown_code, dbh = qmd, re_d = re_dbh)
    woodbio_val <- if (is.list(woodbio)) woodbio$woodbiomass else woodbio
    crownbio_val <- if (is.data.frame(crownbio)) crownbio$biomass else if (is.list(crownbio)) crownbio$biomass else crownbio
    agb <- woodbio_val + crownbio_val
    carb <- biomass2c(agb, method = method, type = sp_type, biome = biome)
    carbon_tree <- if (is.data.frame(carb)) carb$AGC[1] else carb[1]

    n_sp <- nrow(sp_trees)
    n_sp_stratum <- n_sp / total_trees * n_stratum
    sp_carbon <- carbon_tree * n_sp_stratum
    carbon_total <- carbon_total + sp_carbon

    by_rows[[length(by_rows) + 1L]] <- data.frame(
      Species = sp, N_sample = n_sp, QMD_cm = qmd,
      Mean_tariff = mean_tariff, Carbon_t = sp_carbon,
      stringsAsFactors = FALSE
    )
  }

  by_species <- if (length(by_rows)) do.call(rbind, by_rows) else
    data.frame(Species = character(), N_sample = numeric(), QMD_cm = numeric(),
               Mean_tariff = numeric(), Carbon_t = numeric(), stringsAsFactors = FALSE)

  list(
    carbon_t = carbon_total,
    n_stratum = n_stratum,
    trees_per_ha = trees_per_ha,
    by_species = by_species
  )
}


############# WCC Stand-Level Helper Functions ################
#'
#' @title Mean tariff from sample trees (WCC)
#' @description Calculate mean stand tariff from sample tree measurements with
#'   proper uncertainty propagation. Useful for scaling up from sample plots.
#' @param spcode Species code(s) for sample trees. A single code is recycled to
#'   match \code{length(height)} (same species, multiple sample trees).
#' @param height Tree height(s) in metres
#' @param dbh Diameter at breast height in cm
#' @param type Tree type: "broadleaf" or "conifer"
#' @param height_timber Timber height for broadleaves (optional)
#' @param timber_ratio Ratio for estimating timber height when not provided (default 0.85)
#' @param re_h Relative error of height (optional, for uncertainty)
#' @param re_dbh Relative error of DBH (optional, for uncertainty)
#' @param re Relative error of coefficients (default 0.025)
#' @return Mean tariff value, or if re_h/re_dbh provided a list with mean_tariff
#'   and sigma (standard error of the mean)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @export
wcc_mean_tariff <- function(spcode, height, dbh, type = NULL,
                            height_timber = NULL, timber_ratio = 0.85,
                            re_h = NULL, re_dbh = 0.05, re = 0.025) {
  dbh <- floor(dbh)
  n <- length(height)
  if (length(dbh) != n) {
    stop("'height' and 'dbh' must have the same length.")
  }
  if (length(spcode) == 1L && n > 1L) {
    spcode <- rep(spcode, n)
  } else if (length(spcode) != n) {
    stop("'spcode' must have length 1 or the same length as 'height' and 'dbh'.")
  }
  if (!is.null(type)) {
    if (length(type) == 1L && n > 1L) {
      type <- rep(type, n)
    } else if (length(type) != n) {
      stop("'type' must have length 1 or the same length as 'height' and 'dbh'.")
    }
  }
  tariff_out <- tariffs(spcode, height, dbh, type = type,
                        height_timber = height_timber, timber_ratio = timber_ratio,
                        re_h = re_h, re_dbh = re_dbh, re = re)
  if (is.data.frame(tariff_out)) {
    valid <- !is.na(tariff_out$tariff)
    if (!any(valid)) stop("No valid tariff values")
    mean_raw <- mean(tariff_out$tariff[valid])
    # FC_WCC Protocol: "If this is not a whole number it should be rounded down"
    # (Methods B/C, Section 4.1.4) - only when multiple sample trees
    mean_tariff <- if (sum(valid) > 1L) floor(mean_raw) else mean_raw
    sigma <- if ("sigma" %in% names(tariff_out) && any(!is.na(tariff_out$sigma[valid])))
      sqrt(sum(tariff_out$sigma[valid]^2, na.rm = TRUE)) / sum(valid) else NULL
    if (!is.null(sigma)) {
      return(list(mean_tariff = mean_tariff, sigma = sigma))
    }
    return(mean_tariff)
  }
  valid <- !is.na(tariff_out)
  if (!any(valid)) stop("No valid tariff values")
  mean_raw <- mean(tariff_out[valid], na.rm = TRUE)
  if (sum(valid) > 1L) floor(mean_raw) else mean_raw
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
