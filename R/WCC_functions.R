# ==============================================================================
# TreeCarbon - Forestry Commission Woodland Carbon Code: Volume & Tariff
# ==============================================================================
#
# This module implements the Forestry Commission's Woodland Carbon Code (WCC)
# methodology for tariff number and volume calculations. These form the
# foundational steps in the WCC carbon estimation pipeline.
#
# Functions included:
#   - Tariff number calculations (Equations 1-4)
#   - Stand and tree volume estimation (Equations 5a-5c)
#   - Height-tariff relationships
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
############# Estimate Timber Height from Total Height ################
#'
#' @title Estimate timber height from total height using species-specific ratios
#' @description Estimates timber height (height to first branch or merchantable top)
#'   from total height using species-specific ratios. Timber height is required
#'   for WCC broadleaf tariff calculations. If species-specific ratios are not
#'   available, uses a default ratio of 0.85.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height Total tree height in metres
#' @param genus Genus name (optional, for species-specific ratios)
#' @param species Species name (optional, for species-specific ratios)
#' @param spcode Species code (optional, for species-specific ratios)
#' @param type Tree type: "broadleaf" or "conifer" (optional)
#' @param default_ratio Default ratio to use if species-specific ratio not found
#'   (default = 0.85, typical for UK broadleaves)
#' @return Numeric vector of estimated timber heights in metres
#' @details
#'   Timber height (also called merchantable height or clear bole height) is the
#'   height from ground to the first major branch or merchantable top. For
#'   broadleaf species, the WCC protocol requires timber height for tariff
#'   calculations, while other allometric methods use total height.
#'
#'   This function uses species-specific timber:total height ratios when available.
#'   If no species-specific ratio is found, it falls back to a default ratio
#'   (typically 0.85 for broadleaves). For conifers, timber height typically
#'   equals total height (ratio = 1.0).
#'
#'   The ratios are sourced from forestry literature and may vary by:
#'   - Species (genetic differences)
#'   - Age (younger trees may have lower ratios)
#'   - Site conditions (competition, management)
#'   - Management history (pruning, thinning)
#'
#'   For accurate WCC protocol compliance, measured timber height should be used
#'   when available. Timber height estimation is crude; measured values are
#'   required for WCC certification. Estimated timber height may be less
#'   accurate for large, old trees (>20 m height).
#' @references
#'   Jenkins, T.A.R., et al. (2018). FC Woodland Carbon Code: Carbon
#'   Assessment Protocol (v2.0). Forestry Commission, Edinburgh.
#' @examples
#' # Estimate timber height for a broadleaf tree
#' estimate_timber_height(height = 20, genus = "Quercus", species = "robur")
#'
#' # Estimate using species code
#' estimate_timber_height(height = 15, spcode = "OK", type = "broadleaf")
#'
#' # Multiple trees
#' estimate_timber_height(height = c(15, 20, 18), spcode = c("OK", "BI", "AH"))
#' @export
estimate_timber_height <- function(height, genus = NULL, species = NULL,
                                   spcode = NULL, type = NULL,
                                   default_ratio = 0.85) {

  # Validate inputs
  if (!is.numeric(height) || any(height < 0, na.rm = TRUE)) {
    stop("height must be numeric and non-negative")
  }
  if (!is.numeric(default_ratio) || default_ratio <= 0 || default_ratio > 1) {
    stop("default_ratio must be numeric, positive, and <= 1")
  }

  n <- length(height)

  # Initialize result vector
  timber_height <- numeric(n)

  # Check if timber_height_ratios data exists (will be created from PDF)
  # For now, use default ratio with species-specific lookup if available
  # Note: timber_height_ratios data doesn't exist yet, so this will always return FALSE
  # When the data is added, it will be lazy-loaded automatically (LazyData: true)
  has_ratios_data <- exists("timber_height_ratios")

  if (has_ratios_data && exists("timber_height_ratios")) {
    timber_ratios <- get("timber_height_ratios")

    # Lookup species-specific ratios
    ratios <- rep(default_ratio, n)

    # Try lookup by spcode first (most reliable)
    if (!is.null(spcode) && "spcode" %in% colnames(timber_ratios)) {
      match_idx <- match(spcode, timber_ratios$spcode)
      found_idx <- !is.na(match_idx)
      if (any(found_idx)) {
        ratios[found_idx] <- timber_ratios$ratio[match_idx[found_idx]]
      }
    }

    # Try lookup by genus + species
    if (!is.null(genus) && !is.null(species) &&
        "genus" %in% colnames(timber_ratios) && "species" %in% colnames(timber_ratios)) {
      name_match <- paste(genus, species)
      ratio_match <- paste(timber_ratios$genus, timber_ratios$species)
      match_idx <- match(name_match, ratio_match)
      found_idx <- !is.na(match_idx) & ratios == default_ratio  # Only update if not already found
      if (any(found_idx)) {
        ratios[found_idx] <- timber_ratios$ratio[match_idx[found_idx]]
      }
    }

    # For conifers, typically use ratio of 1.0 (timber height = total height)
    if (!is.null(type)) {
      conifer_idx <- type == "conifer"
      if (any(conifer_idx, na.rm = TRUE)) {
        ratios[conifer_idx] <- 1.0
      }
    }

    timber_height <- height * ratios

  } else {
    # No species-specific data available - use default ratio with type-based logic
    ratios <- rep(default_ratio, n)

    # For conifers, typically timber height = total height (ratio = 1.0)
    if (!is.null(type)) {
      conifer_idx <- type == "conifer"
      if (any(conifer_idx, na.rm = TRUE)) {
        ratios[conifer_idx] <- 1.0
      }
    }

    timber_height <- height * ratios
  }

  return(timber_height)
}

############# Tariff number from volume and tree basal area (WCC Eq 1) ############
#'
#' @title Tariff number from volume and basal area (WCC Method A)
#' @description Using the sample tree’s basal area and volume to calculate the
#' tariff number. Implements WCC Protocol Method A (Fell sample trees).
#' Basal area is calculated by ba = (pi * dbh^2)/40000.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param vol tree volume (metres cubed)
#' @param dbh diameter at breast height (centimetres)
#' @param sig_vol sigma for tree volume (optional)
#' @param re_dbh relative measurement error for diameter at breast height (default is 0.025)
#' @param re  relative error of coefficients (default = 0.025)
#' @return  Tariff number or if sigma for inputs are provided, then will return
#' a list of tariff number and sigma for tariff
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). (Equation 1)
#' @examples
#' tariff_vol_area(vol=0.5, dbh=24, sig_vol = 0.001, re_dbh = 0.05)
#' @export
#' @aliases tariff_vol_area
#'
tariff_vol_area <- function(vol, dbh, sig_vol = NULL, re_dbh = 0.025, re = 0.025){

  # Check inputs are numeric and positive
  if (!is.numeric(dbh) || any(dbh < 0, na.rm = TRUE)) {
    stop("dbh must be non-negative and numeric")
  }
  if (!is.numeric(vol) || any(vol < 0, na.rm = TRUE)) {
    stop("vol must be non-negative and numeric")
  }

  # Set constants
  coef_a1 <- 3.174106384
  const_vol <- 0.005002986
  const_ba <- 0.003848451
  const_tariff <- 0.138763302

  # Calculate tree basal area (m^2)
  ba <- (pi * dbh^2) / 40000

  # Calculate tariff number
  a1 <- (vol - const_vol) / (ba - const_ba)
  tariff <- (coef_a1 * a1) + const_tariff

  # Calculate the error if sig_vol is not null
  if(!is.null(sig_vol)){

    # Check inputs required for error calculation
    if (any(sig_vol < 0, na.rm = TRUE) || !is.numeric(sig_vol)) {
      stop("sig_vol must be non-negative numeric")}
    if (!is.numeric(re_dbh) || re_dbh < 0)
      stop("Argument 're_dbh' must be positive and numeric")
    if (!is.numeric(re) || re < 0)
      stop("Argument 're' must be positive and numeric")
    if (re_dbh > 1 || re > 1)
      warning("Relative error indicates high uncertainty to measured value")

    # Calculate error propagation for basal area
    sig_ba <- (pi * 2 * dbh / 40000) * re_dbh*dbh

    # Calculate error propagation for a1
    sig_a1 <- a1 * sqrt(
      (sig_vol / (vol - const_vol))^2 +            # Error from vol
        (sig_ba / (ba - const_ba))^2 +             # Error from ba
        (re * const_vol / (vol - const_vol))^2 +   # Error from const_vol
        (re * const_ba / (ba - const_ba))^2        # Error from const_ba
    )
    # Calculate error propagation for tariff number
    sig_t <- sqrt((coef_a1 * sig_a1)^2 + (a1 * re * coef_a1)^2)

    return(list(tariff=tariff, sigma_tariff=sig_t))
  } else {
    return(tariff)
  }
}

############# Conifer tree tariff number (WCC Eq 3) ############################
#'
#' @title Conifer tree tariff number (WCC Method C)
#' @description Conifer sample tree tariff. Implements WCC Protocol Method C
#' (Conifers only: measure total height of sample trees). Species-specific
#' estimates of a1-a3 are found in the
#'  R data file, 'tariff_coniferdf'.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param dbh diameter at breast height in centimetres
#' @param spcode species code (single)
#' @param re_h relative error of height measurement (optional)
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re  relative error of coefficients (default = 0.025)
#' @return  tariff number or if relative errors are provided returns a list of
#' tariff number and an estimate for sigma
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' conifer_tariff("SP", 74, 24)
#' conifer_tariff("SP", 74, 24, 1, 1)
#' @export
#' @aliases conifer_tariff
#'
conifer_tariff <- function(spcode, height, dbh, re_h = NULL, re_dbh = 0.05, re = 0.025) {

  # Check inputs
  if (!is.numeric(height) | any(height < 0, na.rm = TRUE) |
      !is.numeric(dbh) | any(dbh < 0, na.rm = TRUE)){
    stop("dbh must be numeric and positive")
  }
  if (!(length(spcode) == length(height) && length(height) == length(dbh))) {
    stop("Input vectors for spcode, height & dbh must have the same length.")
  }

  # Lookup coefficients for tariff equation by spcode in tariff_coniferdf
  lookup_index <- match(spcode, tariff_coniferdf$abbreviation)
  tc <- tariff_coniferdf[lookup_index, ]

  # Get a list of spcodes with missing coefficients
  missing_species <- is.na(tc$abbreviation)

  # If there are any spcodes with missing coefficients
  if (any(missing_species)) {

    # Lookup using the short species code
    subcode <- lookup_df$single[match(spcode[missing_species], lookup_df$short)]
    sub_index <- match(subcode, tariff_coniferdf$abbreviation)
    tc[missing_species, ] <- tariff_coniferdf[sub_index, ]

    # If still missing, then use Norway spruce coefficients for the conifer
    still_missing <- is.na(tc$abbreviation)
    tc[still_missing, ] <- tariff_coniferdf[tariff_coniferdf$abbreviation == "NS", ]

    # Output a warning message that we used the general coefficients
    unique_missing <- unique(spcode[still_missing])
    if (length(unique_missing) > 0) {
      warning(paste(paste(as.character(unique_missing), collapse = ", "),
                    "species codes not found, general conifer code used."))}
  }

  # Calculate conifer tariff number using coefficients stored in tc
  tariff <- tc$a1 + (tc$a2 * height) + (tc$a3 * dbh)

  # If re_h is specified and not null, then calculate the error
  if(!is.null(re_h)){

    # Check inputs for calculating the error
    if(!is.numeric(re_dbh) || any(re_dbh<0))stop("must provide a numeric and positive re_dbh with re_h")
    if(!is.numeric(re_h) || any(re_h<0))stop("must provide a numeric and positive re_h with re_dbh")
    if (re_dbh > 1 || re_dbh > 1 || re_h > 1 || re_h > 1)
      warning("Relative errors indicate high uncertainty to measured value")

    # Propagate the error using error_product formula
    sigma <- sqrt(
      (tc$a1 * re)^2 +
        error_product(tc$a2, tc$a2 * re, height, re_h * height) +
        error_product(tc$a3, tc$a3 * re, dbh,   re_dbh * dbh)
    )

    return(data.frame(tariff = tariff, sigma = sigma))
  } else {
    return(tariff)
  }
}

############# Broadleaf tree tariff number (WCC Eq 2) ##########################
#'
#' @title Carbon tariff number for broadleaf tree (WCC Method B)
#' @description Use dbh and timber height (height to first branch or merchantable top).
#' Implements WCC Protocol Method B (Broadleaves only: measure timber height of sample trees).
#' to derive the tariff number of each sample tree. Species-specific estimates of
#' a1-a4 are found in the R data file, 'tariff_broaddf'. According to WCC protocol,
#' broadleaf species require timber height, not total height, for tariff calculations.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height_timber timber height in metres (height to first branch or merchantable top)
#' @param dbh diameter at breast height in centimetres
#' @param spcode species code (single)
#' @param re_h relative error of height measurement (optional)
#' @param re_dbh relative error for dbh/percentage measurement error (optional)
#' @param re relative error of coefficients (default = 0.025)
#' @return  tariff number or if relative errors are provided returns a list of
#' tariff number and an estimate for sigma
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Method B, Equation 4.
#' @importFrom utils data
#' @examples broadleaf_tariff(spcode = 'OK', height_timber = 20, dbh = 75)
#' broadleaf_tariff(spcode = "OK", height_timber = 20, dbh = 15, re_dbh = 0.05, re_h = 0.1)
#' broadleaf_tariff(spcode = "OK", height_timber = 19, dbh = 15, re_dbh = 0.05, re_h = 0.1)
#' @export
#' @aliases broadleaf_tariff
#'
broadleaf_tariff <- function(spcode, height_timber, dbh, re_dbh = NULL, re_h = 0.1, re = 0.025) {

  # Check inputs are numeric and positive
  if (!is.numeric(height_timber) | any(height_timber < 0, na.rm = TRUE) |
      !is.numeric(dbh) | any(dbh < 0, na.rm = TRUE)){
    stop("dbh and height_timber must be numeric and positive")
  }

  # Check input lengths are the same
  if (!(length(spcode) == length(height_timber) && length(height_timber) == length(dbh))) {
    stop("Input vectors for spcode, height_timber & dbh must have the same length.")
  }

  # Lookup coefficients for tariff equation by spcode in tariff_broaddf
  lookup_index <- match(spcode, tariff_broaddf$abbreviation)
  tb <- tariff_broaddf[lookup_index, ]

  # Get a list of spcodes with missing coefficients
  missing_species <- is.na(tb$abbreviation)

  # If there are any spcodes with missing coefficients
  if (any(missing_species)) {

    # Lookup using the short species code
    subcode <- lookup_df$single[match(spcode[missing_species], lookup_df$short)]
    sub_index <- match(subcode, tariff_broaddf$abbreviation)
    tb[missing_species, ] <- tariff_broaddf[sub_index, ]

    # If still missing, then use Birch coefficients for the broadleaf tree
    still_missing <- is.na(tb$abbreviation)
    tb[still_missing, ] <- tariff_broaddf[tariff_broaddf$abbreviation == "BI", ]

    # Output a warning message that we used the general coefficients
    unique_missing <- unique(spcode[still_missing])
    if (length(unique_missing) > 0) {
      warning(paste(paste(as.character(unique_missing), collapse = ", "),
                    "species codes not found, general broadleaf code used."))}
  }

  # Calculate tariff number using coefficients stored in tb
  # Note: Uses timber height (height to first branch) as per WCC protocol
  tariff <- tb$a1 + (tb$a2 * height_timber) + (tb$a3 * dbh) + (tb$a4 * dbh * height_timber)

  # If re_h is specified and not null, then calculate the error
  if (!is.null(re_dbh)) {

    # Check inputs for calculating the error
    if (!is.numeric(re_dbh) || any(re_dbh < 0, na.rm = TRUE)){
      stop("must provide a numeric and positive re_dbh with re_h")}
    if (!is.numeric(re_h) || any(re_h < 0, na.rm = TRUE)){
      stop("must provide a numeric and positive re_h with re_dbh")}
    if (any(re_dbh > 1, na.rm = TRUE) || any(re_h > 1, na.rm = TRUE)){
      warning("Relative errors indicate high uncertainty to measured value")}

    # Propagate the error using error_product formula
    sigma <- sqrt((re * tb$a1)^2 +
                    error_product(tb$a2, re*tb$a2, height_timber, re_h*height_timber) +
                    error_product(tb$a3, re*tb$a3, dbh, re_dbh*dbh) +
                    error_product(tb$a4, re*tb$a4, dbh, re_dbh*dbh, height_timber, re_h*height_timber)
    )

    return(data.frame(tariff = tariff, sigma = sigma))
  } else {
    return(tariff)
  }
}

############# Tariff number by stand height (WCC Eq 4) ################
#'
#' @title Tariff number by stand height (WCC Method D)
#' @description Use the estimated stand top height to calculate the stand
#' tariff number based on species-specific coefficients. This function
#' implements WCC Protocol Method D (Section 4.1.4).
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param stand_height Top height in metres. This is the mean height of
#'   the 100 largest trees per hectare (by DBH), as defined in WCC Protocol
#'   Method D. You can calculate top height from tree data using
#'   \code{calculate_top_height()}.
#' @param spcode species code
#' @param re_h relative error of height measurement (optional)
#' @param re relative error of coefficients (default = 0.025)
#' @return either tariff number or if re_h is provided, then returns a list
#' of the tariff number and an estimate of sigma for tariff number
#' @details
#'   This function implements WCC Protocol Method D (Section 4.1.4). Top height
#'   is a specific forestry measurement defined as the mean height of the 100
#'   largest trees per hectare (by DBH). This method is intended for stand-level
#'   assessments when individual tree DBH measurements are not available.
#'
#'   Use this function when:
#'   - You have measured top height following WCC protocol
#'   - You do NOT have individual tree DBH measurements
#'   - You're doing a stand-level carbon assessment
#'
#'   If:
#'   - You have individual tree DBH → Use \code{broadleaf_tariff()} or
#'     \code{conifer_tariff()}
#'   - You need to calculate top height from tree data → Use \code{calculate_top_height()} first
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 4.1.4, Method D.
#' @examples
#' # Example 1: Using measured top height
#' stand_tariff("OK", stand_height = 18)
#'
#' # Example 2: With uncertainty
#' stand_tariff("OK", stand_height = 18, re_h = 0.05)
#'
#' # Example 3: Calculate top height from tree data, then use stand_tariff
#' dbh <- c(45, 52, 38, 60, 42, 55, 48, 35, 58, 40)
#' height <- c(18, 22, 15, 24, 17, 21, 19, 14, 23, 16)
#' area_ha <- 0.1
#' top_h_result <- calculate_top_height(dbh, height, area_ha, re_h = 0.05)
#' # Use top height and propagate uncertainty
#' tariff <- stand_tariff("OK", stand_height = top_h_result$top_height,
#'                        re_h = top_h_result$re_h)
#' @export
#' @aliases stand_tariff
#'
stand_tariff <- function(spcode, stand_height, re_h = NULL, re = 0.025) {

  # Check stand_height is numeric and positive
  if(!is.numeric(stand_height) || any(stand_height < 0, na.rm = TRUE))
    stop("stand_height must be numeric and positive")

  # Warn if stand_height seems unusually low (might be individual tree height)
  if (any(stand_height < 5, na.rm = TRUE)) {
    warning("Stand height values < 5m are unusual. Ensure you're using the mean height of the 100 largest trees per hectare (top height), not individual tree height or mean of all trees. See ?calculate_top_height for help calculating top height from tree data.")
  }

  # Lookup coefficients for tariff equation by spcode in tarif2heightdf
  lookup_index <- match(spcode, tarif2heightdf$abbreviation)
  rec <- tarif2heightdf[lookup_index, ]

  # Get a list of spcodes with missing coefficients
  missing_species <- is.na(rec$abbreviation)

  # If there are any spcodes with missing coefficients
  if(any(missing_species)) {

    # Lookup using the short species code
    subcode <- lookup_df$stand[match(spcode[missing_species], lookup_df$short)]
    sub_index <- match(subcode, tarif2heightdf$abbreviation)
    rec[missing_species, ] <- tarif2heightdf[sub_index, ]

    # Still missing list
    still_missing <- is.na(rec$abbreviation)

    # If species are still missing after lookup, produce error message
    if(any(still_missing)) {
      warning(paste(paste(unique(spcode[still_missing]), collapse = ", "),
                    "species codes not found"))}
  }
  # Calculate tariff number using coefficients stored in rec
  tariff <- rec$a1 + (rec$a2 * stand_height) + (rec$a3 * stand_height^2)

  # If re_h is specified and not null, then calculate the error
  if(!is.null(re_h)){

    # Check inputs for calculating the error
    if(!is.numeric(re_h) || any(re_h<0))stop("re_h must be numeric and positive")
    if (re_h > 1 || re_h > 1)
      warning("Relative error indicate high uncertainty to measured value")

    # Propagate the error using error_product formula
    sigma <- sqrt((rec$a1 * re)^2 +
                    error_product(rec$a2, rec$a2 * re, stand_height, re_h*stand_height))

    # If rec$a3 is not equal to zero and included in the formula then
    if (any(rec$a3 != 0, na.rm = TRUE)) {

      # Calculate the error for the quadratic term in the formula
      quad <- error_product(rec$a3, rec$a3 * re, stand_height, re_h*stand_height)
      # For the terms with rec$a3 = 0, set the quad term to 0
      quad[is.na(quad)] <- 0
      # Propagate terms using addition equation and sum errors
      sigma <- sqrt(sigma^2 + quad)
    }

    return(data.frame(tariff = tariff, sigma = sigma))
  } else {
    return(tariff)
  }
}

############# Calculate Stand Top Height (WCC Method D) ################
#'
#' @title Calculate stand top height from tree data
#' @description Calculates top height (mean height of the 100 largest trees per
#'   hectare by DBH) from woodland tree data, as defined in WCC Protocol Method D
#'   (Section 4.1.4). Top height is a specific forestry measurement required for
#'   stand-level carbon assessments when individual tree DBH measurements are not
#'   available.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh Diameter at breast height in centimetres (vector of tree measurements)
#' @param height Tree height in metres (vector of tree measurements, same length as dbh)
#' @param area_ha Area of the woodland/stand in hectares. If NULL, assumes data
#'   represents trees per hectare (i.e., length(dbh) = trees per hectare)
#' @param re_h Relative error of height measurement (optional, for uncertainty calculation)
#' @param n_trees Number of largest trees per hectare to use (default = 100, as per WCC protocol)
#' @return If re_h is NULL: numeric value of top height in metres. If re_h is
#'   provided: data.frame with top_height, sigma (uncertainty), and re_h
#'   (relative error for use in stand_tariff).
#' @details
#'   Top height is defined in WCC Protocol v2.0 (Section 4.1.4) as the mean height
#'   of the 100 largest trees per hectare (by DBH). This is a stand-level measurement
#'   used in Method D when individual tree DBH measurements are not available for
#'   all trees.
#'
#'   The function calculates top height by:
#'   1. Determining trees per hectare (from area_ha or assuming data is per hectare)
#'   2. Selecting the n_trees largest trees by DBH (or all trees if fewer than n_trees)
#'   3. Calculating the mean height of those trees
#'   4. Returning the top height with optional uncertainty
#'
#'   **When to use this function:**
#'   - You have tree DBH and height measurements from a sample plot
#'   - You want to calculate top height from plot data for Method D
#'   - You're doing stand-level carbon assessment from plot samples
#'
#'   **When NOT to use this function:**
#'   - You have measured top height directly in the field → Use \code{stand_tariff()} directly
#'   - You have DBH and height for all trees → Use \code{fc_agc()} for individual trees (Method B/C/E)
#'   - You want individual tree carbon estimates → Use \code{fc_agc()}
#'
#'   **Note:** Top height can be measured directly in the field (standard forestry practice)
#'   without needing individual tree measurements. This function is a helper for when
#'   you have plot data and want to calculate top height from it.
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 4.1.4, Method D.
#' @examples
#' # Example 1: Stand with known area
#' dbh <- c(45, 52, 38, 60, 42, 55, 48, 35, 58, 40)  # 10 trees
#' height <- c(18, 22, 15, 24, 17, 21, 19, 14, 23, 16)
#' area_ha <- 0.1  # 0.1 hectare plot
#'
#' top_h <- calculate_top_height(dbh, height, area_ha)
#' print(top_h)
#'
#' # Example 2: With uncertainty
#' top_h_unc <- calculate_top_height(dbh, height, area_ha, re_h = 0.05)
#' print(top_h_unc)
#'
#' # Example 3: Data already represents trees per hectare (omit area_ha)
#' dbh_ha <- c(rep(45, 50), rep(52, 30), rep(38, 20))  # 100 trees/ha
#' height_ha <- c(rep(18, 50), rep(22, 30), rep(15, 20))
#' top_h2 <- calculate_top_height(dbh_ha, height_ha, area_ha = NULL)
#'
#' # Example 4: Use top height with stand_tariff
#' spcode <- "OK"  # Oak
#' tariff <- stand_tariff(spcode, stand_height = top_h)
#' @export
#' @aliases calculate_top_height
#'
calculate_top_height <- function(dbh, height, area_ha = NULL, re_h = NULL, n_trees = 100) {

  # Validate inputs
  if (!is.numeric(dbh) || any(dbh < 0, na.rm = TRUE)) {
    stop("dbh must be numeric and non-negative")
  }
  if (!is.numeric(height) || any(height < 0, na.rm = TRUE)) {
    stop("height must be numeric and non-negative")
  }
  if (length(dbh) != length(height)) {
    stop("dbh and height must have the same length")
  }
  if (!is.null(area_ha) && (!is.numeric(area_ha) || area_ha <= 0)) {
    stop("area_ha must be numeric and positive")
  }
  if (!is.numeric(n_trees) || n_trees <= 0) {
    stop("n_trees must be numeric and positive")
  }

  # Remove invalid values (NA, zero, or negative)
  valid_idx <- !is.na(dbh) & !is.na(height) & dbh > 0 & height > 0
  if (!any(valid_idx)) {
    stop("No valid tree measurements (dbh > 0 and height > 0) found")
  }

  dbh <- dbh[valid_idx]
  height <- height[valid_idx]
  n_measured <- length(dbh)

  # Calculate trees per hectare
  if (!is.null(area_ha)) {
    trees_per_ha <- n_measured / area_ha
  } else {
    # Assume data represents trees per hectare
    trees_per_ha <- n_measured
    warning("area_ha not provided. Assuming data represents trees per hectare. ",
            "If this is a plot sample, provide area_ha to scale correctly.")
  }

  # Select the n_trees largest trees per hectare by DBH
  # If we have more than n_trees per hectare, select proportionally from sample
  if (trees_per_ha > n_trees) {
    n_select <- round((n_trees / trees_per_ha) * n_measured)
    n_select <- max(1, min(n_select, n_measured))  # At least 1, at most n_measured
  } else {
    # Fewer than n_trees per hectare, use all trees
    n_select <- n_measured
  }

  # Sort by DBH (descending) and select the n_select largest trees
  sorted_idx <- order(dbh, decreasing = TRUE)
  selected_idx <- sorted_idx[1:n_select]
  selected_heights <- height[selected_idx]

  # Calculate mean height (top height)
  top_height <- mean(selected_heights, na.rm = TRUE)

  # Calculate uncertainty if re_h is provided
  if (!is.null(re_h)) {
    if (!is.numeric(re_h) || any(re_h < 0, na.rm = TRUE)) {
      stop("re_h must be numeric and non-negative")
    }
    if (any(re_h > 1, na.rm = TRUE)) {
      warning("Relative error indicates high uncertainty to measured value")
    }

    # Uncertainty of mean: σ_mean = re_h * sqrt(sum(h_i²)) / n
    # For single re_h value
    if (length(re_h) == 1) {
      sigma_mean <- re_h * sqrt(sum(selected_heights^2, na.rm = TRUE)) / n_select
    } else {
      # Vector of re_h values
      if (length(re_h) != length(valid_idx)) {
        stop("re_h must be a single value or have the same length as dbh/height")
      }
      re_h_selected <- re_h[valid_idx][selected_idx]
      sigma_mean <- sqrt(sum((re_h_selected * selected_heights)^2, na.rm = TRUE)) / n_select
    }

    # Relative error for use in stand_tariff
    re_h_mean <- sigma_mean / top_height

    return(data.frame(top_height = top_height,
                     sigma = sigma_mean,
                     re_h = re_h_mean))
  } else {
    return(top_height)
  }
}

############# Tariff number depending on inputs ################
#' @title Calculate tariff numbers for individual trees
#' @description Calculate the tariff number using either broadleaf_tariff (Method B) or
#'   conifer_tariff (Method C) for individual trees. Requires both DBH and height measurements.
#'   For stand-level calculations (Method D), use \code{stand_tariff()} directly 
#'   with top height from \code{calculate_top_height()}.
#' @author Isabel Openshaw. I.Openshaw@kew.org, Justin Moat. J.Moat@kew.org
#' @param spcode species code (short)
#' @param height tree height in metres (total height for conifers, used as fallback for broadleaves)
#' @param dbh diameter at breast height in centimetres (required)
#' @param type conifer or broadleaf
#' @param height_timber timber height in metres (height to first branch, for broadleaves only)
#' @param re_h relative error of height measurement (optional)
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re relative error of coefficients (default = 0.025)
#' @return either tariff number or if re_h is provided, then returns a list
#'   of the tariff number and uncertainty
#' @details
#'   This function calculates tariff numbers for individual trees using Methods B 
#'   (broadleaves) or C (conifers) from WCC Protocol. Both DBH and height are required.
#'   
#'   For stand-level calculations (Method D) when DBH is not available, use:
#'   \itemize{
#'     \item \code{calculate_top_height()} to calculate top height from tree data
#'     \item \code{stand_tariff()} to calculate stand tariff from top height
#'   }
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' # Individual trees (Method B/C)
#' tariffs(spcode = "OK", height = 18, dbh = 45)
#' tariffs("OK", 18, 45, re_h = 0.1, re_dbh = 0.05)
#' 
#' # Multiple trees
#' tariffs(spcode = c("OK","NS", "SP"), 
#'         height = c(18, 20, 22), 
#'         dbh = c(45, 38, 42), 
#'         re_h = 0.05, re_dbh = 0.05)
#' @export
#' @aliases tariffs
#'
tariffs <- function(spcode, height, dbh, type = NULL,
                    height_timber = NULL, re_h = NULL, re_dbh = 0.05, re = 0.025) {

  # Validate inputs
  if (!is.character(spcode)) warning("spcode must be characters")
  if (is.null(dbh) || all(is.na(dbh))) {
    stop("dbh is required for individual tree tariff calculations. ",
         "For stand-level calculations (Method D), use stand_tariff() with top height.")
  }

  # Set n as the length of height input
  n <- length(height)
  # Check that input lengths are consistent
  if (length(spcode) != n || length(dbh) != n) {
    stop("Input vectors (spcode, height, dbh) must have the same length.")
  }

  if (!is.null(height_timber)) {
    if (!is.numeric(height_timber)) {
      stop("height_timber must be numeric")
    }
  }

  # Lookup type (conifer or broadleaf classification) using lookup_df
  lookup_index <- match(spcode, lookup_df$short)
  lookup_type <- lookup_df$type[lookup_index]
  use_original <- is.na(lookup_type) | lookup_type == "any"
  type <- ifelse(use_original, type, lookup_type)

  # Initialise output data frame
  results <- data.frame(tariff = rep(NA, n))
  error <- !is.null(re_h)
  if (error) results$sigma <- rep(NA, n)

  # Determine which function to use
  NA_type <- is.na(type) | type == "NA" | type == "any"
  NA_spcode <- is.na(spcode) | spcode == "NA" | spcode == "MX"
  spcode_type <- !(NA_spcode & NA_type)
  height_dbh <- !(is.na(dbh) | is.na(height))

  conifer_indices <- type == "conifer" & height_dbh & spcode_type
  broadleaf_indices <- type == "broadleaf" & height_dbh & spcode_type
  mixed_indices <- ((NA_spcode & is.na(type)) | type == "any" | type == "NA") & height_dbh

  if(any(NA_spcode)) warning("Skipping calculations for unfound spcode inputs")

  # Helper function to extract result from tariff function
  process_result <- function(tariff_output, indices, results, error) {
    if(error && "sigma" %in% names(tariff_output)) {
      results$tariff[indices] <- tariff_output$tariff
      results$sigma[indices]  <- tariff_output$sigma
    } else {
      results$tariff[indices] <- tariff_output
    }
    return(results)
  }

  # Apply tariff functions
  if (any(conifer_indices, na.rm = TRUE)) {
    tariff_output <- suppressWarnings(
      conifer_tariff(spcode[conifer_indices], height[conifer_indices],
                     dbh[conifer_indices], re_h, re_dbh, re))
    results <- process_result(tariff_output, conifer_indices, results, error)
  }
  
  if (any(broadleaf_indices, na.rm = TRUE)) {
    # For broadleaves, use height_timber if provided, otherwise use height
    # WCC protocol requires timber height for broadleaves
    broadleaf_height <- if (!is.null(height_timber)) {
      height_timber[broadleaf_indices]
    } else {
      height[broadleaf_indices]
    }
    tariff_output <- suppressWarnings(
      broadleaf_tariff(spcode[broadleaf_indices], broadleaf_height,
                       dbh[broadleaf_indices], re_h, re_dbh, re))
    results <- process_result(tariff_output, broadleaf_indices, results, error)
  }

  # Handle mixed species case
  if (any(mixed_indices, na.rm = TRUE)) {
    mixed_height_timber <- if (!is.null(height_timber)) {
      height_timber[mixed_indices]
    } else {
      height[mixed_indices]
    }
    broadleaf_values <- broadleaf_tariff(rep("XB", sum(mixed_indices)), 
                                         mixed_height_timber, dbh[mixed_indices], 
                                         re_h, re_dbh, re)
    conifer_values <- conifer_tariff(rep("XC", sum(mixed_indices)), 
                                     height[mixed_indices], dbh[mixed_indices], 
                                     re_h, re_dbh, re)

    if (error) {
      results$tariff[mixed_indices] <- rowMeans(cbind(broadleaf_values$tariff, 
                                                      conifer_values$tariff), na.rm = TRUE)
      results$sigma[mixed_indices] <- sqrt(broadleaf_values$sigma^2 + conifer_values$sigma^2)
    } else {
      results$tariff[mixed_indices] <- rowMeans(cbind(broadleaf_values, conifer_values), na.rm = TRUE)
    }
  }

  # Return output
  if (error) {
    return(results)
  } else {
    return(results$tariff)
  }
}

############# Tree merchantable volume (WCC Eq 5) ################
#'
#' @title Forestry merchantable volume
#' @description Use the tree tariff number and dbh to estimate the mean
#' merchantable tree volume.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param tariff tree or stand tariff number
#' @param dbh diameter at breast height in centimetres
#' @param sig_tariff tariff sigma (optional)
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re relative error of coefficients (default = 0.025)
#' @return  volume in metres cubed and error if sig of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' merchtreevol(dbh = 24, tariff = 24)
#' merchtreevol(dbh = 24, tariff = 24, re_dbh = 0.05, sig_tariff = 1)
#' @export
#' @aliases merchtreevol
#'
merchtreevol <- function(dbh, tariff, re_dbh = 0.05, sig_tariff = NULL, re = 0.025) {
  if( !is.numeric(tariff)) stop("tariff must be numeric")
  if( !is.numeric(dbh) || any(dbh < 0, na.rm = TRUE)){
    warning("dbh must be numeric and positive")
  }

  # Constants
  k1 <- 0.315049301
  k2 <- 0.138763302
  k3 <- 0.0360541
  k4 <- 0.118288

  ba <- (pi * dbh^2) / 40000
  a2 <- k1 * (tariff - k2)
  a1 <- (k3 * tariff) - (a2 * k4)
  vol <- a1 + (a2 * ba)

  if(is.null(sig_tariff)) {
    return(vol)
  } else {
    if(!is.numeric(re_dbh)||re_dbh<0){stop("'re_dbh' must be positive & numeric")}
    if (re_dbh > 1 || re_dbh > 1){
      warning("Relative errors indicate high uncertainty to measured value")
    }

    sig_ba <- (pi * dbh / 20000) * re_dbh * dbh
    sig_a2 <- k1 * sig_tariff
    sig_a1 <- sqrt(
      (k3 - k1 * k4)^2 * sig_tariff^2 +
        (k4 * sig_a2)^2
    )
    sigma <- sqrt(sig_a1^2 + (ba * sig_a2)^2 + (a2 * sig_ba)^2)

    result <- list(volume = vol, sigma = sigma)
    return(result)
  }
}

############# Stem tree volume ################
#'
#' @title Forestry commission tree wood volume
#' @description Calculate the stem volume by multiplying the merchantable tree
#' volume by the appropriate species multiplication factor from stemvoldf.rda
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param mtreevol merchantable tree volume
#' @param dbh diameter at breast height in centimeters (greater than 6.5 cm)
#' @param sig_mtreevol sigma for mtreevol (optional)
#' @param re relative error of conversion factor (default = 0.025)
#' @return volume metres cubed or if sig_mtreevol is provided then additionally
#'  returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' treevol(mtreevol = 10, dbh = 24)
#' treevol(mtreevol = 10, dbh = 24, sig_mtreevol = 0.07)
#' treevol(mtreevol = c(10,10), dbh = c(20,24), sig_mtreevol = c(1,1))
#' @export
#' @aliases treevol
#'
treevol <- function(mtreevol, dbh, sig_mtreevol = NULL, re = 0.025) {
  # Error handling for inputs
  if (!is.numeric(dbh) || any(dbh < 0, na.rm = TRUE))stop("dbh must be numeric and positive")
  if (!is.numeric(mtreevol)) stop("mtreevol must be numeric")

  if (length(dbh) != length(mtreevol)) {
    stop("dbh and mtreevol must have the same length")
  }
  if(any(dbh < 6.5 | dbh > 500, na.rm = TRUE)){
    warning("multiplication factor is not specified for dbh < 6.5 or > 500 cm")
  }

  return_sig <- !is.null(sig_mtreevol)
  if( return_sig ) {
    if (!is.numeric(re) || re < 0) stop("'re' must be positive & numeric")
    if (re > 1) warning("Relative errors indicate high uncertainty to measured value")
    if (!is.numeric(sig_mtreevol) || any(sig_mtreevol < 0, na.rm = TRUE)) {
      stop("sig_mtreevol must be numeric and positive")
    }
  }

  # Round dbh values
  dbh <- round(dbh)

  # Lookup conversion factor (cf) for dbh between 7 and 33, otherwise set to 1
  cf <- ifelse(is.na(dbh), NA, ifelse(dbh >= 7 & dbh < 33,
                                      stemvoldf$X[match(dbh, stemvoldf$dbh..cm.)], 1))

  # Compute stem volume
  stemvol <- ifelse(is.na(cf), NA, cf * mtreevol)

  if (return_sig) {
    sigma <- ifelse(is.na(cf) | is.na(sig_mtreevol), NA,
                    error_product(cf, cf * re, mtreevol, sig_mtreevol, returnv = "sigma"))

    result <- data.frame(stemvolume = stemvol, sigma = sigma)
  } else {
    result <- stemvol
  }
  return(result)
}

############# Wood biomass ################
#'
#' @title Forestry Commission wood biomass (WCC component)
#' @description Multiply the mean total tree volume by the nominal specific
#' gravity (NSG) to give the stem biomass, in oven dry tonnes. This is a
#' component calculation within the Woodland Carbon Code methodology.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param treevol tree volume in metres cubed
#' @param nsg Nominal Specific Gravity (g/cm^3 or t/m^3)
#' @param sig_treevol tree volume sigma (optional)
#' @param sig_nsg sigma for NSG (default = 0.094, from UK timber data)
#' @return biomass in oven dry tonnes, or if sig_treevol is provided,
#'   a list with woodbiomass and sigma.
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Lavers, G.M. and Moore, G.L.
#' (1983) The strength properties of timber. Building Research Establishment
#' Report CI/Sfb i(J3). Building Research Establishment, Garston.
#' @examples
#' woodbiomass(10, 0.56, 5)
#' @export
#'
woodbiomass <- function(treevol, nsg, sig_treevol = NULL, sig_nsg = 0.09413391) {

  # Check inputs
  if(!is.numeric(treevol)) stop("treevol must be numeric")
  if(!is.numeric(nsg) || any(nsg < 0, na.rm = TRUE)){
    warning("nsg must be numeric and positive")
  }

  # Calculate biomass
  woodbio <- treevol * nsg

  # Calculate uncertainty if provided
  if (is.null(sig_treevol)) {
    return(woodbio)
  } else {
    if(!is.numeric(sig_treevol) || any(sig_treevol < 0, na.rm = TRUE)) {
      stop("'sig_treevol' must be positive & numeric")
    }
    if(!is.numeric(sig_nsg) || any(sig_nsg < 0, na.rm = TRUE)) {
      stop("'sig_nsg' must be positive & numeric")
    }

    sigma <- rep(NA, length(treevol))
    v <- !is.na(sig_treevol) & !is.na(treevol) & !is.na(nsg)
    sigma[v] <- error_product(treevol[v], sig_treevol[v], nsg[v], sig_nsg, returnv = "sigma")

    return(list(woodbiomass = woodbio, sigma = sigma))
  }
}

############# Crown biomass (WCC Eq 6 & 7) ################
#'
#' @title Forestry commission crown biomass estimates
#' @description  Function to find crown biomass (composed of branches,
#' stem tips and foliage) depending on species and dbh
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh diameter at breast height in centimetres
#' @param spcode Crown biomass species code, crown_biomasdf$Code or if not
#' defined for species, lookup_df$short to find relating lookup_df$Crown
#' @param re_d relative error for diameter at breast height measurement (default = 0.05)
#' @param re relative error of coefficients (default = 0.025)
#' @return  biomass (oven dry tonnes) and estimated sigma
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.2.
#' @importFrom utils data
#' @examples
#' crownbiomass("CBSP", 25)
#' crownbiomass("CBOK", dbh = 25, re_d = 0.01)
#' crownbiomass(c("CBOK","CBOK"), dbh = c(30,50), re_d = 0.05)
#' @export
#' @aliases crownbiomass
#'
crownbiomass <- function(spcode, dbh, re_d = NULL, re = 0.025) {
  # Check inputs
  if (length(spcode) != length(dbh)) {
    stop("Length of 'spcode' and 'dbh' must be the same")
  }
  if (!is.numeric(dbh) || any(dbh < 0, na.rm = TRUE)) {
    stop("Argument 'dbh' must be numeric and non-negative")
  }
  if (any(dbh < 7, na.rm = TRUE)) {
    warning("Equation is only specified for dbh >= 7")
  }
  if (!is.null(re_d) && (is.na(re_d) || !is.numeric(re_d) || re_d < 0)) {
    stop("Argument 're_d' must be a positive numeric")
  }
  if (is.na(re) || !is.numeric(re) || re < 0) {
    stop("Argument 're' must be a positive numeric")
  }

  # Access lazy-loaded data directly (LazyData: true handles loading automatically)
  # Vectorized lookup for species codes
  id <- match(spcode, crown_biomasdf$Code)

  # Handle species not found: Lookup alternative match
  missing_id <- is.na(id)
  if (any(missing_id, na.rm = TRUE)) {
    spcode_match <- lookup_df$Crown[match(spcode[missing_id], lookup_df$short)]
    id[missing_id] <- match(spcode_match, crown_biomasdf$Code)
  }

  # Identify missing species codes
  if (any(is.na(id), na.rm = TRUE)) {
    warning("species codes not found: ", paste(spcode[is.na(id)], collapse = ", "))
  }

  # Extract coefficients for matched species
  rec_b1 <- crown_biomasdf$b1[id]
  rec_b2 <- crown_biomasdf$b2[id]
  rec_A  <- crown_biomasdf$A[id]
  rec_p  <- crown_biomasdf$p[id]

  # Calculate biomass using vectorized conditions
  dp <- dbh^rec_p
  biomass <- ifelse(dbh <= 50, rec_b1 * dp, rec_A + rec_b2 * dbh)

  # Compute error propagation if re_d is provided
  if (!is.null(re_d)) {
    sigma_dp <- dp * sqrt((rec_p * re_d)^2 + (log(dbh) * rec_p * re)^2)
    sigma <- ifelse(dbh <= 50,
                    error_product(rec_b1, rec_b1 * re, dp, sigma_dp),
                    sqrt((rec_A * re)^2 + (dbh * rec_b2 * re)^2 + (rec_b2 * dbh * re_d)^2))

    return(data.frame(spcode=spcode, dbh=dbh, biomass=biomass, sigma=sigma))

  } else {

    return(biomass)
  }
}

############# Root Biomass (WCC Eq 8 & 9) ################
#'
#' @title Forestry commission root biomass estimates
#' @description Function to calculate the root biomass depending on species and dbh
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh diameter at breast height (1.3 m) in centimetres
#' @param spcode species code
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re relative error of coefficients of the equation (default = 0.025)
#' @return biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.3.
#' @examples
#' rootbiomass(spcode = 'RBRAR', dbh = 50)
#' rootbiomass(spcode = 'RBRAR', dbh = 50, re_dbh = 0.10)
#' rootbiomass(spcode = c('RBRAR','RBRAR'), dbh = c(50, 70))
#' @export
#' @aliases rootbiomass
#'
rootbiomass <- function(spcode, dbh, re_dbh = NULL, re = 0.025) {
  # Check inputs
  if (length(spcode) != length(dbh)) {
    stop("Length of 'spcode' and 'dbh' must be the same")
  }
  if (!is.numeric(dbh) || any(dbh < 0, na.rm = TRUE)) {
    stop("All values of 'dbh' must be numeric and non-negative")
  }
  if (!is.null(re_dbh) && (!is.numeric(re_dbh) || any(re_dbh < 0, na.rm = TRUE))) {
    stop("Argument 're_dbh' must be numeric and non-negative")
  }
  if (!is.numeric(re) || re < 0) {
    stop("Argument 're' must be numeric and non-negative")
  }

  # Vectorized lookup for species codes
  match_idx <- match(spcode, root_biomassdf$Code)

  # Handle species not found: Lookup alternative match
  missing_idx <- is.na(match_idx)
  if (any(missing_idx, na.rm = TRUE)) {
    spcode_match <- lookup_df$Root[match(spcode[missing_idx], lookup_df$short)]
    match_idx[missing_idx] <- match(spcode_match, root_biomassdf$Code)
  }

  # Identify missing species codes
  if (any(is.na(match_idx), na.rm = TRUE)) {
    warning("Some species codes not found: ", paste(spcode[is.na(match_idx)], collapse = ", "))
  }

  # Extract coefficients for matched species
  rec_b1 <- root_biomassdf$b1[match_idx]
  rec_b2 <- root_biomassdf$b2[match_idx]
  rec_a  <- root_biomassdf$a[match_idx]

  # Calculate root biomass using vectorized conditions
  root_biomass <- ifelse(dbh <= 30, rec_b1 * dbh^2.5, rec_a + rec_b2 * dbh)

  # Compute error propagation if re_dbh is provided
  if (!is.null(re_dbh)) {
    sig_diam <- dbh^2.5 * sqrt((2.5 * re_dbh)^2 + (log(dbh) * 2.5 * re)^2)
    sigma <- ifelse(dbh <= 30,
                    error_product(rec_b1, rec_b1 * re, dbh^2.5, sig_diam),
                    sqrt((rec_a * re)^2 + error_product(rec_b2, rec_b2 * re, dbh, re_dbh * dbh))
    )

    # Return full dataframe with errors
    results <- data.frame(spcode = spcode, dbh = dbh, rootbiomass = root_biomass, sigma = sigma)
  } else {
    # Return just the biomass vector if re_dbh is NULL
    return(root_biomass)
  }

  return(results)
}


