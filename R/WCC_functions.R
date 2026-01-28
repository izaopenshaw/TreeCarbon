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
############# Tariff number from volume and tree basal area (WCC Eq 1) ############
#'
#' @title Tariff number from volume and basal area
#' @description Using the sample tree’s basal area and volume to calculate the
#' tariff number. Basal area is calculated by ba = (pi * dbh^2)/40000.
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
#' @title Conifer tree tariff number
#'  sample tree. species-specific estimates of a1-a3 are found in the
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
#' @title Carbon tariff number for broadleaf tree
#' @description Use dbh and tree height to derive the tariff number of each
#' sample tree. species-specific estimates of a1-a4 are found in the
#' R data file, 'tariff_broaddf'.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param dbh diameter at breast height in centimetres
#' @param spcode species code (single)
#' @param re_h relative error of height measurement (optional)
#' @param re_dbh relative error for dbh/percentage measurement error (optional)
#' @param re relative error of coefficients (default = 0.025)
#' @return  tariff number or if relative errors are provided returns a list of
#' tariff number and an estimate for sigma
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Method B, Equation 2.
#' @importFrom utils data
#' @examples broadleaf_tariff(spcode = 'OK', height = 25, dbh = 75)
#' broadleaf_tariff(spcode = "OK", height = 25, dbh = 15, re_dbh = 0.05, re_h = 0.1)
#' broadleaf_tariff(spcode = "OK", height = 24, dbh = 15, re_dbh = 0.05, re_h = 0.1)
#' @export
#' @aliases broadleaf_tariff
#'
broadleaf_tariff <- function(spcode, height, dbh, re_dbh = NULL, re_h = 0.1, re = 0.025) {

  # Check inputs are numeric and positive
  if (!is.numeric(height) | any(height < 0, na.rm = TRUE) |
      !is.numeric(dbh) | any(dbh < 0, na.rm = TRUE)){
    stop("dbh must be numeric and positive")
  }

  # Check input lengths are the same
  if (!(length(spcode) == length(height) && length(height) == length(dbh))) {
    stop("Input vectors for spcode, height & dbh must have the same length.")
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
  tariff <- tb$a1 + (tb$a2 * height) + (tb$a3 * dbh) + (tb$a4 * dbh * height)

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
                    error_product(tb$a2, re*tb$a2, height, re_h*height) +
                    error_product(tb$a3, re*tb$a3, dbh, re_dbh*dbh) +
                    error_product(tb$a4, re*tb$a4, dbh, re_dbh*dbh, height, re_h*height)
    )

    return(data.frame(tariff = tariff, sigma = sigma))
  } else {
    return(tariff)
  }
}

############# Tariff number by stand height (WCC Eq 4) ################
#'
#' @title Tariff number by stand height
#' @description Use the estimated stand top height to calculate the stand
#' tariff number based on species-specific coefficients.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param spcode species code
#' @param re_h relative error of height measurement (optional)
#' @param re relative error of coefficients (default = 0.025)
#' @return either tariff number or if re_h is provided, then returns a list
#' of the tariff number and an estimate of sigma for tariff number
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' stand_tariff("OK", height = 10)
#' stand_tariff("OK", height = 10, re_h = 0.01)
#' stand_tariff(spcode = "AH", height = 10, re_h = 0.05)
#' @export
#' @aliases stand_tariff
#'
stand_tariff <- function(spcode, height, re_h = NULL, re = 0.025) {

  # Check height is numeric and positive
  if(!is.numeric(height) || any(height < 0, na.rm = TRUE))
    stop("height must be numeric and positive")

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
  tariff <- rec$a1 + (rec$a2 * height) + (rec$a3 * height^2)

  # If re_h is specified and not null, then calculate the error
  if(!is.null(re_h)){

    # Check inputs for calculating the error
    if(!is.numeric(re_h) || any(re_h<0))stop("re_h must be numeric and positive")
    if (re_h > 1 || re_h > 1)
      warning("Relative error indicate high uncertainty to measured value")

    # Propagate the error using error_product formula
    sigma <- sqrt((rec$a1 * re)^2 +
                    error_product(rec$a2, rec$a2 * re, height, re_h*height))

    # If rec$a3 is not equal to zero and included in the formula then
    if (any(rec$a3 != 0, na.rm = TRUE)) {

      # Calculate the error for the quadratic term in the formula
      quad <- error_product(rec$a3, rec$a3 * re, height, re_h*height)
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

############# Tariff number depending on inputs ################
#' @title Calculate tariff numbers for list of trees
#' @description Depending on inputs, calcualte the tariff number using either
#' stand_tariff, broadleaf_tariff or conifer_tariff for a list of trees.
#' @author Isabel Openshaw. I.Openshaw@kew.org, Justin Moat. J.Moat@kew.org
#' @param spcode species code (short)
#' @param height tree height in metres
#' @param re_h relative error of height measurement (optional)
#' @param dbh diameter at breast height in centimetres
#' @param type conifer or broadleaf
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re relative error of coefficients (default = 0.025)
#' @return either tariff number or if re_h is provided, then returns a list
#' of the tariff number and uncertainty
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' tariffs(spcode = "OK", height= 10, dbh = 20)
#' tariffs("OK", 10, 20, re_h = 0.1, re_dbh = 0.05)
#' tariffs(spcode = c("OK","NS", "NS", "SP", "SP"), height = c(10,10,5,10,10),
#' dbh = c(20,20,10,20,NA), re_h = 0.01, re_dbh = 0.05)
#' @export
#' @aliases tariffs
#'
tariffs <- function(spcode, height, dbh = NULL, type = NULL,
                    re_h = NULL, re_dbh = 0.05, re = 0.025) {

  # Check spcode is a character
  if (!is.character(spcode)) warning("spcode must be characters")

  # Set n as the length of height input
  n <- length(height)
  # Check that input lengths are consistent
  if (!(length(spcode) == n || length(dbh) == n )) {
    stop("Input vectors (spcode, height, dbh) must have the same length.")
  }

  # Lookup type (conifer or broadleaf classification) using lookup_df
  lookup_index <- match(spcode, lookup_df$short) # index lookup for efficiency
  lookup_type <- lookup_df$type[lookup_index]  # list of types
  # Set use_original to if lookup_type is NA or 'any' from lookup_df
  use_original <- is.na(lookup_type) | lookup_type == "any"
  # If use_original is NA or 'any' then use the input classification type
  type <- ifelse(use_original, type, lookup_type)

  # Initialise an output data frame
  results <- data.frame(tariff = rep(NA, n))
  # If re_h is NA then create df for the error
  error <- !is.null(re_h)
  if (error) results$sigma <- rep(NA, n)

  # Logical lists to know which function to use depending on inputs
  # NA_type is TRUE if type is NA or equal to 'any'
  NA_type <- is.na(type)   | type == "NA" | type == "any"
  # NA_spcode is TRUE if spcode is NA or equal to 'MX'
  NA_spcode <- is.na(spcode) | spcode == "NA" | spcode == "MX"
  # spcode_type is TRUE if spcode or type are inputted
  spcode_type <- !(NA_spcode & NA_type)
  # height_dbh is TRUE if height and dbh are both inputted
  height_dbh <- !(is.na(dbh) | is.na(height))

  # For each input get the index depended on inputs for the function to use
  stand_indices <- is.na(dbh) & !is.na(height) & !NA_spcode
  conifer_indices   <- type == "conifer" & height_dbh & spcode_type
  broadleaf_indices <- type == "broadleaf" & height_dbh & spcode_type
  mixed_indices <- ((NA_spcode & is.na(type) ) | type == "any" | type == "NA") & height_dbh

  if(any(NA_spcode)) warning ("Skipping calculations for unfound spcode inputs")

  # Helper Function to extract result from tariff function
  process_result <- function(tariff_output, indices, results, error) {
    if(error && "sigma" %in% names(tariff_output)) {
      results$tariff[indices] <- tariff_output$tariff
      results$sigma[indices]  <- tariff_output$sigma
    } else {
      results$tariff[indices] <- tariff_output
    }
    return(results)
  }

  # Apply Tariff Functions Using the Helper Function
  if (any(conifer_indices, na.rm = TRUE)) {
    tariff_output <- suppressWarnings(
      conifer_tariff(spcode[conifer_indices], height[conifer_indices],
                     dbh[conifer_indices], re_h, re_dbh, re))
    results <- process_result(tariff_output, conifer_indices, results, error)
  }
  if (any(broadleaf_indices, na.rm = TRUE)) {
    tariff_output <- suppressWarnings(
      broadleaf_tariff(spcode[broadleaf_indices], height[broadleaf_indices],
                       dbh[broadleaf_indices], re_h, re_dbh, re))
    results <- process_result(tariff_output, broadleaf_indices, results, error)
  }
  if (any(stand_indices, na.rm = TRUE)) {
    tariff_output <- suppressWarnings(
      stand_tariff(spcode[stand_indices], height[stand_indices], re_h, re))
    results <- process_result(tariff_output, stand_indices, results, error)
  }

  # Handle Mixed species Case
  if (any(mixed_indices, na.rm = TRUE)) {
    broadleaf_values <- broadleaf_tariff(rep("XB", sum(mixed_indices)), height[mixed_indices], dbh[mixed_indices], re_h, re_dbh, re)
    conifer_values <- conifer_tariff(rep("XC", sum(mixed_indices)), height[mixed_indices], dbh[mixed_indices], re_h, re_dbh, re)

    if (error) {
      results$tariff[mixed_indices] <- rowMeans(cbind(broadleaf_values$tariff, conifer_values$tariff), na.rm = TRUE)
      results$sigma[mixed_indices] <- sqrt(broadleaf_values$sigma^2 + conifer_values$sigma^2)
    } else {
      results$tariff[mixed_indices] <- rowMeans(cbind(broadleaf_values, conifer_values), na.rm = TRUE)
    }
  }

  # Return Final Output
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

