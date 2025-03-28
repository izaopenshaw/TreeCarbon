############## Functions file for Woodland Carbon Code ########################
# TODO:
# error for ctoco2e ?
# todo error for nsg
# search not found and check that intermediate species are found in lookup_df
# single crown root dont have a code for Mixed species
# species specific sd for nsg?

############# FC Tariff number from volume and tree basal area (WCC Eq 1) ############
#'
#' @title Tariff number from volume and basal area
#' @description Using the sample tree’s basal area and volume to calculate the
#' tariff number. Basal area is calculated by ba = (pi * dbh^2)/40000.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param vol tree volume in metres cubed
#' @param dbh diameter at breast height in centimetres
#' @param sig_vol sigma for tree volume (optional)
#' @param re_dbh relative measurement error for diameter at breast height (optional)
#' @param re  relative error of coefficients (default = 2.5%)
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

  if (!is.numeric(vol) || !is.numeric(dbh) || any(dbh < 0, na.rm = TRUE)) {
    stop("vol and dbh must be non-negative numeric values")
  }

  # Constants
  coef_a1 <- 3.174106384
  const_vol <- 0.005002986
  const_ba <- 0.003848451
  const_tariff <- 0.138763302

  ba <- (pi * dbh^2) / 40000                # tree basal area in m^2
  a1 <- (vol - const_vol) / (ba - const_ba)
  tariff <- (coef_a1 * a1) + const_tariff

  if(!is.null(sig_vol)){
    if (any(sig_vol < 0, na.rm = TRUE) || !is.numeric(sig_vol)) {
      stop("sigm_vol must be non-negative numeric")}
    if (!is.numeric(re_dbh) || re_dbh < 0)
      stop("Argument 're_dbh' must be positive and numeric")
    if (!is.numeric(re) || re < 0)
      stop("Argument 're' must be positive and numeric")
    if (re_dbh > 1 || re > 1)
      warning("Relative error indicates high uncertainty to measured value")

    sig_ba <- (pi * 2 * dbh / 40000) * re_dbh*dbh
    sig_a1 <- a1 * sqrt(
      (sig_vol / (vol - const_vol))^2 +         # Error from vol
        (sig_ba / (ba - const_ba))^2 +            # Error from ba
        (re * const_vol / (vol - const_vol))^2 +   # Error from const_vol
        (re * const_ba / (ba - const_ba))^2        # Error from const_ba
    )
    sig_t <- sqrt((coef_a1 * sig_a1)^2 + (a1 * re * coef_a1)^2)

    return(list(tariff=tariff, sigma_tariff=sig_t))
  } else {
    return(tariff)
  }
}

############# FC conifer tree tariff number (WCC Eq 3) ############################
#'
#' @title Conifer tree tariff number
#'  sample tree. Species-specific estimates of a1 – a3 are found in the
#'  R data file, 'tariff_coniferdf'.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param dbh diameter at breast height in centimetres
#' @param spcode species code (single)
#' @param re_h relative error of height measurement (optional)
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re  relative error of coefficients (default = 2.5%)
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
conifer_tariff <- function(spcode, height, dbh, re_h = NA, re_dbh = 0.05, re = 0.025) {

  # Check inputs
  if (!is.numeric(height) | any(height < 0, na.rm = TRUE) |
      !is.numeric(dbh) | any(dbh < 0, na.rm = TRUE)){
    stop("dbh must be numeric and positive")
  }
  if (!(length(spcode) == length(height) && length(height) == length(dbh))) {
    stop("Input vectors for spcode, height & dbh must have the same length.")
  }

  lookup_index <- match(spcode, tariff_coniferdf$abbreviation)
  tc <- tariff_coniferdf[lookup_index, ]

  # Missing species codes
  missing_species <- is.na(tc$abbreviation)

  if (any(missing_species)) {

    # Find substitute species
    subcode <- lookup_df$single[match(spcode[missing_species], lookup_df$short)]
    sub_index <- match(subcode, tariff_coniferdf$abbreviation)
    tc[missing_species, ] <- tariff_coniferdf[sub_index, ]

    # Completely missing species
    still_missing <- is.na(tc$abbreviation)
    tc[still_missing, ] <- tariff_coniferdf[tariff_coniferdf$abbreviation == "NS", ]

    unique_missing <- unique(spcode[still_missing])
    if (length(unique_missing) > 0) {
      warning(paste(paste(as.character(unique_missing), collapse = ", "),
                    "species codes not found, general conifer code used."))}
  }

  tariff <- tc$a1 + (tc$a2 * height) + (tc$a3 * dbh)

  if(!is.na(re_h)){
    if(!is.numeric(re_dbh) || any(re_dbh<0))stop("must provide a numeric and positive re_dbh with re_h")
    if(!is.numeric(re_h) || any(re_h<0))stop("must provide a numeric and positive re_h with re_dbh")
    if (re_dbh > 1 || re_dbh > 1 || re_h > 1 || re_h > 1)
      warning("Relative errors indicate high uncertainty to measured value")

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

############# FC broadleaf tree tariff number (WCC Eq 2) ##########################
#'
#' @title Carbon tariff number for broadleaf tree
#' @description Use dbh and tree height to derive the tariff number of each
#' sample tree. Species-specific estimates of a1 – a4 are found in the
#' R data file, 'tariff_broaddf'.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in meters
#' @param dbh diameter at breast height in centimetres
#' @param spcode species code (single)
#' @param re_h relative error of height measurement (optional)
#' @param re_dbh relative error for DBH/percentage measurement error (optional)
#' @param re relative error of coefficients (default = 2.5%)
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
broadleaf_tariff <- function(spcode, height, dbh, re_dbh = NA, re_h = 0.1, re = 0.025) {

  # Check inputs
  if (!is.numeric(height) | any(height < 0, na.rm = TRUE) |
      !is.numeric(dbh) | any(dbh < 0, na.rm = TRUE)){
    stop("dbh must be numeric and positive")
  }

  if (!(length(spcode) == length(height) && length(height) == length(dbh))) {
    stop("Input vectors for spcode, height & dbh must have the same length.")
  }

  lookup_index <- match(spcode, tariff_broaddf$abbreviation)
  tb <- tariff_broaddf[lookup_index, ]

  # Missing species codes
  missing_species <- is.na(tb$abbreviation)

  if (any(missing_species)) {
    # Find substitute species
    subcode <- lookup_df$single[match(spcode[missing_species], lookup_df$short)]
    sub_index <- match(subcode, tariff_broaddf$abbreviation)
    tb[missing_species, ] <- tariff_broaddf[sub_index, ]

    # Missing species
    still_missing <- is.na(tb$abbreviation)
    tb[still_missing, ] <- tariff_broaddf[tariff_broaddf$abbreviation == "BI", ]

    # Warning
    unique_missing <- unique(spcode[still_missing])
    if (length(unique_missing) > 0) {
      warning(paste(paste(as.character(unique_missing), collapse = ", "),
                    "species codes not found, general broadleaf code used."))}
  }

  # Tariff Calculation
  tariff <- tb$a1 + (tb$a2 * height) + (tb$a3 * dbh) + (tb$a4 * dbh * height)

  # Relative errors
  if (!is.na(re_dbh)) {
    if (!is.numeric(re_dbh) || any(re_dbh < 0, na.rm = TRUE)){
      stop("must provide a numeric and positive re_dbh with re_h")}
    if (!is.numeric(re_h) || any(re_h < 0, na.rm = TRUE)){
      stop("must provide a numeric and positive re_h with re_dbh")}
    if (any(re_dbh > 1, na.rm = TRUE) || any(re_h > 1, na.rm = TRUE)){
      warning("Relative errors indicate high uncertainty to measured value")}

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

############# FC tariff number by stand height (WCC Eq 4) ################
#'
#' @title Tariff number by stand height
#' @description Use the estimated stand top height to calculate the stand
#' tariff number.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param spcode species code
#' @param re_h relative error of height measurement (optional)
#' @param re relative error of coefficients (default = 2.5%)
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
stand_tariff <- function(spcode, height, re_h = NA, re=0.025) {
  if(!is.numeric(height) || any(height < 0, na.rm = TRUE))
    stop("height must be numeric and positive")

  lookup_index <- match(spcode, tarif2heightdf$abbreviation)
  rec <- tarif2heightdf[lookup_index, ]

  # Missing Species Codes
  missing_species <- is.na(rec$abbreviation)

  if(any(missing_species)) {
    # Find substitute species using lookup table
    subcode <- lookup_df$stand[match(spcode[missing_species], lookup_df$short)]
    sub_index <- match(subcode, tarif2heightdf$abbreviation)
    rec[missing_species, ] <- tarif2heightdf[sub_index, ]

    # Completely missing species
    still_missing <- is.na(rec$abbreviation)

    # If species are still missing after lookup, stop execution
    if(any(still_missing)) {
      warning(paste(paste(unique(spcode[still_missing]), collapse = ", "),
                    "species codes not found"))}
  }
  tariff <- rec$a1 + (rec$a2 * height) + (rec$a3 * height^2)

  if(!anyNA(re_h)){
    if(!is.numeric(re_h) || any(re_h<0))stop("re_h must be numeric and positive")
    if (re_h > 1 || re_h > 1)
      warning("Relative error indicate high uncertainty to measured value")

    sigma <- sqrt((rec$a1 * re)^2 +
                    error_product(rec$a2, rec$a2 * re, height, re_h*height))

    if (any(rec$a3 != 0, na.rm = TRUE)) { # Include the quadratic term if rec$a3 is nonzero
      quad <- error_product(rec$a3, rec$a3 * re, height, re_h*height)
      quad[is.na(quad)] <- 0
      sigma <- sqrt(sigma^2 + quad)
    }

    return(data.frame(tariff = tariff, sigma = sigma))
  } else {
    return(tariff)
  }
}

############# FC tariff number by stand height (WCC Eq 4) ################
#' @title Tariff number by stand height
#' @description Use the estimated stand top height to calculate the stand
#' tariff number.
#' @author Isabel Openshaw. I.Openshaw@kew.org, Justin Moat. J.Moat@kew.org
#' @param spcode species code (short)
#' @param height tree height in metres
#' @param re_h relative error of height measurement (optional)
#' @param dbh diameter at breast height in centimetres
#' @param type conifer or broadleaf
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re relative error of coefficients (default = 2.5%)
#' @return either tariff number or if re_h is provided, then returns a list
#' of the tariff number and uncertainty
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' tariffs("OK", height= 10, dbh = 20)
#' tariffs("OK", 10, 20, re_h = 0.1, re_dbh = 0.05)
#' tariffs(c("OK","NS", "NS", "SP", "SP"), c(10,10,5,10,10), c(20,20,10,20,NA),
#'  re_h = 0.01, re_dbh = 0.05)
#' @export
#' @aliases tariffs
#'
tariffs <- function(spcode, height, dbh = NULL, type = NA, re_h = NA, re_dbh = 0.05, re = 0.025) {
  if (!is.character(spcode)) warning("spcode must be characters")

  n <- length(height)
  if (!(length(spcode) == n || (length(dbh) == n && anyNA(dbh)))) {
    stop("Input vectors (spcode, height, dbh) must have the same length.")
  }

  # Lookup type
  lookup_index <- match(spcode, lookup_df$short)
  lookup_type <- lookup_df$type[lookup_index]
  use_original <- is.na(lookup_type) | lookup_type == "any"
  type <- ifelse(use_original, type, lookup_type)

  # Initialize output
  results <- data.frame(tariff = rep(NA, n))
  error <- !(is.na(re_h) | is.na(re_dbh) | is.na(re))
  if (error) results$sigma <- rep(NA, n)

  # Vectorized Tariff Calculation
  NA_ty <- is.na(type)   | type == "NA" | type == "any"
  NA_sp <- is.na(spcode) | spcode == "NA" | spcode == "MX"
  sp_ty <- !(NA_sp & NA_ty)
  h_d <- !(is.na(dbh) & is.na(height))

  stand_indices <- is.na(dbh) & !is.na(height) & sp_ty
  conifer_indices   <- type == "conifer" & h_d & sp_ty
  broadleaf_indices <- type == "broadleaf" & h_d & sp_ty
  mixed_indices <- ((NA_sp | is.na(type) | type == "any" | type == "NA" | spcode == "MX")) & h_d

  if(any(NA_sp)) warning ("NA spcode not found, skipping record")

  # Helper Function
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
    tariff_output <- conifer_tariff(spcode[conifer_indices], height[conifer_indices], dbh[conifer_indices], re_h, re_dbh, re)
    results <- process_result(tariff_output, conifer_indices, results, error)
  }
  if (any(broadleaf_indices, na.rm = TRUE)) {
    tariff_output <- broadleaf_tariff(spcode[broadleaf_indices], height[broadleaf_indices], dbh[broadleaf_indices], re_h, re_dbh, re)
    results <- process_result(tariff_output, broadleaf_indices, results, error)
  }
  if (any(stand_indices, na.rm = TRUE)) {
    tariff_output <- stand_tariff(spcode[stand_indices], height[stand_indices], re_h, re)
    results <- process_result(tariff_output, stand_indices, results, error)
  }

  # Handle Mixed Species Case
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

############# FC tree merchantable volume (WCC Eq 5) ################
#'
#' @title Forestry merchantable volume
#' @description Use the tree tariff number and dbh to estimate the mean
#' merchantable tree volume.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param tariff tree or stand tariff number
#' @param dbh diameter at breast height in centimetres
#' @param sig_tariff tariff sigma (optional)
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re relative error of coefficients (default = 2.5%)
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

############# FC stem tree volume ################
#'
#' @title Forestry commission tree wood volume
#' @description Calculate the stem volume by multiplying the merchantable tree
#' volume by the appropriate species multiplication factor from stemvoldf.rda
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param mtreevol merchantable tree volume
#' @param dbh diameter at breast height in centimeters (greater than 6.5 cm)
#' @param sig_mtreevol sigma for mtreevol (optional)
#' @param re relative error of conversion factor (default = 2.5%)
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

############# Propagation of error for a product ################
#' @title Analytical error progression for a product or a quotient
#' @description Calculates sigma squared for f when either f = a * b, f = a / b
#'  or f = a * b * c, f = a / b * c, f = a * b / c
#' @author Isabel Openshaw. I.Openshaw@kew.org, Justin Moat. J.Moat@kew.org
#' @param a first variable in function
#' @param sig_a sigma for a
#' @param b second variable in function
#' @param sig_b sigma for b
#' @param c (optional) third variable in function
#' @param sig_c (optional) sigma for c
#' @param returnv return value, either 'sigma' (standard deviation) or
#' 'sigma squared' (variance)
#' @param fn function describing how the variables are related. If not specified
#'  assumes that the variables are related by a product.
#' @return either an estimate for sigma or sigma squared error propagation for a
#' product or a quotient of two or three variables.
#' @references Taylor, J. R. (1997). An Introduction to Error Analysis: The
#' Study of Uncertainties in Physical Measurements (2nd ed.). University
#' Science Books.
#' @examples
#' error_product(5, 0.01, 10, 0.1)
#' error_product(5, 0.01, 10, 0.1, 5, 0.01)
#' @export
#' @aliases error_product
#'
error_product <- function(a, sig_a, b, sig_b, c = NULL, sig_c = NULL,
                          returnv = "sigmasquared", fn = NULL){
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(sig_a) ||
      !is.numeric(sig_b)) {
    stop("inputs must be numeric")
  }
  if (!is.character(returnv) || !(returnv %in% c("sigma", "sigmasquared"))) {
    stop("returnv must be either 'sigma' or 'sigmasquared'")
  }

  if(is.null(c)) { # function is either a*b or a/b

    if(is.null(fn)){ fn <- a * b }

    if(returnv == "sigma") {
      # sigma
      result <- abs(fn) * sqrt((sig_a / a)^2 + (sig_b / b)^2)
    } else {
      # sigma_squared
      result <- (fn)^2 * (sig_a / a)^2 + (sig_b / b)^2
    }

  } else { # if c is not null, and function is a*b*c or a*b/c or a/b*c
    if (!is.numeric(c) || !is.numeric(sig_c)) {
      stop("c and sig_c must be numeric")
    }
    if(is.null(fn)){ fn <- a * b * c}


    if(returnv == "sigma") {
      # sigma
      result <- abs(fn) * sqrt((sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2)
    } else {
      # sigma_squared
      result <- (fn)^2 * ( (sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2 )
    }
  }

  return(result)
}

############# FC wood biomass ################
#'
#' @title Forestry commission wood biomass
#' @description Multiply the mean total tree volume by the nominal specific
#' gravity to give the biomass, in oven dry tonnes.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param treevol tree volume in metres cubed
#' @param nsg Nominal Specific Gravity
#' @param sig_treevol tree volume sigma (optional)
#' @param sig_nsg sigma for nsg (optional)
#' @return  biomass in oven dry tonnes or if sig_treevol is provided then
#' additionally returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Lavers, G.M. and Moore, G.L.
#' (1983) The strength properties of timber. Building Research Establishment
#' Report CI/Sfb i(J3). Building Research Establishment, Garston.
#' @examples
#' woodbiomass(10, 0.56, 5)
#' @export
#' @aliases woodbiomass
#'
woodbiomass <- function(treevol, nsg, sig_treevol = NULL, sig_nsg = 0.09413391) {

  if(!is.numeric(treevol))stop("treevol must be numeric")
  if(!is.numeric(nsg) || any(nsg < 0, na.rm = TRUE)){
    warning("nsg must be numeric and positive")
  }

  woodbio <- treevol * nsg
  sigma <- rep(NA, length(treevol))

  if(is.null(sig_treevol)) {
    return(woodbio)
    } else {
      if(!is.numeric(sig_treevol) || any(sig_treevol < 0, na.rm = TRUE)) {
        stop("'sig_treevol' must be positive & numeric")}
      if(!is.numeric(sig_nsg) || any(sig_nsg < 0, na.rm = TRUE)) {
        stop("'sig_nsg' must be positive & numeric")}

    v <- !is.na(sig_treevol) & !is.na(treevol) & !is.na(nsg)
    sigma[v] <- error_product(treevol[v], sig_treevol[v], nsg[v], sig_nsg, returnv = "sigma")
    return(list(woodbiomass = woodbio, sigma = sigma))
  }

}

############# FC crown biomass (WCC Eq 6 & 7) ################
#'
#' @title Forestry commission crown biomass estimates
#' @description  Function to find crown biomass (composed of branches,
#' stem tips and foliage) depending on species and dbh
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh diameter at breast height in centimetres
#' @param spcode Crown biomass species code, crown_biomasdf$Code or if not
#' defined for species, lookup_df$short to find relating lookup_df$Crown
#' @param re_d relative error for diameter at breast height measurement (default = 5%)
#' @param re relative error of coefficients (default = 2.5%)
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
    warning("Species codes not found: ", paste(spcode[is.na(id)], collapse = ", "))
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

############# FC Root Biomass (WCC Eq 8 & 9) ################
#'
#' @title Forestry commission root biomass estimates
#' @description Function to calculate the root biomass depending on species and dbh
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh diameter at breast height (1.3 m) in centimetres
#' @param spcode species code
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re relative error of coefficients of the equation (default = 2.5%)
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

############# Carbon to CO2e ################
#'
#' @title Carbon to CO2 equivalent
#' @description Function to convert from carbon to carbon dioxide equivalent
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param carbon carbon
#' @return carbon dioxide equivalent
#' @examples
#' ctoco2e(448)
#' ctoco2e(c(448, 450))
#' @aliases ctoco2e
#' @export
#'
ctoco2e <- function(carbon) {
  # Ensure carbon is numeric and positive
  if (any(!is.numeric(carbon) | carbon < 0, na.rm = TRUE)) {
    stop("All values of 'carbon' must be numeric and positive")
  }

  # Calculate CO2 equivalents for each carbon value
  co2e_values <- carbon * (44 / 12)

  return(co2e_values)
}


############# Plant biomass conversion to carbon ################
#' @title Convert Biomass to Carbon
#' @description Converts biomass values to carbon values using the carbon
#' fraction (CF) from the chosen method or citation.
#' @author Justin Moat <J.Moat@kew.org>, Isabel Openshaw <I.Openshaw@kew.org>
#' @param biomass Numeric vector, representing biomass values (typically in kg
#' or metric tonnes).
#' @param method Character. Method defining the carbon volatile fraction (CF).
#' Supported methods:
#' \itemize{
#'   \item `"Matthews1"`: Simplest, CF = 50% (Matthews, 1993).
#'   \item `"Matthews2"`: CF based on type (broadleaf or conifer).
#'   \item `"IPCC1"`: CF = 47.7% (IPCC, 2006).
#'   \item `"IPCC2"`: Lookup CF by type and biome.
#'   \item `"Thomas"`: Lookup by type and biome (Thomas & Martin, 2012).
#' }
#' Method defining carbon volatile fraction (CF) see CVF_df.RData
#' [1: Matthews, 1993] "Matthews1": Simplest, CF = 50%.
#' "Matthews2": CF based on type (broadleaf or conifer)
#' [2: IPCC, 2006] "IPCC1": CF = 47.7%
#' "IPCC2": Lookup CF by type and biome
#' [3: Thomas and Martin, 2012] "Thomas": Lookup by type and biome
#'
#' @param type Character vector. `"broadleaf"` or `"conifer"`. Required for
#' `"Matthews2"`, `"IPCC2"`, or `"Thomas"`.
#' @param biome Character vector. Biome classification, required for `"IPCC2"`
#' and `"Thomas"` methods. Accepted values: `"tropical"`, `"subtropical"`,
#' `"mediterranean"`, `"temperate"`, or `"boreal"`.
#' @param sig_biomass Numeric vector. Biomass uncertainty (optional, only used
#' with `"IPCC2"` and `"Thomas"` methods).
#' @return Numeric vector of carbon values. If `sig_biomass` is provided,
#' returns a data frame with columns `"AGC"` (above-ground carbon) and
#' `"sig_AGC"` (associated uncertainty).
#'
#' @references
#' Thomas, S.C., & Martin, A.R. (2012). Carbon content of tree tissues: A synthesis.
#' *Forests, 3*(2), 332-352. \doi{10.3390/f3020332}
#'
#' IPCC. (2006). Forest lands. *Intergovernmental Panel on Climate Change Guidelines*
#' for National Greenhouse Gas Inventories, Volume 4, p. 83.
#'
#' Matthews, G.A.R. (1993). *The Carbon Content of Trees.* Forestry Commission Technical
#' Paper 4, Forestry Commission, Edinburgh, 21 pp. ISBN: 0-85538-317-8.
#'
#' [1] Thomas, Sean C., and Adam R. Martin. "Carbon content of tree
#' tissues: a synthesis." Forests 3.2 (2012): 332-352.
#'  https://www.mdpi.com/1999-4907/3/2/332.
#' [2] IPCC. Forest lands. Intergovernmental Panel on Climate Change Guidelines
#'  for National Greenhouse Gas Inventories; Institute for Global Environmental
#'   Strategies (IGES): Hayama,Japan, 2006; Volume 4, p. 83.
#' [3] Matthews, G.A.R. (1993) The Carbon Content of Trees. Forestry Commission
#'  Technical Paper 4. Forestry Commission, Edinburgh. 21pp. ISBN: 0-85538-317-8
#' @examples
#' # Basic conversion using IPCC2 method
#' biomass2c(1, method = "IPCC2", type = "conifer", biome = "temperate")
#'
#' # Vectorized conversion with uncertainty
#' biomass2c(biomass=c(0.5, 0.75, 2, 7), method = "IPCC2", type = rep("broadleaf", 4),
#'           sig_biomass = rep(0.2, 4), biome = "temperate")
#'
#' @importFrom utils globalVariables
#' @aliases biomass2c
#' @export
#'
biomass2c <- function(biomass, method, type = NA, sig_biomass = NULL, biome = 'temperate') {

  # Input validation
  valid_methods <- c("Matthews1", "Matthews2", "IPCC1", "IPCC2", "Thomas")
  if (!(method %in% valid_methods)) stop("Invalid method. Choose from:",
                                         paste(valid_methods, collapse = ", "))

  if (length(type) != length(biomass)) {
    stop("'type' and 'biomass' must have the same length.")} # do i need this for ippc1?

  if (any(!is.numeric(biomass))){
    stop("Biomass values must be numeric and positive")
  }
  # Subset conversion factor table
  CF_data <- CVF_df[CVF_df$method == method, ]

  # Match CF and confidence values
  if (method %in% c("IPCC1", "Matthews1") ) {
    CF <- rep(CF_data$CVF, length(biomass))/ 100

  } else if (method == "Matthews2") {
    index <- match(type, CF_data$type)
    CF <- CF_data$CVF[index] / 100

  } else {
      if(!all(biome %in% CF_data$biome)) {
        stop("Invalid biome for the chosen method. Choose from: ",
             paste(unique(CF_data$biome), collapse = ", "))
      }
      CF_data <- CF_data[CF_data$biome == biome, ]
      index <- match(type, CF_data$type)
      CF <- CF_data$CVF[index] / 100
      sig_CF <- CF_data$confidence[index] / 100 / 1.96

  }

  # Calculate AGC (carbon)
  AGC <- biomass * CF

  # Propagate error if sig_biomass is provided
  if (!is.null(sig_biomass) & method %in% c("Thomas", "IPCC2")) {
    if (length(sig_biomass) != length(biomass)){
      stop("Length of sig_biomass must match biomass length.")
      }
    #sigma_AGC <- AGC * sqrt((sig_biomass / biomass)^2 + (re / 100)^2)
    sigma_AGC <- error_product(biomass, sig_biomass, CF, sig_CF, returnv = "sigma")

    return(data.frame(AGC = AGC, sig_AGC = sigma_AGC))
  } else {
    return(AGC)
  }
}

############# FC Seedlings and saplings to carbon ################
#'
#' @title Seedlings and saplings to carbon
#' @description Calculate the total carbon content of tree seedlings
#' (below and above ground)
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param heightincm tree height in centimetres
#' @param type 'conifer' or 'broadleaf'
#' @param re relative error of estimates (default = 2.5%)
#' @param re_h relative error of height measurement in cm (optional)
#' @return carbon in tonnes or if re_h provided then additionally the error
#' @note just uses simple linear relationship to get between measures
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018)
#' @importFrom utils data head tail
#' @import remotes
#' @examples
#' sap_seedling2C(50, 'conifer')
#' sap_seedling2C(heightincm = 900, type = 'broadleaf', re_h = 0.05)
#' @aliases sap_seedling2C
#' @export
#'
sap_seedling2C <- function(heightincm, type, re_h = NA, re = 0.025) {
  if (!is.numeric(heightincm)){
     stop("Argument 'heightincm' must be numeric.")
   }
  if (any(heightincm < 1 | heightincm > 1000, na.rm=TRUE)){
    warning("heightincm is only defined for values between 1 and 1000 cm.")
  }
  if (!any(type %in% c("broadleaf", "conifer"))){
    warning("type must be defined as 'broadleaf', 'conifer' for each tree")
  }
  if (length(heightincm) != length(type)) stop("'heightincm' and 'type' must have the same length")

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

  if (!is.na(re_h)) {
      if (!is.numeric(re_h) | re_h < 0 | !is.numeric(re) | re < 0) {
        stop("'re' and 're_h' must be numeric and positive")
      }

      carbon_sd <- 10 * re * df$carbon

      return(list(carbon = df$carbon, sd = carbon_sd))
  } else {
      return(df$carbon)
    }
}

############# FC Lookup Species Code ################
#'
#' @title Lookup species code
#' @description  Function that looks up species codes for Woodland Carbon Code
#' @author Isabel Openshaw I.Openshaw@kew.org
#' @param name name of species (common or botanical). See lookup_df.Rda
#' @param type either 'broadleaf' or 'conifer'
#' @param code either 'short', 'single', 'stand', 'Root' or 'Crown' Or other
#' column names from lookup_df.Rda
#' @param returnv either 'code' for just the code output or 'all' (default) with
#'  spname and matchtype for checking
#' @return Species code
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom stringr word str_trim
#' @importFrom utils data
#' @import remotes
#' @examples
#' lookupcode(name="Pine")
#' lookupcode(name='Quercus robur', code='single')
#' lookupcode(name=c('Oak', 'Quercus', 'Quercus rubra', 'Hawthorn', NA) ,
#'  type = c(rep("broadleaf", 4), NA), code='short')
#' @export
#' @aliases lookupcode
#'
lookupcode <- function(name, type = NA, code = "short", returnv = "all") {
  if (!is.character(name)) stop("'name' must be a character vector.")

  # Identify the column index for the selected 'code'
  col_index <- which(names(lookup_df) == code)
  if (length(col_index) == 0) stop("Invalid 'code' column selected.")

  # Clean Inputs
  clean_name <- tolower(stringr::str_trim(name))

  # Vectorized Matching
  match_binomial <- match(clean_name, tolower(lookup_df$latin_name))
  match_common <- match(clean_name, tolower(lookup_df$common_name))
  match_genus  <- match(stringr::word(clean_name, 1), tolower(lookup_df$General.genus))
  match_genus2 <- match(stringr::word(clean_name, 1), tolower(lookup_df$Genus))

  # Create Result DataFrame
  result_df <- data.frame(spname = name, code = NA,
                          matchtype = NA, stringsAsFactors = FALSE)
  unmatched <- c()

  # Species Binomial Matches
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

  # Genus Matches
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

  # Fallback to Mixed Species
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
#' @title Calculate above ground carbon
#' @description  Function that inputs tree species code, dbh, height and method
#' for converting biomass to carbon, and returns the carbon estimate
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param name species name, either binomial or common
#' @param type either 'broadleaf' or 'conifer'
#' @param dbh diameter at breast height in centimetres
#' @param height in metres
#' @param method method of converting biomass to carbon. Either 'Thomas' or 'IPCC2' as these specify the error associated with the carbon volatile fraction
#' @param biome temperate, boreal, mediterranean, tropical, subtropical or all
#' @param output.all if TRUE (default) outputs all data from processing, else outputs carbon estimate
#' @param nsg nominal specific gravity. Optionally specified, else will use that
#'  given by the WCC
#' @return either Above ground carbon (AGC) in tonnes, or a list with tariff
#' number, merchantable volume (metres cubed), stem volume (metres cubed),
#' stem biomass (tonnes), stem carbon (tonnes), canopy carbon (tonnes),
#' root carbon (tonnes) and AGC.
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @import remotes
#' @examples
#' fc_agc('Quercus robur', dbh=74, height=24, output.all = FALSE)
#' # Input wood density and sd from BIOMASS package
#' wd <- BIOMASS::getWoodDensity('Quercus', 'robur', region='Europe')
#' fc_agc('beech', 72, 24, nsg = wd$meanWD)
#' @export
#' @aliases fc_agc
#'
fc_agc <- function(name, dbh, height, type = NA, method = "IPCC2", biome =
                     "temperate", output.all = TRUE, nsg = NA){

  # Check arguments
  if(length(name) != length(dbh) || length(name) != length(height) ||
     length(height) != length(dbh))stop("input lengths must be the same")
  if(!is.character(name))stop("name must be a character")
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }
  methods <- c("Matthews1", "Matthews2", "IPCC1", "IPCC2", "Thomas")
  if (!(method %in% methods)) {
    stop("Invalid method. Choose from: ", paste(methods, collapse = ", "))
  }

  class <- c("broadleaf", "conifer")
  if(any(!type %in% c(class, NA, "NA"))){
    stop("type must equal either conifer, broadleaf or NA")
  }
  if (any(dbh < 0, na.rm = TRUE) || !is.numeric(dbh) || anyNA(dbh)) {
    warning("dbh must be numeric and positive")}

  # Lookup species codes and type
  spcodes <- lookupcode(name, type, code = 'short')
  rec <- lookup_df[match(spcodes$code, lookup_df$short), ]
  type <- rec$type

  if(output.all){
    r <- data.frame(name=name, type=type, dbh=dbh, height=height, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, NSG=rec$NSG, tariff=NA,
                    mercvol_m.3=NA, stemvol_m.3=NA, stembiomass_t=NA, crownbiomass_t=NA,
                    rootbiomass_t=NA, AGC_WCC_t=NA, stringsAsFactors=FALSE)
  } else {
    r <- data.frame(name=name, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, AGC_WCC_t=NA, stringsAsFactors=FALSE)
  }

  # Trees with dbh < 7m
  small_id <- type %in% class & !is.na(height) & (!is.na(dbh) & dbh < 7 | is.na(dbh) & height < 10)

  if (any(small_id, na.rm = TRUE)) {
    r$AGC_WCC_t[small_id] <- sap_seedling2C(heightincm =
                                height[small_id] * 100, type[small_id])
  }

  # Trees with dbh >= 7 m
  tall_id <- dbh >= 7 & !is.na(dbh) & !is.na(height)

  if (any(tall_id, na.rm = TRUE)) {
    tariff <- tariffs(spcode= spcodes$code[tall_id],height= height[tall_id],
                      dbh=dbh[tall_id], type=type[tall_id])

    # Volume & Biomass
    mercvol <- merchtreevol(dbh[tall_id], tariff)
    stemvol <- treevol(mercvol, dbh = dbh[tall_id])
    woodbio <- woodbiomass(stemvol, rec$NSG[tall_id])
    crownbio <- crownbiomass(rec$Crown[tall_id], dbh[tall_id])

    # Above Ground Biomass
    AGB <- woodbio + crownbio

    # Carbon Conversion
    convert <- type[tall_id] %in% class

    if (any(convert, na.rm = TRUE)) {
      r$AGC_WCC_t[tall_id][convert] <- biomass2c(AGB[convert], method, type[tall_id][convert], biome = biome)
    } else {
      r$AGC_WCC_t[tall_id] <- AGB * 0.4885
      warning("Type must be specified as 'broadleaf' or 'conifer' for carbon
              conversion.")
    }
    r$AGC_WCC_t[tall_id] <- AGB

    if(output.all){
      # Root Biomass
      rootbio <- rootbiomass(rec$Root[tall_id], dbh[tall_id])

      r[tall_id, c("tariff", "mercvol_m.3", "stemvol_m.3",
                      "stembiomass_t", "crownbiomass_t", "rootbiomass_t")] <-
        list(tariff, mercvol, stemvol,
             woodbio, crownbio, rootbio)

    }
  }

  return(r)
}

############# FC Above Ground Carbon with error ################
#'
#' @title Calculate above ground carbon
#' @description  Function that inputs tree species code, dbh, height and method
#' for converting biomass to carbon, and returns the carbon estimate
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param name species name, either binomial or common
#' @param type either 'broadleaf' or 'conifer'
#' @param dbh diameter at breast height in centimetres
#' @param height in metres
#' @param method method of converting biomass to carbon. Either 'Thomas' or 'IPCC2' as these specify the error associated with the carbon volatile fraction
#' @param biome temperate, boreal, mediterranean, tropical, subtropical or all
#' @param output.all if TRUE outputs all data from processing, else just outputs carbon estimates
#' @param re_dbh relative measurement error for diameter at breast height, single value
#' @param re_h relative error of height measurement, single value
#' @param sig_nsg sigma for nominal specific gravity (NSG) or wood density
#' @param re relative error of coefficients (default = 2.5%)
#' @param nsg nominal specific gravity. Optionally specified, else will use that
#'  given by the WCC
#' @return either Above ground carbon, AGC in tonnes, or if output.all = TRUE,
#' a list of tariff number, merchantable volume (metres cubed), stem volume
#' (metres cubed), stem biomass (tonnes), stem carbon (tonnes), canopy carbon
#' (tonnes) and root carbon (tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' fc_agc_error(name='Quercus robur', dbh=74, height=24, output.all = FALSE)
#' fc_agc_error('Oak', dbh=74, height=24, method="IPCC2",
#' biome="temperate", output.all = FALSE, re_dbh=1, re_h=1)
#' # Input wood density and sd from BIOMASS package
#' wd <- BIOMASS::getWoodDensity('Quercus', 'robur', region='Europe')
#' fc_agc_error('Oak', 72, 24, nsg = wd$meanWD, sig_nsg = wd$sdWD)
#' fc_agc_error(c('Quercus robur','beech'), c(74,23), c(24,24), output.all = FALSE)
#' @export
#' @aliases fc_agc_error
#'
fc_agc_error <- function(name, dbh, height, type = NA, method = "IPCC2", biome =
                           "temperate", output.all = TRUE, re_dbh = 0.05, re_h =
                           0.1, re = 0.025, nsg = NA, sig_nsg = 0.09413391){
  # Check arguments
  if(!is.character(name)) stop ("name must be a character")
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }
  class <- c("broadleaf", "conifer")
  if(any(!type %in% c(class, NA, "NA"))){
    stop("type must equal either conifer, broadleaf or NA")
  }
  if(!is.numeric(re_dbh) || any(re_dbh<=0)){
    stop("re_dbh must be numeric & positive")
  }
  if(!is.numeric(re_h) || any(re_h<0)) stop ("re_h must be numeric & positive")

  if (!(method %in% c("IPCC2", "Thomas"))) {
    stop("Invalid method. Choose from: 'IPCC2', 'Thomas'")
  }
  biomes <- c("tropical", "subtropical", "mediterranean", "temperate", "boreal")
  if (!missing(biome) && !(biome %in% biomes)) {
    stop("Invalid biome. Choose from: ", paste(biomes, collapse = ", "))
  }
  if (any(dbh < 0, na.rm = TRUE) || !is.numeric(dbh) || anyNA(dbh)) {
    warning("dbh must be numeric and positive")}

  # Lookup species codes and type
  spcodes <- lookupcode(name, type, code = 'short')
  rec <- lookup_df[match(spcodes$code, lookup_df$short), ]
  type <- ifelse(is.na(rec$type) | rec$type == "MX", type, rec$type)

  # Create results table
  if(output.all){
    r <- data.frame(name=name, type=type, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, dbh=dbh, height=height,
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
    carbon <- sap_seedling2C(heightincm = height[small_id] * 100,
                             type[small_id], re_h, re)
    r$AGC_WCC_t[small_id] <- carbon$carbon
    r$sig_AGC[small_id] <- carbon$sd
  }

  # Trees with dbh >= 7 m
  tall_id <- dbh >= 7 & !is.na(dbh) & !is.na(height)

  if (any(tall_id, na.rm = TRUE)) {
    tariff <- tariffs(spcodes$code[tall_id], height[tall_id],
                      dbh[tall_id], type[tall_id], re_h, re_dbh, re = re)

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
                        sig_AGB[convert], biome)
      r$AGC_WCC_t[tall_id][convert] <- AGC$AGC
      r$sig_AGC[tall_id][convert] <- AGC$sig_AGC
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

  return(r)
}

############# Progression of errors ###########
#'
#' @title Carbon progression of errors
#' @description Progression of errors through monte carlo simulation
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param vol volume
#' @param den density
#' @param biom biomass
#' @param sig_vol sigma for volume
#' @param sig_den sigma for wood density
#' @param biomsd biomass sd
#' @param nruns number of iteration, suggest 10,000 as min and 100,000 is a good number
#' @param returnv if null then mean and sd is returned else vector of
#' quantiles ie c(5,50,95)/100 will return 5%, mean and 95% quantiles.
#' @return  either vector of mean and sd or vector of quantiles
#' @references todo** and to write at export
#' @importFrom stats quantile rnorm sd
#' @import remotes
#' @export
#' @aliases pro_error_carbon
#'
#vol <- 100
#volsd <- 10
#den <- 0.5
#densd <- 0.005
#biom <- 0.5
#biomsd <- 0.0025
#nruns <-100000
#returnsv <- c(5,50,95)/100
# pro_error_carbon(vol,volsd,den,densd,biom,biomsd,nruns=100000,returnsv=c(5,50,95)/100)
# pro_error_carbon(vol,volsd,den,densd,biom,biomsd,nruns=100000)
pro_error_carbon <- function(vol,sig_vol,den,sig_den,biom,biomsd,nruns=10000,
                             returnv=NULL) {
  vol <- stats::rnorm(nruns,mean=vol,sd=volsd)
  den <- stats::rnorm(nruns,mean=den,sd=densd) # middle of the road
  biomass <- stats::rnorm(nruns,mean=biom,sd=biomsd) # conifer or angiosperm
  carbt <- vol * den * biomass
  if (!is.null(returnv)){
    stats::quantile(carbt,probs=returnv)
  } else {
    c(mean = mean(carbt),sd= stats::sd(carbt))
  }
}
#AGB = 0.0673 * (WD * H * D^2)^0.976

############# Bunce Equation carbon calculation ########################
#'
#' @title Bunce biomass equation
#' @description Calculates dry weight based on species and dbh
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param name species name (common or binomial)
#' @param type 'broadleaf' or 'conifer' (optional)
#' @param dbh diameter at breast height
#' @param re_dbh relative measurement error for diameter at breast height (optional)
#' @param re  relative error of coefficients (default = 2.5%)
#' @return  biomass in kg
#' @references Bunce, R. G. H. "Biomass and Production of Trees in a Mixed
#' Deciduous Woodland: I. Girth and Height as Parameters for the Estimation of
#' Tree Dry Weight" (1968)
#' @importFrom utils data
#' @examples
#' Bunce("Oak", 24)
#' Bunce(c("Oak", "Beech"), c(23,23))
#' @export
#' @aliases Bunce
#'
Bunce <- function(name, dbh, type = NA, re_dbh = NULL, re = 0.025) {
  # Check inputs
  if (missing(name) || missing(dbh)) stop("Both 'name' and 'dbh' are required.")
  if (!is.character(name)) stop("'name' must be a character vector.")
  if (!is.numeric(dbh)) stop("'dbh' must be numeric.")
  if (length(name) != length(dbh)) stop("'name' and 'dbh' must have the same length.")

  lookup <- lookupcode(name, type = type, code = "short")

  r <- data.frame(species_name = name, dbh = as.numeric(dbh), biomass = NA,
                  spcode = lookup$code, a = NA, b = NA, stringsAsFactors = FALSE, row.names = NULL)

  r$spcode[r$spcode == "MX"] <- "XB"

  # Find matching spcode indices
  matched_index <- match(r$spcode, buncedf$spcode)
  r$a <- buncedf$a[matched_index]
  r$b <- buncedf$b[matched_index]

  # Fallback coefficients
  r$a[is.na(r$a)] <- buncedf$a[6]
  r$b[is.na(r$b)] <- buncedf$b[6]

  # Calculate Biomass
  r$biomass <- exp(r$a + r$b * log(pi * r$dbh))

  if(!is.null(re_dbh)){
    r$sigma <- r$biomass * sqrt((r$a*re / r$biomass)^2 + (log(pi * dbh) * r$b * re)^2 + (r$b * re_dbh)^2)
  }

  # Remove coefficients from dataframe
  r <- r[ , !(names(r) %in% c("a", "b"))]

  return(r)
}

############# BIOMASS Package carbon calculation ==========================
#'
#' @title Estimate Tree Carbon using Biomass package functions
#' @description Using the Biomass package to calculate carbon
#' @param DBH Diameter at breast height
#' @param Height Height of tree (optional, if not specified will estimate height)
#' @param Genus First part of Species binomial
#' @param Species Second part of Species binomial
#' @param coords either a vector of coordinates of the site or a matrix of
#' coordinates for each tree of longitude and latitude
#' @param region of the World. See ?getWoodDensity for the list of regions
#' @param output.all if TRUE outputs all data from processing, else just outputs carbon figures
#' @return Above-ground Biomass in kg. If output.all = FALSE, then returns
#' columns 'Genus_corrected','Species', 'Family', 'Latitude', 'Longitude',
#' 'DBH', 'AGB_Biomass_kg'. If output.all = TRUE then additionally returns
#' columns 'Wood_Density', 'Wood_Density_sd', 'Height_est', 'RSE'
#' (Residual Standard Error of the model), 'Height_1' (which is inputed height
#' filled in with Height estimate where missing).
#' @importFrom utils install.packages
#' @import remotes
#' @references Réjou-Méchain, M., Tanguy, A., Piponiot, C., Chave, J., & Hérault
#' , B. (2017). BIOMASS: an R package for estimating above-ground biomass and
#' its uncertainty in tropical forests. Methods in Ecology and Evolution, 8(9),
#' 1163-1167
#' @examples
#' coords <- c(-0.088837, 51.071610)
#' biomass(12, 12, 'Quercus', 'robus', coords)
#' @aliases biomass
#' @export
#'
biomass <- function(DBH, Height = NULL, Genus, Species, coords, region = "World", output.all = TRUE) {
  # Ensure the required package is installed
  if (nchar(system.file(package = 'BIOMASS')) == 0) {
    utils::install.packages("BIOMASS", dependencies = TRUE)
  }

  # Input validation
  if (!is.numeric(DBH)) stop("'DBH' must be numeric.")
  if (!is.character(Genus) || !is.character(Species)) {
    stop("'Genus' and 'Species' must be character vectors.")
  }
  if (!is.null(Height) && !is.numeric(Height)) {
    stop("'Height' must be numeric or NULL.")
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
  if (length(DBH) != length(Genus) || length(DBH) != length(Species)) {
    stop("Lengths of 'DBH', 'Genus', and 'Species' must be equal.")
  }

  df <- data.frame(DBH = DBH, Height = Height, Genus = Genus, Species = Species,
                   stringsAsFactors = FALSE)

  # Correct taxonomic names
  correct <- BIOMASS::correctTaxo(genus = df$Genus, species = df$Species)
  df$Genus_corrected <- correct$genusCorrected
  df$Species_corrected <- correct$speciesCorrected
  df$Modified <- correct$nameModified

  # Get wood density
  wd <- BIOMASS::getWoodDensity(df$Genus_corrected, df$Species, region = region)
  df$Wood_Density <- wd$meanWD
  df$Wood_Density_sd <- wd$sdWD
  df$Family <- wd$family

  # Get height estimates (if Height is not provided)
  h <- BIOMASS::retrieveH(D = as.numeric(df$DBH), coord = coords)
  df$RSE <- h$RSE
  df$Height_est <- h[["H"]]

  # Combine height data with height estimates
  df$Height_1 <- ifelse(is.na(df$Height), df$Height_est, df$Height)

  # Calculate Above-Ground Biomass (AGB)
  df$AGB_Biomass_kg <- BIOMASS::computeAGB(
    D = as.numeric(df$DBH),
    WD = as.numeric(df$Wood_Density),
    H = df$Height_1
  ) * 1000

  # Output results
  if (output.all) {
    return(df)
  } else {
    return(df$AGB_Biomass_kg)
  }
}

############# allodb Package carbon calculation ==========================
# ==== Inputs:
# df: data frame containing columns; DBH (in cm), Genus_corrected, Species (from output of biomass function)
# coords: either the coordinates of the site, a vector of longitude and latitude
#         or a matrix of coordinates for each tree
# ==== Optional Inputs:
# output.all: if TRUE outputs the coefficients of the model, a*DBH^b+e {e ~ N(0,sigma^2}
#' @title Estimate Tree Carbon using allodb package functions
#' @description Use the aalodb R package to calculate carbon
#' @param DBH Diameter at breast height (cm)
#' @param Genus First part of Species binomial
#' @param Species Second part of Species binomial
#' @param new.eqtable a subset or extension of the allometric equation table. Create with allodb::new_equations
#' @param coords either a vector of coordinates of the site or a matrix of
#' coordinates for each tree of longitude and latitude
#' @param output.all if TRUE outputs all data from processing, else just outputs carbon figures
#' @references Gonzalez-Akre, E., Piponiot, C., Lepore, M., & Anderson-Teixeira,
#' K. (2020). allodb: An R package for biomass estimation at globally
#' distributed extratropical forest plots. Methods in Ecology and Evolution,
#' 11(10), 1273-1280
#' @return Above-ground Biomass in kg. If output.all = TRUE then returns columns
#'  'DBH', 'Genus', 'Species', 'AGB_allodb_kg', 'allodb_a', 'allodb_b',
#'  'allodb_sigma' (where AGB = a*DBH^b+e {e ~ N(0,sigma^2}).
#' @examples
#' coords <- c(-0.088837,51.071610)
#' allodb(81.887, "Pinus", "nigra", coords, output.all = FALSE)
#' allodb(c(76, 76), c("Pinus","Pinus"), c("nigra", "abies"), coords)
#'
#' @import remotes
#' @export
#' @aliases allodb
#'
allodb <- function(DBH, Genus, Species, coords, output.all = TRUE, new.eqtable = NULL){

  # Check Inputs
  if (!is.numeric(DBH)) stop("'DBH' must be numeric.")
  if (!is.character(Genus)) stop("'Genus' must be a character vector.")
  if (!is.character(Species)) stop("'Species' must be a character vector.")

  if (length(DBH) != length(Genus) || length(DBH) != length(Species)) {
    stop("Lengths of 'DBH', 'Genus', and 'Species' must be equal.")
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

  # Ensure the allodb package is installed
  if (nchar(system.file(package = 'allodb')) == 0) {
    warning("The 'allodb' package is not installed. Installing it now...")
    remotes::install_github("ropensci/allodb")
  }

  if(output.all == TRUE & is.null(new.eqtable)){
    # Output dataframe
    df <- data.frame(DBH = DBH, Genus = Genus, Species = Species,
                     AGB_allodb_kg = NA, allodb_a = NA, allodb_b = NA,
                     allodb_sigma = NA, stringsAsFactors = FALSE)

    Names <- as.factor(paste(df$Genus, df$Species))
    for(i in 1:length(levels(Names))){
      name <- levels(Names)[i]
      treeID <- which(Names == name)

      # Get parameters and sigma: AGB = a*DBH^b+e {e ~ N(0,sigma^2}
      params <- allodb::est_params(genus = as.character(df$Genus[treeID][1]),
                                   species = as.character(df$Species[treeID][1]),
                                   coords = coords)
      df$allodb_a[treeID] <- params$a
      df$allodb_b[treeID] <- params$b
      df$allodb_sigma[treeID] <- params$sigma
    }

    # Calculate Biomass
    df$AGB_allodb_kg <- df$allodb_a * df$DBH ^ df$allodb_b

    # Clean df
    NAs <- which(is.na(df$AGB_allodb_kg))
    df$allodb_a[NAs] <- df$allodb_b[NAs] <- df$allodb_sigma[NAs] <- NA

    return(df)

  } else {
    biomass <- allodb::get_biomass(dbh = as.numeric(DBH),
                                   genus = as.character(Genus),
                                   species = as.character(Species),
                                   coords = coords,
                                   new_eqtable = new.eqtable)
    return(biomass)
  }
}

############# Summarise totals per unit area with errors =========================

#' @title Calculate Total per unit area with Error for Biomass or Carbon
#' @description This function calculates the total carbon or biomass per unit
#' area with propagated error.
#' @author Isabel Openshaw. I.Openshaw@kew.org, Justin Moat. J.Moat@kew.org
#' @param input Vector of tree-level above-ground biomass or carbon or a list of
#'  multiple areas to be summarised separately.
#' @param sigma_input Vector of standard deviations associated with input or a
#' list of multiple areas to be summarised separately.
#' @param area Total area sampled (whole habitat or sampled plots) or a vector
#' of multiple areas.
#' @param sigma_area Standard deviation of plot area measurement or a vector.
#' @param returnv error returned as standard deviation, 'sigma' (default) or
#' variance, 'sigmasquared'
#' @param plots if plots = TRUE then the list is treated as individual plots to
#' be summed as individual plots then taken the mean to get the weighted per
#' area mean. Default is plots = FALSE, then items in the list are treated as
#' separate habitats and metrics outputted seperately.
#' @return A list containing the estimated total per unit area and its
#' propagated standard deviation.
#' @examples
#' AGB <- c(2.3, 1.8, 3.2)  # Biomass estimates for trees
#' SD_AGB <- c(0.2, 0.15, 0.3)  # Standard deviations of biomass estimates
#' summary_per_area(AGB, SD_AGB, 0.5, 0.05)
#'
#' AGB_2 <- c(5.9, 7.5, 2.1)  # Biomass estimates for treesz
#' SD_AGB_2 <- c(0.7, 1.02, 0.3)  # Standard deviations of biomass estimates
#'
#' summary_per_area(input = list(AGB, AGB_2), sigma_input = list(SD_AGB,
#' SD_AGB_2), area = c(0.5, 0.7), sigma_area = c(0.05, 0.09))
#'
#' summary_per_area(AGB, SD_AGB, 0.5, 0.09)
#'
#' Carbon <- AGB * 0.5  # Convert biomass to carbon
#' SD_Carbon <- SD_AGB * 0.5  # Approximate error scaling
#' summary_per_area(Carbon, SD_Carbon, 2.5, 0.09)
#'
#' @references Taylor, J. R. (1997). An Introduction to Error Analysis: The
#' Study of Uncertainties in Physical Measurements (2nd ed.). University
#' Science Books.
#' @export
#'
summary_per_area <- function(input, sigma_input, area, sigma_area,
                             returnv = "sigma", plots = FALSE) {

  # Check for dimension consistency
  if (length(input) != length(area) || length(sigma_input) != length(sigma_area)) {
    stop("Mismatch in number of habitats/areas provided.")
  }

  if(is.list(input)){
    total_per_area <- numeric(length(input))
    error_per_area <- numeric(length(input))

    for (i in seq_along(input)) {
      total <- sum(input[[i]], na.rm = TRUE)
      sigma_total <- sqrt(sum(sigma_input[[i]]^2, na.rm = TRUE))
      total_per_area[i] <- total / area[i]
      error_per_area[i] <- error_product(total, sigma_total,
                                         area[i], sigma_area[i],
                                         fn = total_per_area[i],
                                         returnv = returnv)
    }
  } else {
    total <- sum(input, na.rm = TRUE)
    sigma_total <- sqrt(sum(sigma_input^2, na.rm = TRUE))
    total_per_area <- total / area
    error_per_area <- error_product(total, sigma_total,
                                    area, sigma_area,
                                    fn = total_per_area,
                                    returnv = returnv)
  }

  return(list(total_per_area = total_per_area, error_per_area = error_per_area))
}

#############  Standard Deviation for Area Measurement =========================

#' @title Standard Deviation for Measurement of the total Area
#' @description This function calculates the standard deviation of the area
#' based on the perimeter and Root Mean Square Error (RMSE).
#' @author Isabel Openshaw. I.Openshaw@kew.org, Justin Moat. J.Moat@kew.org
#' @param perimeter A numeric vector of perimeters (in meters).
#' @param RMSE A numeric value representing the Root Mean Square Error (RMSE)
#' of the perimeter measurement (in meters).
#' @param sum_plots if equal to TRUE then sum all of the standard deviations of
#' the plots. If false then outputs the standard deviation of each input.
#' @return A numeric vector representing the standard deviation of the area for
#' each corresponding perimeter (in hectares).
#' @examples
#' # Example with multiple perimeters of 200m, 300m, and 400m.
#' # If measured to the nearest 50cm within 100m plot, the RMSE = 0.5m
#' sd_area(c(200, 300, 400), 0.5)
#' @references Taylor, J. R. (1997). An Introduction to Error Analysis: The
#' Study of Uncertainties in Physical Measurements (2nd ed.). University
#' Science Books.
#' @references Goodchild, M. F., & Hunter, G. J. (1997). "A simple positional
#' accuracy measure for linear features." International Journal of Geographical
#' Information Science, 11(3), 299-306.
#' @export
#'
sd_area <- function(perimeter, RMSE, sum_plots = FALSE) {
  # Input validation
  if (!is.numeric(perimeter) || any(perimeter <= 0)) {
    stop("perimeter must be a numeric vector of positive values.")
  }
  if (!is.numeric(RMSE) || RMSE <= 0) {
    stop("RMSE must be a positive numeric value.")
  }

  # Calculate standard deviation of the area for each perimeter value
  result <- (2 * RMSE * perimeter) / 10000

  if(sum_plots){
    result <- sqrt(sum(result^2))
  }

  return(result)
}

#############  Compare Allometries #############
#' @title Calculate above ground carbon
#' @description  Function that inputs tree species code, dbh, height and method
#' for converting biomass to carbon, and returns the carbon estimate
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param Genus First part of Species binomial
#' @param Species Second part of Species binomial
#' @param type either 'broadleaf' or 'conifer'
#' @param dbh diameter at breast height in centimetres
#' @param height in metres
#' @param method method of converting biomass to carbon. Either 'Thomas' or 'IPCC2' as these specify the error associated with the carbon volatile fraction
#' @param output.all if TRUE outputs all data from processing, else just outputs carbon estimates
#' @param region of the World. See ?getWoodDensity for the list of regions
#' @param biome temperate, boreal, mediterranean, tropical, subtropical or all
#' @param coords either a vector of coordinates of the site or a matrix of
#' coordinates for each tree of longitude and latitude
#' @param re_dbh relative measurement error for diameter at breast height, single value
#' @param re_h relative error of height measurement, single value
#' @param sig_nsg sigma for nominal specific gravity (NSG) or wood density
#' @param re relative error of coefficients (default = 2.5%)
#' @param nsg nominal specific gravity. Optionally specified, else will use that
#'  given by the WCC
#' @returns either Above ground carbon, AGC in tonnes, or if output.all = TRUE,
#' a list of tariff number, merchantable volume (metres cubed), stem volume
#' (metres cubed), stem biomass (tonnes), stem carbon (tonnes), canopy carbon
#' (tonnes) and root carbon (tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#'  Carbon Assessment Protocol (v2. 0)." (2018).
#' Réjou-Méchain, M., Tanguy, A., Piponiot, C., Chave, J., & Hérault, B. (2017).
#'  BIOMASS: an R package for estimating above-ground biomass and its
#'  uncertainty in tropical forests. Methods in Ecology and Evolution, 8(9),
#'  1163-1167
#' Gonzalez-Akre, E., Piponiot, C., Lepore, M., & Anderson-Teixeira, K. (2020).
#'  allodb: An R package for biomass estimation at globally distributed
#'  extratropical forest plots. Methods in Ecology and Evolution, 11(10),
#'  1273-1280
#' Bunce, R. G. H. "Biomass and Production of Trees in a Mixed Deciduous
#'  Woodland: I. Girth and Height as Parameters for the Estimation of Tree Dry
#'  Weight" (1968)
#' @importFrom utils data
#' @examples
#' allometries("Quercus", "robur", 20, 10)
#' @export
#'
allometries <- function(genus, species, dbh, height, type = NA, method ="IPCC2",
                        output.all = FALSE, returnv = "AGC", nsg = NA,
                        region = "Europe", biome = "temperate",
                        coords = c(-0.088837,51.071610), re_dbh = 0.05,
                        re_h = 0.1, re = 0.025, sig_nsg = 0.09413391){

  bio <- biomass(dbh, height, genus, species, coords, region=region, output.all = TRUE)
  if(!output.all){ bio <- bio[, c(1:7, 14)]}

  if(any(bio$Modified == TRUE, na.rm = TRUE)){
    genus <- bio$Genus_corrected
    species <- bio$Species_corrected
  }

  allo <- allodb(dbh, genus, species, coords, TRUE)

  name = paste(genus, species)

  if(method %in% c("Thomas", "IPCC2")){
    WCC <- fc_agc_error(name, dbh, height, type, method,
                        biome, TRUE, re_dbh, re_h, re, nsg, sig_nsg)
  } else {
    WCC <- fc_agc(paste(genus, species), dbh, height, type, method, output.all = TRUE)
  }
  type0 <- ifelse(is.na(WCC$type) | WCC$type == "any", as.character(type), as.character(WCC$type))
  WCC$AGB_WCC_t <- WCC$crownbiomass_t + WCC$stembiomass_t

  AGB_Bunce_kg <- Bunce(name, dbh, re_dbh, re)

  if(!output.all){
    allo <- allo[, !colnames(allo) %in% c("allodb_a", "allodb_b")]

    if(returnv == "AGC"){
      WCC <- WCC[, colnames(WCC) %in% c("AGC_WCC_t", "sig_AGC")]

    } else {
      WCC <- WCC[, colnames(WCC) == c("AGB_WCC_t")]
    }

  } else {
    WCC <- WCC[, !colnames(WCC) %in% c("name", "dbh", "height")]
  }

  allo <- allo[, !colnames(allo) %in% c("Genus", "Species", "DBH")]
  df <- cbind(bio, allo, WCC)

  if(returnv == "AGC"){
    colnames(df)[colnames(df) == "WCC"] <- "AGC_WCC_t"

    # Biomass to Carbon
    df$AGC_biomass_t <- biomass2c(df$AGB_Biomass_kg*0.001, method, type0, biome = biome)
    allo <- biomass2c(df$AGB_allodb_kg*0.001, method, type0, df$allodb_sigma*0.001, biome)
    bunce <- biomass2c(AGB_Bunce_kg$biomass*0.001, method, type0, AGB_Bunce_kg$sigma, biome)

    df$AGC_allodb_t <- allo$AGC
    df$AGC_Bunce_t <- bunce$AGC

    df <- df[, !colnames(df) %in% c("AGB_allodb_kg", "AGB_Biomass_kg", "DBH")]

    if(!anyNA(allo$sig_AGC)){
      df$sig_allodb <- allo$sig_AGC
      df$sig_Bunce <- bunce$sig_AGC
    }
  } else {
    colnames(df)[colnames(df) == "WCC"] <- "AGB_WCC_t"
  }

  colnames(df)[colnames(df) == "sig_AGC"] <- "sig_WCC"

  return(df)

}


#' Global Wood Density Lookup
#'
#' Retrieves wood density values (g/cm^3) from the Global Wood Density Database
#' based on binomial name and a specified region.
#' If no value exists for a species in a region, it calculates a mean based on available data from broader categories (global species mean, genus in region, genus globally, family in region, family globally).
#'
#' @param binomial Character vector of binomial names (Genus species).
#' @param region A single character string specifying the region to match.
#' Choose from: "Africa (extratropical)", "Africa (tropical)", "Australia",
#' "Australia/PNG (tropical)", "Central America (tropical)", "China", "India",
#' "Europe", "Mexico", "Madagascar", "NorthAmerica", "Oceania",
#' "South America (extratropical)", "South America (tropical)",
#' "South-East Asia", "South-East Asia (tropical)"
#'
#' @return Numeric vector of wood density values corresponding to the inputs. Returns NA if no relevant match is found.
#'
#' @references Zanne, A.E., et al. (2009). Global wood density database. Dryad. http://hdl.handle.net/10255/dryad.235
#'
#' @examples
#' global_wd(binomial = c("Quercus alba", "Pinus sylvestris"), region = "Europe")
#' @export
#'
global_wd <- function(binomial, region = "World") {
  # Check inputs
  if (!is.character(binomial)) stop("'binomial' must be a character vector")
  regions <- c("Africa (extratropical)", "Africa (tropical)", "Australia",
               "Australia/PNG (tropical)", "Central America (tropical)",
               "China", "India", "Europe", "Mexico", "Madagascar",
               "NorthAmerica", "Oceania", "South America (extratropical)",
               "South America (tropical)", "South-East Asia",
               "South-East Asia (tropical)", "World")
  if (!region %in% regions) stop("'region' must be a single character string")

  # Filter dataset by region
  if(region == "World"){
    region_wd <- wd_df_zanne
  } else {
    region_wd <- wd_df_zanne[wd_df_zanne$Region == region, ]
  }

  # Lookup species-level wood density in the specified region
  match_idx <- match(binomial, region_wd$Binomial)
  wd <- region_wd$Wood.density[match_idx]
  sd <- region_wd$sd[match_idx]

  # If missing, lookup species wd across the world
  missing <- is.na(wd)
  if (any(missing)) {

    global_region_wd <- wd_df_zanne[wd_df_zanne$Binomial %in% binomial[missing], ]
    wd[missing] <- tapply(global_region_wd$Wood.density,
                          global_region_wd$Binomial,
                          mean, na.rm = TRUE)[binomial[missing]]
    sd[missing] <- tapply(global_region_wd$sd,
                          global_region_wd$Binomial,
                          mean, na.rm = TRUE)[binomial[missing]]

    # Lookup genus in region
    missing <- is.na(wd)
    if (any(missing)) {
      genus <- sapply(strsplit(binomial, " "), `[`, 1)
      genus_data <- region_wd[region_wd$Genus %in% genus[missing], ]
      wd[missing] <- tapply(genus_data$Wood.density,
                            genus_data$Genus,
                            mean, na.rm = TRUE)[genus[missing]]
      sd[missing] <- tapply(genus_data$sd,
                            genus_data$Genus,
                            mean, na.rm = TRUE)[genus[missing]]

      # Lookup genus across world
      missing <- is.na(wd)
      if (any(missing)) {
        global_genus_data <- wd_df_zanne[wd_df_zanne$Genus %in% genus[missing], ]
        wd[missing] <- tapply(global_genus_data$Wood.density,
                              global_genus_data$Genus,
                              mean, na.rm = TRUE)[genus[missing]]
        sd[missing] <- tapply(global_genus_data$sd,
                              global_genus_data$Genus,
                              mean, na.rm = TRUE)[genus[missing]]

        # Lookup family in region
        missing <- is.na(wd)
        if (any(missing)) {
          family <- wd_df_zanne$Family[match(binomial, wd_df_zanne$Binomial)]
          family_data <- region_wd[region_wd$Family %in% family[missing], ]
          wd[missing] <- tapply(family_data$Wood.density,
                                family_data$Family,
                                mean, na.rm = TRUE)[family[missing]]
          sd[missing] <- tapply(family_data$sd,
                                family_data$Family,
                                mean, na.rm = TRUE)[family[missing]]

          # Lookup family across the world
          missing <- is.na(wd)
          if (any(missing)) {
            global_family_data <- wd_df_zanne[wd_df_zanne$Family %in% family[missing], ]
            wd[missing] <- tapply(global_family_data$Wood.density,
                                  global_family_data$Family,
                                  mean, na.rm = TRUE)[family[missing]]
            sd[missing] <- tapply(global_family_data$sd,
                                  global_family_data$Family,
                                  mean, na.rm = TRUE)[family[missing]]

            missing <- is.na(wd)
            if(any(missing)){
              wd[missing] = mean(region_wd$Wood.density)
              sd[missing] = mean(region_wd$sd)
            }
          }
        }
      }
    }
  }

  return(list(wd = wd, sd = sd))
}
