############## Functions file for Woodland Carbon Code ########################
# TODO:
# error for ctoco2e ?
# todo error for nsg
# search not found and check that intermediate species are found in lookup_df
# check biomass2c that error is confidence percentage by checking references

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
#' @param re_c  relative error of coefficients (default = 2.5%)
#' @returns  Tariff number or if sigma for inputs are provided, then will return
#' a list of tariff number and sigma for tariff
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). (Equation 1)
#' @examples
#' tariff_vol_area(vol=0.5, dbh=24, sig_vol = 0.001, re_dbh = 0.05)
#' @export
#'
tariff_vol_area <- function(vol, dbh, sig_vol = NA, re_dbh = 0.025, re_c = 0.025){
  if (!is.numeric(vol) || !is.numeric(dbh)) { #if (!is.numeric(vol) || any(vol < 0) || !is.numeric(dbh) || any(dbh < 0)) {
    stop("vol and dbh must be non-negative numeric values")  #  stop("vol and dbh must be non-negative numeric values")
  }

  # Constants
  coef_a1 <- 3.174106384
  const_vol <- 0.005002986
  const_ba <- 0.003848451
  const_tariff <- 0.138763302

  ba <- (pi * dbh^2) / 40000                # tree basal area in m^2
  a1 <- (vol - const_vol) / (ba - const_ba)
  tariff <- (coef_a1 * a1) + const_tariff

  if(!anyNA(sig_vol)){
    if (any(sig_vol < 0) || !is.numeric(sig_vol)) {stop("sigm_vol must be non-negative numeric")}
    if (!is.numeric(re_dbh) || re_dbh < 0)
      stop("Argument 're_dbh' must be positive and numeric")
    if (!is.numeric(re_c) || re_c < 0)
      stop("Argument 're_c' must be positive and numeric")
    if (re_dbh > 1 || re_c > 1)
      warning("Relative error indicates high uncertainty to measured value")

    sig_ba <- (pi * 2 * dbh / 40000) * re_dbh*dbh
    sig_a1 <- a1 * sqrt(
      (sig_vol / (vol - const_vol))^2 +         # Error from vol
        (sig_ba / (ba - const_ba))^2 +            # Error from ba
        (re_c * const_vol / (vol - const_vol))^2 +   # Error from const_vol
        (re_c * const_ba / (ba - const_ba))^2        # Error from const_ba
    )
    sig_t <- sqrt((coef_a1 * sig_a1)^2 + (a1 * re_c * coef_a1)^2)

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
#' @param re_a  relative error of coefficients (default = 2.5%)
#' @returns  tariff number or if relative errors are provided returns a list of
#' tariff number and an estimate for sigma
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' conifer_tariff("SP", 74, 24)
#' conifer_tariff("SP", 74, 24, 5, 5)
#' @export
#'
conifer_tariff <- function(spcode, height, dbh, re_h = NA, re_dbh = NA, re_a = 0.025) {
  if(any(!is.numeric(height)))stop("height must be numeric") else if(any(height<=0))stop("height must be positive")
  if(any(!is.numeric(dbh)))   stop("dbh must be numeric") else if(any(dbh<=0))   stop("dbh must be positive")

  if (!(length(spcode) == length(height) && length(height) == length(dbh))) {
    stop("Input vectors for spcode, height & dbh must have the same length.")
  }

    tc <- tariff_coniferdf[tariff_coniferdf$abbreviation == spcode, ]

    if(nrow(tc) == 0){
      subcode <- lookup_df$single[lookup_df$short == spcode]
      tc <- tariff_coniferdf[tariff_coniferdf$abbreviation == subcode, ]

      if(nrow(tc) == 0){
        warning(spcode, " spcode not found, general conifer code used")
        tc <- tariff_coniferdf[tariff_coniferdf$abbreviation == "NS", ]
      }
    }

    tariff <- tc$a1 + (tc$a2 * height) + (tc$a3 * dbh)

    if(!is.na(re_dbh) || !is.na(re_h)){
      if(!is.numeric(re_dbh) || any(re_dbh<0))stop("must provide a numeric and positive re_dbh with re_h")
      if(!is.numeric(re_h) || any(re_h<0))stop("must provide a numeric and positive re_h with re_dbh")
      if (re_dbh > 1 || re_dbh > 1 || re_h > 1 || re_h > 1)
        warning("Relative errors indicate high uncertainty to measured value")

      sigma <- sqrt(
        (tc$a1 * re_a)^2 +
          error_product(tc$a2, tc$a2 * re_a, height, re_h * height) +
          error_product(tc$a3, tc$a3 * re_a, dbh,   re_dbh * dbh)
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
#' @param re_a relative error of coefficients (default = 2.5%)
#' @returns  tariff number or if relative errors are provided returns a list of
#' tariff number and an estimate for sigma
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Method B, Equation 2.
#' @importFrom utils data
#' @examples broadleaf_tariff(spcode = 'OK', height = 25, dbh = 75)
#' broadleaf_tariff(spcode = "OK", height = 25, dbh = 15, re_dbh = 0.05, re_h = 0.1)
#' broadleaf_tariff(spcode = "OK", height = 24, dbh = 15, re_dbh = 0.05, re_h = 0.1)
#' @export
#'
broadleaf_tariff <- function(spcode, height, dbh, re_dbh = NA, re_h = NA, re_a = 0.025) {
  if(any(!is.numeric(height)))stop("height must be numeric")
  if(any(!is.numeric(dbh)))   stop("dbh must be numeric")
  if(any(height<=0))stop("height must be positive")
  if(any(dbh<=0))   stop("dbh must be positive")

  #utils::data(tariff_broaddf, envir = environment())
  tb <- tariff_broaddf[tariff_broaddf$abbreviation == spcode, ]

  if(nrow(tb) == 0){
    subcode <- lookup_df$single[lookup_df$short == spcode]
    tb <- tariff_broaddf[tariff_broaddf$abbreviation == subcode, ]

    if(nrow(tb) == 0){
      warning(spcode, " spcode not found. General broafleaf code used.")
      tb <- tariff_broaddf[tariff_broaddf$abbreviation == "BI", ]
    }

  }

  tariff <- tb$a1 + (tb$a2 * height) + (tb$a3 * dbh) + (tb$a4 * dbh * height)

  if(!is.na(re_dbh) || !is.na(re_h)){
    if(!is.numeric(re_dbh) || any(re_dbh<0))stop("must provide a numeric and positive re_dbh with re_h")
    if(!is.numeric(re_h) || any(re_h<0))stop("must provide a numeric and positive re_h with re_dbh")
    if (re_dbh > 1 || re_dbh > 1 || re_h > 1 || re_h > 1)
      warning("Relative errors indicate high uncertainty to measured value")


    sigma <- sqrt((re_a * tb$a1)^2 +
        error_product(tb$a2, re_a*tb$a2, height, re_h*height) +
        error_product(tb$a3, re_a*tb$a3, dbh, re_dbh*dbh) +
        error_product(tb$a4, re_a*tb$a4, dbh, re_dbh*dbh, height, re_h*height)
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
#' @param re_a relative error of coefficients (default = 2.5%)
#' @returns either tariff number or if re_h is provided, then returns a list
#' of the tariff number and an estimate of sigma for tariff number
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' stand_tariff("OK", height = 10)
#' stand_tariff("OK", height = 10, re_h = 0.01)
#' stand_tariff(spcode = "AH", height = 10, re_h = 0.05)
#' @export
#'
stand_tariff <- function(spcode, height, re_h = NA, re_a=0.025) {
  if(any(!is.character(spcode)))stop("spcode must be a character")
  if(any(!is.numeric(height)))stop("height must be numeric")
  if(any(height<=0))stop("height must be positive")

  #utils::data(tarif2heightdf, envir = environment())
  rec <- tarif2heightdf[tarif2heightdf$abbreviation == spcode, ]

  if(nrow(rec)==0){
    spcode <- lookup_df$stand[lookup_df$short == spcode]
    rec <- tarif2heightdf[tarif2heightdf$abbreviation == spcode, ]

    if(nrow(rec)==0){
      stop("spcode not found in data(tarif2heightdf) or data(lookup_df)")
    }

  }

  tariff <- rec$a1 + (rec$a2 * height) + (rec$a3 * height^2)

  if(!anyNA(re_h)){
    if(!is.numeric(re_h) || any(re_h<0))stop("re_h must be numeric and positive")
    if (re_h > 1 || re_h > 1)
      warning("Relative error indicate high uncertainty to measured value")

    sigma <- sqrt((rec$a1 * re_a)^2 +
                    error_product(rec$a2, rec$a2 * re_a, height, re_h*height))

    if (rec$a3 != 0) { # Include the quadratic term if rec$a3 is nonzero
      sigma <- sqrt(sigma^2 + error_product(rec$a3, rec$a3 * re_a, height, re_h*height))
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
#' @param re_dbh relative error for diameter at breast height (optional)
#' @param re_a relative error of coefficients (default = 2.5%)
#' @returns either tariff number or if re_h is provided, then returns a list
#' of the tariff number and uncertainty
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' tariffs("OK", height= 10, dbh = 20)
#' tariffs("OK", 10, 20, re_h = 0.1, re_dbh = 0.05)
#' tariffs(c("OK","NS", "NS", "SP", "SP"), c(10,10,5,10,10), c(20,20,10,20,NA), re_h = 0.01, re_dbh = 0.05)
#' @export
#'
tariffs <- function(spcode, height, dbh = NA, re_h = NA, re_dbh = NA, re_a = 0.025) {

  # Ensure all inputs have the same length
  n <- length(height)
  if (!(length(spcode) == n && length(dbh) == n || is.na(dbh))) {
    stop("All input vectors (spcode, height, dbh, re_h, re_dbh) must have the same length.")
  }
  # Prepare output lists
  tariffs_result <- rep(NA, n)
  if(!anyNA(re_h)){errors_result <- rep(NA, n)}

  # Loop over each tree to calculate tariffs
  for (i in 1:n) {
    result <- NA
    if (is.na(height[i])) {
      warning("Height is missing for spcode ", spcode[i], ", skipping record ", i)
      next
    } else if (!is.numeric(height[i]) || height[i] <= 0) {
      stop("Height must be numeric and positive for spcode ", spcode[i], "index ", i)
    } else {
      # Determine tree type
      type <- lookup_df$type[lookup_df$short == spcode[i]]
      if (anyNA(type) || length(type) == 0) {
        warning("Species code ", spcode[i], " not found, skipping record ", i)
      } else {
        # Call the appropriate tariff function
        if (is.na(dbh[i])) {
          result <- stand_tariff(spcode[i], height[i], re_h, re_a)
        } else if (type == "conifer") {
          result <- conifer_tariff(spcode[i], height[i], dbh[i], re_h, re_dbh, re_a)
        } else if (type == "broadleaf") {
          result <- broadleaf_tariff(spcode[i], height[i], dbh[i], re_h, re_dbh, re_a)
        } else if (type == "any") {
          result <- mean(c(broadleaf_tariff("XB", height[i], dbh[i]),
                           conifer_tariff("XC", height[i], dbh[i])))
          if(!is.na(re_dbh) | !is.na(re_h)){
            result <- list(tariff = result, sigma = as.numeric(sqrt(
              broadleaf_tariff("XB", height[i],dbh[i],re_h, re_dbh, re_a)[2]^2 +
              conifer_tariff("XC", height[i],dbh[i], re_h, re_dbh, re_a)[2]^2)))
          }
          warning("Species not found, using mixed species numbers")

        } else {
          warning("Unrecognized species type for spcode ", spcode[i], ".")
        }
      }
    }
      if(length(result) == 1){
         tariffs_result[i] <- result
       } else {
        tariffs_result[i] <- as.numeric(result[1])
        errors_result[i]  <- as.numeric(result[2])
       }
    }
  if (anyNA(re_dbh) || anyNA(re_h)) {
    return(tariffs_result)
  } else {
    return(data.frame(tariff = tariffs_result, sigma = errors_result))
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
#' @returns  volume in metres cubed and error if sig of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' merchtreevol(dbh = 24, tariff = 24)
#' merchtreevol(dbh = 24, tariff = 24, re_dbh = 0.05, sig_tariff = 1)
#' @export
#'
merchtreevol <- function(dbh, tariff, re_dbh = 0.05, sig_tariff = NA, re = 0.025) {
  if(anyNA(tariff) || !is.numeric(tariff))
    stop("tariff must be numeric")
  if(!is.numeric(dbh) || any(dbh<=0))
    warning("dbh must be numeric and positive")

  # Constants
  k1 <- 0.315049301
  k2 <- 0.138763302
  k3 <- 0.0360541
  k4 <- 0.118288

  ba <- (pi * dbh^2) / 40000
  a2 <- k1 * (tariff - k2)
  a1 <- (k3 * tariff) - (a2 * k4)
  vol <- a1 + (a2 * ba)

  if(anyNA(sig_tariff)) {
    return(vol)
  } else {
    if(anyNA(tariff) || !is.numeric(tariff))
      stop("tariff must be numeric")
    if(!is.numeric(dbh) || any(dbh<=0))
      warning("dbh must be numeric and positive")

    # Constants
    k1 <- 0.315049301
    k2 <- 0.138763302
    k3 <- 0.0360541
    k4 <- 0.118288

    ba <- (pi * dbh^2) / 40000
    a2 <- k1 * (tariff - k2)
    a1 <- (k3 * tariff) - (a2 * k4)
    vol <- a1 + (a2 * ba)

    if(anyNA(sig_tariff)) {
      return(vol)
    } else {
      if(!is.numeric(re_dbh)||re_dbh<0){stop("'re_dbh' must be positive & numeric")}
      if (re_dbh > 1 || re_dbh > 1)
        warning("Relative errors indicate high uncertainty to measured value")

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
#' @returns volume metres cubed or if sig_mtreevol is provided then additionally
#'  returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' treevol(mtreevol = 10, dbh = 24)
#' treevol(mtreevol = 10, dbh = 24, sig_mtreevol = 0.07)
#' treevol(mtreevol = c(10,10), dbh = c(20,24), sig_mtreevol = c(1,1))
#' @export
#'
treevol <- function(mtreevol, dbh, sig_mtreevol = NA, re = 0.025) {
  # Error handling for inputs
  if (!is.numeric(dbh) || any(dbh <= 0))stop("dbh must be numeric and positive")
  if (!is.numeric(mtreevol)) stop("mtreevol must be numeric")

  if (length(dbh) != length(mtreevol)) {
    stop("dbh and mtreevol must have the same length")
  }

  # Ensure relative error is valid
  if (!is.numeric(re) || re < 0) stop("'re' must be positive & numeric")
  if (re > 1) warning("Relative errors indicate high uncertainty to measured value")

  # Load stemvoldf data
  utils::data(stemvoldf, envir = environment())

  # Define a helper function to process individual values
  process_tree <- function(mtreevol, dbh, sig_mtreevol) {
    dbh <- round(dbh)
    if (dbh < 500 & dbh > 6.5) {
      cf <- stemvoldf$X[stemvoldf$dbh..cm. == dbh]
    } else if (dbh < 6.5) {
      warning(paste(dbh, "dbh is less than 6.5 cm, multiplication factor is not specified"))
      cf <- 1
    } else if (dbh > 500) {
      warning("dbh is above 5 m")
      cf <- 1
    }

    stemvol <- cf * mtreevol

    if (is.na(sig_mtreevol)) {
      return(stemvol)
    } else {
      if (!is.numeric(sig_mtreevol) || sig_mtreevol <= 0) stop("sig_mtreevol must be numeric and positive")
      sigma <- error_product(cf, cf * re, mtreevol, sig_mtreevol, returnv = "sigma")
      return(c(stemvolume = stemvol, sigma = sigma))
    }
  }

  # Apply the function to each pair of inputs
  if (all(is.na(sig_mtreevol))) {
    # Return only volumes if sig_mtreevol is not provided
    result <- mapply(process_tree, mtreevol, dbh,
                     MoreArgs = list(sig_mtreevol = NA),
                     SIMPLIFY = TRUE)
  } else {
    # Return a single list with volumes and sigmas if sig_mtreevol is provided
    result <- mapply(process_tree, mtreevol, dbh, sig_mtreevol,
                     SIMPLIFY = FALSE)
    result <- do.call(rbind, result) # Combine into a single matrix
  }

  return(result)
}

############# Propagation of error for a product ################
#' @title Analytical error progression for a product
#' @description Calculates sigma squared for x when x = a * b or x = a * b * c
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param a first variable in product
#' @param sig_a sigma for a
#' @param b second variable in product
#' @param sig_b sigma for b
#' @param c (optional) third variable in product
#' @param sig_c sigma for c (if c provided then provide sig_c also)
#' @param returnv return value, either 'sigma' (standard deviation) or
#' 'sigma squared' (variance)
#' @returns either an estimate for sigma or sigma squared of a product
#' @references Taylor, J. R. (1997). An Introduction to Error Analysis: The Study of Uncertainties in Physical Measurements (2nd ed.). University Science Books.
#' OR Bevington, P. R., & Robinson, D. K. (2003). Data Reduction and Error Analysis for the Physical Sciences (3rd ed.). McGraw-Hill.
#' @examples
#' error_product(5, 0.01, 10, 0.1)
#' error_product(5, 0.01, 10, 0.1, 5, 0.01)
#' @export
#'
error_product <- function(a, sig_a, b, sig_b, c = NA, sig_c = NA,
                          returnv = "sigmasquared")
  {
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(sig_a) ||
      !is.numeric(sig_b)) {
    stop("inputs must be numeric")
  }
  if (!is.character(returnv) || !(returnv %in% c("sigma", "sigmasquared"))) {
    stop("returnv must be either 'sigma' or 'sigmasquared' (default)")
  }

  if (anyNA(c)) {

    if(returnv == "sigma") {
      # sigma for a*b
      result <- abs(a * b) * sqrt((sig_a / a)^2 + (sig_b / b)^2)
    } else {
      # sigma_squared for a*b
      result <- (a * b)^2 * (sig_a / a)^2 + (sig_b / b)^2
    }

  } else {
    if (!is.numeric(c) || !is.numeric(sig_c)) {
      stop("c and sig_c must be numeric")
    }

    if(returnv == "sigma") {
      # sigma for a*b*c
      result <- abs(a * b * c) * sqrt((sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2)
    } else {
      # sigma_squared for a*b*c
      result <- (a * b * c)^2 * (sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2
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
#' @returns  biomass in oven dry tonnes or if sig_treevol is provided then
#' additionally returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Lavers, G.M. and Moore, G.L.
#' (1983) The strength properties of timber. Building Research Establishment
#' Report CI/Sfb i(J3). Building Research Establishment, Garston.
#' @examples
#' woodbiomass(10, 0.56, 5)
#' @export
#'
woodbiomass <- function(treevol, nsg, sig_treevol = NA, sig_nsg = 0.09413391) {

  if(!is.numeric(treevol))stop("treevol must be numeric") #todo and positive??
  if(!is.numeric(nsg) || any(nsg<0)){
    sig_nsg = 0.09413391
  }

  woodbio <- treevol * nsg
  sigma <- NA

  if(!anyNA(sig_treevol)){
    if(!is.numeric(sig_treevol)||sig_treevol<0){stop("'sig_treevol' must be positive & numeric")}
    if(!is.numeric(sig_nsg)||sig_nsg<0){stop("'sig_nsg' must be positive & numeric")}

    sigma <- error_product(treevol, sig_treevol, nsg, sig_nsg, returnv = "sigma")
    return(list(woodbiomass = woodbio, sigma = sigma))
  } else {
    return(woodbio)
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
#' @returns  biomass (oven dry tonnes) and estimated sigma
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.2.
#' @importFrom utils data
#' @examples
#' crownbiomass("CBSP", 25)
#' crownbiomass("CBOK", dbh = 25, re_d = 0.01)
#' crownbiomass(c("CBOK","CBOK"), dbh = c(30,50), re_d = 0.01)
#' @export
#'
crownbiomass <- function(spcode, dbh, re_d = 0.05, re = 0.025) {

  # Check inputs
  if (length(spcode) != length(dbh)) stop("Length of 'spcode' and 'dbh' must be the same")
  if (!is.numeric(dbh) || any(dbh < 0)){
    stop("Argument 'dbh' must be numeric and non-negative")
  }
  if (is.na(re_d) || !is.numeric(re_d)){
    stop("Argument 're_d' must be numeric")
  }
  if (is.na(re) || !is.numeric(re)){
    stop("Argument 're' must be numeric")
  }
  results <- data.frame(spcode = spcode, dbh = dbh, biomass = NA, sigma = NA)

  if (any(!is.numeric(dbh) | dbh < 0)) stop("All values of 'dbh' must be numeric and non-negative")

  # Loop over each entry in spcode
  for (i in 1:length(spcode)) {

    diam <- dbh[i]

    # Find the record in the data for the species code
    rec <- crown_biomasdf[crown_biomasdf$Code == spcode[i], ]
    if (nrow(rec) == 0) {
      spcode_match <- lookup_df$Crown[lookup_df$short == spcode[i]]
      rec <- crown_biomasdf[crown_biomasdf$Code == spcode_match, ]
      if (nrow(rec) == 0) warning(paste("The species code", spcode[i], "is not found"))
    }

    # Calculate biomass and error
    if (diam <= 50) {
      dp <- diam^rec$p
      crown_biomass <- rec$b1 * dp
      results$biomass[i] <- crown_biomass

      sigma_dp <- dp * sqrt((rec$p*re_d)^2 + (log(diam)*rec$p*re)^2)
      results$sigma[i] <- error_product(rec$b1, rec$b1*re, dp, sigma_dp)

    } else {
      crown_biomass <- rec$A + rec$b2 * diam
      results$biomass[i] <- crown_biomass

      # Error calculation for diam > 50
      if (!is.na(re_d)) {
        if (!is.numeric(re_d) || re_d < 0)
          stop("Argument 're_dbh' must be numeric and non-negative")

        results$sigma[i] <- sqrt((rec$A*re)^2 +
                              (diam * rec$b2*re)^2 + (rec$b2 * diam * re_d)^2)
      }
    }
  }

  # Warning for dbh < 7
  if (any(dbh < 7)) warning(paste("Equation is only specified for dbh >= 7"))

  return(results)
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
#' @returns biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.3.
#' @examples
#' rootbiomass(spcode = 'RBRAR', dbh = 50)
#' rootbiomass(spcode = 'RBRAR', dbh = 50, re_dbh = 0.10)
#' rootbiomass(spcode = c('RBRAR','RBRAR'), dbh = c(50, 70))
#' @export
#'
rootbiomass <- function(spcode, dbh, re_dbh = NA, re = 0.025) {

  # Initialise
  if (length(spcode) != length(dbh)) {
    stop("Length of 'spcode' and 'dbh' must be the same")
  }
  if (any(!is.numeric(dbh) | dbh < 0)) {
    stop("All values of 'dbh' must be numeric and non-negative")
  }
  if (length(dbh) > 1){
    if (!anyNA(re_dbh)) {
      if (length(re_dbh) != 1 && length(re_dbh) != length(dbh)) {
        stop("Length of 're_dbh' must be either 1 or match the length of 'dbh'")
      }
      if (!is.numeric(re) || re < 0) {
        stop("Argument 're' must be numeric and non-negative")
      }
      if (!is.numeric(re_dbh) || re_dbh < 0) {
        stop("Argument 're_dbh' must be numeric and non-negative")
      }

      results <- data.frame(spcode = spcode, dbh = dbh, rootbiomass = NA, sigma = NA)
    } else {
      results <- data.frame(spcode = spcode, dbh = dbh, rootbiomass = NA)
    }
  } else {
    results <- c()
  }

  # Iterate through each species code and dbh value
  for (i in seq_along(spcode)) {
    diam <- dbh[i]
    sigma <- if (!anyNA(re_dbh) && length(re_dbh) > 1) re_dbh[i] else re_dbh

    # Find the record in the data for the species code
    rec <- root_biomassdf[root_biomassdf$Code == spcode[i], ]

    if (nrow(rec) == 0) {

      spcode <- lookup_df$Root[lookup_df$short == spcode[i]]
      rec <- crown_biomasdf[crown_biomasdf$Code == spcode, ]

      if (nrow(rec) == 0) {
        warning(paste("The species code", spcode[i], "is not found in lookup_df$Root"))
      }
    }


    # Calculate root biomass based on dbh value
    if (diam <= 30) {
      root_biomass <- rec$b1 * diam^2.5

      # Calculate error if re_dbh is provided
      if (!anyNA(re_dbh)) {
        sig_diam <- diam^2.5 * sqrt((2.5*re_dbh)^2 + (log(diam)*re*2.5)^2)
        sig_root <- error_product(rec$b1, rec$b1*re, diam^2.5, sig_diam, returnv = "sigma")

      }
    } else {
      root_biomass <- rec$a + rec$b2 * diam

      # Calculate error if re_dbh is provided
      if (!anyNA(sigma)) {
        sig_root <- sqrt((rec$a*re)^2 + error_product(rec$b2, rec$b2*re, diam, re_dbh*diam))



      }
    }

    # Store results in the results data frame
    results$rootbiomass[i] <- root_biomass
    if (!anyNA(sigma)) {
      results$sigma[i] <- ifelse(exists("sig_root"), sig_root, NA)
    }
  }

  return(results)
}

############# Carbon to CO2e ################
#'
#' @title Carbon to CO2 equivalent
#' @description Function to convert from carbon to carbon dioxide equivalent
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param carbon carbon
#' @returns carbon dioxide equivalent
#' @examples
#' ctoco2e(448)
#' ctoco2e(c(448, 450))
#' @export
#'
ctoco2e <- function(carbon) {
  # Ensure carbon is numeric and positive
  if (any(!is.numeric(carbon) | carbon < 0)) {
    stop("All values of 'carbon' must be numeric and positive")
  }

  # Calculate CO2 equivalents for each carbon value
  co2e_values <- carbon * (44 / 12)

  return(co2e_values)
}


############# Plant biomass conversion to carbon ################
#'
#' @title Calculates biomass conversion to carbon
#' @description Converts biomass to carbon values using the carbon volatile
#'  fraction from chosen method/citation.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param biomass (usually kg or metric tonnes)
#' @param method Method of calculating the carbon volatile fraction.
#' "Matthews1": Simplest with the carbon volatile fraction, CVF = 50%
#' "Matthews2": CVF based on type (broadleaf or conifer) (Matthews, 1993)
#' "IPCC1": CVF = 47.7%
#' "IPCC2": Lookup CVF by type and biome  (IPCC, 2006)
#' "Thomas": Lookup by type and biome  (Thomas and Martin, 2012)
#' @param type broadleaf or conifer. Only required for method = 'Matthews2',
#' 'IPCC2' or 'Thomas'
#' @param biome tropical, subtropical, mediterranean, temperate or boreal.
#' Only needed for 'IPCC2' and 'Thomas' methods.
#' @param sig_biomass biomass uncertainty (optional, only available with 'IPCC2'
#'  and 'Thomas' methods)
#' @returns either carbon or if 'sig_biomass' is provided, returns a list of
#' carbon value and error. Errors only given with method = 'IPCC2' or 'Thomas'.
#' @references (1) Thomas, Sean C., and Adam R. Martin. "Carbon content of tree
#' tissues: a synthesis." Forests 3.2 (2012): 332-352.
#'  https://www.mdpi.com/1999-4907/3/2/332.
#' (2) IPCC. Forest lands. Intergovernmental Panel on Climate Change Guidelines
#'  for National Greenhouse Gas Inventories; Institute for Global Environmental
#'   Strategies (IGES): Hayama,Japan, 2006; Volume 4, p. 83.
#' (3) Matthews, G.A.R. (1993) The Carbon Content of Trees. Forestry Commission
#'  Technical Paper 4. Forestry Commission, Edinburgh. 21pp. ISBN: 0-85538-317-8
#'  @examples
#'  biomass2c(1, method="IPCC2", c("conifer"), "temperate")
#'
#'  @importFrom utils globalVariables
#' @export
#'
biomass2c <- function(biomass, method, type = NA, biome = 'temperate',
                      sig_biomass = NULL) {
  if (!is.na(type) & length(type) != length(biomass)) {
    stop("'type' and 'biomass' must have the same length.")}

  # Check arguments
  if (anyNA(biomass) | any(!is.numeric(biomass) | biomass < 0)) {
    warning("Biomass values must be numeric and positive")
  }

  valid_methods <- c("Matthews1", "Matthews2", "IPCC1", "IPCC2", "Thomas")
  if (!(method %in% valid_methods)) stop("Invalid method. Choose from:",
                                         paste(valid_methods,collapse = ", "))

  valid_types <- c("broadleaf", "conifer")
  if (method %in% c("Matthews2","IPCC2","Thomas") &
      !all(type %in% valid_types))
    stop("Type = 'broadleaf' or 'conifer' required for the chosen method as a
         list of same length of biomass and sig_biomass.")

  valid_biomes <- c("tropical", "subtropical", "mediterranean",
                    "temperate", "boreal")
  if (method %in% c("IPCC2", "Thomas") && !all(biome %in% valid_biomes))
    stop("Invalid biome.")

  n <- length(biomass)
  CVF <- re <- rep(NA, n)

  #utils::globalVariables(c("CVF_df"))

  # Retrieve CVF and re values using the lookup table
  for (i in seq_len(n)) {
    filter_conditions <- !is.na(CVF_df$method) & CVF_df$method == method &
      (is.na(CVF_df$type) | CVF_df$type == type[i]) &
      (is.na(CVF_df$biome) | CVF_df$biome == biome)
    matching_row <- subset(CVF_df, filter_conditions)

    if (nrow(matching_row) == 1) {
      CVF[i] <- matching_row$CVF
      re[i] <- matching_row$confidence
    } else {
      stop("Could not find a matching CVF value for the provided parameters.")
    }
  }

  # Calculate carbon values
  AGC <- biomass * CVF / 100

  # Calculate uncertainty
  if (method %in% c("IPCC2", "Thomas") && !is.null(sig_biomass)) {
    if (length(sig_biomass) != n) {
      stop("Length of sig_biomass must match length of biomass")
    }
    if (anyNA(sig_biomass) | any(!is.numeric(sig_biomass) | sig_biomass < 0)) {
      warning("sig_biomass must be numeric and positive to get sigma")
    }

    sigma <- sqrt((sig_biomass / biomass)^2 + (re / 100)^2) * AGC # TODO
    return(data.frame(AGC = AGC, sig_AGC = sigma))
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
#' @returns carbon in tonnes or if re_h provided then additionally the error
#' @note just uses simple linear relationship to get between measures
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018)
#' @importFrom utils data head tail
#' @examples
#' sap_seedling2C(50, 'conifer')
#' sap_seedling2C(heightincm = 900, type = 'broadleaf', re_h = 0.05)
#' @export
#'
sap_seedling2C <- function(heightincm, type, re_h = NA, re = 0.025) {
  if(!is.numeric(heightincm) || heightincm < 0)stop("
  Argument 'heightincm' must be numeric and non-negative")
  if(heightincm > 1000)stop("Maximum for 'heightincm' is 1000cm")
  if(heightincm < 1)   stop("Minimum for 'heightincm' is 1cm")

  if(type == "broadleaf"){
    data <- seedlings_broad
  } else if(type == "conifer"){
    data <- seedlings_conifer
  } else {
    stop('define type as broadleaf or conifer.')
  }

  # test

  # Find the bounding rows for interpolation
  lower_bound <- utils::tail(data[data$height.cm <= heightincm, ], 1)
  upper_bound <- utils::head(data[data$height.cm >= heightincm, ], 1)

  # If heightincm matches exactly a row in data
  if (lower_bound$height.cm == upper_bound$height.cm) {
    carbon_value <- lower_bound$Carbon.kg
    if(is.na(re_h)){
      return(carbon_value)
    } else {
      if(!is.numeric(re_h) || re_h < 0)stop("'re_h' must be numeric and postive")
      if(!is.numeric(re) || re < 0)stop("'re' must be numeric and postive")

      carbon_sd <- 10 * re * carbon_value
      return(list(carbon = carbon_value, sd = carbon_sd))
    }
  }

  # Perform linear interpolation if within bounds
  if (nrow(lower_bound) == 1 && nrow(upper_bound) == 1) {
    height_diff <- upper_bound$height.cm - lower_bound$height.cm
    carbon_diff <- upper_bound$Carbon.kg - lower_bound$Carbon.kg
    proportion <- (heightincm - lower_bound$height.cm) / height_diff
    interpolated_carbon <- lower_bound$Carbon.kg + proportion * carbon_diff


   if(!is.na(re_h)) {
     if(!is.numeric(re_h) || re_h < 0)stop("'re_h' must be numeric and postive")
     if(!is.numeric(re) || re < 0)stop("'re' must be numeric and postive")

     carbon_sd <- sqrt((re * interpolated_carbon)^2 +
                       ((carbon_diff / height_diff) * re_h*heightincm)^2)

    return(list(carbon = interpolated_carbon, sd = carbon_sd))
   } else {
    return(interpolated_carbon)
  }
  }
}

############# FC Lookup Species Code ################
#'
#' @title Lookup species code
#' @description  Function that looks up species codes for Woodland Carbon Code
#' @author Isabel Openshaw I.Openshaw@kew.org
#' @param name name of species
#' @param type either 'broadleaf' or 'conifer'
#' @param returnv either 'all', short', 'single', 'stand', 'root' from lookup_df.Rda
#' @returns Species code
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom stringr word str_trim
#' @importFrom utils data
#' @examples
#' lookspcode(name='Quercus robur', type = "broadleaf", returnv='short')
#' lookspcode(c("Scots pine", "Oak"), c("conifer", "NA"), "short")
#' lookspcode(c("Scots pine", "Oak"), c("conifer", "broadleaf"))
#' lookspcode(c('Quercus robur', 'Quercus') , type = c("broadleaf", "broadleaf"), returnv='short')
#' @export
#'
lookspcode <- function(name, type = NA, returnv = "all") {
  # Validate inputs
  if (!is.character(name)) stop("'name' must be a character vector or list")
  #if (!all(is.na(type)) | length(type) != length(name))
  #  stop("'type' and 'name' must have the same length.")
  code = c("short", "single", "stand", "root", "all")
  if (!is.character(returnv) || !returnv %in% code) {
    stop("'returnv' must be a character, either ", paste(code, collapse = ", "))
  }

  # Define a helper function for matching
  match_species <- function(search_name, search_type) {
    # Match by common name
    rec <- lookup_df[tolower(lookup_df$common_name) == search_name, ]
    if (nrow(rec) == 1) return(cbind(rec, matchtype = "Common name"))

    # Match by species binomial
    rec <- lookup_df[tolower(lookup_df$latin_name) == search_name, ]
    if (nrow(rec) == 1) return(cbind(rec, matchtype = "Species binomial"))

    # Match by genus
    rec <- lookup_df[tolower(lookup_df$General.genus) == stringr::word(search_name, 1), ]
    if (nrow(rec) == 1) return(cbind(rec, matchtype = "Genus"))

    # Fallback to type broadleaf/conifer
    if (!is.na(search_type) && search_type != "") {
      rec <- lookup_df[lookup_df$General.type == search_type, ]
      if (nrow(rec) == 1) return(cbind(rec, matchtype = search_type))
    }

    # Fallback to mixed species code
    rec <- lookup_df[lookup_df$General.type == "mixed", ]
    if (nrow(rec) > 0) return(cbind(rec[1, ], matchtype = "Mixed"))

    # If no match found, return NA row
    return(data.frame(spname = NA, spcode = NA, General.genus = NA,
                      General.type = NA, short = NA, single = NA, stand = NA,
                      Root = NA, matchtype = NA, stringsAsFactors = FALSE))
  }

  # Apply helper function over all inputs
  results <- mapply(match_species,
    search_name = stringr::str_trim(tolower(name)),
    search_type = ifelse(is.na(type), "mixed", type), # Replace NA with "mixed"
    SIMPLIFY = FALSE)

  # Combine results into a dataframe
  result_df <- do.call(rbind, results)

  # Handle return values
  if (returnv != "all") {
    result_df <- data.frame(
      spname = name,
      spcode = switch(returnv, short = result_df$short,
                      single = result_df$single, stand = result_df$stand,
                      root = result_df$Root), matchtype = result_df$matchtype,
      stringsAsFactors = FALSE)
  }

  return(result_df)
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
#' @returns either Above ground carbon (AGC) in tonnes, or a list with tariff
#' number, merchantable volume (metres cubed), stem volume (metres cubed),
#' stem biomass (tonnes), stem carbon (tonnes), canopy carbon (tonnes),
#' root carbon (tonnes) and AGC.
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' fc_agc(spcode='OK', dbh=74, height=24, output.all = FALSE)
#' # Input wood density and sd from BIOMASS package
#' wd <- BIOMASS::getWoodDensity('Quercus', 'robur', region='Europe')
#' fc_agc('OK', 72, 24, nsg = wd$meanWD)
#' @export
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
  biomes <- c("tropical", "subtropical", "mediterranean", "temperate", "boreal")
  if (!missing(biome) && !(biome %in% biomes)) {
    stop("Invalid biome. Choose from: ", paste(biomes, collapse = ", "))
  }
  n = length(name)
  spcodes <- lookspcode(name, type, returnv = 'short')

  # Create results table
  if(output.all){
    r <- data.frame(name=name, type=type, dbh=dbh, height=height, spcode=spcodes$spcode,
                    matchtype=spcodes$matchtype, NSG=NA, tariff=NA,
                    mercvol_m.3=NA, stemvol_m.3=NA, stembiomass_t=NA, crownbiomass_t=NA,
                    rootbiomass_t=NA, AGC_WCC_t=NA, stringsAsFactors=FALSE)
  } else {
    r <- data.frame(name=name, spcode=spcodes$spcode,
                    matchtype=spcodes$matchtype, AGC_WCC_t=NA, stringsAsFactors=FALSE)
  }
  r <- r[1:n,]

  # Loop over all trees
  for (i in 1:n) {
    if (dbh[i]<=0 || !is.numeric(dbh[i]) || is.na(dbh[i])) {
      warning("dbh is not numeric or positive for index: ", i)
      next
    }

    # Lookup species data from code
    rec <- lookup_df[lookup_df$short == r$spcode[i], ]

    # Get current type if not provided
    if(is.na(type[i]) | type[i] == "NA") {
      r$type[i] <- rec$type }

    # If height less than 6.5 use sapling model
    if (is.na( height[i]) | height[i] < 0.01 ) {
      warning("Height is missing or less than 1cm minimum, skipping record ", i)
      next
      } else if( height[i] < 6.5 ){
      if(!r$type[i] %in% c("broadleaf", "conifer")){
        next("Type must be specified as 'broadleaf' or 'conifer' for
             sap_seedling2C function for height < 6.5 m for index ", i)
      }
      carbon <- sap_seedling2C(heightincm = height[i]*100, r$type[i])
      r$AGC_WCC_t[i] <- carbon
    } else {
      # Get tariff number depending on broadleaf or conifer
      tariff <- tariffs(r$spcode[i], height[i], dbh = dbh[i])

      if (length(tariff) == 0) {
        warning("Error in", r$type[i], "_tariff function at index: ", i)
      }
      # If nsg not specified lookup from rec
      if(is.na(nsg)){ nsg <- rec$NSG }

      # Calculate volumes and biomass
      mercvol <- merchtreevol(dbh[i], tariff)             # Tree volume
      stemvol <- treevol(mercvol, dbh = dbh[i])           # Stem volume
      woodbio <- woodbiomass(stemvol, nsg)                # Stem  Biomass
      crownbio <- crownbiomass(rec$Crown, dbh[i])         # Crown Biomass
      AGB <- woodbio + crownbio$biomass                   # Above ground Biomass

      if(method  %in% c("IPCC2", "Thomas") &
         !r$type[i] %in% c("broadleaf", "conifer")){
        warning("Type must be specified as 'broadleaf' or 'conifer' for the
        chosen method to convert biomass to carbon for index ", i)
      } else {
        AGC <- biomass2c(AGB, method=method, r$type[i], biome=biome)  # Carbon
        r$AGC_WCC_t[i] <- AGC
      }
    }

    if(output.all){
      r$NSG[i] <- rec$NSG

      if(height[i] >= 6.5){
        r$tariff[i] <- tariff
        r$mercvol_m.3[i] <- mercvol
        r$stemvol_m.3[i] <- stemvol
        r$stembiomass_t[i] <- woodbio
        r$crownbiomass_t[i] <- crownbio$biomass

        # Root Biomass
        rootbio <- rootbiomass(rec$Root, dbh[i])
        r$rootbiomass_t[i] <- rootbio$rootbiomass
      }
    }
  }
  if(output.all){
    return(r)
  } else {
    return(r$AGC_WCC_t)
  }
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
#' @returns either Above ground carbon, AGC in tonnes, or if output.all = TRUE,
#' a list of tariff number, merchantable volume (metres cubed), stem volume
#' (metres cubed), stem biomass (tonnes), stem carbon (tonnes), canopy carbon
#' (tonnes) and root carbon (tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' fc_agc_error('Quercus robur', dbh=74, height=24, output.all = FALSE)
#' fc_agc_error('Oak', dbh=74, height=24, method="IPCC2",
#' biome="temperate", output.all = FALSE, re_dbh=10, re_h=1)
#' # Input wood density and sd from BIOMASS package
#' wd <- BIOMASS::getWoodDensity('Quercus', 'robur', region='Europe')
#' fc_agc_error('Oak', 72, 24, nsg = wd$meanWD, sig_nsg = wd$sdWD)
#' fc_agc_error(c('Quercus robur','beech'), c(74,23), c(24,24), output.all = FALSE)
#' @export
#'
fc_agc_error <- function(name, dbh, height, type = NA, method = "IPCC2", biome =
                           "temperate", output.all = TRUE, re_dbh = 0.05, re_h =
                           0.1, re = 0.025, nsg = NA, sig_nsg = 0.09413391){

  # Check arguments
  if(!is.character(name)) stop ("name must be a character")
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }
  #if(length(name) != length(dbh) || length(name) != length(height) ||
  #   length(height) != length(dbh))
  #  stop("input lengths must be the same")
  if(any(!type %in% c("broadleaf", "conifer", NA, "NA")))
    stop("type must equal either conifer, broadleaf or NA")
  if(!is.numeric(re_dbh) || any(re_dbh<=0))
    stop("re_dbh must be numeric & positive")
  if(!is.numeric(re_h) || any(re_h<0)) stop ("re_h must be numeric & positive")

  if (!(method %in% c("IPCC2", "Thomas"))) {
    stop("Invalid method. Choose from: 'IPCC2', 'Thomas'")
  }
  biomes <- c("tropical", "subtropical", "mediterranean", "temperate", "boreal")
  if (!missing(biome) && !(biome %in% biomes)) {
    stop("Invalid biome. Choose from: ", paste(biomes, collapse = ", "))
  }

  # Lookup species codes
  spcodes <- lookspcode(name, type, returnv = 'short')
  n = length(name)

  # Create results table
  if(output.all){
    r <- data.frame(name=name, type=type, spcode=spcodes$spcode,
                    matchtype=spcodes$matchtype, dbh=dbh, height=height, NSG=NA,
                    tariff=NA, sig_tariff=NA, mercvol_m.3=NA, sig_mercvol=NA,
                    stemvol_m.3=NA, sig_stemvol=NA,stembiomass_t=NA,
                    sig_stembiomass=NA, crownbiomass_t=NA, sig_crownbiomass=NA,
                    rootbiomass_t=NA, sig_rootbiomass=NA, AGC_WCC_t=NA, sig_AGC=NA,
                    stringsAsFactors=FALSE)
  } else {
    r <- data.frame(name=name, AGC_WCC_t=NA, sig_AGC=NA, spcode=spcodes$spcode,
                    matchtype=spcodes$matchtype, stringsAsFactors=FALSE)
  }
  r <- r[1:n,]
  #utils::data(lookup_df, envir = environment())

  # Loop over all trees
  for (i in 1:n) {
    if (dbh[i]<=0 || !is.numeric(dbh[i]) || is.na(dbh[i])) {
      warning("dbh is not numeric or positive for index:", i)
      next
    }
    # Lookup species data from code
    rec <- lookup_df[lookup_df$short == r$spcode[i], ]

    # Get current type if not provided
    if(is.na(type[i]) | type[i] == "NA") {
      r$type[i] <- rec$type }

    # If height less than 6.5 use sapling model
    if (is.na(height[i]) | height[i] < 0.01) {
      warning("Height is missing or less than 1cm minimum, skipping record ", i)
      next
    } else if(height[i] < 6.5){
      if(!r$type[i] %in% c("broadleaf", "conifer")){
        next("Type must be specified as 'broadleaf' or 'conifer' for
             sap_seedling2C function for height < 6.5 m for index ", i)
      }
      carbon <- sap_seedling2C(heightincm = height[i]*100, r$type[i],
                               re_h = re_h, re = re)
      r$AGC_WCC_t[i] <- carbon$carbon
      r$sig_AGC[i] <- carbon$sd
    } else {
      # Get tariff number depending on broadleaf or conifer
      tariff <- tariffs(r$spcode[i], height[i], dbh = dbh[i],
                        re_h = re_h, re_dbh = re_dbh, re_a = re)

      if (length(tariff) == 0) {
        warning("Error in", r$type[i], "_tariff function at index: ", i)
      }
      # If nsg not specified lookup from rec
      if(is.na(nsg)){ nsg <- rec$NSG }

      # Calculate volumes and biomass
      # Tree volume
      mercvol <- merchtreevol(dbh[i], tariff$tariff, re_dbh,
                              as.numeric(tariff[2]),  re)
      # Stem volume
      stemvol <- treevol(as.numeric(mercvol[1]), dbh = dbh[i],
                         as.numeric(mercvol[2]), re)
      # Stem  Biomass
      woodbio <- woodbiomass(as.numeric(stemvol)[1], nsg,
                             as.numeric(stemvol)[2], sig_nsg = sig_nsg)
      # Crown Biomass
      crownbio <- crownbiomass(rec$Crown, dbh[i], re_dbh, re)
      # Above ground Biomass
      AGB <- woodbio$woodbiomass + crownbio$biomass
      sig_AGB <- sqrt(woodbio$sigma^2 + as.numeric(crownbio[4])^2)   # tocheck

      if(!r$type[i] %in% c("broadleaf", "conifer")){
        AGC <- AGB * 0.4885
        warning("Type must be specified as 'broadleaf' or 'conifer' to convert
                biomass to carbon and to generate error for index ", i)
      } else {
        # Above ground Carbon
        AGC <- biomass2c(AGB, method, r$type[i], biome, sig_biomass=sig_AGB)
        r$AGC_WCC_t[i] <- AGC$AGC
        r$sig_AGC[i] <- AGC$sig_AGC
      }
    }

    if(output.all){
      r$NSG[i] <- nsg

      if(height[i] >= 6.5){
        r$tariff[i] <- tariff$tariff
        r$sig_tariff[i] <- tariff$sigma
        r$mercvol_m.3[i] <- mercvol$volume
        r$sig_mercvol[i] <- mercvol$sigma
        r$stemvol_m.3[i] <- as.numeric(stemvol[1])
        r$sig_stemvol[i] <- as.numeric(stemvol[2])
        r$stembiomass_t[i] <- woodbio$woodbiomass
        r$sig_stembiomass[i] <- woodbio$sigma
        r$crownbiomass_t[i] <- crownbio$biomass
        r$sig_crownbiomass[i] <- as.numeric(crownbio[4])  # tocheck

        # Root Biomass
        rootbio <- rootbiomass(rec$Root, dbh[i], re_dbh)
        r$rootbiomass_t[i] <- rootbio$rootbiomass
        r$sig_rootbiomass[i] <- rootbio$sigma
      }
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
#' @returns  either vector of mean and sd or vector of quantiles
#' @references todo** and to write at export
#' @importFrom stats quantile rnorm sd
#' @export
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
#' @returns  biomass in kg
#' @references Bunce, R. G. H. "Biomass and Production of Trees in a Mixed
#' Deciduous Woodland: I. Girth and Height as Parameters for the Estimation of
#' Tree Dry Weight" (1968)
#' @importFrom utils data
#' @examples
#' bunce("Oak", 24)
#' bunce(c("Oak", "Beech"), c(24,23))
#' @export
#'
bunce <- function(name, dbh, type = NA) {
  # Convert DBH to numeric, coercing invalid values to NA
  dbh <- as.numeric(dbh)

  # Ensure 'name' and 'dbh' have the same length or allow recycling
  if (length(name) != length(dbh)) {
    if (length(name) == 1) {
      name <- rep(name, length(dbh))
    } else if (length(dbh) == 1) {
      dbh <- rep(dbh, length(name))
    } else {
      stop("'name' and 'dbh' must be of the same length")
    }
  }

  # Pre-compute combined coefficients
  combined_coeffs <- buncedf[buncedf$spcode == "XB",]

  # Helper function to calculate biomass for each name and DBH
  calculate_biomass <- function(species_name, DBH) {
    if (is.na(species_name) || species_name == "NA" || is.na(DBH) || DBH <= 0) {
      return(list(biomass = NA, spcode = NA, species_name = species_name))
    }

    # Lookup species code from the name
    lookup <- lookspcode(species_name, type = type, returnv = "short")
    if (nrow(lookup) == 0) {
      return(list(biomass = NA, spcode = NA, species_name = species_name))
    }
    spcode <- lookup$spcode[1]

    # Extract coefficients for the given species code
    coeffs <- buncedf[buncedf$spcode == spcode,]
    if (nrow(coeffs) == 0) {
      coeffs <- combined_coeffs
      spcode <- "XB"  # Indicate fallback code
    }

    # Calculate biomass
    biomass <- coeffs$a + coeffs$b * log(pi * DBH)
    return(list(biomass = exp(biomass), spcode = spcode, species_name = species_name))
  }

  # Apply the calculation over all inputs
  results <- mapply(calculate_biomass, name, dbh, SIMPLIFY = FALSE)

  # Extract the biomass, species names, and spcodes
  biomass <- sapply(results, function(x) x$biomass)
  spcodes <- sapply(results, function(x) x$spcode)
  species_names <- sapply(results, function(x) x$species_name)

  # Return as a data frame
  output <- data.frame(
    species_name = species_names,
    dbh = dbh,
    biomass = biomass,
    spcode = spcodes,
    stringsAsFactors = FALSE
  )

  return(output)
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
#' @returns  your dataframe back with added columns of carbon estimates. If
#' output.all = FALSE, then returns columns 'Genus_corrected','Species',
#' 'Family','Latitude','Longitude','DBH','AGB_Biomass_kg'. If output.all = TRUE
#' then additionally returns columns 'Wood_Density', 'Wood_Density_sd',
#' 'Height_est', 'RSE' (Residual Standard Error of the model), 'Height_1' (which
#' is inputed height filled in with Height estimate where missing)
#' @importFrom utils install.packages
#' @examples
#' coords <- c(-0.088837,51.071610)
#' biomass(12, 12, 'Quercus', 'robus', coords)
#'
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
#' @title Estimate Tree Carbon using Biomass package functions
#' @description Using the Biomass package to calculate carbon
#' @param DBH Diameter at breast height (cm)
#' @param Genus First part of Species binomial
#' @param Species Second part of Species binomial
#' @param new.eqtable a subset or extension of the allometric equation table. Create with allodb::new_equations
#' @param coords either a vector of coordinates of the site or a matrix of
#' coordinates for each tree of longitude and latitude
#' @param output.all if TRUE outputs all data from processing, else just outputs carbon figures
#' @returns  your dataframe back with added columns of carbon estimates. If
#' output.all = FALSE, then returns columns 'Genus_corrected','Species',
#' 'Family','Latitude','Longitude','DBH','AGB_Biomass_kg'. If output.all = TRUE
#' then additionally returns columns 'Wood_Density', 'Wood_Density_sd',
#' 'Height_est', 'RSE' (Residual Standard Error of the model), 'Height_1' (which
#' is inputed height filled in with Height estimate where missing)
#' @export
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
