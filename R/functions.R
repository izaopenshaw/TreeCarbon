############## Functions file for Woodland Carbon Code ########################
# TODO:
# to find seedling function and add it to fc function
# error for ctoco2e ?
# todo error for nsg
# search not found and check that intermediate species are found in lookup_df
# check biomass2c that error is confidence percentage by checking references

# species specific sd for nsg?

# fall back species code: MX :: lookup_df[lookup_df$short == "MX", ]

############# FC Tariff number from volume and tree basal area (WCC Eq 1) ############
#'
#' @title Tariff number from volume and basal area
#' @description Using the sample tree’s basal area and volume to calculate the
#' tariff number. Basal area is calculated by ba = (pi * dbh^2)/40000.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param vol tree volume in metres cubed
#' @param dbh diameter at breast height in centimetres
#' @param sig_vol sigma for tree volume (optional)
#' @param sig_dbh sigma for diameter at breast height (optional)
#' @param conf_a 1 - the percentage confidence interval for coefficients or
#' the relative error of coefficients (default = 5%)
#' @returns  Tariff number or if sigma for inputs are provided, then will return
#' a list of tariff number and sigma for tariff
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). (Equation 1)
#' @examples
#' tariff_vol_area(vol=0.5, dbh=24, sig_vol = 0.001, sig_dbh = 1)
#' @export
#'
tariff_vol_area <- function(vol, dbh, sig_vol = NA, sig_dbh = NA, conf_a = 0.025){
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

  if(!anyNA(sig_vol) || !anyNA(sig_dbh)){
    if (any(sig_vol < 0) || !is.numeric(sig_vol)) {stop("sigm_vol must be non-negative numeric")}
    if (any(sig_dbh < 0) || !is.numeric(sig_dbh)) {stop("sig_dbh must be non-negative numeric")}

    sig_ba <- (pi * 2 * dbh / 40000) * sig_dbh
    sig_a1 <- a1 * sqrt(
      (sig_vol / (vol - const_vol))^2 +         # Error from vol
        (sig_ba / (ba - const_ba))^2 +            # Error from ba
        (conf_a * const_vol / (vol - const_vol))^2 +   # Error from const_vol
        (conf_a * const_ba / (ba - const_ba))^2        # Error from const_ba
    )
    sig_t <- sqrt((coef_a1 * sig_a1)^2 + (a1 * conf_a * coef_a1)^2)

    return(list(tariff=tariff, sigma_tariff=sig_t))
  } else {
    return(tariff)
  }
}

############# FC conifer tree tariff number (WCC Eq 3) ############################
#'
#' @title Conifer tree tariff number
#' @description Use dbh and tree height to calculate the tariff number of each
#'  sample tree. Species-specific estimates of a1 – a3 are found in the
#'  R data file, 'tariff_coniferdf'.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param dbh diameter at breast height in centimetres
#' @param spcode species code (single)
#' @param sig_h uncertainty in height
#' @param sig_dbh uncertainty in dbh
#' @param conf_a 1 - the percentage confidence interval for coefficients or
#' the relative error of coefficients (default = 5%)
#' @returns  tariff number
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' conifer_tariff("SP", 74, 24)
#' conifer_tariff("SP", 74, 24, 5, 5)
#' @export
#'
conifer_tariff <- function(spcode, height, dbh, sig_h = NA, sig_dbh = NA, conf_a = 0.025) {
  if(any(!is.numeric(height)))stop("height must be numeric") else if(any(height<=0))stop("height must be positive")
  if(any(!is.numeric(dbh)))   stop("dbh must be numeric") else if(any(dbh<=0))   stop("dbh must be positive")

  if (!(length(spcode) == length(height) && length(height) == length(dbh))) {
    stop("All input vectors (spcode, height, dbh, sig_h, sig_dbh) must have the same length.")
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

    if(!is.na(sig_dbh) || !is.na(sig_h)){
      if(!is.numeric(sig_dbh) || any(sig_dbh<0))stop("must provide a numeric and positive sig_dbh with sig_h")
      if(!is.numeric(sig_h) || any(sig_h<0))stop("must provide a numeric and positive sig_h with sig_dbh")

      error <- sqrt(tc$a1*conf_a^2 + error_product(tc$a2, tc$a2*conf_a, height, sig_h)
                      + error_product(tc$a3, tc$a3*conf_a, dbh, sig_dbh))

      return(data.frame(tariff = tariff, error = error))
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
#' @param sig_h sigma for tree height (optional)
#' @param sig_dbh sigma for diameter at breast height (optional)
#' @param conf_a 1 - the percentage confidence interval for coefficients or
#' the relative error of coefficients (default = 5%)
#' @returns  tariff number and error if sigma of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Method B, Equation 2.
#' @importFrom utils data
#' @examples broadleaf_tariff(spcode = 'OK', height = 25, dbh = 75)
#' broadleaf_tariff(spcode = "OK", height = 25, dbh = 15, sig_dbh = 5, sig_h = 1)
#' broadleaf_tariff(spcode = "OK", height = 24, dbh = 15, sig_dbh = 5, sig_h = 1)
#' @export
#'
broadleaf_tariff <- function(spcode, height, dbh, sig_dbh = NA, sig_h = NA, conf_a = 0.025) {
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

  if(!is.na(sig_dbh) || !is.na(sig_h)){
    if(!is.numeric(sig_dbh) || any(sig_dbh<0))stop("must provide a numeric and positive sig_dbh with sig_h")
    if(!is.numeric(sig_h) || any(sig_h<0))stop("must provide a numeric and positive sig_h with sig_dbh")

    error <- sqrt((conf_a*tb$a1)^2 +
                error_product(tb$a3, conf_a*tb$a3, dbh, sig_dbh) +
                error_product(tb$a2, conf_a*tb$a2, height, sig_h) +
                error_product(tb$a4, conf_a*tb$a4, dbh, sig_dbh, height, sig_h)
                )
    return(data.frame(tariff = tariff, error = error))
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
#' @param sig_h sigma for height (optional)
#' @param conf_a 1 - the percentage confidence interval for coefficients or
#' the relative error of coefficients (default = 5%)
#' @returns  either tariff number or if sig_h is provided, then returns a list
#' of the tariff number and uncertainty
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' stand_tariff("OK", height = 10)
#' stand_tariff("OK", height = 10, sig_h = 1)
#' stand_tariff(spcode = "AH", height = 10, sig_h = 0.5)
#' conifer_tariff(spcode=c("SP", "NS", "OK"), c(74, 25,2), c(74, 25,2), 5, 5)
#' @export
#'
stand_tariff <- function(spcode, height, sig_h = NA, conf_a=0.025) {
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

  if(!anyNA(sig_h)){
    if(!is.numeric(sig_h) || any(sig_h<0))stop("sig_h must be numeric and positive")

    error <- sqrt(rec$a1 * conf_a^2 +
                    error_product(rec$a2, rec$a2 * conf_a, height, sig_h))

    if (rec$a3 != 0) { # Include the quadratic term if rec$a3 is nonzero
      error <- sqrt(error^2 + error_product(rec$a3, rec$a3 * conf_a, height, sig_h))
    }

    return(data.frame(tariff = tariff, error = error))
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
#' @param sig_h sigma for height (optional)
#' @param dbh diameter at breast height in centimetres
#' @param sig_dbh sigma for diameter at breast height (optional)
#' @param conf_a 1 - the percentage confidence interval for coefficients or
#' the relative error of coefficients (default = 5%)
#' @returns either tariff number or if sig_h is provided, then returns a list
#' of the tariff number and uncertainty
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' tariffs("OK", height= 10, dbh = 20)
#' tariffs("OK", 10, 20, sig_h = 1, sig_dbh = 1)
#' tariffs(c("OK","NS", "NS", "SP", "SP"), c(10,10,5,10,10), c(20,20,10,20,NA), sig_h = 1, sig_dbh = 1)
#' @export
#'
tariffs <- function(spcode, height, dbh = NA, sig_h = NA, sig_dbh = NA, conf_a = 0.025) {

  # Ensure all inputs have the same length
  n <- length(height)
  if (!(length(spcode) == n && length(dbh) == n || is.na(dbh))) {
    stop("All input vectors (spcode, height, dbh, sig_h, sig_dbh) must have the same length.")
  }
  # Prepare output lists
  tariffs_result <- rep(NA, n)
  if(!anyNA(sig_h)){errors_result <- rep(NA, n)}

  # Loop over each tree to calculate tariffs
  for (i in 1:n) {
    result <- NA
    if (is.na(height[i])) {
      warning("Height is missing for spcode ", spcode[i], ", skipping record ", i)
      next
    } else if (!is.numeric(height[i]) || height[i] <= 0) {
      stop("Height must be numeric and positive for spcode ", spcode[i], "index", i)
    } else {
      # Determine tree type
      type <- lookup_df$type[lookup_df$short == spcode[i]]
      if (length(type) == 0) {
        warning("Species code ", spcode[i], " not found, skipping record ", i)
      } else {
        # Call the appropriate tariff function
        if (is.na(dbh[i])) {
          result <- stand_tariff(spcode[i], height[i], sig_h, conf_a)
        } else if (type == "conifer") {
          result <- conifer_tariff(spcode[i], height[i], dbh[i], sig_h, sig_dbh, conf_a)
        } else if (type == "broadleaf") {
          result <- broadleaf_tariff(spcode[i], height[i], dbh[i], sig_h, sig_dbh, conf_a)
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
  if (anyNA(sig_dbh) || anyNA(sig_h)) {
    return(tariffs_result)
  } else {
    return(data.frame(tariff = tariffs_result, error = errors_result))
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
#' @param sig_dbh diameter at breast height sig (optional)
#' @param conf_cf 1 - the percentage confidence interval for conversion factor
#' or the relative error of coefficients (default = 5%)
#' @returns  volume in metres cubed and error if sig of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' merchtreevol(dbh = 24, tariff = 24)
#' merchtreevol(dbh = 24, tariff = 24, sig_dbh = 1, sig_tariff = 1)
#' @export
#'
merchtreevol <- function(dbh, tariff, sig_dbh = NA, sig_tariff = NA, conf = 0.025) {
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

  if(anyNA(sig_dbh)) {
    return(vol)
  } else {
    sig_ba <- (pi * dbh / 20000) * sig_dbh
    sig_a2 <- 0.315049301 * sig_tariff
    sig_a1 <- sqrt(
      (0.0360541 - 0.315049301 * 0.118288)^2 * sig_tariff^2 +
        (0.118288 * sig_a2)^2
    )
    error <- sqrt(
      sig_a1^2 +
        (ba * sig_a2)^2 +
        (a2 * sig_ba)^2
    )

    result <- list(volume = vol, error = error)
    return(result)
  }
}

############# FC stem tree volume ################
#'
#' @title Forestry commission tree wood volume
#' @description Calculate the stem volume by multiplying the merchantable
#' tree volume by the appropriate species multiplication factor from stemvol.rda
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param mtreevol merchantable tree volume
#' @param dbh diameter at breast height in centimeters (greater than 6.5 cm)
#' @param sig_mtreevol sigma for mtreevol (optional)
#' @param conf_cf 1 - the percentage confidence interval for conversion factor
#' or the relative error of coefficients (default = 5%)
#' @returns  volume metres cubed or if sig_mtreevol is provided then additionally
#'  returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' treevol(mtreevol = 10, dbh = 24)
#' treevol(mtreevol = 10, dbh = 24, sig_mtreevol = 0.07)
#' @export
#'
treevol <- function(mtreevol, dbh, sig_mtreevol = NA, conf_cf=0.025) {

  if(!is.numeric(dbh) || any(dbh<=0))stop("dbh must be numeric and positive")
#  if(!is.numeric(mtreevol) || any(mtreevol<=0))stop("mtreevol must be numeric and positive") # todo

  dbh <- round(dbh)
  if (dbh < 500 & dbh > 6.5) {
    #utils::data(stemvol, envir = environment())
    cf <- stemvol$X[stemvol$dbh..cm. == dbh]

  } else if (dbh < 6.5){
    warning("dbh is less than 6.5 cm, multiplication factor is not specified")
    cf <- 1
  } else if (dbh > 500){
    warning("dbh is above 5 m")
    cf <- 1
  }
  stemvol <- cf * mtreevol

  if(anyNA(sig_mtreevol)){
    return(stemvol)
  } else {
    if(!is.numeric(sig_mtreevol) || any(sig_mtreevol<=0))stop("sig_mtreevol must be numeric and positive")
    error <- error_product(cf, cf*conf_cf, mtreevol, sig_mtreevol)

    result <- list(stemvolume = stemvol, error = error)
    return(result)
  }
}

############# Propagation of error for a product ################
#' @title Analytical error progression for a product
#' @description Calculates the error for x when x = a * b or x = a * b * c
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param a first variable in product
#' @param sig_a sigma for a
#' @param b second variable in product
#' @param sig_b sigma for b
#' @param c (optional) third variable in product
#' @param sig_c sigma for c (if c provided then provide sig_c also)
#' @returns error for x = (abs(a * b) * sqrt((sig_a / a)^2 + (sig_b / b)^2))^2
#' or (abs(a * b * c) * sqrt((sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2))^2
#' @references Taylor, J. R. (1997). An Introduction to Error Analysis: The Study of Uncertainties in Physical Measurements (2nd ed.). University Science Books.
#' OR Bevington, P. R., & Robinson, D. K. (2003). Data Reduction and Error Analysis for the Physical Sciences (3rd ed.). McGraw-Hill.
#' @examples
#' error_product(5, 0.01, 10, 0.1)
#' error_product(5, 0.01, 10, 0.1, c=5, sig_c=0.01)
#' @export
#'
error_product <- function(a, sig_a, b, sig_b, c=NA, sig_c=NA)
  {
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(sig_a) ||
      !is.numeric(sig_b)) {
    stop("inputs must be numeric")
  }

  if (anyNA(c)) {
    sigmasquared <- (a * b * sqrt((sig_a / a)^2 + (sig_b / b)^2))^2
  } else {
    if (!is.numeric(c) || !is.numeric(sig_c)) {
      stop("c and sig_c must be numeric")
    }
    sigmasquared <- (a * b * c * sqrt((sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2))^2
  }

  return(sigmasquared)
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

#  if(!is.numeric(treevol) || any(treevol<0))stop("treevol must be numeric and positive") #todo
  if(!is.numeric(nsg) || any(nsg<0))stop("nsg must be numeric and positive")

  woodbio <- treevol * nsg
  error <- NA

  if(!anyNA(sig_treevol)){
    error <- error_product(treevol, sig_treevol, nsg, sig_nsg) # TODO ***
  }
  return(list(woodbiomass = woodbio, error = error))
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
#' @param sig_dbh dbh sigma
#' @param conf one minus percentage confidence or relative error for
#' coefficients of the equation
#' @returns  biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.2.
#' @importFrom utils data
#' @examples
#' crownbiomass("CBSP", 25)
#' crownbiomass("CBOK", dbh = 25, sig_dbh = 10)
#' crownbiomass(c("CBOK","CBOK"), dbh = c(30,50), sig_dbh = 1)
#' @export
#'
crownbiomass <- function(spcode, dbh, sig_dbh = NA, conf = 0.025) {

  # Check inputs
  if (length(spcode) != length(dbh)) stop("Length of 'spcode' and 'dbh' must be the same")

  if (length(dbh) > 1){
    if (!anyNA(sig_dbh)) {
      if (length(sig_dbh) != 1 && length(sig_dbh) != length(dbh)) {
        stop("Length of 'sig_dbh' must be either 1 or match the length of 'dbh'")
      }
      results <- data.frame(spcode = spcode, dbh = dbh, biomass = NA, error = NA)
    } else {
      results <- data.frame(spcode = spcode, dbh = dbh, biomass = NA)
    }
  } else {
    results <- c()
  }

  if (any(!is.numeric(dbh) | dbh < 0)) stop("All values of 'dbh' must be numeric and non-negative")

  # Loop over each entry in spcode
  for (i in 1:length(spcode)) {

    diam <- dbh[i]
    # Set sigma based on whether sig_dbh is a vector or single value
    sigma <- if (!anyNA(sig_dbh)) sig_dbh[i] else sig_dbh

    # Warning for dbh < 7
    if (diam < 7) warning("Equation is only specified for dbh >= 7")

    # Find the record in the data for the species code
    rec <- crown_biomasdf[crown_biomasdf$Code == spcode[i], ]
    if (nrow(rec) == 0) {
      spcode_match <- lookup_df$Crown[lookup_df$short == spcode[i]]
      rec <- crown_biomasdf[crown_biomasdf$Code == spcode_match, ]
      if (nrow(rec) == 0) warning(paste("The species code", spcode[i], "is not found"))
    }

    # Calculate biomass and error
    if (diam <= 50) {
      crown_biomass <- rec$b1 * diam^rec$p
      results$biomass[i] <- crown_biomass

      # Error calculation for diam <= 50
      if (!is.na(sigma)) {
        if (!is.numeric(sigma) || sigma < 0)
          stop("Argument 'sig_dbh' must be numeric and non-negative")

        results$error[i] <- sqrt((rec$b1 * conf * diam^rec$p)^2 +
            (rec$b1 * rec$p * diam^(rec$p - 1) * sigma)^2)
      }

    } else {
      crown_biomass <- rec$A + rec$b2 * diam
      results$biomass[i] <- crown_biomass

      # Error calculation for diam > 50
      if (!is.na(sigma)) {
        if (!is.numeric(sigma) || sigma < 0)
          stop("Argument 'sig_dbh' must be numeric and non-negative")

        results$error[i] <- sqrt((rec$b2 * conf * diam)^2 + (rec$b2 * sigma)^2
        )
      }
    }
  }

  return(results)
}

############# FC Root Biomass (WCC Eq 8 & 9) ################
#'
#' @title Forestry commission root biomass estimates
#' @description Function to calculate the root biomass depending on species and dbh
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh diameter at breast height (1.3 m) in centimetres
#' @param spcode species code
#' @param sig_dbh sigma for DBH (optional)
#' @param conf one minus percentage confidence or relative error for
#' coefficients of the equation
#' @returns biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.3.
#' @examples
#' rootbiomass(spcode = 'RBRAR', dbh = 50)
#' rootbiomass(spcode = 'RBRAR', dbh = 50, sig_dbh = 10)
#' rootbiomass(spcode = c('RBRAR','RBRAR'), dbh = c(50, 70))
#' @export
#'
rootbiomass <- function(spcode, dbh, sig_dbh = NA, conf = 0.025) {

  # Initialise
  if (length(spcode) != length(dbh)) {
    stop("Length of 'spcode' and 'dbh' must be the same")
  }
  if (any(!is.numeric(dbh) | dbh < 0)) {
    stop("All values of 'dbh' must be numeric and non-negative")
  }
  if (length(dbh) > 1){
    if (!anyNA(sig_dbh)) {
      if (length(sig_dbh) != 1 && length(sig_dbh) != length(dbh)) {
        stop("Length of 'sig_dbh' must be either 1 or match the length of 'dbh'")
      }
      results <- data.frame(spcode = spcode, dbh = dbh, rootbiomass = NA, error = NA)
    } else {
      results <- data.frame(spcode = spcode, dbh = dbh, rootbiomass = NA)
    }
  } else {
    results <- c()
  }

  #utils::data(root_biomassdf, envir = environment())

  # Iterate through each species code and dbh value
  for (i in seq_along(spcode)) {
    diam <- dbh[i]
    sigma <- if (!anyNA(sig_dbh) && length(sig_dbh) > 1) sig_dbh[i] else sig_dbh

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

      # Calculate error if sig_dbh is provided
      if (!anyNA(sigma)) {
        if (!is.numeric(sigma) || sigma < 0) {
          stop("Argument 'sig_dbh' must be numeric and non-negative")
        }
        sig_root <- root_biomass * sqrt(
          (rec$b1 * conf / rec$b1)^2 + (2.5 * sigma / diam)^2
        )

      }
    } else {
      root_biomass <- rec$a + rec$b2 * diam

      # Calculate error if sig_dbh is provided
      if (!anyNA(sigma)) {
        if (!is.numeric(sigma) || sigma < 0) {
          stop("Argument 'sig_dbh' must be numeric and non-negative")
        }
        sig_root <- sqrt((rec$a*conf)^2 + error_product(rec$b2, rec$b2*conf, diam, sigma))
      }
    }

    # Store results in the results data frame
    results$rootbiomass[i] <- root_biomass
    if (!anyNA(sigma)) {
      results$error[i] <- ifelse(exists("sig_root"), sig_root, NA)
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
#' "Matthews1": Simplest with the carbon volatile fraction, CVF = 50% (Matthews, 1993)
#' "Matthews2": CVF based on type (broadleaf or conifer)
#' "IPCC1": CVF = 47.7% (IPCC, 2006)
#' "IPCC2": Lookup CVF by type and biome
#' "Thomas1": CVF = 48.3% and 95% CI of 0.3% (Thomas and Martin, 2012)
#' "Thomas2": Lookup by type and biome
#' @param type broadleaf or conifer. Only required for method = 'Matthews2',
#' 'IPCC2' or 'Thomas'
#' @param biome tropical, subtropical, mediterranean, temperate or boreal.
#' Only needed for 'IPCC2' and 'Thomas' methods.
#' @param sig_biomass biomass uncertainty (optional)
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
#'  biomass2c(4, method="IPCC2", c("conifer"), "temperate")
#'  biomass2c(c(3,4), method="IPCC2", c("conifer","conifer"), "temperate", c(0.7,1))
#'  @importFrom utils globalVariables
#'  @export
#'
biomass2c <- function(biomass, method, type = NULL, biome = NULL, sig_biomass = NA) {
  # Check arguments
  if (any(!is.numeric(biomass) | biomass < 0)) {
    warning("All biomass values must be numeric and positive")
    }

  valid_methods <- c("Matthews1", "Matthews2", "IPCC1", "IPCC2", "Thomas")
  if (!(method %in% valid_methods)) stop("Invalid method. Choose from:", paste(valid_methods,collapse = ", "))

  valid_types <- c("broadleaf", "conifer")
  if (method %in% c("Matthews2", "IPCC2", "Thomas") && !all(type %in% valid_types)) stop("Invalid type.")

  valid_biomes <- c("tropical", "subtropical", "mediterranean", "temperate", "boreal")
  if (method %in% c("IPCC2", "Thomas") && !all(biome %in% valid_biomes)) stop("Invalid biome.")

  n <- length(biomass)
  CVF <- conf <- rep(NA, n)

  #utils::globalVariables(c("CVF_df"))

  # Retrieve CVF and conf values using the lookup table
  for (i in seq_len(n)) {
    filter_conditions <- !is.na(CVF_df$method) & CVF_df$method == method &
      (is.na(CVF_df$type) | CVF_df$type == type[i]) &
      (is.na(CVF_df$biome) | CVF_df$biome == biome)
    matching_row <- subset(CVF_df, filter_conditions)

    if (nrow(matching_row) == 1) {
      CVF[i] <- matching_row$CVF
      conf[i] <- matching_row$conf
    } else {
      stop("Could not find a matching CVF value for the provided parameters.")
    }
  }

  # Calculate carbon values
  AGC <- biomass * CVF / 100

  # Calculate uncertainty if sig_biomass is provided
  if (!anyNA(sig_biomass)) {
    if (length(sig_biomass) != n) stop("Length of sig_biomass must match length of biomass")
    if (any(!is.numeric(sig_biomass) | sig_biomass < 0)) stop("sig_biomass must be numeric and positive")

    error <- sqrt((sig_biomass / biomass)^2 + (conf / 100)^2) * AGC
    return(list(AGC = AGC, sig_AGC = error))
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
#' @param sig_h estimated measurement error of height
#' @param conf relative error of coefficients in percentage
#' @param type 'conifer' or 'broadleaf' tree
#' @returns  carbon in tonnes
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018) Table 6.1.2.
#' @importFrom utils data head tail
#' @examples
#' con_sap_seedling2C(10)
#' con_sap_seedling2C(50)
#' @export
#'
sap_seedling2C <- function(heightincm, type, sig_h = NA, conf = 0.05){
  #utils::data(seedlings_conifer, envir = environment())

  if(heightincm < 0 || anyNA(heightincm))stop("heightincm must be numeric and postive")
  if(heightincm > 1000)warning("not defined for seedlings over 10 m")
  if(type < 0 || anyNA(heightincm))stop("heightincm must be numeric and postive")

  b <- utils::tail(seedlings_conifer[seedlings_conifer$height.cm <= heightincm,],1)
  t <- utils::head(seedlings_conifer[seedlings_conifer$height.cm >= heightincm,],1)
  rt <- (t$height.cm - heightincm)/(t$height.cm-b$height.cm)
  if ((nrow(b) == 0) |  (nrow (t) == 0)){
    NULL
  } else if (is.nan(rt)){
    return(t$Carbon.kg)
  } else {
    carbon <- b$Carbon.kg + ((t$Carbon.kg - b$Carbon.kg) * rt)

    if(!is.na(sig_h)){
      # Partial derivatives
      d_t <- (b$height.cm - heightincm) / (t$height.cm - b$height.cm)^2
      d_b <- -(t$height.cm - heightincm) / (t$height.cm - b$height.cm)^2

      # Propagate errors
      sigma_rt <- sqrt((d_t * sig_h)^2 + (d_b * sig_h)^2)

      sigma_carbon <- sqrt(((1 - rt) * conf * b$Carbon.kg)^2 +
                             (rt * conf * t$Carbon.kg)^2 +
                             (t$Carbon.kg - b$Carbon.kg * sigma_rt)^2)
      return(c(carbon = carbon, error = sigma_carbon))
    } else {
      return(carbon)
    }
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
#' @param conf relative error or 1-percentage confidence interval on estimates
#' @returns carbon in tonnes
#' @note just uses simple linear relationship to get between measures
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018)
#' @importFrom utils data head tail
#' @examples
#' sap_seedling2C(50)
#' sap_seedling2C(heightincm = 900, type = 'broadleaf', sig_height = 5)
#' @export
#'
sap_seedling2C <- function(heightincm, type, sig_height = NA, conf = 0.05) {
  if(!is.numeric(heightincm) || heightincm < 0)stop("
  Argument 'heightincm' must be numeric and non-negative")
  if(heightincm > 1000)warning("Maximum for 'heightincm' is 1000cm")
  if(heightincm < 1)   warning("Minimum for 'heightincm' is 1cm")

  if(type == "broadleaf"){
    data <- seedlings_broad
  } else if(type == "conifer"){
    data <- seedlings_conifer
  } else {
    stop('define type as broadleaf or conifer.')
  }

  # Find the bounding rows for interpolation
  lower_bound <- utils::tail(data[data$height.cm <= heightincm, ], 1)
  upper_bound <- utils::head(data[data$height.cm >= heightincm, ], 1)

  # If heightincm matches exactly a row in data
  if (lower_bound$height.cm == upper_bound$height.cm) {
    carbon_value <- lower_bound$Carbon.kg
    if(is.na(sig_height)){
      return(carbon_value)
    } else {
      carbon_sd <- conf * carbon_value
      return(list(carbon = carbon_value, sd = carbon_sd))
    }
  }

  # Perform linear interpolation if within bounds
  if (nrow(lower_bound) == 1 && nrow(upper_bound) == 1) {
    height_diff <- upper_bound$height.cm - lower_bound$height.cm
    carbon_diff <- upper_bound$Carbon.kg - lower_bound$Carbon.kg
    proportion <- (heightincm - lower_bound$height.cm) / height_diff
    interpolated_carbon <- lower_bound$Carbon.kg + proportion * carbon_diff


   if(!is.na(sig_height)) {
    interpolated_sd <- sqrt((conf * interpolated_carbon)^2 +
                       ((carbon_diff / height_diff) * sig_height)^2)

    return(list(carbon = interpolated_carbon, sd = interpolated_sd))
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
  if (length(type) != length(name)) stop("'type' and 'name' must have the same length.")
  if (!is.character(returnv) || !returnv %in% c("short", "single", "stand", "root", "all")) {
    stop("'returnv' must be a character, either 'short', 'single', 'stand', 'root', or 'all'.")
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

    # Fallback to type classification
    if (!is.na(search_type) && search_type != "") {
      rec <- lookup_df[lookup_df$General.type == search_type, ]
      if (nrow(rec) == 1) return(cbind(rec, matchtype = search_type))
    }

    # Fallback to mixed type when classification is NA or no match found
    rec <- lookup_df[lookup_df$General.type == "mixed", ]
    if (nrow(rec) > 0) return(cbind(rec[1, ], matchtype = "Mixed"))

    # If no match found, return NA row
    return(data.frame(
      spname = NA,
      spcode = NA,
      General.genus = NA,
      General.type = NA,
      short = NA,
      single = NA,
      stand = NA,
      Root = NA,
      matchtype = NA,
      stringsAsFactors = FALSE
    ))
  }

  # Apply helper function over all inputs
  results <- mapply(
    match_species,
    search_name = stringr::str_trim(tolower(name)),
    search_type = ifelse(is.na(type), "mixed", type), # Replace NA with "mixed"
    SIMPLIFY = FALSE
  )

  # Combine results into a dataframe
  result_df <- do.call(rbind, results)

  # Handle return values
  if (returnv != "all") {
    result_df <- data.frame(
      spname = name,
      spcode = switch(returnv,
                      short = result_df$short,
                      single = result_df$single,
                      stand = result_df$stand,
                      root = result_df$Root),
      matchtype = result_df$matchtype,
      stringsAsFactors = FALSE
    )
  }

  return(result_df)
}

############# FC Above Ground Carbon ################
#'
#' @title Calculate above ground carbon
#' @description  Function that inputs tree species code, dbh, height and method
#' for converting biomass to carbon, and returns the carbon estimate
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param spcode species code (single)
#' @param dbh diameter at breast height in centimetres
#' @param height in metres
#' @param type conifer or broadleaf tree
#' @param method method of converting biomass to carbon. See biomass2c function
#' @param biome tropical, Subtropical, Mediterranean,Temperate, Boreal or all
#' @param returnv To return either 'AGC' (default) or 'All'
#' @returns either Above ground carbon, AGC in tonnes, or a list of tariff
#' number, merchantable volume (metres cubed), stem volume (metres cubed),
#' stem biomass (tonnes), stem carbon (tonnes), canopy carbon (tonnes) and
#' root carbon (tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @export
#'
fc_agc <- function(spcode, dbh, height, type, method = "Matthews1", biome,
                   returnv = "AGC"){

  # Check arguments
  if(length(spcode) != length(dbh) || length(spcode) != length(height) ||
     length(height) != length(dbh))stop("input lengths must be the same")
  if(!is.character(spcode))stop("spcode must be a character")
  if(!is.numeric(dbh) || any(dbh<=0))stop("dbh must be numeric & positive")
  if(!is.numeric(height)||any(height<=0))stop("height must be numeric & positive")

  if (!(method %in% c("Matthews1", "Matthews2", "IPCC1", "IPCC2", "Thomas"))) {
    stop("Invalid method. Choose from: 'Matthews1', 'Matthews2', 'IPCC1',
         'IPCC2', 'Thomas'. See R helpfile.")
  }
  if ((method %in% c("IPCC2", "Thomas")) && !missing(biome) &&
      !(biome %in% c("tropical", "subtropical", "mediterranean",
                     "temperate", "boreal"))) {
    stop("Invalid biome. Choose from: 'tropical', 'subtropical','mediterranean',
         'temperate', 'boreal'")
  }
  n = length(spcode)
  #if(n != length(dbh) || n != length(height) || length(dbh) != length(height)) {
  #  stop("spcode, dbh, and height must be of the same length")}

  # Create results table
  r <- data.frame(spcode=NA, dbh=NA, height=NA, tariff=NA, mercvol=NA, stemvol=NA,
                  stembiomass=NA, crownbiomass=NA, rootbiomass=NA, AGC=NA, stringsAsFactors=FALSE)
  r <- r[1:n,]
  #utils::data(lookup_df, envir = environment())
  #    data("lookup_df", package = "WoodlandCarbonCode")

  # Loop over all trees
  for (i in 1:n) {
    if(dbh[i]<=0)warning("dbh must be numeric & positive for index:", i)

#    # Lookup species data from code
    rec <- lookup_df[lookup_df$short == spcode[i], ]
    tarifflokupcode <- rec$single
    type <- rec$type

    # Check if the species code was found
    if (nrow(rec) == 0) {
      warning("The spcode value was not found: ", spcode[i])
      next
    }

    # Get tariff number depending on broadleaf or conifer
    if (type[i] == "broadleaf") {
      broad <- broadleaf_tariff(spcode[i], height[i], dbh[i])
      if (length(broad) == 0) {
        stop("Error in broadleaf_tariff at index: ", i)
      }
      r$tariff[i] <- broad
    } else if (type[i] == "conifer") {
      conifer <- conifer_tariff(spcode[i], height[i], dbh[i])
      if (length(conifer) == 0) {
        stop("Error in conifer_tariff at index: ", i)
      }
      r$tariff[i] <- conifer
    }

    # Calculate volumes and biomass
    r$mercvol[i] <- merchtreevol(dbh[i], r$tariff[i])      # Merchantable tree volume
    r$stemvol[i] <- treevol(r$mercvol[i], dbh[i])          # Stem volume
    r$stembiomass[i] <- woodbiomass(r$stemvol[i], rec$NSG) # Stem Biomass
    r$crownbiomass[i] <- crownbiomass(rec$Crown, dbh[i])   # Crown Biomass
    AGB <- r$stembiomass[i] + r$crownbiomass[i]               # Above ground Biomass
    r$AGC[i] <- biomass2c(AGB,method=method,type,biome=biome) # Above ground Carbon

    if(returnv != "AGC"){
      r$rootbiomass <- rootbiomass(rec$Root, dbh[i])       # Root Biomass
      r$spcode[i] <- spcode[i]
      r$dbh[i]    <- dbh[i]
      r$height[i] <- height[i]
    }
  }
  if (returnv == "AGC") {
    return(r$AGC)
  } else {
    return(r)
  }
}

#'
#' @title Calculate above ground carbon
#' @description  Function that inputs tree species code, dbh, height and method
#' for converting biomass to carbon, and returns the carbon estimate
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param spcode species code (single)
#' @param dbh diameter at breast height in centimetres
#' @param height in metres
#' @param method method of converting biomass to carbon. Either 'Thomas' or 'IPCC2' as these specify the error associated with the carbon volatile fraction
#' @param biome temperate, boreal, mediterranean, tropical, subtropical or all
#' @param returnv To return either 'AGC' (default) or 'All'
#' @param sig_dbh error in sampling of dbh, single value
#' @param sig_h error in sampling of height, single value
#' @param conf_a 1 - the percentage confidence interval for coefficients or
#' the relative error of coefficients (default = 5%)
#' @returns either Above ground carbon, AGC in tonnes, or a list of tariff
#' number, merchantable volume (metres cubed), stem volume (metres cubed),
#' stem biomass (tonnes), stem carbon (tonnes), canopy carbon (tonnes) and
#' root carbon (tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' fc_agc_error(spcode='OK', dbh=74, height=24, returnv ="All", sig_dbh=10, sig_h=1)
#' fc_agc_error(spcode='OK', dbh=74, height=24, method="IPCC2", biome="temperate", returnv ="AGC", sig_dbh=10, sig_h=1)
#' @export
#'
#'
fc_agc_error <- function(spcode, dbh, height, method = "IPCC2", biome = "temperate",
                   returnv = "All", sig_dbh = 10, sig_h = 1, conf = 0.025, sig_nsg = 0.09413391){

  # Check arguments
  if(length(spcode) != length(dbh) || length(spcode) != length(height) ||
     length(height) != length(dbh))stop("input lengths must be the same")
  if(!is.character(spcode))stop("spcode must be a character")
#  if(!is.numeric(height)||any(height<=0))stop("height must be numeric & positive") not sure if i need this if checks are within functions
  if(!is.numeric(sig_dbh)||any(sig_dbh<=0))stop("sig_dbh must be numeric & positive")
  if(!is.numeric(sig_h)||any(sig_h<0)){
    stop("sig_h must be numeric & positive")
    }
  if (!(method %in% c("IPCC2", "Thomas"))) {
    stop("Invalid method. Choose from: 'IPCC2', 'Thomas'")
  }
  if (!missing(biome) && !(biome %in% c("tropical", "subtropical", "mediterranean", "temperate", "boreal"))) {
    stop("Invalid biome. Choose from: 'tropical', 'subtropical','mediterranean',
         'temperate', 'boreal'")
  }
  n = length(spcode)
  #if(n != length(dbh) || n != length(height) || length(dbh) != length(height)) {
  #  stop("spcode, dbh, and height must be of the same length")}

  # Create results table
  if(returnv == 'All'){
    r <- data.frame(spcode=NA, spname=NA, dbh=NA, height=NA, NSG=NA, tariff=NA, sig_tariff=NA, mercvol_m.3=NA, sig_mercvol=NA, stemvol_m.3=NA, sig_stemvol=NA,
                    stembiomass_t=NA, sig_stembiomass=NA, crownbiomass_t=NA, sig_crownbiomass=NA, rootbiomass_t=NA, sig_rootbiomass=NA, AGC_t=NA, sig_AGC=NA, stringsAsFactors=FALSE)
  } else {
    r <- data.frame(AGC_t=NA, sig_AGC=NA, stringsAsFactors=FALSE)
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
    rec <- lookup_df[lookup_df$short == spcode[i], ]
    tarifflokupcode <- rec$single
    type <- rec$type

    # Check if the species code was found
    if (nrow(rec) == 0) {
      warning("The spcode value was not found: ", spcode[i], "Index:", i)
      next
    }

    # If height less than 6.5 use sapling model
    if(height < 6.5){
      carbon <- sap_seedling2C(heightincm = height*100, type, sig_h = sig_h, conf = conf)
      r$AGC_t[i] <- carbon$carbon
      r$sig_AGC[i] <- carbon$sd
    } else {
      # Get tariff number depending on broadleaf or conifer
      tariff <- tariffs(spcode[i], height[i], dbh = dbh[i], sig_h = sig_h, sig_dbh = sig_dbh, conf_a = conf)

      if (length(tariff) == 0) {
        warning("Error in", type, "_tariff function at index: ", i)
      }

      # Calculate volumes and biomass
      mercvol <- merchtreevol(dbh[i], tariff$tariff, sig_dbh, as.numeric(tariff[2]), conf) # Merchantable tree volume
      stemvol <- treevol(mercvol$volume, dbh = dbh[i], mercvol$error, conf)         # Stem volume
      woodbio <- woodbiomass(stemvol$stemvolume, rec$NSG, stemvol$error, sig_nsg)   # Stem Biomass
      crownbio <- crownbiomass(rec$Crown, dbh[i], sig_dbh, conf)    # Crown Biomass
      AGB <- woodbio$woodbiomass + crownbio$biomass             # Above ground Biomass
      sig_AGB <- sqrt(woodbio$error^2 + crownbio$error^2)
      AGC <- biomass2c(AGB, method=method, type, biome=biome, sig_biomass = sig_AGB) # Above ground Carbon
      r$AGC_t[i] <- AGC$AGC
      r$sig_AGC[i] <- AGC$sig_AGC
    }

    if(returnv == "All"){
      r$spcode[i] <- spcode[i]
      r$spname[i] <- rec$latin_name
      r$dbh[i]    <- dbh[i]
      r$height[i] <- height[i]
      r$NSG[i] <- rec$NSG

      if(height >= 6.5){
        r$tariff[i] <- tariff$tariff
        r$sig_tariff[i] <- tariff$error
        r$mercvol_m.3[i] <- mercvol$volume
        r$sig_mercvol[i] <- mercvol$error
        r$stemvol_m.3[i] <- stemvol$stemvolume
        r$sig_stemvol[i] <- stemvol$error
        r$stembiomass_t[i] <- woodbio$woodbiomass
        r$sig_stembiomass[i] <- woodbio$error
        r$crownbiomass_t[i] <- crownbio$biomass
        r$sig_crownbiomass[i] <- crownbio$error

        # Root Biomass
        rootbio <- rootbiomass(rec$Root, dbh[i], sig_dbh)
        r$rootbiomass_t[i] <- rootbio$rootbiomass
        r$sig_rootbiomass[i] <- rootbio$error
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
#' @param spcode species code
#' @param dbh diameter at breast height
#' @returns  biomass in kg
#' @references Bunce, R. G. H. "Biomass and Production of Trees in a Mixed
#' Deciduous Woodland: I. Girth and Height as Parameters for the Estimation of
#' Tree Dry Weight" (1968)
#'
#' @importFrom utils data
#' @examples
#' bunce("OK", 24)
#' bunce(c("OK","OK"), c(24,20))
#' @export
#'
bunce <- function(spcode, dbh) {
  # Ensure all DBH values are numeric and non-negative
  if (any(!is.numeric(dbh) | dbh < 0))
    stop("Argument 'dbh' must be numeric and non-negative")

  # Ensure spcode and dbh are of the same length or allow recycling
  if (length(spcode) != length(dbh)) {
    if (length(spcode) == 1) {
      spcode <- rep(spcode, length(dbh))
    } else if (length(dbh) == 1) {
      dbh <- rep(dbh, length(spcode))
    } else {
      stop("'spcode' and 'dbh' must be of the same length or one of them must have length 1")
    }
  }

  calculate_biomass <- function(spcodes, dbh_value) {
    # Extract coefficients for the given species code
    coeffs <- buncedf[buncedf$spcode == spcodes,]
    used_spcode <- spcodes  # Default to the input spcode

    # Use combined coefficients if species code is not found
    if (nrow(coeffs) == 0) {
      # warning(spcodes, " not found in data (buncedf), combined coefficients will be used")
      coeffs <- buncedf[buncedf$spcode == "XB",]
      used_spcode <- "XB"  # Indicate fallback code
    }

    # Calculate biomass
    biomass <- coeffs$a + coeffs$b * log(pi * dbh_value)
    return(list(biomass = exp(biomass), spcode = used_spcode))
  }

  # Apply the calculation over all inputs
  results <- mapply(calculate_biomass, spcode, dbh, SIMPLIFY = FALSE)

  # Separate the biomass and species code into a named list
  biomass <- sapply(results, function(x) x$biomass)
  spcodes_used <- sapply(results, function(x) x$spcode)

  # Return biomass with species codes as names
  names(biomass) <- spcodes_used
  return(biomass)
}

############# BIOMASS Package carbon calculation ==========================
#'
#' @title Estimate Tree Carbon using Biomass package functions
#' @description Using the Biomass package to calculate carbon
#' @param df dataframe containing columns; Genus, Species, DBH (in cm),
#' Height (in m). Height is optional, but df must include this column.
#' @param coords either a vector of coordinates of the site or a matrix of
#' coordinates for each tree of longitude and latitude
#' @param region of the World. See ?getWoodDensity for the list of regions
#' @param output.all if TRUE outputs all data from processing, else just outputs carbon figures
#' @returns  your dataframe back with added columns of carbon estimates. If
#' output.all = FALSE, then returns columns 'Genus_corrected','Species_corrected',
#' 'Family','Latitude','Longitude','DBH','AGB_Biomass_kg'. If output.all = TRUE
#' then additionally returns columns 'Wood_Density', 'Wood_Density_sd',
#' 'Height_est', 'RSE' (Residual Standard Error of the model), 'Height_1' (which
#' is inputed height filled in with Height estimate where missing)
#' @importFrom utils install.packages
#' @export
#'
biomass <- function(df, coords, region = "World", output.all = TRUE){

  if(nchar(system.file(package='BIOMASS')) == 0 ){
    utils::install.packages("BIOMASS",dependencies = TRUE)}

  if (!is.data.frame(df)) {stop("'df' must be a data frame.")}

  if (!all(c("Genus", "Species", "DBH") %in% names(df))) {
    stop("'df' must contain columns 'Genus', 'Species', and 'DBH'.")}

  if (!is.numeric(coords) || length(coords) != 2) {
    stop("'coords' must be a vector of latitude and longitude.")
  }

  if (!is.character(region) || length(region) != 1) {
    stop("'region' must be a single character string.")
  }

  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }


  # Correct name
  correct <- BIOMASS::correctTaxo(genus = df$Genus, species = df$Species)
  df$Genus_corrected   <- correct$genusCorrected
  df$Species_corrected <- correct$speciesCorrected
  df$Modified          <- correct$nameModified

  # Get Wood Density
  # data("wdData") # Global wd database
  wd <- BIOMASS::getWoodDensity(df$Genus_corrected, df$Species_corrected, region=region)
  df$Wood_Density    <- wd$meanWD
  df$Wood_Density_sd <- wd$sdWD
  df$Family          <- wd$family

  # Get height estimates using Chave E
  h <- BIOMASS::retrieveH(D = as.numeric(df$DBH), coord = coords)
  df$RSE        <- h$RSE
  df$Height_est <- h[["H"]]

  # Combine height data with height estimates
  df$Height_1 <- NA
  h.data <- df[!is.na(df$Height),]
  h.est  <- df[is.na(df$Height),]
  h.data$Height_1  <- h.data$Height
  h.est$Height_1   <- h.est$Height_est

  df <- rbind(h.data, h.est)

  # Calculate Carbon using Biomass package
  df$AGB_Biomass_kg <- BIOMASS::computeAGB(D=as.numeric(df$DBH),
                                  WD=as.numeric(df$Wood_Density),
                                  H=df$Height_1)*1000

  if(output.all == FALSE){
    df <- df[c('Genus_corrected','Species_corrected','Family','Latitude',
               'Longitude','DBH','AGB_Biomass_kg')]
    }

  return(df)
}

############# allodb Package carbon calculation ==========================
# ==== Inputs:
# df: data frame containing columns; DBH (in cm), Genus_corrected, Species_corrected (from output of biomass function)
# coords: either the coordinates of the site, a vector of longitude and latitude
#         or a matrix of coordinates for each tree
# ==== Optional Inputs:
# output.all: if TRUE outputs the coefficients of the model, a*DBH^b+e {e ~ N(0,sigma^2}
#' @title Estimate Tree Carbon using Biomass package functions
#' @description Using the Biomass package to calculate carbon
#' @param df dataframe containing columns; Genus, Species, DBH (in cm),
#' Height (in m). Height is optional, but df must include this column.
#' @param new.eqtable a subset or extension of the allometric equation table. Create with allodb::new_equations
#' @param coords either a vector of coordinates of the site or a matrix of
#' coordinates for each tree of longitude and latitude
#' @param output.all if TRUE outputs all data from processing, else just outputs carbon figures
#' @returns  your dataframe back with added columns of carbon estimates. If
#' output.all = FALSE, then returns columns 'Genus_corrected','Species_corrected',
#' 'Family','Latitude','Longitude','DBH','AGB_Biomass_kg'. If output.all = TRUE
#' then additionally returns columns 'Wood_Density', 'Wood_Density_sd',
#' 'Height_est', 'RSE' (Residual Standard Error of the model), 'Height_1' (which
#' is inputed height filled in with Height estimate where missing)
#' @export
#'
allodb <- function(df, coords, output.all = TRUE, new.eqtable = NULL){

  # Call allodb package
  if(nchar(system.file(package='allodb')) == 0 ){
    remotes::install_github("ropensci/allodb")}

  # Biomass for all data. By default all equations will be used
  df$AGB_allodb_kg <- allodb::get_biomass(dbh = as.numeric(df$DBH),
                                  genus = df$Genus_corrected,
                                  species = df$Species_corrected,
                                  coords = coords,
                                  new_eqtable = new.eqtable)

  if(output.all == TRUE){
    df$allodb_a <- df$allodb_b <- df$allodb_sigma <- NA

    # Get parameters and sigma: AGB= a*DBH^b+e {e ~ N(0,sigma^2}
    params <- allodb::est_params(genus = df$Genus_corrected,
                         species = df$Species_corrected,
                         coords = coords)
    # Create a name column in params and df
    params$name <- paste(params$genus, params$species)
    df$Name <- paste(df$Genus_corrected, df$Species_corrected)
    df$Name <- as.factor(df$Name)
    # Create an empty df with same columns
    df.p <- df[-c(1:nrow(df)),]
    for(i in 1:length(levels(df$Name))){
      name <- levels(df$Name)[i]
      trees <- df[df$Name == name,]
      trees$allodb_a <- params$a[params$name == name]
      trees$allodb_b <- params$b[params$name == name]
      trees$allodb_sigma <-  params$sigma[params$name == name]
      df.p <- rbind(df.p, trees)
    }
    df <- df.p
  }

  return(df)
}



