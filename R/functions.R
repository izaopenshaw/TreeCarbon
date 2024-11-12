############## Functions file for Woodland Carbon Code ########################
# TODO:
# error for ctoco2e ?
# search not found and check that intermediate species are found in lookup_df
# check biomass2c that error is confidence percentage by checking references

############ Tariff number from volume and tree basal area (FC Eq 1) ############
#'
#' @title Tariff number from volume and basal area
#' @description Using the sample tree’s basal area and volume to calculate the
#' tariff number. Basal area is calculated by ba = (pi * dbh^2)/40000.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param vol tree volume in metres cubed
#' @param dbh diameter at breast height in centimetres
#' @param sig_vol sigma for tree volume (optional)
#' @param sig_dbh sigma for diameter at breast height (optional)
#' @returns  Tariff number or if sigma for inputs are provided, then will return
#' a list of tariff number and sigma for tariff
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). (Equation 1)
#' @examples
#' tariff_vol_area(vol=50, dbh=24, sig_vol = 10, sig_dbh = 1)
#' @export
#'
tariff_vol_area <- function(vol, dbh, sig_vol = NA, sig_dbh = NA){
  if (!is.numeric(vol) || !is.numeric(dbh)) { #if (!is.numeric(vol) || any(vol < 0) || !is.numeric(dbh) || any(dbh < 0)) {
    stop("vol and dbh must be non-negative numeric values")  #  stop("vol and dbh must be non-negative numeric values")
  }
  ba <- (pi * dbh^2)/40000                # tree basal area in m^2
  a1 <- (vol - 0.005002986)/(ba - 0.003848451)
  tariff <- (3.174106384 * a1) + 0.138763302

  if(!anyNA(sig_vol) || !anyNA(sig_dbh)){
    if (any(sig_vol < 0) || !is.numeric(sig_vol)) {stop("sigm_vol must be non-negative numeric")}
    if (any(sig_dbh < 0) || !is.numeric(sig_dbh)) {stop("sig_dbh must be non-negative numeric")}

    sig_ba <- (pi * 2 * dbh / 40000) * sig_dbh
    partial_a_v <- 1 / (ba - 0.003848451)
    partial_a_ba <- -(vol - 0.005002986) / (ba - 0.003848451)^2
    sig_a <- sqrt((partial_a_v * sig_vol)^2 + (partial_a_ba * sig_ba)^2)
    sig_t <- 3.174106384 * sig_a

    return(c(tariff, sig_t))
  } else {
    return(tariff)
  }
}

############# FC conifer tree tariff number (FC Eq 3) ############################
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
#' @returns  tariff number
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' conifer_tariff("SP", 74, 24)
#' conifer_tariff("SP", 74, 24, 5, 5)
#' @export
#'
conifer_tariff <- function(spcode, height, dbh, sig_h = NA, sig_dbh = NA) {
  if (!is.numeric(height) || !is.numeric(dbh) || any(height < 0) || any(dbh < 0)) {
    stop("height and dbh must be non-negative numeric values")
  }
  if (!(length(spcode) == length(height) && length(height) == length(dbh))) {
    stop("All input vectors (spcode, height, dbh, sig_h, sig_dbh) must have the same length.")
  }

  #utils::data(tariff_coniferdf, envir = environment())

  n <- length(spcode)

  tariffs <- numeric(n)
  sigmas <- numeric(n)

  return_error <- !anyNA(sig_h) || !anyNA(sig_dbh)

  if(return_error){
    if (any(sig_h < 0) || !is.numeric(sig_h)) {stop("sig_h must be non-negative numeric")}
    if (any(sig_dbh < 0) || !is.numeric(sig_dbh)) {stop("sig_dbh must be non-negative numeric")}
  }

  for(i in 1:n){
    rec <- tariff_coniferdf[tariff_coniferdf$abbreviation == spcode[i], ]

    if(nrow(rec) == 0){
    spcode <- lookup_df$single[lookup_df$short == spcode]
    rec <- tariff_coniferdf[tariff_coniferdf$abbreviation == spcode[i], ]

    if(nrow(rec)==0){
      warning(spcide[i], "spcode not found in data(tariff_coniferdf) or data(lookup_df$short)")
    }
  }

    tariffs[i] <- rec$a1 + (rec$a2 * height[i]) + (rec$a3 * dbh[i])

    if(return_error){
      sigmas[i] <- sqrt(1.857442^2 + error_product(rec$a2, 0.2465285, height[i], sig_h)
                      + error_product(rec$a3, 0.1834052, dbh[i], sig_dbh))
      }
  }

  if(return_error){
      return(data.frame(tariff = tariffs, error = sigmas))
    } else {
      return(tariffs)
    }
}


############# FC broadleaf tree tariff number (FC Eq 2) ##########################
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
#' @param sig_a sigma for coefficients of formula (default = 0)
#' @returns  tariff number and error if sigma of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Method B, Equation 2.
#' @importFrom utils data
#' @examples broadleaf_tariff(spcode = 'OK', height = 25, dbh = 75)
#' broadleaf_tariff(spcode = "OK", height = 25, dbh = 1.5, sig_dbh = 10, sig_h = 1)
#' broadleaf_tariff(spcode = "OK", height = 24, dbh = 1.5, sig_dbh = 10, sig_h = 1)
#' @export
#'
broadleaf_tariff <- function(spcode, height, dbh, sig_dbh = NA, sig_h = NA, sig_a = 0) {
  if(!is.numeric(dbh) || any(dbh<=0))stop("dbh must be numeric and positive")
  if(!is.numeric(height) || any(height<=0))stop("height must be numeric and positive")

  #utils::data(tariff_broaddf, envir = environment())
  tb <- tariff_broaddf[tariff_broaddf$abbreviation == spcode, ]

  if(nrow(tb) == 0){
    spcode <- lookup_df$single[lookup_df$short == spcode]
    tb <- tariff_broaddf[tariff_broaddf$abbreviation == spcode, ]

    if(nrow(tb)==0){
      warning("spcode not found in data(tariff_broaddf) or data(lookup_df$short)")
    }
  }

  tariff <- tb$a1 + (tb$a2 * height) + (tb$a3 * dbh) + (tb$a4 * dbh * height)

  if(!is.na(sig_dbh) && !is.na(sig_h)){
    if(!is.numeric(sig_dbh) || any(sig_dbh<0))stop("sig_dbh must be numeric and positive")
    if(!is.numeric(sig_h) || any(sig_h<0))stop("sig_h must be numeric and positive")

    error <- sqrt(2.085826^2 +
                error_product(tb$a3, 0.05184218, dbh, sig_dbh) +
                error_product(tb$a2, 0.3908223, height, sig_h) +
                error_product(tb$a4, 0.01108647, dbh, sig_dbh, height, sig_h)
                )
    result <- list(tariff = tariff, error = error)
    return(result)
  } else {
    return(tariff)
  }
}


############# FC tariff number by stand height (FC Eq 4) ################
#'
#' @title Tariff number by stand height
#' @description Use the estimated stand top height to calculate the stand
#' tariff number.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param spcode species code
#' @param sig_h sigma for height (optional)
#' @returns  either tariff number or if sig_h is provided, then returns a list
#' of the tariff number and uncertainty
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' stand_tariff("OK", 10)
#' stand_tariff("OK", 10, sig_h = 1)
#' stand_tariff("AH", 10, sig_h = 1)
#' @export
#'
stand_tariff <- function(spcode, height, sig_h = NA) {
  if(!is.character(spcode))stop("spcode must be a character")
  if(!is.numeric(height) || any(height<0))stop("height must be numeric and positive")

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

    error <- sqrt(1.68949^2 + error_product(rec$a2, 0.1314351, height, sig_h)
         + error_product(rec$a3, 0.003293529, height, sig_h, height, sig_h))

    return(c(tariff = tariff, error = error))
  } else {
    return(tariff)
  }
}

############# FC tariff number by stand height (FC Eq 4) ################
#' @title Tariff number by stand height
#' @description Use the estimated stand top height to calculate the stand
#' tariff number.
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param spcode species code
#' @param sig_h sigma for height (optional)
#' @returns  either tariff number or if sig_h is provided, then returns a list
#' of the tariff number and uncertainty
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' tariffs("OK", 10, 20)
#' tariffs("OK", 10, 20, sig_h = 1)
#' @export
#'
tariffs <- function(spcode, height, dbh, sig_dbh = NA, sig_h = NA) {
  n <- length(dbh)
  if (length(spcode) != n || length(height) != n) {
    stop("spcode, dbh, and height must be the same length")
  }

  if(!is.numeric(dbh) || any(dbh<=0))stop("dbh must be numeric and positive")
  if(!is.numeric(height) || any(height<=0))stop("height must be numeric and positive")

  tariffs <- errors <- numeric(n)

  #utils::data(tariff_broaddf, envir = environment())
  for (i in 1:n) {
    tb <- tariff_broaddf[tariff_broaddf$abbreviation == spcode[i], ]

    if(nrow(tb)==0){
      spcode <- lookup_df$stand[lookup_df$short == spcode]
      tb <- tarif2heightdf[tarif2heightdf$abbreviation == spcode, ]

      if(nrow(tb)==0){
        warning(paste("Species code not found for", spcode[i]))
      }

    }

    # Calculate tariff for this individual tree
    tariffs[i] <- tb$a1 + (tb$a2 * height[i]) + (tb$a3 * dbh[i]) + (tb$a4 * dbh[i] * height[i])

    if (!is.na(sig_dbh) && !is.na(sig_h)) {
      if (!is.numeric(sig_dbh) || sig_dbh < 0) stop("sig_dbh must be numeric and positive")
      if (!is.numeric(sig_h) || sig_h < 0) stop("sig_h must be numeric and positive")

      if(length(sig_dbh) > 1) {
        errors[i] <- sqrt(
          2.085826^2 +
            error_product(tb$a3, 0.05184218, dbh[i], sig_dbh[i]) +
            error_product(tb$a2, 0.3908223, height[i], sig_h[i]) +
            error_product(tb$a4, 0.01108647, dbh[i], sig_dbh[i], height[i], sig_h[i]))
      } else {
        errors[i] <- sqrt( 2.085826^2 +
                             error_product(tb$a3, 0.05184218, dbh[i], sig_dbh) +
                             error_product(tb$a2, 0.3908223, height[i], sig_h) +
                             error_product(tb$a4, 0.01108647, dbh[i], sig_dbh, height[i], sig_h))
      }
    }
  }

  if (!is.na(sig_dbh) && !is.na(sig_h)) {
    return(tariff)
  } else {
    return(list(tariff = tariffs, error = errors))
  }
}

############# FC tree merchantable volume (FC Eq 5) ################
#'
#' @title Forestry merchantable volume
#' @description Use the tree tariff number and dbh to estimate the mean
#' merchantable tree volume.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param tariff tree or stand tariff number
#' @param dbh diameter at breast height in centimetres
#' @param sig_tariff tariff sigma (optional)
#' @param sig_dbh diameter at breast height sig (optional)
#' @returns  volume in metres cubed and error if sig of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' merchtreevol(dbh = 24, tariff = 24)
#' merchtreevol(dbh = 24, tariff = 24, sig_dbh = 1, sig_tariff = 1)
#' @export
#'
merchtreevol <- function(dbh, tariff, sig_dbh = NA, sig_tariff = NA) {
  if(anyNA(tariff) || !is.numeric(tariff))
    stop("tariff must be numeric")
  if(!is.numeric(dbh) || any(dbh<=0))
    warning("dbh must be numeric and positive")

  ba <- (pi * dbh^2) / 40000
  a2 <- 0.315049301 * (tariff - 0.138763302)
  a1 <- (0.0360541 * tariff) - (a2 * 0.118288)
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
#' @returns  volume metres cubed or if sig_mtreevol is provided then additionally
#'  returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @export
#'
treevol <- function(mtreevol, dbh, sig_mtreevol = NA) {

  if(!is.numeric(dbh) || any(dbh<=0))stop("dbh must be numeric and positive")
#  if(!is.numeric(mtreevol) || any(mtreevol<=0))stop("mtreevol must be numeric and positive") # todo

  dbh <- round(dbh)
  if (dbh < 500 & dbh > 6.5) {
    #utils::data(stemvol, envir = environment())
    cf <- stemvol[stemvol$dbh..cm. == dbh, ]$X

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
    error <- error_product(1, 0.01794557, mtreevol, sig_mtreevol)

    result <- list(stemvolume = stemvol, error = error)
    return(result)
  }
}

############# Error for product ################
#' @title Analytical error progression for product
#' @description Calculates the error for x when x = a * b or x = a * b * c
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param a first variable in product
#' @param sig_a sigma for a
#' @param b second variable in product
#' @param sig_b sigma for b
#' @param c (optional) third variable in product
#' @param sig_c (optional) sigma for c
#' @returns error for x = (abs(a * b) * sqrt((sig_a / a)^2 + (sig_b / b)^2))^2
#' or (abs(a * b * c) * sqrt((sig_a / a)^2 + (sig_b / b)^2 + (sig_c / c)^2))^2
#' @references Taylor, J. R. (1997). An Introduction to Error Analysis: The Study of Uncertainties in Physical Measurements (2nd ed.). University Science Books.
#' OR Bevington, P. R., & Robinson, D. K. (2003). Data Reduction and Error Analysis for the Physical Sciences (3rd ed.). McGraw-Hill.
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
woodbiomass <- function(treevol, nsg, sig_treevol = NA) {

#  if(!is.numeric(treevol) || any(treevol<0))stop("treevol must be numeric and positive") #todo
  if(!is.numeric(nsg) || any(nsg<0))stop("nsg must be numeric and positive")

  woodbio <- treevol * nsg
  error <- NA

  if(!anyNA(sig_treevol)){
    error <- error_product(treevol, sig_treevol, nsg, 0.08222824)
  }
  return(list(woodbiomass = woodbio, error = error))
}

############# FC crown biomass (FC Eq 6 & 7) ################
#'
#' @title Forestry commission crown biomass estimates
#' @description  Function to find crown biomass (composed of branches,
#' stem tips and foliage) depending on species and dbh
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh diameter at breast height in centimetres
#' @param spcode Crown biomass species code, crown_biomasdf$Code or if not
#' defined for species, lookup_df$short to find relating lookup_df$Crown
#' @param sig_dbh dbh sigma
#' @returns  biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.2.
#' @importFrom utils data
#' @examples
#' crownbiomass("CBSP", 25)
#' crownbiomass("CBOK", 25, sig_dbh = 10)
#' crownbiomass(c("CBOK","CBOK"), dbh = c(30,30), sig_dbh = c(10,50))
#' @export
#'
crownbiomass <- function(spcode, dbh, sig_dbh = NA) {

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

  if (any(!is.numeric(dbh) | dbh < 0)) stop("All values of 'dbh' must be numeric
                                            and non-negative")

  # Load data for crown biomass calculations
  #utils::data(crown_biomasdf, envir = environment())

  for (i in 1:length(spcode)) {

    diam <- dbh[i]
    sigma <- ifelse(!anyNA(sig_dbh) && length(sig_dbh) > 1, sig_dbh[i], sig_dbh)

    # Warn for dbh < 7
    if (diam < 7) {
      warning("Equation is only specified for dbh equal to or greater than 7")
    }

    # Find the record in the data for the species code
    rec <- crown_biomasdf[crown_biomasdf$Code == spcode[i], ]
    if (nrow(rec) == 0) {

      spcode <- lookup_df$Crown[lookup_df$short == spcode[i]]
      rec <- crown_biomasdf[crown_biomasdf$Code == spcode, ]

      if (nrow(rec) == 0) {
        warning(paste("The species code", spcode[i], "is not found in lookup_df$Crown"))
        }
    }

    # Calculate biomass and error if sig_dbh is provided
    if (diam <= 50) {
      crown_biomass <- rec$b1 * diam^rec$p
      results$biomass[i] <- crown_biomass

      if (!anyNA(sigma)) {
        if (!is.numeric(sigma) || sigma < 0) stop("Argument 'sig_dbh' must be numeric and non-negative")
        sig_crown_biomass <- sqrt((diam^rec$p * 0.00001000607)^2 +
                                      (rec$b1 * rec$p * diam^(rec$p - 1) * sigma)^2)
        results$error[i] <- sig_crown_biomass
      }

    } else {
      crown_biomass <- rec$A + rec$b2 * diam
      results$biomass[i] <- crown_biomass

      if (!anyNA(sigma)) {
        if (!is.numeric(sigma) || sigma < 0) stop("Argument 'sig_dbh' must be numeric and non-negative")
        sig_crown_biomass <- sqrt(0.1058422^2 +
                                      error_product(rec$b2, 0.003282096, diam, sigma))
        results$error[i] <- sig_crown_biomass
      }
    }
  }

  return(results)
}

############# FC Root Biomass (FC Eq 8 & 9) ################
#'
#' @title Forestry commission root biomass estimates
#' @description Function to calculate the root biomass depending on species and dbh
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh diameter at breast height (1.3 m) in centimetres
#' @param spcode species code
#' @param sig_dbh sigma for DBH (optional)
#' @returns biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.3.
#' @examples
#' rootbiomass(spcode = 'RBRAR', dbh = 50)
#' rootbiomass(spcode = 'RBRAR', dbh = 50, sig_dbh = 10)
#' rootbiomass(spcode = c('RBRAR','RBRAR'), dbh = c(50, 70))
#' @export
#'
rootbiomass <- function(spcode, dbh, sig_dbh = NA) {

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
        sig_root <- sqrt((diam^2.5 * 0.000004703532)^2 + (2.5 * rec$b1 * diam^1.5 * sigma)^2)
      }
    } else {
      root_biomass <- rec$a + rec$b2 * diam

      # Calculate error if sig_dbh is provided
      if (!anyNA(sigma)) {
        if (!is.numeric(sigma) || sigma < 0) {
          stop("Argument 'sig_dbh' must be numeric and non-negative")
        }
        sig_root <- sqrt((1 * 0.03623626)^2 + (diam * 0.001980745)^2 + (rec$b2 * sigma)^2)
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
#' Matthews1 - Simplest with the carbon volatile fraction, CVF = 50% (Matthews, 1993)
#' Matthews2 - CVF based on type (broadleaf or conifer)
#' IPCC1 - CVF = 47.7% (IPCC, 2006)
#' IPCC2 - Lookup CVF by type and biome
#' Thomas1 - CVF = 48.3% and 95% CI of 0.3% (Thomas and Martin, 2012)
#' Thomas2 - Lookup by type and biome
#' @param type broadleaf or conifer. Only required for method = 'Matthews2',
#' 'IPCC2' or 'Thomas'
#' @param biome tropical, subtropical, mediterranean, temperate or boreal.
#' Only needed for 'IPCC2' and 'Thomas' methods.
#' @param sig_biomass biomass uncertainty (optional)
#' @returns either carbon or if 'sig_biomass' is provided, returns a list of carbon value and error. Only associated errors with method = 'IPCC2' and 'Thomas'
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
  CVF <- confidence <- rep(NA, n)

  # Retrieve CVF and confidence values using the lookup table
  for (i in seq_len(n)) {
    filter_conditions <- !is.na(CVF_df$method) & CVF_df$method == method &
      (is.na(CVF_df$type) | CVF_df$type == type[i]) &
      (is.na(CVF_df$biome) | CVF_df$biome == biome)
    matching_row <- subset(CVF_df, filter_conditions)

    if (nrow(matching_row) == 1) {
      CVF[i] <- matching_row$CVF
      confidence[i] <- matching_row$confidence
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

    error <- sqrt((sig_biomass / biomass)^2 + (confidence / 100)^2) * AGC
    return(list(AGC = AGC, sig_AGC = error))
  } else {
    return(AGC)
  }
}

############# FC Conifer seedlings and saplings to carbon ################
#'
#' @title Conifer seedlings and saplings to carbon
#' @description todo*
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param heightincm tree height in centimetres
#' @returns  carbon in tonnes
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018)
#' @importFrom utils data head tail
#' @export
#'
con_sap_seedling2C <- function(heightincm){
  #utils::data(seedlings_conifer, envir = environment())
  b <- utils::tail(seedlings_conifer[seedlings_conifer$height.cm <= heightincm,],1)
  t <- utils::head(seedlings_conifer[seedlings_conifer$height.cm >= heightincm,],1)
  rt <- (t$height.cm - heightincm)/(t$height.cm-b$height.cm)
  if ((nrow(b) == 0) |  (nrow (t) == 0)){
    NULL
  } else if (is.nan(rt)){
    t$Carbon.kg
  } else {
    b$Carbon.kg + ((t$Carbon.kg - b$Carbon.kg) * rt)
  }
}

############# FC Broadleaf seedlings and saplings to carbon ################
#'
#' @title Broadleaf seedlings and saplings to carbon
#' @description todo*
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param heightincm tree height in centimetres
#' @returns  carbon in tonnes
#' @note just uses simple linear relationship to get between measures
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018)
#' @importFrom utils data head tail
#' @export
#'
broad_sap_seedling2C <- function(heightincm){
  if(!is.numeric(heightincm) || heightincm < 0)stop("
  Argument 'heightincm' must be numeric and non-negative")
  if(heightincm > 1000)warning("Maximum for 'heightincm' is 1000cm")
  if(heightincm < 1)   warning("Minimum for 'heightincm' is 1cm")

  #utils::data(seedlings_broad, envir = environment())
  #get first and last
  b <- utils::tail(seedlings_broad[seedlings_broad$height.cm <= heightincm,],1)
  t <- utils::head(seedlings_broad[seedlings_broad$height.cm >= heightincm,],1)
  rt <- (t$height.cm - heightincm)/(t$height.cm-b$height.cm)
  if ((nrow(b) == 0) |  (nrow (t) == 0)){
    NULL
  } else if (is.nan(rt)){
    t$Carbon.kg
  } else {
    b$Carbon.kg + ((t$Carbon.kg - b$Carbon.kg) * rt)
  }
}

############# Lookup Species Code ################
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
#' lookspcode('Quercus robur', type = "broadleaf", returnv='short')
#' lookspcode(c("Scots pine", "Oak"), c("conifer", "broadleaf"), "short")
#' lookspcode(c("Scots pine", "Oak"), c("conifer", "broadleaf"))
#' lookspcode(c('Quercus robur', 'Quercus') , type = c("broadleaf", "broadleaf"), returnv='short')
#' @export
#'
lookspcode <- function(name, type, returnv = "all") {

  # Check inputs
  if (!is.character(name)) stop("'name' must be a character vector or list")
  if (length(type) != length(name)) stop("'type' and 'name' must have the same length.")
  if (!all(type %in% c("broadleaf", "conifer", NA, ""))) stop("'type' must be either 'broadleaf', 'conifer'")
  if (!is.character(returnv) || !returnv %in% c("short", "single", "stand", "root", "all")) {
    stop("'returnv' must be a character, either 'short', 'single', 'stand', 'root', or 'all'.")
  }

  # Load or define lookup_df (ensure it's available)
  #utils::data(lookup_df, envir = environment())

  # Initialize result dataframe based on returnv
  n <- length(name)

  if (returnv == 'all') {
    r <- lookup_df[0,]  # Create an empty dataframe with the same structure as lookup_df
  } else {
    r <- data.frame(spname = rep(NA, n), spcode = rep(NA, n), matchtype = rep(NA, n), stringsAsFactors = FALSE)
  }

  # For each tree in the input vectors
  for (i in 1:n) {
    match_type <- spcode <- NA

    # Case insensitive and trim spaces
    search_name <- stringr::str_trim(tolower(name[i]))

    # Lookup common name
    rec <- lookup_df[tolower(lookup_df$common_name) == search_name, ]

    if (nrow(rec) == 1) {
      match_type <- "Common name"
    } else {

      # Lookup species binomial
      rec <- lookup_df[tolower(lookup_df$latin_name) == search_name, ]

      if (nrow(rec) == 1) {
        match_type <- "Species binomial"
      } else {
        rec <- lookup_df[tolower(lookup_df$General.genus) == stringr::word(search_name, 1), ]
        if (nrow(rec) == 1) {
          match_type <- "Genus"
         }
        }
       }

    # If name not been matched, fall back to type classification
    if (is.na(match_type) || nrow(rec) == 0) {
      rec <- lookup_df[lookup_df$General.type == type[i], ]
      if (nrow(rec) == 1) {
        match_type <- type[i]  # e.g., broadleaf or conifer
      } else {
        match_type <- "Not found"
      }
    }

    # Populate result dataframe based on returnv
    if (nrow(rec) > 0) {
      if (returnv == 'all') {
        r[i, ] <- rec[1, ]  # Assign the first matching row from rec
      } else {
        spcode <- switch(returnv, short = rec$short, single = rec$single, stand = rec$stand, root = rec$Root)
        r[i, "spname"] <- name[i]
        r[i, "spcode"] <- spcode
        r[i, "matchtype"] <- match_type
      }
    } else {
      # Handle case where no match is found (leave NA)
      r[i, "spname"] <- name[i]
      r[i, "spcode"] <- NA
      r[i, "matchtype"] <- "Not found"
    }
  }
  return(r)
}

############# Above Ground Carbon ################
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
#' @returns either Above ground carbon, AGC in tonnes, or a list of tariff
#' number, merchantable volume (metres cubed), stem volume (metres cubed),
#' stem biomass (tonnes), stem carbon (tonnes), canopy carbon (tonnes) and
#' root carbon (tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' fc_agc_error(spcode='OK', dbh=74, height=24, method="IPCC2", biome="temperate", returnv ="All", sig_dbh=10, sig_h=1)
#' fc_agc_error(spcode='OK', dbh=74, height=24, method="IPCC2", biome="temperate", returnv ="AGC", sig_dbh=10, sig_h=1)
#' @export
#'
#'
fc_agc_error <- function(spcode, dbh, height, method = "IPCC2", biome = "temperate",
                   returnv = "All", sig_dbh = 20, sig_h = 8){

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

    # Get tariff number depending on broadleaf or conifer
    if (type == "broadleaf") {
      tariff <- broadleaf_tariff(spcode[i], height[i], dbh[i], sig_dbh = sig_dbh, sig_h = sig_h)

    } else if (type == "conifer") {
      tariff <- conifer_tariff(spcode[i], height[i], dbh[i], sig_dbh = sig_dbh, sig_h = sig_h)
    }

    if (length(tariff) == 0) {
      stop("Error in", type, "_tariff function at index: ", i)
    }

    # Calculate volumes and biomass
    mercvol <- merchtreevol(dbh[i], tariff$tariff, sig_dbh, as.numeric(tariff[2]))  # Merchantable tree volume
    stemvol <- treevol(mercvol$volume, dbh = dbh[i], mercvol$error)          # Stem volume
    woodbio <- woodbiomass(stemvol$stemvolume, rec$NSG, stemvol$error)       # Stem Biomass
    crownbio <- crownbiomass(rec$Crown, dbh[i], sig_dbh)    # Crown Biomass
    AGB <- woodbio$woodbiomass + crownbio$biomass             # Above ground Biomass
    sig_AGB <- sqrt(woodbio$error^2 + crownbio$error^2)
    AGC <- biomass2c(AGB, method=method, type, biome=biome, sig_biomass = sig_AGB) # Above ground Carbon
    r$AGC_t[i] <- AGC$AGC
    r$sig_AGC[i] <- AGC$sig_AGC

    if(returnv == "All"){
      r$spcode[i] <- spcode[i]
      r$spname[i] <- rec$latin_name
      r$dbh[i]    <- dbh[i]
      r$height[i] <- height[i]
      r$NSG[i] <- rec$NSG
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

######################### Bunce Eq ########################
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
#' @export
#'
bunce <- function(spcode, dbh){

  if(!is.numeric(dbh) || dbh < 0)stop("Argument 'dbh' must be numeric and non-negative")

  data("buncedf", envir = environment())
  coeffs <- buncedf[buncedf$spcode == spcode,]

  if(nrow(coeffs)==0){warning("The species code, 'spcode' is not found in
                        data(buncedf), therefore, the combined coefficients
                           will be used")}

  biomass <- coeffs$a + coeffs$b*log(pi*dbh)

  return(exp(biomass))
}


#=============== Calculate Carbon using Biomass ==========================
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

#=============== Calculate Carbon using allodb ==========================
# ==== Inputs:
# df: data frame containing columns; DBH (in cm), Genus_corrected, Species_corrected (from output of biomass function)
# coords: either the coordinates of the site, a vector of longitude and latitude
#         or a matrix of coordinates for each tree
# ==== Optional Inputs:
# output.all: if TRUE outputs the coefficients of the model, a*DBH^b+e {e ~ N(0,sigma^2}
# new.eqtable: a subset or extension of the allometric equation table. Create with new_equations
#' @title Estimate Tree Carbon using Biomass package functions
#' @description Using the Biomass package to calculate carbon
#' @param df dataframe containing columns; Genus, Species, DBH (in cm),
#' Height (in m). Height is optional, but df must include this column.
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



