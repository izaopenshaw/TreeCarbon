############## Functions file for Woodland Carbon Code ########################
# TODO:
# error for c2co2e ?
# check biomass2c that error is confidence percentage by checking references

############ Tariff number from volume and tree basal area (FC Eq 1) ############
#'
#' @title Tariff number from volume and basal area
#' @description Using the sample tree’s basal area and volume to calculate the
#' tariff number. Basal area is calculated by ba = (pi * dbh^2)/40000.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param vol tree volume in metres cubed
#' @param dbh diameter at breast height in centimetres
#' @param sigma_vol sigma for tree volume (optional)
#' @param sigma_dbh sigma for diameter at breast height (optional)
#' @returns  Tariff number or if sigma for inputs are provided, then will return
#' a list of tariff number and sigma for tariff
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). (Equation 1)
#' @examples
#' tariff_vol_area(vol=50, dbh=24, sigma_vol = 10, sigma_dbh = 1)
#' @export
#'
tariff_vol_area <- function(vol, dbh, sigma_vol = NA, sigma_dbh = NA){
  if (!is.numeric(vol) || !is.numeric(dbh)) { #if (!is.numeric(vol) || any(vol < 0) || !is.numeric(dbh) || any(dbh < 0)) {
    stop("vol and dbh must be non-negative numeric values")  #  stop("vol and dbh must be non-negative numeric values")
  }
  ba <- (pi * dbh^2)/40000                # tree basal area in m^2
  a1 <- (vol - 0.005002986)/(ba - 0.003848451)
  tariff <- (3.174106384 * a1) + 0.138763302

  if(!anyNA(sigma_vol) || !anyNA(sigma_dbh)){
    if (any(sigma_vol < 0) || !is.numeric(sigma_vol)) {stop("sigm_vol must be non-negative numeric")}
    if (any(sigma_dbh < 0) || !is.numeric(sigma_dbh)) {stop("sigma_dbh must be non-negative numeric")}

    sigma_ba <- (pi * 2 * dbh / 40000) * sigma_dbh
    partial_a_v <- 1 / (ba - 0.003848451)
    partial_a_ba <- -(vol - 0.005002986) / (ba - 0.003848451)^2
    sigma_a <- sqrt((partial_a_v * sigma_vol)^2 + (partial_a_ba * sigma_ba)^2)
    sigma_t <- 3.174106384 * sigma_a

    return(c(tariff, sigma_t))
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
#' @param sigma_h uncertainty in height
#' @param sigma_dbh uncertainty in dbh
#' @returns  tariff number
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' conifer_tariff("SP", 74, 24)
#' conifer_tariff("SP", 74, 24, 5, 5)
#' @export
#'
conifer_tariff <- function(spcode, height, dbh, sigma_h = NA, sigma_dbh = NA) {
  if (!is.numeric(height) || !is.numeric(dbh) || any(height < 0) || any(dbh < 0)) {
    stop("height and dbh must be non-negative numeric values")
  }
  if (!(length(spcode) == length(height) && length(height) == length(dbh))) {
    stop("All input vectors (spcode, height, dbh, sigma_h, sigma_dbh) must have the same length.")
  }

  utils::data(tariff_coniferdf, envir = environment())

  n <- length(spcode)

  tariffs <- numeric(n)
  sigmas <- numeric(n)

  return_error <- !anyNA(sigma_h) || !anyNA(sigma_dbh)

  if(return_error){
    if (any(sigma_h < 0) || !is.numeric(sigma_h)) {stop("sigma_h must be non-negative numeric")}
    if (any(sigma_dbh < 0) || !is.numeric(sigma_dbh)) {stop("sigma_dbh must be non-negative numeric")}
  }

  for(i in 1:n){
    rec <- tariff_coniferdf[tariff_coniferdf$abbreviation == spcode[i], ]
    if(nrow(rec) == 0)stop("The specified 'spcode' is not found in tariff_coniferdf.rda")

    tariffs[i] <- rec$a1 + (rec$a2 * height[i]) + (rec$a3 * dbh[i])

    if(return_error){
      sigmas[i] <- sqrt(1.857442^2 + error_product(rec$a2, 0.2465285, height[i], sigma_h)
                      + error_product(rec$a3, 0.1834052, dbh[i], sigma_dbh))
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
#' @param sigma_h sigma for tree height (optional)
#' @param sigma_dbh sigma for diameter at breast height (optional)
#' @returns  tariff number and error if sigma of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Method B, Equation 2.
#' @importFrom utils data
#' @examples broadleaf_tariff(spcode = 'OK', height = 25, dbh = 75)
#' 55.96704
#' broadleaf_tariff(spcode = "OK", height = 25, dbh = 1.5, sigma_dbh = 10, sigma_h = 1)
#' broadleaf_tariff(spcode = "OK", height = 24, dbh = 1.5, sigma_dbh = 10, sigma_h = 1)
#' plot(seq(1,20,1), broadleaf_tariff(spcode = 'OK', height = 25, dbh = seq(1,20,1)), ylab= "broadleaf_tariff", xlab = "dbh")
# # TODO: why is this negative????
#' @export
#'
broadleaf_tariff <- function(spcode, height, dbh, sigma_dbh = NA, sigma_h = NA) {
  if(!is.numeric(dbh) || any(dbh<=0))stop("dbh must be numeric and positive")
  if(!is.numeric(height) || any(height<=0))stop("height must be numeric and positive")

  utils::data(tariff_broaddf, envir = environment())
  tb <- tariff_broaddf[tariff_broaddf$abbreviation == spcode, ]
  tariff <- tb$a1 + (tb$a2 * height) + (tb$a3 * dbh) + (tb$a4 * dbh * height)

  if(!anyNA(sigma_dbh) | !anyNA(sigma_h)){
    if(!is.numeric(sigma_dbh) || any(sigma_dbh<0))stop("sigma_dbh must be numeric and positive")
    if(!is.numeric(sigma_h) || any(sigma_h<0))stop("sigma_h must be numeric and positive")

    error <- sqrt(2.085826^2 +
                error_product(tb$a3, 0.05184218, dbh, sigma_dbh) +
                error_product(tb$a2, 0.3908223, height, sigma_h) +
                error_product(tb$a4, 0.01108647, dbh, sigma_dbh, height, sigma_h)
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
#' @param sigma_h sigma for height (optional)
#' @returns  either tariff number or if sigma_h is provided, then returns a list
#' of the tariff number and uncertainty
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' stand_tariff("OK", 10)
#' stand_tariff("OK", 10, sigma_h = 1)
#' @export
#'
stand_tariff <- function(spcode, height, sigma_h = NA) {
  if(!is.character(spcode))stop("spcode must be a character")
  if(!is.numeric(height) || any(height<0))stop("height must be numeric and positive")

  utils::data(tarif2heightdf, envir = environment())
  rec <- tarif2heightdf[tarif2heightdf$abbreviation == spcode, ]

  if(nrow(rec)==0){stop("The species code, 'spcode' is not found in data(tarif2heightdf)")}

  tariff <- rec$a1 + (rec$a2 * height) + (rec$a3 * height^2)

  if(!anyNA(sigma_h)){
    if(!is.numeric(sigma_h) || any(sigma_h<0))stop("sigma_h must be numeric and positive")

    error <- sqrt(1.68949^2 + error_product(rec$a2, 0.1314351, height, sigma_h)
         + error_product(rec$a3, 0.003293529, height, sigma_h, height, sigma_h))

    return(c(tariff = tariff, error = error))
  } else {
    return(tariff)
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
#' @param sigma_tariff tariff sigma (optional)
#' @param sigma_dbh diameter at breast height sigma (optional)
#' @returns  volume metres cubed and error if sigma of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @examples
#' merchtreevol(dbh = 24, tariff = 24)
#' merchtreevol(dbh = 24, tariff = 24, sigma_dbh = 1, sigma_tariff = 1)
#' @export
#'
merchtreevol <- function(dbh, tariff, sigma_dbh = NA, sigma_tariff = NA) {
  if(anyNA(tariff) || !is.numeric(tariff))
    stop("tariff must be numeric")
  if(!is.numeric(dbh) || any(dbh<=0))
    warning("dbh must be numeric and positive")

  ba <- (pi * dbh^2) / 40000
  a2 <- 0.315049301 * (tariff - 0.138763302)
  a1 <- (0.0360541 * tariff) - (a2 * 0.118288)
  vol <- a1 + (a2 * ba)

  if(anyNA(sigma_dbh)) {
    return(vol)
  } else {
    sig_ba <- (pi * dbh / 20000) * sigma_dbh
    sig_a2 <- 0.315049301 * sigma_tariff
    sig_a1 <- sqrt(
      (0.0360541 - 0.315049301 * 0.118288)^2 * sigma_tariff^2 +
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
#' @param sigma_mtreevol sigma for mtreevol (optional)
#' @returns  volume metres cubed or if sigma_mtreevol is provided then additionally
#'  returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @export
#'
treevol <- function(mtreevol, dbh, sigma_mtreevol = NA) {

  if(!is.numeric(dbh) || any(dbh<=0))stop("dbh must be numeric and positive")
#  if(!is.numeric(mtreevol) || any(mtreevol<=0))stop("mtreevol must be numeric and positive") # todo

  dbh <- round(dbh)
  if (dbh < 500 & dbh > 6.5) {
    utils::data(stemvol, envir = environment())
    cf <- stemvol[stemvol$dbh..cm. == dbh, ]$X

  } else if (dbh < 6.5){
    warning("dbh is less than 6.5 cm, multiplication factor is not specified")
    cf <- 1
  } else if (dbh > 500){
    warning("dbh is above 5 m")
    cf <- 1
  }
  stemvol <- cf * mtreevol

  if(anyNA(sigma_mtreevol)){
    return(stemvol)
  } else {
    if(!is.numeric(sigma_mtreevol) || any(sigma_mtreevol<=0))stop("sigma_mtreevol must be numeric and positive")
    error <- error_product(1, 0.01794557, mtreevol, sigma_mtreevol)

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
#' @param sigma_treevol tree volume in metres sigma (optional)
#' @returns  biomass in oven dry tonnes or if sigma_treevol is provided then
#' additionally returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Lavers, G.M. and Moore, G.L.
#' (1983) The strength properties of timber. Building Research Establishment
#' Report CI/Sfb i(J3). Building Research Establishment, Garston.
#' @examples
#' woodbiomass(10, 0.56, 5)
#'
#' @export
#'
woodbiomass <- function(treevol, nsg, sigma_treevol = NA) {

#  if(!is.numeric(treevol) || any(treevol<0))stop("treevol must be numeric and positive") #todo
  if(!is.numeric(nsg) || any(nsg<0))stop("nsg must be numeric and positive")

  woodbio <- treevol * nsg
  error <- NA

  if(!anyNA(sigma_treevol)){
    error <- error_product(treevol, sigma_treevol, nsg, 0.08222824)
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
#' @param spcode Crown biomass species code, crown_biomasdf$Code which
#' relates to sp_lookupdf$Crown
#' @param sigma_dbh dbh sigma
#' @returns  biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.2.
#' @importFrom utils data
#' @examples
#' crownbiomass("CBSP", 25)
#' crownbiomass("CBOK", 25, sigma_dbh = 10)
#' crownbiomass(c("CBOK","CBOK"), dbh = c(30,30), sigma_dbh = c(10,50))
#' @export
#'
crownbiomass <- function(spcode, dbh, sigma_dbh = NA) {

  # Check inputs
  if (length(spcode) != length(dbh)) stop("Length of 'spcode' and 'dbh' must be the same")

  if (length(dbh) > 1){
    if (!anyNA(sigma_dbh)) {
      if (length(sigma_dbh) != 1 && length(sigma_dbh) != length(dbh)) {
        stop("Length of 'sigma_dbh' must be either 1 or match the length of 'dbh'")
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
  utils::data(crown_biomasdf, envir = environment())

  for (i in seq_along(spcode)) {

    diam <- dbh[i]
    sigma <- ifelse(!anyNA(sigma_dbh) && length(sigma_dbh) > 1, sigma_dbh[i], sigma_dbh)

    # Warn for dbh < 7
    if (diam < 7) {
      warning("Equation is only specified for dbh equal to or greater than 7")
    }

    # Find the record in the data for the species code
    rec <- crown_biomasdf[crown_biomasdf$Code == spcode[i], ]
    if (nrow(rec) == 0) {
      stop(paste("The species code", spcode[i], "is not found in data(crown_biomasdf), see data(sp_lookupdf) column 'Crown'"))
    }

    # Calculate biomass and error if sigma_dbh is provided
    if (diam <= 50) {
      crown_biomass <- rec$b1 * diam^rec$p
      results$biomass[i] <- crown_biomass

      if (!anyNA(sigma)) {
        if (!is.numeric(sigma) || sigma < 0) stop("Argument 'sigma_dbh' must be numeric and non-negative")
        sigma_crown_biomass <- sqrt((diam^rec$p * 0.00001000607)^2 +
                                      (rec$b1 * rec$p * diam^(rec$p - 1) * sigma)^2)
        results$error[i] <- sigma_crown_biomass
      }

    } else {
      crown_biomass <- rec$A + rec$b2 * diam
      results$biomass[i] <- crown_biomass

      if (!anyNA(sigma)) {
        if (!is.numeric(sigma) || sigma < 0) stop("Argument 'sigma_dbh' must be numeric and non-negative")
        sigma_crown_biomass <- sqrt(0.1058422^2 +
                                      error_product(rec$b2, 0.003282096, diam, sigma))
        results$error[i] <- sigma_crown_biomass
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
#' @param sigma_dbh sigma for DBH (optional)
#' @returns biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.3.
#' @examples
#' rootbiomass(spcode = 'RBRAR', dbh = 50)
#' rootbiomass(spcode = 'RBRAR', dbh = 50, sigma_dbh = 10)
#' rootbiomass(spcode = c('RBRAR','RBRAR'), dbh = c(50, 70))
#' @export
#'
rootbiomass <- function(spcode, dbh, sigma_dbh = NA) {

  # Initialise
  if (length(spcode) != length(dbh)) {
    stop("Length of 'spcode' and 'dbh' must be the same")
  }
  if (any(!is.numeric(dbh) | dbh < 0)) {
    stop("All values of 'dbh' must be numeric and non-negative")
  }
  if (length(dbh) > 1){
    if (!anyNA(sigma_dbh)) {
      if (length(sigma_dbh) != 1 && length(sigma_dbh) != length(dbh)) {
        stop("Length of 'sigma_dbh' must be either 1 or match the length of 'dbh'")
      }
      results <- data.frame(spcode = spcode, dbh = dbh, rootbiomass = NA, error = NA)
    } else {
      results <- data.frame(spcode = spcode, dbh = dbh, rootbiomass = NA)
    }
  } else {
    results <- c()
  }

  utils::data(root_biomassdf, envir = environment())

  # Iterate through each species code and dbh value
  for (i in seq_along(spcode)) {
    diam <- dbh[i]
    sigma <- if (!anyNA(sigma_dbh) && length(sigma_dbh) > 1) sigma_dbh[i] else sigma_dbh

    # Find the record in the data for the species code
    rec <- root_biomassdf[root_biomassdf$Code == spcode[i], ]
    if (nrow(rec) == 0) {
      stop(sprintf("The species code '%s' is not found in data(root_biomassdf).", spcode[i]))
    }

    # Calculate root biomass based on dbh value
    if (diam <= 30) {
      root_biomass <- rec$b1 * diam^2.5

      # Calculate error if sigma_dbh is provided
      if (!anyNA(sigma)) {
        if (!is.numeric(sigma) || sigma < 0) {
          stop("Argument 'sigma_dbh' must be numeric and non-negative")
        }
        sigma_root <- sqrt((diam^2.5 * 0.000004703532)^2 + (2.5 * rec$b1 * diam^1.5 * sigma)^2)
      }
    } else {
      root_biomass <- rec$a + rec$b2 * diam

      # Calculate error if sigma_dbh is provided
      if (!anyNA(sigma)) {
        if (!is.numeric(sigma) || sigma < 0) {
          stop("Argument 'sigma_dbh' must be numeric and non-negative")
        }
        sigma_root <- sqrt((1 * 0.03623626)^2 + (diam * 0.001980745)^2 + (rec$b2 * sigma)^2)
      }
    }

    # Store results in the results data frame
    results$rootbiomass[i] <- root_biomass
    if (!anyNA(sigma)) {
      results$error[i] <- ifelse(exists("sigma_root"), sigma_root, NA)
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
#' c2co2e(448)
#' c2co2e(c(448, 450))
#' @export
#'
c2co2e <- function(carbon) {
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
#' @param sigma_biomass biomass uncertainty (optional)
#' @returns either carbon or if 'sigma_biomass' is provided, returns a list of carbon value and error. Only associated errors with method = 'IPCC2' and 'Thomas'
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
biomass2c <- function(biomass, method, type, biome, sigma_biomass = NA) {

  # Check arguments
#  if (any(!is.numeric(biomass) | biomass < 0)) {
#    stop("All biomass values must be numeric and positive")
#  } TODO

  valid_methods <- c("Matthews1", "Matthews2", "IPCC1", "IPCC2", "Thomas")
  if (!(method %in% valid_methods)) {
    stop(paste("Invalid method. Choose from:", paste(valid_methods,
                collapse = ", "), ". See R helpfile for more details."))
  }

  valid_types <- c("broadleaf", "conifer")
  if ((method %in% c("Matthews2", "IPCC2", "Thomas")) && !missing(type) &&
      !all(type %in% valid_types)) {
    stop("Invalid type. Choose from: 'broadleaf', 'conifer'")
  }

  valid_biomes <- c("tropical", "subtropical", "mediterranean", "temperate", "boreal")
  if ((method %in% c("IPCC2", "Thomas")) && !missing(biome) && !(biome %in% valid_biomes)) {
    stop("Invalid biome. Choose from: 'tropical', 'subtropical', 'mediterranean', 'temperate', 'boreal'")
  }

  n <- length(biomass)

  if(is.vector(type) && length(type) != n) {
    stop("Length of 'type' must match the length of 'biomass'")
  }
  CVF <- confidence <- rep(NA,n)

  # Set CVF and confidence based on method, type, and biome
  if (method == "Matthews1") {            CVF <- 50
  } else if (method == "Matthews2") {
    if (type[i] == "broadleaf") {         CVF <- 49.3
    } else if (type[i] == "conifer") {    CVF <- 50.1
    }
  } else if (method == "IPCC1") {         CVF <- 47.7
  } else if (method == "IPCC2") {
    if (biome == "tropical" ||
        biome == "subtropical") {         CVF <- 47  ;  confidence <- 0.05
    } else if (biome == "temperate" ||
               biome == "boreal") {
      for (i in 1:n){
        if (type[i] == "broadleaf") {      CVF[i] <- 48  ;  confidence[i] <- 0.04
        } else if (type[i] == "conifer") { CVF[i] <- 51  ;  confidence[i] <- 0.08
        }
      }
    }
  } else if (method == "Thomas") {
    if (biome == "tropical") {
      for (i in 1:n){
        if (type[i] == "broadleaf") {       CVF[i] <- 47.1  ;  confidence[i] <- 0.4
        } else if (type[i] == "conifer") {  CVF[i] <- 49.3  ;  confidence[i] <- 0.6
        }
      }
    } else if (biome == "subtropical" || biome == "mediterranean") {
      for (i in 1:n){
      if (type[i] == "broadleaf") {       CVF[i] <- 48.1  ;  confidence[i] <- 0.9
      } else if (type[i] == "conifer") {  CVF[i] <- 50.54 ;  confidence[i] <- 2.8
      }
      }
    } else if (biome == "temperate" || biome == "boreal") {
      for (i in 1:n){
      if (type[i] == "broadleaf") {       CVF[i] <- 48.8  ;  confidence[i] <- 0.6
      } else if (type[i] == "conifer") {  CVF[i] <- 50.8  ;  confidence[i] <- 0.6
      }
      }
    }
  }

  if (method %in% c("Matthews1", "Matthews2", "IPCC1")){
    CVF <- rep(CVF, n)
    confidence <- rep(confidence, n)
  }

  # Calculate carbon values
  AGC <- biomass * CVF / 100

  if(!anyNA(sigma_biomass)){
    if (length(sigma_biomass) != n) {
      stop("Length of sigma_biomass must match the length of biomass")
    }
    if (any(!is.numeric(sigma_biomass) | sigma_biomass < 0)) {
      stop("sigma_biomass must be numeric and positive")
    }

    error <- error_product(biomass, sigma_biomass, CVF/100, confidence/100)
    #error <- mapply(error_product, biomass, sigma_biomass, MoreArgs = list(CVF / 100, confidence / 100))

    return(list(AGC = AGC, sigma_AGC = error))
  } else {
    return(AGC)
    }
}

biomass2c <- function(biomass, method, type = NULL, biome = NULL, sigma_biomass = NA) {
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
    filter_conditions <- !is.na(CVF_data$method) & CVF_data$method == method &
      (is.na(CVF_data$type) | CVF_data$type == type[i]) &
      (is.na(CVF_data$biome) | CVF_data$biome == biome)
    matching_row <- subset(CVF_data, filter_conditions)

    if (nrow(matching_row) == 1) {
      CVF[i] <- matching_row$CVF
      confidence[i] <- matching_row$confidence
    } else {
      stop("Could not find a matching CVF value for the provided parameters.")
    }
  }

  # Calculate carbon values
  AGC <- biomass * CVF / 100

  # Calculate uncertainty if sigma_biomass is provided
  if (!anyNA(sigma_biomass)) {
    if (length(sigma_biomass) != n) stop("Length of sigma_biomass must match length of biomass")
    if (any(!is.numeric(sigma_biomass) | sigma_biomass < 0)) stop("sigma_biomass must be numeric and positive")

    error <- sqrt((sigma_biomass / biomass)^2 + (confidence / 100)^2) * AGC
    return(list(AGC = AGC, sigma_AGC = error))
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
  utils::data(seedlings_conifer, envir = environment())
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

  utils::data(seedlings_broad, envir = environment())
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
#' @param name_type either 'botanical' or 'common'
#' @param type either 'broadleaf' or 'conifer'
#' @param returnv either 'all', short', 'single', 'stand', 'root' from sp_lookupdf.Rda
#' @returns Species code
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom stringr word str_trim
#' @importFrom utils data
#' @examples
#' lookspcode('Quercus robur', type = "broadleaf", returnv='short')
#' lookspcode(c("Scots pine", "Oak"), c("conifer", "broadleaf"),name_type= "common", "short")
#' lookspcode(c("Scots pine", "Oak"), c("conifer", "broadleaf"))
#' lookspcode(c('Quercus robur', 'Quercus') , type = c("broadleaf", "broadleaf"), returnv='short')
#' @export
#'
lookspcode <- function(name, type, name_type = "botanical", returnv = "all") {

  # Check inputs
  if (!is.character(name)) stop("'name' must be a character vector or list")
  if (!is.character(name_type)) stop("'name_type' must be either 'botanical' or 'common'.")
  if (length(type) != length(name)) stop("'type' and 'name' must have the same length.")
  if (!all(type %in% c("broadleaf", "conifer", NA, ""))) stop("'type' must be either 'broadleaf', 'conifer'")
  if (!is.character(returnv) || !returnv %in% c("short", "single", "stand", "root", "all")) {
    stop("'returnv' must be a character, either 'short', 'single', 'stand', 'root', or 'all'.")
  }

  # Load or define sp_lookupdf (ensure it's available)
  utils::data(sp_lookupdf, envir = environment())

  # Initialize result dataframe based on returnv
  n <- length(name)

  if (returnv == 'all') {
    r <- sp_lookupdf[0,]  # Create an empty dataframe with the same structure as sp_lookupdf
  } else {
    r <- data.frame(spname = rep(NA, n), spcode = rep(NA, n), matchtype = rep(NA, n), stringsAsFactors = FALSE)
  }

  # For each tree in the input vectors
  for (i in 1:n) {
    match_type <- spcode <- NA

    # Case insensitive and trim spaces
    search_name <- stringr::str_trim(tolower(name[i]))

    # Lookup based on name_type
    if (name_type == "common") {
      rec <- sp_lookupdf[tolower(sp_lookupdf$common_name) == search_name, ]
      if (nrow(rec) == 1) {
        match_type <- "Common name"
      } else {
        rec <- sp_lookupdf[tolower(sp_lookupdf$General.genus) == stringr::word(search_name, 1), ]
        if (nrow(rec) == 1) {
          match_type <- "Genus"
        }
      }
    } else {  # Botanical name lookup
      rec <- sp_lookupdf[tolower(sp_lookupdf$latin_name) == search_name, ]
      if (nrow(rec) == 1) {
        match_type <- "Botanical name"
      } else {
        rec <- sp_lookupdf[tolower(sp_lookupdf$General.genus) == stringr::word(search_name, 1), ]
        if (nrow(rec) == 1) {
          match_type <- "Genus"
        }
      }
    }

    # If neither name nor genus matched, fall back to type classification
    if (anyNA(match_type) || nrow(rec) == 0) {
      rec <- sp_lookupdf[sp_lookupdf$General.type == type[i], ]
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
  utils::data(sp_lookupdf, envir = environment())
  #    data("sp_lookupdf", package = "WoodlandCarbonCode")

  # Loop over all trees
  for (i in 1:n) {
    if(dbh[i]<=0)warning("dbh must be numeric & positive for index:", i)

#    # Lookup species data from code
#    rec <- sp_lookupdf[sp_lookupdf$short == spcode[i], ]
#    tarifflokupcode <- rec$single
#    type <- rec$type

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
#' @param sigma_dbh error in sampling of dbh, single value
#' @param sigma_h error in sampling of height, single value
#' @returns either Above ground carbon, AGC in tonnes, or a list of tariff
#' number, merchantable volume (metres cubed), stem volume (metres cubed),
#' stem biomass (tonnes), stem carbon (tonnes), canopy carbon (tonnes) and
#' root carbon (tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @examples
#' fc_agc_error(spcode='OK', dbh=74, height=24, method="IPCC2", biome="temperate", returnv ="All", sigma_dbh=10, sigma_h=1)
#' fc_agc_error(spcode='OK', dbh=74, height=24, method="IPCC2", biome="temperate", returnv ="AGC", sigma_dbh=10, sigma_h=1)
#' @export
#'
#'
#test <- fc_agc_error(spcode=rep('OK',21), dbh=seq(5,105,5), height=rep(10,21), method="IPCC2", biome="temperate", returnv ="All", sigma_dbh=10, sigma_h=1)
#test <- fc_agc_error(spcode=c(rep('OK',21)), dbh=c(seq(10,110,5)), height=c(rep(10,21)), method="IPCC2", biome="temperate", returnv ="All", sigma_dbh=10, sigma_h=1)


fc_agc_error <- function(spcode, dbh, height, method = "IPCC2", biome = "temperate",
                   returnv = "All", sigma_dbh = 20, sigma_h = 8){

  # Check arguments
  if(length(spcode) != length(dbh) || length(spcode) != length(height) ||
     length(height) != length(dbh))stop("input lengths must be the same")
  if(!is.character(spcode))stop("spcode must be a character")
#  if(!is.numeric(height)||any(height<=0))stop("height must be numeric & positive") not sure if i need this if checks are within functions
  if(!is.numeric(sigma_dbh)||any(sigma_dbh<=0))stop("sigma_dbh must be numeric & positive")
  if(!is.numeric(sigma_h)||any(sigma_h<0)){
    stop("sigma_h must be numeric & positive")
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
    r <- data.frame(spcode=NA, spname=NA, dbh=NA, height=NA, NSG=NA, tariff=NA, sig_tariff=NA, mercvol=NA, sig_mercvol=NA, stemvol=NA, sig_stemvol=NA,
                    stembiomass=NA, sig_stembiomass=NA, crownbiomass=NA, sig_crownbiomass=NA, rootbiomass=NA, sig_rootbiomass=NA, AGC=NA, sig_AGC=NA, stringsAsFactors=FALSE)
  } else {
    r <- data.frame(AGC=NA, sig_AGC=NA, stringsAsFactors=FALSE)
  }
  r <- r[1:n,]
  utils::data(sp_lookupdf, envir = environment())

  # Loop over all trees
  for (i in 1:n) {
    if (dbh[i]<=0 || !is.numeric(dbh[i])) {
      warning("dbh is not numeric or positive for index:", i)
      next
    }

    # Lookup species data from code
    rec <- sp_lookupdf[sp_lookupdf$short == spcode[i], ]
    tarifflokupcode <- rec$single
    type <- rec$type

    # Check if the species code was found
    if (nrow(rec) == 0) {
      warning("The spcode value was not found: ", spcode[i], "Index:", i)
      next
    }

    # Get tariff number depending on broadleaf or conifer
    if (type == "broadleaf") {
      tariff <- broadleaf_tariff(spcode[i], height[i], dbh[i], sigma_dbh = sigma_dbh, sigma_h = sigma_h)

    } else if (type == "conifer") {
      tariff <- conifer_tariff(spcode[i], height[i], dbh[i], sigma_dbh = sigma_dbh, sigma_h = sigma_h)
    }

    if (length(tariff) == 0) {
      stop("Error in", type, "_tariff function at index: ", i)
    }

    # Calculate volumes and biomass
    mercvol <- merchtreevol(dbh[i], tariff$tariff, sigma_dbh, as.numeric(tariff[2]))  # Merchantable tree volume
    stemvol <- treevol(mercvol$volume, dbh = dbh[i], mercvol$error)          # Stem volume
    woodbio <- woodbiomass(stemvol$stemvolume, rec$NSG, stemvol$error)       # Stem Biomass
    crownbio <- crownbiomass(rec$Crown, dbh[i], sigma_dbh)    # Crown Biomass
    AGB <- woodbio$woodbiomass + crownbio$biomass             # Above ground Biomass
    sigma_AGB <- sqrt(woodbio$error^2 + crownbio$error^2)
    AGC <- biomass2c(AGB, method=method, type, biome=biome, sigma_biomass = sigma_AGB) # Above ground Carbon
    r$AGC[i] <- AGC$AGC
    r$sig_AGC[i] <- AGC$sigma_AGC

    if(returnv == "All"){
      r$spcode[i] <- spcode[i]
      r$spname[i] <- rec$latin_name
      r$dbh[i]    <- dbh[i]
      r$height[i] <- height[i]
      r$NSG[i] <- rec$NSG
      r$tariff[i] <- tariff$tariff
      r$sig_tariff[i] <- tariff$error
      r$mercvol[i] <- mercvol$volume
      r$sig_mercvol[i] <- mercvol$error
      r$stemvol[i] <- stemvol$stemvolume
      r$sig_stemvol[i] <- stemvol$error
      r$stembiomass[i] <- woodbio$woodbiomass
      r$sig_stembiomass[i] <- woodbio$error
      r$crownbiomass[i] <- crownbio$biomass
      r$sig_crownbiomass[i] <- crownbio$error

      # Root Biomass
      rootbio <- rootbiomass(rec$Root, dbh[i], sigma_dbh)
      r$rootbiomass[i] <- rootbio$rootbiomass
      r$sig_rootbiomass[i] <- rootbio$error
    } else {
      # TODO
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
#' @param sigma_vol sigma for volume
#' @param sigma_den sigma for wood density
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
pro_error_carbon <- function(vol,sigma_vol,den,sigma_den,biom,biomsd,nruns=10000,
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
                        data(bunce), therefore, the combined coefficients
                           will be used")}

  biomass <- coeffs$a + coeffs$b*log(pi*dbh)

  return(exp(biomass))
}
