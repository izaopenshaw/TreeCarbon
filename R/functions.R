############## Functions file for Woodland Carbon Code ########################

############ Tariff number from volume and tree basal area (Eq 1) ############
#'
#' @title Tariff number from volume and basal area
#' @description Using the sample tree’s basal area and volume to calculate the
#' tariff number. Basal area is calculated by ba = (pi * dbh^2)/40000.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param vol tree volume in metres cubed
#' @param dbh diameter at breast height in centimetres
#' @returns  Tariff number
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). (Equation 1)
#'
tariff_vol_area <- function(vol, dbh){
  if (!is.numeric(vol) || any(vol < 0) || !is.numeric(dbh) || any(dbh < 0)) {
    stop("vol and dbh must be non-negative numeric values")
  }
  ba <- (pi * dbh^2)/40000                      # tree basal area in m^2
  a1 <- (vol - 0.005002986)/(ba - 0.003848451)
  tariff <- (3.174106384 * a1) + 0.138763302
  return(tariff)
}

############# FC conifer tree tariff number (Eq 3) ############################
#'
#' @title Conifer tree tariff number
#' @description Use DBH and tree height to calculate the tariff number of each
#'  sample tree. Species-specific estimates of a1 – a3 are found in the
#'  R data file, 'tariff_coniferdf'.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param dbh diameter at breast height in centimetres
#' @param spcode species code
#' @returns  tariff number
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom utils data
#' @export
#'
conifer_tariff <- function(spcode, height, dbh) {
  if (!is.numeric(height) || !is.numeric(dbh) || height < 0 || dbh < 0) {
    stop("height and dbh must be non-negative numeric values")
  }
  utils::data(tariff_coniferdf, envir = environment())
  rec <- tariff_coniferdf[tariff_coniferdf$abbreviation == spcode, ]
  if(nrow(rec) == 0)stop("The specified 'spcode' is not found in tariff_coniferdf.rda")

  tariff <- rec$a1 + (rec$a2 * height) + (rec$a3 * dbh)
  return(tariff)
}


############# FC broadleaf tree tariff number (Eq 2) ##########################
#'
#' @title Carbon tariff number for broadleaf tree
#' @description Use DBH and tree height to derive the tariff number of each
#' sample tree. Species-specific estimates of a1 – a4 are found in the
#' R data file, 'tariff_broaddf'.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in meters
#' @param dbh diameter at breast height in centimetres
#' @param spcode species code
#' @param height_sd tree height sigma (optional)
#' @param dbh_sd diameter at breast height sigma (optional)
#' @returns  tariff number and error if sigma of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Method B, Equation 2.
#' @importFrom utils data
#' @export
#' @examples broadleaf_tariff(spcode = 'OK', height = 25, dbh = 1.5)
#' 55.96704
#'
broadleaf_tariff <- function(spcode, height, dbh, dbh_sd = NA, height_sd = NA) {
  if(!is.numeric(dbh) || any(dbh<0))stop("dbh must be numeric and positive")
  if(!is.numeric(height) || any(height<0))stop("height must be numeric and positive")

  utils::data(tariff_broaddf, envir = environment())
  tb <- tariff_broaddf[tariff_broaddf$abbreviation == spcode, ]
  tariff <- tb$a1 + (tb$a2 * height) + (tb$a3 * dbh) + (tb$a4 * dbh * height)

  if(!is.na(dbh_sd) | !is.na(height_sd)){
    if(!is.numeric(dbh_sd) || any(dbh_sd<0))stop("dbh_sd must be numeric and positive")
    if(!is.numeric(height_sd) || any(height_sd<0))stop("height_sd must be numeric and positive")

    error <- sqrt(2.085826^2 +
                error_product(tb$a3, 0.05184218, dbh, dbh_sd) +
                error_product(tb$a2, 0.3908223, height, height_sd) +
                error_product(tb$a4, 0.01108647, dbh, dbh_sd, height, height_sd)
                )
    return(c(tariff, error))
  } else {
    return(tariff)
  }
}

############# FC tariff number by stand height (Eq 4) ################
#'
#' @title Tariff number by stand height
#' @description Use the estimated stand top height to calculate the stand
#' tariff number.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param spcode species code
#' @returns  tariff number
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#'
stand_tariff <- function(spcode, height) {
  if(!is.character(spcode))stop("spcode must be a character")
  if(!is.numeric(height) || any(height<0))stop("height must be numeric and positive")

  utils::data(tarif2heightdf, envir = environment())
  rec <- tarif2heightdf[tarif2heightdf$abbreviation == spcode, ]

  if(nrow(rec)==0){stop("The species code, 'spcode' is not found in data(tarif2heightdf)")}

  tariff <- rec$a1 + (rec$a2 * height) + (rec$a3 * height^2)
  return(tariff)
}

############# FC tree merchantable volume (Eq 5) ################
#'
#' @title Forestry merchantable volume
#' @description Use the tree tariff number and DBH to estimate the mean
#' merchantable tree volume.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param tariff tree or stand tariff number
#' @param dbh diameter at breast height in centimetres
#' @param tariff_sd tariff sigma (optional)
#' @param dbh_sd diameter at breast height sigma (optional)
#' @returns  volume metres cubed and error if sigma of variables inputted
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#'
merchtreevol <- function(dbh, tariff, dbh_sd = NA, tariff_sd = NA) {
  if(is.na(tariff)||!is.numeric(tariff)||any(tariff<0))
    stop("tariff must be numeric and positive")
  if(is.na(dbh)   ||!is.numeric(dbh)   ||any(dbh<0))
    stop("dbh must be numeric and positive")

  ba <- (pi * dbh^2) / 40000
  a2 <- 0.315049301 * (tariff - 0.138763302)
  a1 <- (0.0360541 * tariff) - (a2 * 0.118288)
  vol <- a1 + (a2 * ba)
  if (vol < 0) {
    vol <- 0
  }
  if(is.na(dbh_sd)) {
    return(vol)
  } else {
    ba_sd <- (pi * dbh / 20000) * dbh_sd
    a2_sd <- 0.315049301 * tariff_sd
    a1_sd <- sqrt(
      (0.0360541 - 0.315049301 * 0.118288)^2 * tariff_sd^2 +
        (0.118288 * a2_sd)^2
    )
    error <- sqrt(
      a1_sd^2 +
        (ba * a2_sd)^2 +
        (a2 * ba_sd)^2
    )

    return(c(vol, error))
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
#' @param mtreevol_sd sigma for mtreevol (optional)
#' @returns  volume metres cubed or if mtreevol_sd is provided then additionally
#'  returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#'
treevol <- function(mtreevol, dbh, mtreevol_sd = NA) {

  if(!is.numeric(dbh) || any(dbh<0))stop("dbh must be numeric and positive")
  if(!is.numeric(mtreevol) || any(mtreevol<0))stop("mtreevol must be numeric and positive")

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

  if(is.na(mtreevol_sd)){
    return(stemvol)
  } else {
    if(!is.numeric(mtreevol_sd) || any(mtreevol_sd<0))stop("mtreevol_sd must be numeric and positive")
    error <- error_product(1, 0.01794557, mtreevol, mtreevol_sd)
    return(c(stemvol, error))
  }
}

############# Error for product ################
#' @title Analytical error progression for product
#' @description Calculates the error for x when x = a * b or x = a * b * c
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param a first variable in product
#' @param a_sd sigma for a
#' @param b second variable in product
#' @param b_sd sigma for b
#' @param c (optional) third variable in product
#' @param c_sd (optional) sigma for c
#' @returns error for x = (abs(a * b) * sqrt((a_sd / a)^2 + (b_sd / b)^2))^2
#' or (abs(a * b * c) * sqrt((a_sd / a)^2 + (b_sd / b)^2 + (c_sd / c)^2))^2
#' @references *todo
#'
error_product <- function(a, a_sd, b, b_sd, c=NA, c_sd=NA) {
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(a_sd) || !is.numeric(b_sd)) {
    stop("inputs must be numeric")}

  if (is.na(c)) {
    sigmasquared <- (a * b * sqrt((a_sd / a)^2 + (b_sd / b)^2))^2
  } else {
    if (!is.numeric(c) || !is.numeric(c_sd)) {
      stop("c and c_sd must be numeric")
    }

    sigmasquared <- (a * b * c * sqrt((a_sd / a)^2 + (b_sd / b)^2 + (c_sd / c)^2))^2
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
#' @param treevol_sd tree volume in metres sigma (optional)
#' @returns  biomass in oven dry tonnes or if treevol_sd is provided then
#' additionally returns the error as a list
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Lavers, G.M. and Moore, G.L.
#' (1983) The strength properties of timber. Building Research Establishment
#' Report CI/Sfb i(J3). Building Research Establishment, Garston.
#'
woodbiomass <- function(treevol, nsg, treevol_sd = NA) {

  if(!is.numeric(treevol) || any(treevol<0))stop("treevol must be numeric and positive")
  if(!is.numeric(nsg) || any(nsg<0))stop("nsg must be numeric and positive")

  woodbio <- treevol * nsg

  if(is.na(treevol_sd)){
    return(woodbio)
  } else {
    error <- error_product(treevol, treevol_sd, nsg, 0.08222824)
    return(c(woodbio, error))
  }

}

############# FC crown biomass (Eq 6 & 7) ################
#'
#' @title Forestry commission crown biomass estimates
#' @description  Function to find crown biomass (composed of branches,
#' stem tips and foliage) depending on species and DBH
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh diameter at breast height in centimetres
#' @param spcode Crown biomass species code, crown_biomasdf$Code which
#' relates to sp_lookupdf$Crown
#' @returns  biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.2.
#' @importFrom utils data
#'
crownbiomass <- function(spcode, dbh) {
  if(!is.numeric(dbh) || dbh < 0)stop("Argument 'dbh' must be numeric and non-negative")
  if(dbh < 7){warning("equation is only specifed for dbh equal to or greater than 7")}

  utils::data(crown_biomasdf, envir = environment())
  rec <- crown_biomasdf[crown_biomasdf$Code == spcode, ]
  if(nrow(rec)==0){
    stop("The species code, 'spcode' is not found in data(crown_biomasdf),
         see data(sp_lookupdf) column 'Crown'")}

  if (dbh <= 50) {
    crownbiomass <- rec$b1 * dbh^rec$p
  } else {
    crownbiomass <- rec$A + rec$b2 * dbh
  }
  return(crownbiomass)
}

############# FC Root Biomass (Eq 8 & 9) ################
#'
#' @title Forestry commission root biomass estimates
#' @description Function to calculate the root biomass depending on species and DBH
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param dbh diameter at breast height (1.3 m) in centimetres
#' @param spcode species code
#' @returns biomass (oven dry tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018). Section 5.2.3.

rootbiomass <- function(spcode,dbh){
  if(!is.numeric(dbh) || dbh < 0)stop("Argument 'dbh' must be numeric and non-negative")

  # data(root_biomassdf)
  rec <- root_biomassdf[root_biomassdf$Code == spcode,]
  if(nrow(rec)==0){stop("The species code, 'spcode' is not found in
                        data(root_biomassdf), see data(sp_lookupdf) column 'Root'")}

  if (dbh <= 30) {
    root.biomass <- rec$b1 * dbh^2.5
  } else{
    root.biomass <- rec$a + rec$b2 * dbh}
  return (root.biomass)
}
############# Carbon to CO2e ################
#'
#' @title Carbon to CO2 equivalent
#' @description Function to convert from carbon to carbon dioxide equivalent
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param carbon carbon
#' @returns carbon dioxide equivalent
#'
c2co2e <- function(carbon){
  if(!is.numeric(carbon) || carbon<0)stop("carbon must be numeric and positive")

  carbon * 44/12
}

############# Plant biomass conversion to carbon ################
#'
#' @title Calculates biomass conversion to carbon
#' @description Converts biomass to carbon values using the carbon volatile
#'  fraction from chosen method/citation.
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param biomass (usually kg or metric tonnes)
#' @param method Method of calculating the carbon volatile fraction.
#' Matthews1 - Simplest with the carbon volatile fraction, CVF = 50% (Matthews 1993)
#' Matthews2 - CVF based on type (broadleaf or conifer)
#' IPCC1 - CVF = 47.7% (IPCC 2006)
#' IPCC2 - Lookup CVF by type and biome
#' Thomas1 - CVF = 48.3% and 95% CI of 0.3% (Thomas and Martin 2012)
#' Thomas2 - Lookup by type and biome
#' @param type broadleaf or conifer. Only required for method = 'Matthews2',
#' 'IPCC2' or 'Thomas'
#' @param biome tropical, subtropical, mediterranean, temperate or boreal.
#' Only needed for 'IPCC2' and 'Thomas' methods.
#' @param return either 'carbon' = just carbon value or 'error' = list of carbon
#'  value with error. Only associated errors with method = 'IPCC2' and 'Thomas'
#' @return either carbon value or list of carbon value with error
#' @references (1) Thomas, Sean C., and Adam R. Martin. "Carbon content of tree
#' tissues: a synthesis." Forests 3.2 (2012): 332-352.
#'  https://www.mdpi.com/1999-4907/3/2/332.
#' (2) IPCC. Forest lands. Intergovernmental Panel on Climate Change Guidelines
#'  for National Greenhouse Gas Inventories; Institute for Global Environmental
#'   Strategies (IGES): Hayama,Japan, 2006; Volume 4, p. 83.
#' (3) Matthews, G.A.R. (1993) The Carbon Content of Trees. Forestry Commission
#'  Technical Paper 4. Forestry Commission, Edinburgh. 21pp. ISBN: 0-85538-317-8
#'
biomass2c <- function(biomass, method, type, biome, return="carbon") {
  # Check arguments
  if (!is.numeric(biomass) || biomass < 0) {
    stop("biomass value must be numeric and positive")
  }
  valid_methods <- c("Matthews1", "Matthews2", "IPCC1", "IPCC2", "Thomas")
  if (!(method %in% valid_methods)) {
    stop("Invalid method. Choose from: 'Matthews1', 'Matthews2', 'IPCC1',
         'IPCC2', 'Thomas'. See R helpfile.")
  }
  valid_types <- c("broadleaf", "conifer")
  if ((method %in% c("Matthews2", "IPCC2", "Thomas")) && !missing(type) &&
      !(type %in% valid_types)) {
    stop("Invalid type. Choose from: 'broadleaf', 'conifer'")
  }
  valid_biomes <- c("tropical", "subtropical", "mediterranean", "temperate",
                    "boreal")
  if ((method %in% c("IPCC2", "Thomas")) && !missing(biome) &&
      !(biome %in% valid_biomes)) {
    stop("Invalid biome. Choose from: 'tropical', 'subtropical',
         'mediterranean', 'temperate', 'boreal'")
  }

  # Specify carbon volatile fraction based on method
  if (method == "Matthews1") {       CVF <- 50}

  if (method == "Matthews2") {
    if (type == "broadleaf") {       CVF <- 49.9
    } else if (type == "conifer") {  CVF <- 49.8
    }
  }

  if (method == "IPCC1") {           CVF <- 47.7}

  if (method == "IPCC2") {
    if (biome == "tropical" || biome == "subtropical") {CVF <- 47; error <- 0.05
    } else if (biome == "temperate" || biome == "boreal") {
      if (type == "broadleaf") {     CVF <- 48 ; error <- 0.04
      } else if (type == "conifer") {CVF <- 51 ; error <- 0.08
      }
    }
  }

  if (method == "Thomas") {
    if (biome == "tropical") {
      if (type == "broadleaf") {     CVF <- 47.1 ; error <- 0.4
      } else if (type == "conifer") {CVF <- 49.3
      }
    } else if (biome == "subtropical" || biome == "mediterranean") {
      if (type == "broadleaf") {     CVF <- 48.1 ; error <- 0.9
      } else if (type == "conifer") {CVF <- 50.54 ; error <- 2.8
      }
    } else if (biome == "temperate" || biome == "boreal") {
      if (type == "broadleaf") {     CVF <- 48.8 ; error <- 0.6
      } else if (type == "conifer") {CVF <- 50.8 ; error <- 0.6
      }
    }
  }
  error <- NA # If error isn't specified

  # Return carbon value
  AGC <- biomass * CVF
  if(return == "carbon"){return(AGC)
    } else {return(c(AGC, error))}
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
#'
con_sap_seedling2C <- function(heightincm){
  # data(seedlings_conifer)
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
#'
broad_sap_seedling2C <- function(heightincm){
  #get first and last
  #note max is 1000
  #min is 1 cm
  # data(seedlings_broad)
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


############# Above Ground Carbon ################
#'
#' @title Lookup species code
#' @description  Function that looks up species codes for Woodland Carbon Code
#' @author Isabel Openshaw I.Openshaw@kew.org
#' @param name name of species
#' @param name_type either 'botanical' or 'common'
#' @param classification either 'broadleaf' or 'conifer'
#' @param returnv either 'short', 'single', 'stand', 'root' from sp_lookupdf.Rda
#' @returns Species code
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @importFrom stringr word
#'
lookspcode <- function(name, name_type="botanical", classification, returnv="short"){
  # ** to check inputs

  if(name_type == "common"){
    rec <- sp_lookupdf[sp_lookupdf$common_name == name,]
    if(nrow(rec) == 0){    # if species is not found, search genus
      rec <- sp_lookupdf[sp_lookupdf$General.for.genus == word(name, 1),]
      if(nrow(rec) == 0){   # if genus not found, get general code for broadleaf/conifer
        rec <- sp_lookupdf[sp_lookupdf$General.for.classification == classification,]
      }
    }
  } else {
    # Search genus and species
    rec <- sp_lookupdf[sp_lookupdf$latin_name == name,]
    # if species is not found, search genus
    if(nrow(rec) == 0){
      rec <- sp_lookupdf[sp_lookupdf$General.for.genus == stringr::word(name, 1),]
      # if genus not found, get general code for broadleaf/conifer
      if(nrow(rec) == 0){
        rec <- sp_lookupdf[sp_lookupdf$General.for.classification == classification,]
      }
    }
  }

  if(returnv == "short"){spcode <- rec$short}
  else if (returnv == "single"){spcode <- rec$single}
  else if(returnv == "stand"){spcode <- rec$stand}
  else if(returnv == "root"){spcode <- rec$Root}

  return(spcode)
}

############# Above Ground Carbon ################
#'
#' @title Calculate above ground carbon
#' @description  Function that inputs tree species code, DBH, height and method
#' for converting biomass to carbon, and returns the carbon estimate
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param spcode species code
#' @param DBH diameter at breast height in centimetres
#' @param height in metres
#' @param method method of converting biomass to carbon. See biomass2c function
#' @param biome tropical, Subtropical, Mediterranean,Temperate, Boreal or all
#' @param returnv To return either 'AGC' [default] or 'All'
#' @returns either Above ground carbon, AGC in tonnes, or a list of tariff
#' number, merchantable volume (metres cubed), stem volume (metres cubed),
#' stem biomass (tonnes), stem carbon (tonnes), canopy carbon (tonnes) and
#' root carbon (tonnes)
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#'
fc_agc <- function(spcode, DBH, height, method = "Matthews1", biome,
                   returnv = "AGC"){

  # Check arguments
  if(length(spcode) != length(DBH) || length(spcode) != length(height) ||
     length(height) != length(DBH))stop("input lengths must be the same")
  if(!is.character(spcode))stop("spcode must be a character")
  if(!is.numeric(DBH) || any(DBH<0))stop("DBH must be numeric and positive")
  if(!is.numeric(height) || any(height<0))
    stop("height must be numeric and positive")

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
  #if(n != length(DBH) || n != length(height) || length(DBH) != length(height)) {
  #  stop("spcode, DBH, and height must be of the same length")}

  # Create results table
  r <- data.frame(spcode=NA, DBH=NA, height=NA, tariff=NA, mercvol=NA, stemvol=NA,
                  stembiomass=NA, crownbiomass=NA, rootbiomass=NA, AGC=NA, stringsAsFactors=FALSE)
  r <- r[1:n,]

  # Loop over all species
  for (i in 1:n) {
    # Lookup species data from code
#    data(sp_lookupdf, envir = environment())
    rec <- sp_lookupdf[sp_lookupdf$short == spcode[i], ]

    tarifflokupcode <- rec$single
    type <- rec$type

    # Check if the species code was found
    if (nrow(rec) == 0) {
      warning("The spcode value was not found: ", spcode[i])
      next
    }

    # Get tariff number depending on broadleaf or conifer
    if (type == "broadleaf") {
      r$tariff[i] <- broadleaf_tariff(tarifflokupcode, height[i], DBH[i])
    } else if (type == "conifer") {
      r$tariff[i] <- conifer_tariff(tarifflokupcode, height[i], DBH[i])
    }

    # Calculate volumes and biomass
    r$mercvol[i] <- merchtreevol(r$tariff[i], DBH[i])      # Merchantable tree volume
    r$stemvol[i] <- treevol(r$mercvol[i], DBH[i])          # Stem volume
    r$stembiomass[i] <- woodbiomass(r$stemvol[i], rec$NSG) # Stem Biomass
    r$crownbiomass[i] <- crownbiomass(rec$Crown, DBH[i])   # Crown Biomass
    AGB <- r$stembiomass[i] + r$crownbiomass[i]               # Above ground Biomass
    r$AGC[i] <- biomass2c(AGB,method=method,type,biome=biome) # Above ground Carbon

    if(returnv != "AGC"){
      r$rootbiomass <- rootbiomass(rec$Root, DBH[i])       # Root Biomass
      r$spcode[i] <- spcode[i]
      r$DBH[i]    <- DBH[i]
      r$height[i] <- height[i]
    }
  }
  if (returnv == "AGC") {
    return(r$AGC)
  } else {
    return(r)
  }
}

############# Progression of errors ###########
#'
#' @title Carbon progression of errors
#' @description Progression of errors through monte carlo simulation
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param volsd volume sd
#' @param densd wood density sd
#' @param biomsd biomass sd
#' @param nruns number of iteration, suggest 10,000 as min and 100,000 is a good number
#' @param returnv if null then mean and sd is returned else vector of
#' quantiles ie c(5,50,95)/100 will return 5%, mean and 95% quantiles.
#' @returns  either vector of mean and sd or vector of quantiles
#' @references todo**
#' @importFrom stats quantile rnorm sd

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
pro_error_carbon <- function(vol,volsd,den,densd,biom,biomsd,nruns=10000,
                             returnsv=NULL) {
  vol <- stats::rnorm(nruns,mean=vol,sd=volsd)
  den <- stats::rnorm(nruns,mean=den,sd=densd) # middle of the road
  biomass <- stats::rnorm(nruns,mean=biom,sd=biomsd) # conifer or angiosperm
  carbt <- vol * den * biomass
  if (!is.null(returnsv)){
    stats::quantile(carbt,probs=returnsv)
  } else {
    c(mean = mean(carbt),sd= stats::sd(carbt))
  }
}
#AGB = 0.0673 * (WD * H * D^2)^0.976

######################### Bunce Eq ########################
#'
#' @title Bunce biomass equation
#' @description Calculates dry weight based on species and DBH
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param spcode species code
#' @param dbh diameter at breast height
#' @returns  biomass in kg
#' @references Bunce, R. G. H. "Biomass and Production of Trees in a Mixed
#' Deciduous Woodland: I. Girth and Height as Parameters for the Estimation of
#' Tree Dry Weight" (1968)
#'
#' @importFrom utils data
#'
bunce <- function(spcode,dbh){

  if(!is.numeric(dbh) || dbh < 0)stop("Argument 'dbh' must be numeric and non-negative")

  data("bunce", package = "WoodlandCarbonCode")
  coeffs <- buncedf[buncedf$spcode == spcode,]

  if(nrow(coeffs)==0){warning("The species code, 'spcode' is not found in
                        data(bunce), therefore, the combined coefficients
                           will be used")}

  biomass <- coeffs$a + coeffs$b*log(pi*dbh)

  return(exp(biomass))
}
