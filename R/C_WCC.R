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
#' @author Isabel Openshaw I.Openshaw@kew.org
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
lookupcode <- function(name, type = NULL, code = "short", returnv = "all") {
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
  match_genus2 <- match(stringr::word(clean_name, 1), tolower(lookup_df$genus))

  # Create Result DataFrame
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
#' @param height tree height in metres
#' @param method method of converting biomass to carbon. Both 'Thomas' or 'IPCC2' require type.
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
#' @importFrom utils data
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
fc_agc <- function(name, dbh, height, type = NULL, method = "IPCC2", biome =
                     "temperate", output.all = TRUE, nsg = NULL,
                   rich_output = FALSE){

  # ==== Check arguments ====
  if(!is.character(name)) stop ("name must be a character")
  if (!is.logical(output.all) || length(output.all) != 1) {
    stop("'output.all' must be a single logical value (TRUE or FALSE).")
  }
  class <- c("broadleaf", "conifer")
  if(any(!type %in% c(class, NA, "NA"))){
    warning("type must equal either conifer, broadleaf or NA")
  }
  if (!(method %in% c("IPCC2", "Thomas"))) {
    stop("Invalid method. Choose from: 'IPCC2', 'Thomas'")
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

  # ==== Create results table ====
  if(output.all){
    r <- data.frame(name=name, type=type, spcode=spcodes$code,
                    matchtype=spcodes$matchtype, dbh=dbh, height=height,
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
    tariff <- tariffs(spcodes$code[tall_id], height[tall_id],
                      dbh[tall_id], type[tall_id])

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
      return(create_allometry_result(
        value = r$AGC_WCC_t[1],
        method = "WCC",
        measure = "AGC",
        unit = "t",
        uncertainty = NULL,
        validity_warnings = warnings_list,
        flags = flags,
        inputs = list(
          name = name,
          dbh = dbh,
          height = height,
          type = type,
          biome = biome
        )
      ))
    }

    # For multiple trees
    results_list <- lapply(seq_len(nrow(r)), function(i) {
      create_allometry_result(
        value = r$AGC_WCC_t[i],
        method = "WCC",
        measure = "AGC",
        unit = "t",
        uncertainty = NULL,
        validity_warnings = warnings_list,
        flags = flags,
        inputs = list(
          name = name[i],
          dbh = dbh[i],
          height = height[i],
          type = if (!is.null(type)) type[min(i, length(type))] else NULL,
          biome = biome
        )
      )
    })

    combined_df <- do.call(rbind, lapply(results_list, as.data.frame.allometry_result))
    combined_df$tree_id <- seq_len(nrow(combined_df))
    combined_df$name <- name

    result <- list(
      trees = results_list,
      summary_table = combined_df,
      n_trees = length(dbh),
      total_AGC_t = sum(r$AGC_WCC_t, na.rm = TRUE),
      mean_AGC_t = mean(r$AGC_WCC_t, na.rm = TRUE),
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
#' @param height tree height in metres
#' @param method method of converting biomass to carbon. Either 'Thomas' or 'IPCC2' as these specify the error associated with the carbon volatile fraction
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
#' @importFrom utils data
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
fc_agc_error <- function(name, dbh, height, type = NULL, method = "IPCC2", biome =
                           "temperate", output.all = TRUE, re_dbh = 0.05, re_h =
                           0.1, re = 0.025, nsg = NULL, sig_nsg = 0.09413391,
                         rich_output = FALSE){

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

  if (!(method %in% c("IPCC2", "Thomas"))) {
    stop("Invalid method. Choose from: 'IPCC2', 'Thomas'")
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
    carbon <- sap_seedling2C(height = height[small_id],
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
                       biome, sig_AGB[convert])
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

  # ==== Return rich output if requested ====
  if (rich_output) {
    if (length(dbh) == 1) {
      return(create_allometry_result(
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
          type = type,
          biome = biome,
          re_dbh = re_dbh,
          re_h = re_h
        )
      ))
    }

    # Multiple trees
    results_list <- lapply(seq_len(nrow(r)), function(i) {
      create_allometry_result(
        value = r$AGC_WCC_t[i],
        method = "WCC",
        measure = "AGC",
        unit = "t",
        uncertainty = r$sig_AGC[i],
        validity_warnings = warnings_list,
        flags = flags,
        inputs = list(
          name = name[i],
          dbh = dbh[i],
          height = height[i],
          type = if (!is.null(type)) type[min(i, length(type))] else NULL,
          biome = biome
        )
      )
    })

    combined_df <- do.call(rbind, lapply(results_list, as.data.frame.allometry_result))
    combined_df$tree_id <- seq_len(nrow(combined_df))
    combined_df$name <- name

    result <- list(
      trees = results_list,
      summary_table = combined_df,
      n_trees = length(dbh),
      total_AGC_t = sum(r$AGC_WCC_t, na.rm = TRUE),
      mean_AGC_t = mean(r$AGC_WCC_t, na.rm = TRUE),
      total_uncertainty = sqrt(sum(r$sig_AGC^2, na.rm = TRUE)),
      validation = validation,
      detailed_output = if (output.all) r else NULL
    )
    class(result) <- c("wcc_multi_result", "list")
    return(result)
  }

  return(r)
}

############# Seedlings and saplings to Carbon ################
#'
#' @title Seedlings and saplings to carbon
#' @description Calculate the total carbon content of tree seedlings
#' (below and above ground)
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw. I.Openshaw@kew.org
#' @param height tree height in metres
#' @param type 'conifer' or 'broadleaf'
#' @param re relative error of estimates (default = 0.025)
#' @param re_h relative error of height measurement (optional)
#' @return carbon in tonnes or if re_h provided then additionally the error
#' @note just uses simple linear relationship to get between measures
#' @references Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018)
#' @importFrom utils data head tail
#' @import remotes
#' @examples
#' sap_seedling2C(0.5, 'conifer')
#' sap_seedling2C(height = 9, type = 'broadleaf', re_h = 0.05)
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

