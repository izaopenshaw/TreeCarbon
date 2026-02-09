# ==============================================================================
# TreeCarbon - Core Carbon Conversion Functions
# ==============================================================================
#
# This module provides core utility functions for carbon calculations,
# including biomass-to-carbon conversion using multiple methodologies.
#
# Functions included:
#   - Carbon to CO2 equivalent conversion (ctoco2e)
#   - Biomass to carbon conversion (biomass2c)
#   - Carbon fraction lookup by method/type/biome
#
# Authors: Justin Moat (J.Moat@kew.org), Isabel Openshaw (I.Openshaw@kew.org)
#
# References:
#   - Matthews, G.A.R. (1993). The Carbon Content of Trees. FC Technical Paper 4.
#   - IPCC (2006). Guidelines for National Greenhouse Gas Inventories.
#   - Thomas, S.C. & Martin, A.R. (2012). Carbon content of tree tissues.
#     Forests, 3(2), 332-352. https://doi.org/10.3390/f3020332
#
# ==============================================================================
# DEVELOPMENT NOTES / TODO:
# =============================================================================
#  - if input MX then tariffs uses the average of broadleaf and conifer?
#  - could change: if type is not inputted into biomass2c with method
#    requiring type then default to type = IPCC1: 47%
#  - error for ctoco2e?
#  - todo error for nsg
#  - search not found and check that intermediate species are found in lookup_df
#  - single crown root dont have a code for Mixed species
#  - species specific sd for nsg?
#  - app1: to select between carbon calculation method?
# =============================================================================

############# Carbon to CO2e Conversion function ################
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

############# Plant Biomass to Carbon Conversion ################
#' @title Convert Biomass to Carbon
#' @description Converts biomass values to carbon values using the carbon
#' fraction (CF) from the chosen method or reference.
#'
#'   When \code{rich_output = TRUE}, returns a comprehensive result object
#'   including conversion method metadata, assumptions, and uncertainty
#'
#' @author Justin Moat <J.Moat@kew.org>, Isabel Openshaw <I.Openshaw@kew.org>
#' @param biomass Numeric vector, representing biomass values (typically in kg
#' or metric tonnes).
#' @param method Character. Method defining the carbon volatile fraction (CF).
#' Supported methods:
#' \itemize{
#'   \item `"Matthews1"`: Simplest, CF = 50 percent (Matthews, 1993).
#'   \item `"Matthews2"`: CF based on type (broadleaf or conifer).
#'   \item `"IPCC1"`: CF = 47.7 percent (IPCC, 2006).
#'   \item `"IPCC2"`: Lookup CF by type and biome.
#'   \item `"Thomas"`: Lookup by type and biome (Thomas & Martin, 2012).
#' }
#' @param type Character vector. `"broadleaf"` or `"conifer"`. Required for
#' `"Matthews2"`, `"IPCC2"`, or `"Thomas"`.
#' @param biome Character vector. Biome classification, required for `"IPCC2"`
#' and `"Thomas"` methods. Accepted values: `"tropical"`, `"subtropical"`,
#' `"mediterranean"`, `"temperate"`, or `"boreal"`.
#' @param sig_biomass Numeric vector. Biomass uncertainty (optional, only used
#' with `"IPCC2"` and `"Thomas"` methods).
#' @param rich_output Logical. If TRUE, returns a rich result object with
#'   metadata including: value, method, reference, assumptions, and uncertainty.
#'   Default FALSE for backwards compatibility.
#' @return If \code{rich_output = FALSE}: Numeric vector of carbon values. If
#'   `sig_biomass` is provided, returns a data frame with columns `"AGC"` and
#'   `"sig_AGC"`.
#'   If \code{rich_output = TRUE}: a list with carbon values and conversion metadata.
#'
#' @references
#' Thomas, S.C., & Martin, A.R. (2012). Carbon content of tree tissues: A synthesis.
#' Forests, 3(2), 332-352. \doi{10.3390/f3020332}
#'
#' IPCC. (2006). Forest lands. Intergovernmental Panel on Climate Change Guidelines
#' for National Greenhouse Gas Inventories, Volume 4, p. 83.
#'
#' Matthews, G.A.R. (1993). The Carbon Content of Trees. Forestry Commission Technical
#' Paper 4, Forestry Commission, Edinburgh, 21 pp. ISBN: 0-85538-317-8.
#'
#' @examples
#' # Basic conversion using IPCC2 method
#' biomass2c(1, method = "IPCC2", type = "conifer", biome = "temperate")
#'
#' # Vectorized conversion with uncertainty
#' biomass2c(biomass=c(0.5, 0.75, 2, 7), method = "IPCC2",
#' type = rep("broadleaf", 4), biome = "temperate", sig_biomass = rep(0.2, 4))
#'
#' # Rich output with conversion metadata
#' result <- biomass2c(1.5, "Thomas", "broadleaf", "temperate", rich_output = TRUE)
#' print(result)
#'
#' @importFrom utils globalVariables
#' @aliases biomass2c
#' @export
#'
biomass2c <- function(biomass, method, type = NULL, biome = 'temperate',
                      sig_biomass = NULL, rich_output = FALSE) {

  # ==== Conversion method metadata ====
  conversion_metadata <- list(
    Matthews1 = list(
      reference = "Matthews, G.A.R. (1993). The Carbon Content of Trees. Forestry Commission Technical Paper 4.",
      carbon_fraction = 0.50,
      assumptions = c("Simple 50% carbon fraction", "No distinction by tree type or biome"),
      source_type = "government publication"
    ),
    Matthews2 = list(
      reference = "Matthews, G.A.R. (1993). The Carbon Content of Trees. Forestry Commission Technical Paper 4.",
      carbon_fraction = "varies by type",
      assumptions = c("Carbon fraction varies by broadleaf/conifer", "Based on UK forestry data"),
      source_type = "government publication"
    ),
    IPCC1 = list(
      reference = "IPCC (2006). Forest lands. IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 4.",
      carbon_fraction = 0.477,
      assumptions = c("Default 47.7% carbon fraction", "IPCC Tier 1 approach"),
      source_type = "intergovernmental guidelines"
    ),
    IPCC2 = list(
      reference = "IPCC (2006). Forest lands. IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 4.",
      carbon_fraction = "varies by type and biome",
      assumptions = c("Carbon fraction varies by tree type and biome", "IPCC Tier 2 approach"),
      source_type = "intergovernmental guidelines"
    ),
    Thomas = list(
      reference = "Thomas, S.C. & Martin, A.R. (2012). Carbon content of tree tissues: A synthesis. Forests, 3(2), 332-352.",
      carbon_fraction = "varies by type and biome",
      assumptions = c("Synthesis of empirical measurements", "Carbon fraction varies by type and biome", "Most detailed approach"),
      source_type = "peer-reviewed"
    )
  )

  # ===== Input validation =====
  valid_methods <- c("Matthews1", "Matthews2", "IPCC1", "IPCC2", "Thomas")
  if (!method %in% valid_methods) stop("Invalid method. Choose from: ",
                                       paste(valid_methods, collapse = ", "))

  flags <- character()

  # Check type given method
  if (method %in% c("Matthews2", "IPCC2", "Thomas")){
    if(is.null(type) || anyNA(type) || any(!type %in% c("broadleaf", "conifer"))) {
      warning ("Using method = 'IPCC1' for any trees where type is undefined
               (type is required for the chosen method)")
      flags <- c(flags, "Type_undefined_using_IPCC1_fallback")
    } else {
      if(length(type) != length(biomass))
        stop("'type' and 'biomass' must have the same length.")
    }

    # Check biome given method
    valid_biome <- c("tropical", "temperate", "subtropical", "boreal")
    if (method %in% c("IPCC2", "Thomas") && !(biome %in% valid_biome))
      stop("Invalid biome. Choose from: ", paste(valid_biome, collapse = ", "))
  }

  # Check biomass
  if(any(!is.numeric(biomass)))stop("biomass values must be numeric & positive")

  # Check sig_biomass
  sig_CF <- NULL
  if(!is.null(sig_biomass)){
    if(!is.numeric(sig_biomass))stop("sig_biomass must be numeric & positive")

    if(length(sig_biomass) != length(biomass))
      stop("'sig_biomass' and 'biomass' must have the same length.")
  }

  # ===== Get carbon fraction (CF) values =====
  # Subset conversion factor table for method
  CF_data <- CVF_df[CVF_df$method == method, ]

  # Replicate the CF for each input tree's biomass ready for multiplication
  if (method %in% c("IPCC1", "Matthews1")) {
    CF <- rep(CF_data$CVF, length(biomass)) / 100

  } else if (method == "Matthews2") {
    # Get index values depending on type
    index <- match(type, CF_data$type)

    # Change to a percentage
    CF <- CF_data$CVF[index] / 100

  } else {
    # Check biome
    if(!all(biome %in% CF_data$biome)) {
      stop("Invalid biome for the chosen method. Choose from: ",
           paste(unique(CF_data$biome), collapse = ", "))
    }
    CF_data <- CF_data[CF_data$biome == biome, ]
    index <- match(type, CF_data$type)
    CF <- CF_data$CVF[index] / 100
    sig_CF <- CF_data$confidence[index] / 100 / 1.96
  }
  # If CF is not matched, then use IPCC1
  IPCC1 <- CVF_df$CVF[CVF_df$method == "IPCC1"]/100
  if (length(CF) == 0) {
    CF <- IPCC1
    flags <- c(flags, "Using_IPCC1_fallback")
  } else {
    if (any(is.na(CF))) {
      flags <- c(flags, "Some_CF_values_using_IPCC1_fallback")
    }
    CF[is.na(CF)] <- IPCC1
  }

  # Calculate AGC (carbon)
  AGC <- biomass * CF

  # Propagate error if sig_biomass is provided
  sigma_AGC <- NULL
  if (!is.null(sig_biomass) && method %in% c("Thomas", "IPCC2")) {
    if (length(sig_biomass) != length(biomass)){
      stop("Length of sig_biomass must match biomass length.")
    }
    sigma_AGC <- error_product(biomass, sig_biomass, CF, sig_CF, returnv = "sigma")
  }

  # ==== Return rich output if requested ====
  if (rich_output) {
    meta <- conversion_metadata[[method]]

    result <- list(
      # Values
      carbon = AGC,
      uncertainty = sigma_AGC,
      carbon_fraction_used = CF,

      # Method info
      conversion_method = method,
      reference = meta$reference,
      source_type = meta$source_type,

      # Assumptions
      assumptions = meta$assumptions,

      # Flags
      flags = if (length(flags) > 0) flags else "None",

      # Inputs
      inputs = list(
        biomass = biomass,
        type = type,
        biome = biome
      )
    )
    class(result) <- c("biomass2c_result", "list")
    return(result)
  }

  # Standard return
  if (!is.null(sigma_AGC)) {
    return(data.frame(AGC = AGC, sig_AGC = sigma_AGC))
  } else {
    return(AGC)
  }
}

#' @title Print method for biomass2c result
#' @description Formatted display for biomass to carbon conversion results
#' @param x A biomass2c_result object
#' @param ... Additional arguments (unused)
#' @export
print.biomass2c_result <- function(x, ...) {
  cat("------------------- BIOMASS TO CARBON CONVERSION RESULT -------------------\n")

  cat(sprintf("CARBON ESTIMATE: %.4f (same units as biomass input)\n", sum(x$carbon)))
  if (!is.null(x$uncertainty)) {
    cat(sprintf("UNCERTAINTY: +/- %.4f (SD)\n", sqrt(sum(x$uncertainty^2))))
  }
  cat(sprintf("CARBON FRACTION USED: %.3f (%.1f%%)\n", mean(x$carbon_fraction_used),
              mean(x$carbon_fraction_used) * 100))
  cat(" \n")

  cat("--- CONVERSION METHOD ---\n")
  cat(sprintf("Method: %s\n", x$conversion_method))
  cat(sprintf("Reference: %s\n", x$reference))
  cat(sprintf("Source: %s\n", x$source_type))
  cat(" \n")

  cat("--- ASSUMPTIONS ---\n")
  for (i in seq_along(x$assumptions)) {
    cat(sprintf("  %d. %s\n", i, x$assumptions[i]))
  }
  cat(" \n")

  if (any(x$flags != "None")) {
    cat("--- FLAGS ---\n")
    for (f in x$flags) {
      cat(sprintf("  - %s\n", f))
    }
  }

  invisible(x)
}

############# Global Wood Density Lookup #############
#'
#' Retrieves wood density values (g/cm^3) from the Global Wood Density Database
#' based on binomial name and a specified region.
#' If no value exists for a species in a region, it calculates a mean based on available data from broader categories (global species mean, genus in region, genus globally, family in region, family globally).
#'
#' @param binomial Character vector of binomial names (genus species).
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
    region_wd <- WD_Zanne
  } else {
    region_wd <- WD_Zanne[WD_Zanne$Region == region, ]
  }

  # Lookup species-level wood density in the specified region
  match_idx <- match(binomial, region_wd$Binomial)
  wd <- region_wd$Wood.density[match_idx]
  sd <- region_wd$wd_sd[match_idx]

  # If missing, lookup species wd across the world
  missing <- is.na(wd)
  if (any(missing)) {

    global_region_wd <- WD_Zanne[WD_Zanne$Binomial %in% binomial[missing], ]
    wd[missing] <- tapply(global_region_wd$Wood.density,
                          global_region_wd$Binomial,
                          mean, na.rm = TRUE)[binomial[missing]]
    sd[missing] <- tapply(global_region_wd$wd_global_sd,
                          global_region_wd$Binomial,
                          mean, na.rm = TRUE)[binomial[missing]]

    # Lookup genus in region
    missing <- is.na(wd)
    if (any(missing)) {
      genus <- sapply(strsplit(binomial, " "), `[`, 1)
      genus_data <- region_wd[region_wd$genus %in% genus[missing], ]
      wd[missing] <- tapply(genus_data$Wood.density,
                            genus_data$genus,
                            mean, na.rm = TRUE)[genus[missing]]
      sd[missing] <- tapply(genus_data$wd_sd,
                            genus_data$genus,
                            mean, na.rm = TRUE)[genus[missing]]

      # Lookup genus across world
      missing <- is.na(wd)
      if (any(missing)) {
        global_genus_data <- WD_Zanne[WD_Zanne$genus %in% genus[missing], ]
        wd[missing] <- tapply(global_genus_data$Wood.density,
                              global_genus_data$genus,
                              mean, na.rm = TRUE)[genus[missing]]
        sd[missing] <- tapply(global_genus_data$wd_global_sd,
                              global_genus_data$genus,
                              mean, na.rm = TRUE)[genus[missing]]

        # Lookup family in region
        missing <- is.na(wd)
        if (any(missing)) {
          family <- WD_Zanne$Family[match(binomial, WD_Zanne$Binomial)]
          family_data <- region_wd[region_wd$Family %in% family[missing], ]
          wd[missing] <- tapply(family_data$Wood.density,
                                family_data$Family,
                                mean, na.rm = TRUE)[family[missing]]
          sd[missing] <- tapply(family_data$wd_sd,
                                family_data$Family,
                                mean, na.rm = TRUE)[family[missing]]

          # Lookup family across the world
          missing <- is.na(wd)
          if (any(missing)) {
            global_family_data <- WD_Zanne[WD_Zanne$Family %in% family[missing], ]
            wd[missing] <- tapply(global_family_data$Wood.density,
                                  global_family_data$Family,
                                  mean, na.rm = TRUE)[family[missing]]
            sd[missing] <- tapply(global_family_data$wd_global_sd,
                                  global_family_data$Family,
                                  mean, na.rm = TRUE)[family[missing]]

            missing <- is.na(wd)
            if(any(missing)){
              wd[missing] = mean(region_wd$Wood.density)
              sd[missing] = mean(region_wd$wd_sd)
            }
          }
        }
      }
    }
  }

  return(list(wd = wd, sd = sd))
}

############# Summarise Total per unit area with errors ############

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
#' variance, 'sigmasquared' or 'standarderror' for plot = TRUE.
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

  if (returnv == "standarderror") returnv <- "sigma"

  # Handle list input (multiple habitats/areas)
  if (is.list(input)) {
    # Check for dimension consistency - only for list input
    if (length(input) != length(area) || length(sigma_input) != length(sigma_area)) {
      stop("Mismatch in number of habitats/areas provided.")
    }
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
    # Vector input: sum all tree values and divide by area
    # Validate inputs
    if (length(input) != length(sigma_input)) {
      stop("'input' and 'sigma_input' must have the same length.")
    }
    if (length(area) != length(sigma_area)) {
      stop("'area' and 'sigma_area' must have the same length.")
    }

    total <- sum(input, na.rm = TRUE)
    sigma_total <- sqrt(sum(sigma_input^2, na.rm = TRUE))
    total_per_area <- total / area
    error_per_area <- error_product(total, sigma_total,
                                    area, sigma_area,
                                    fn = total_per_area,
                                    returnv = returnv)
  }

  if (plots) {
    n <- length(input)
    total_per_area <- mean(total_per_area, na.rm = TRUE)
    error_per_area <- sqrt(sum(error_per_area^2)) / sqrt(n)
  }

  return(list(total_per_area = total_per_area, error_per_area = error_per_area))
}

############# Standard Deviation for Area Measurement ##############

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
