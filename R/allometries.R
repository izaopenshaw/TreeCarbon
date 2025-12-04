#############  Calculate Carbon/Biomass using different Allometries #############
#' @title Calculate above ground carbon
#' @description  Function that inputs tree species code, dbh, height and method
#' for converting biomass to carbon, and returns the carbon estimate
#' @author Justin Moat. J.Moat@kew.org, Isabel Openshaw I.Openshaw@kew.org
#' @param genus First part of species binomial
#' @param species Second part of species binomial
#' @param type either 'broadleaf' or 'conifer'
#' @param dbh diameter at breast height in centimetres
#' @param height in metres
#' @param method method of converting biomass to carbon. Either 'Thomas' or
#' 'IPCC2' has the error associated and "Matthews1", "Matthews2" or "IPCC1" do not.
#' @param region of the World. See ?getWoodDensity for the list of regions
#' @param biome temperate, boreal, mediterranean, tropical, subtropical or all
#' @param coords either a vector of coordinates of the site or a matrix of
#' coordinates for each tree of longitude and latitude
#' @param re_dbh relative measurement error for diameter at breast height,
#' single value
#' @param re_h relative error of height measurement, single value
#' @param sig_nsg sigma for nominal specific gravity (NSG) or wood density
#' @param re relative error of coefficients (default = 2.5%)
#' @param nsg nominal specific gravity. Optionally specified, else will use that
#'  given by the WCC
#' @param returnv either 'AGC' or 'AGB' for above ground carbon or biomass,
#' respectively
#' @param checkTaxo If TRUE then BIOMASS::correctTaxo will check spelling of
#' species name. This is included in output if there were modifications made.
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
#'  Woodland: I. Girth and height as Parameters for the Estimation of Tree Dry
#'  Weight" (1968)
#' @importFrom utils data
#' @examples
#' allometries("Quercus", "robur", 20, 10)
#' @export
#'
allometries <- function(genus, species, dbh, height, type = NULL, method ="IPCC2",
                        returnv = "AGC", region = "Europe", biome = "temperate",
                        coords = c(-0.088837,51.071610), re_dbh = 0.05,
                        re_h = 0.1, re = 0.025, nsg = NULL, sig_nsg = 0.09413391,
                        checkTaxo = FALSE){

  # ==== Input checks ====
  if (!is.character(genus) || !is.character(species))
    stop("genus and species must be character strings.")

  if (!is.numeric(dbh) | any(dbh < 0, na.rm = TRUE))
    stop("dbh must be numeric and positive")

  if (!is.numeric(height) | any(height < 0, na.rm = TRUE))
    stop("height must be numeric and positive")

  if (!is.numeric(coords) || length(coords) != 2) {
    stop("coords must be a numeric vector of length 2 (longitude, latitude).")}

  error_terms <- c(re_dbh, re_h, re, sig_nsg)
  if(anyNA(error_terms) || !is.numeric(error_terms) || any(error_terms < 0)){
    stop("Error terms (re_dbh, re_h, re, sig_nsg) must be postive and numeric.")}

  if (!is.character(region) || !is.character(biome)){
    stop("region and biome must be character strings.")}

  #### Check spelling ####
  if(checkTaxo){

    correct <- BIOMASS::correctTaxo(genus = genus, species = species, useCache = TRUE)

    # Proceed if any taxa were corrected
    modified_names <- any(correct$nameModified == "TRUE", na.rm = TRUE)
    if (modified_names) {

      # Store original and corrected values
      genus_input   <- genus
      species_input <- species
      genus         <- correct$genusCorrected
      species       <- correct$speciesCorrected
      name_modified      <- correct$nameModified
    }
  }

  # ==== Calculate AGB ====
  #### BIOMASS package ####
  bio <- suppressMessages(suppressWarnings(
    BIOMASS(dbh, height, genus, species, coords, region, output.all = TRUE)))

  #### Woodland Carbon Code ####
  name <- paste(genus, species)

  if(method %in% c("Thomas", "IPCC2")){ # These methods contain errors for biomass conversion
    WCC <- suppressWarnings(fc_agc_error(name, dbh, height, type, method,
                                         biome, TRUE, re_dbh, re_h, re, nsg, sig_nsg))
  } else if((method %in% c("Matthews1", "Matthews2", "IPCC1"))) {
    WCC <- suppressWarnings(fc_agc(name, dbh, height, type,
                                   method, output.all = TRUE))
  } else {
    stop("Invalid method specified. Chose from 'Thomas', 'IPCC2', 'Matthews1',
         'Matthews2', or 'IPCC1'.")
  }

  # New type variable: either looked up type from species name or type inputted
  type0 <- ifelse(is.na(WCC$type) | WCC$type == "any",
                  as.character(type), as.character(WCC$type))
  WCC$AGB_WCC_t <- WCC$crownbiomass_t + WCC$stembiomass_t

  #### allodb package ####
  allo <- allodb(dbh, genus, species, coords, TRUE)

  #### Bunce ####
  AGB_Bunce_kg <- suppressWarnings(Bunce(name, dbh, re_dbh, re))

  # ==== Convert Biomass to Carbon ====
  if(returnv == "AGC"){

    # Output one warning message for missing entries that will be skipped
    if (any(is.na(type0) | !type0 %in% c("broadleaf", "conifer"))) {
      warning("Skipping entries with undefined 'type', see biomass2c function.")
    }

    # Calculate carbon in tonnes
    suppressWarnings({
      bio_AGC <- biomass2c(bio$AGB_Biomass_kg*0.001, method, type0, biome)
      allo_AGC <- biomass2c(allo$AGB_allodb_kg*0.001, method, type0, biome, allo$allodb_sigma*0.001)
      bunce_AGC <- biomass2c(AGB_Bunce_kg$biomass*0.001, method, type0, biome, AGB_Bunce_kg$sigma)
    })

    # Output data frame
    df <- data.frame(genus, species, family = bio$Family, dbh, height,
                     allodb_C_t = allo_AGC$AGC,  allodb_C_sig = allo_AGC$sig_AGC,
                     biomass_C_t = bio_AGC,      biomass_C_sig = bio_AGC,
                     WCC_C_t = WCC$AGB_WCC_t,    WCC_C_sig = WCC$sig_AGC,
                     Bunce_C_t = bunce_AGC$AGC,  Bunce_C_sig = bunce_AGC$sig_AGC)

  } else {

    df <- data.frame(genus, species, family = bio$Family, dbh, height,
                     allodb_B_t = allo$AGB_allodb_kg/1000,  allodb_B_sig = allo$allodb_sigma/1000,
                     biomass_B_t = bio$AGB_Biomass_kg/1000, biomass_B_sig=bio$AGB_Biomass_kg/1000,
                     WCC_B_t = WCC$AGB_WCC_t,               WCC_B_sig = WCC$sig_AGC,
                     Bunce_B_t = AGB_Bunce_kg$biomass/1000, Bunce_B_sig = AGB_Bunce_kg$sigma/1000)

  }

  # If height was predicted add these columns
  if('height_pred' %in% names(bio)){
    df$input_height = bio$height_input
    df$pred_height = bio$height_pred
  }

  # If spelling was checked and modified, then add to output
  if(checkTaxo){
    if(modified_names){
      df$input_genus <- genus_input
      df$input_species <- species_input
      df$name_modified <- name_modified
    }
  }

  # If type not entered then provide WCC calculations (used in biomass2c fn)
  if(is.null(type) | anyNA(type)){
    df$type <- type0
    df$input_type <- type

  }

  return(df)

}

#############  Helper fn: calculate jitter width ########
#' Helper function to calculate jitter width based on data range
#' @description Calculates an appropriate jitter width for scatter plots based on the range of the data.
#' This prevents overplotting when multiple methods have similar values by adding small random offsets.
#' @param data_range Numeric value representing the range of the data (max - min)
#' @param default_width Default width to use if range is 0 or invalid (default: 0.1)
#' @param proportion Proportion of the data range to use for jitter width (default: 0.02, i.e., 2%)
#' @returns Numeric value representing the jitter width to use
#' @noRd
calculate_jitter_width <- function(data_range, default_width = 0.1, proportion = 0.02) {
  if (length(data_range) != 1 || is.na(data_range) || data_range <= 0) {
    return(default_width)
  }
  return(max(data_range * proportion, default_width))
}

#############  Helper fn: setup jitter positions ########
#' Helper function to set up jitter positions and error bar width
#' @description Sets up position adjustments for points and error bars based on whether jitter is enabled.
#' If jitter is enabled, calculates jitter width from data range and creates position_jitter objects.
#' If jitter is disabled, returns identity positioning and default error bar width.
#' @param data_range Numeric value representing the range of the data (max - min)
#' @param jitter Logical, whether to apply jitter (default: TRUE)
#' @param seed Random seed for jitter reproducibility (default: 123)
#' @returns List with three elements: point_position (position adjustment for points),
#'   error_position (position adjustment for error bars, or NULL if no jitter),
#'   and error_width (width of error bar caps)
#' @noRd
setup_jitter_positions <- function(data_range, jitter = TRUE, seed = 123) {
  if (jitter) {
    jitter_width <- calculate_jitter_width(data_range)
    return(list(
      point_position = position_jitter(width = jitter_width, seed = seed),
      error_position = position_jitter(width = jitter_width, seed = seed),
      error_width = jitter_width * 2
    ))
  } else {
    return(list(
      point_position = "identity",
      error_position = NULL,
      error_width = 0.1
    ))
  }
}

#############  Helper fn: add error bars ########
#' Helper function to add error bars to a ggplot
#' @description Adds error bars to a ggplot object if error data is available and show_errors is TRUE.
#' Checks for the presence of standard error (se) and ymin/ymax columns before adding error bars.
#' @param p ggplot object to which error bars will be added
#' @param plot_df Data frame containing the plotting data, must have columns: se (standard error), ymin, and ymax
#' @param show_errors Logical, whether to show error bars (if FALSE, returns plot unchanged)
#' @param position Position adjustment for error bars (e.g., position_jitter, position_dodge). If NULL, uses default positioning
#' @param width Width of error bar caps in plot units (default: 0.1)
#' @returns ggplot object with error bars added (if applicable), or the original plot if error bars cannot be added
#' @noRd
add_error_bars <- function(p, plot_df, show_errors, position = NULL, width = 0.1) {
  if (!show_errors) {
    return(p)
  }

  # Check if se column exists and has valid values
  if (!"se" %in% colnames(plot_df) || all(is.na(plot_df$se))) {
    return(p)
  }

  # Check if ymin/ymax exist and have valid values
  if (!"ymin" %in% colnames(plot_df) || !"ymax" %in% colnames(plot_df)) {
    return(p)
  }

  if (all(is.na(plot_df$ymin)) && all(is.na(plot_df$ymax))) {
    return(p)
  }

  # Build errorbar call - use linewidth (not size) for ggplot2 3.4.0+
  # Also set inherit.aes = FALSE and specify required aesthetics to avoid inheriting size from points
  errorbar_aes <- aes(ymin = ymin, ymax = ymax)

  if (is.null(position)) {
    p <- p + geom_errorbar(errorbar_aes, width = width, linewidth = 0.5, na.rm = TRUE)
  } else {
    p <- p + geom_errorbar(errorbar_aes, position = position, width = width, linewidth = 0.5, na.rm = TRUE)
  }

  return(p)
}

#############  Plot Allometries ################
#' @title Create comparison plots for allometry methods
#' @description Creates ggplot2 or plotly plots comparing different allometric methods (WCC, BIOMASS, allodb, Bunce).
#' Supports multiple plot types including bar charts, scatter plots, density plots, and residual plots.
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param results_df Data frame with results from allometries function. Must contain columns for method estimates
#'   (e.g., WCC_C_t, biomass_C_t, allodb_C_t, Bunce_C_t for AGC, or corresponding _B_t columns for AGB)
#'   and optionally error columns (e.g., WCC_C_sig, biomass_C_sig, etc.)
#' @param plot_type Character string specifying the type of plot. Options: "bar" (bar chart comparing methods),
#'   "dbh" (scatter plot against DBH), "height" (scatter plot against height), "tree_index" (scatter plot against tree index),
#'   "density" (density distribution plot), or "residual" (residual plot showing differences from mean)
#' @param returnv Character string, either "AGC" for above ground carbon or "AGB" for above ground biomass.
#'   The function will auto-detect if the specified type doesn't match available columns
#' @param show_errors Logical, whether to show error bars representing uncertainty estimates (default: TRUE)
#' @param interactive Logical, whether to return an interactive plotly plot instead of a static ggplot (default: FALSE)
#' @param log_scale Logical, whether to use logarithmic scale on y-axis. Only applies to scatter plots (dbh, height),
#'   not bar plots, density plots, or residual plots (default: FALSE)
#' @param color_scheme Character string specifying the color scheme. Options: "default" (ggplot2 default colors),
#'   "viridis" (viridis color palette), or "brewer" (RColorBrewer Set2 palette) (default: "default")
#' @param font_size Numeric value specifying the base font size for plot text in points (default: 12)
#' @param size_scale Character string or NULL. For scatter plots, specifies variable to map point size to:
#'   "Height" maps size to height values, "DBH" maps size to DBH values, or NULL/"none" for fixed size points (default: NULL)
#' @param theme ggplot2 theme function to apply to the plot. Can be any ggplot2 theme function
#'   (e.g., theme_classic, theme_gray, theme_light, theme_dark) or NULL to use default ggplot2 theme.
#'   If NULL, defaults to theme_minimal with the specified font_size (default: NULL)
#' @param x_min Numeric value, minimum x-axis value for axis limits (optional, default: NULL)
#' @param x_max Numeric value, maximum x-axis value for axis limits (optional, default: NULL)
#' @param y_min Numeric value, minimum y-axis value for axis limits (optional, default: NULL)
#' @param y_max Numeric value, maximum y-axis value for axis limits (optional, default: NULL)
#' @param jitter Logical, whether to apply jitter to points and error bars in scatter plots (dbh, height).
#'   Jitter helps prevent overplotting when multiple methods have similar values. The jitter width is calculated
#'   as 2% of the data range (x-axis), with a minimum of 0.1 units (default: TRUE)
#' @details
#' This function creates visualizations comparing estimates from different allometric methods. For scatter plots
#' (dbh, height), jitter can be applied to points and error bars to prevent overplotting when multiple
#' methods have similar values. The jitter width is calculated as 2% of the data range (x-axis), with a minimum
#' of 0.1 units. This helps distinguish overlapping points while maintaining the overall pattern of the data.
#'
#' Interactive plots (when interactive = TRUE) display hover tooltips with formatted values: species name (if available),
#' method name, value (3 decimal places), and additional context-specific information (DBH, Height, Tree index, etc.).
#'
#' The function will warn if log scale is selected but data contains values less than or equal to zero, as log scale
#' cannot display such values. It will also warn if some methods have no data available.
#' @returns ggplot or plotly object depending on the interactive parameter. Returns NULL with a warning if no data
#'   is available to plot or if no matching method columns are found in the results data frame.
#' @import ggplot2
#' @importFrom scales viridis_pal
#' @importFrom RColorBrewer brewer.pal
#' @examples
#' results <- allometries(c("Quercus","Quercus"), c("robur","robur"), c(20, 30), c(10, 15))
#' plot_allometries(results, "bar", "AGC", interactive = FALSE)
#' @export
#'
plot_allometries <- function(results_df, plot_type = "bar", returnv = "AGC",
                             show_errors = TRUE, interactive = FALSE,
                             log_scale = FALSE, color_scheme = "default",
                             font_size = 12, size_scale = NULL, theme = NULL,
                             jitter = TRUE, point_size = 2, x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL) {

  # ==== Input validation ====
  # Check results_df
  if (missing(results_df) || is.null(results_df)) {
    stop("'results_df' is required and cannot be NULL")
  }
  if (!is.data.frame(results_df)) {
    stop("'results_df' must be a data frame")
  }
  if (nrow(results_df) == 0) {
    stop("'results_df' must contain at least one row")
  }

  # Check plot_type
  valid_plot_types <- c("bar", "dbh", "height", "tree_index", "density", "residual")
  if (!is.character(plot_type) || length(plot_type) != 1 || !plot_type %in% valid_plot_types) {
    stop("'plot_type' must be one of: ", paste(valid_plot_types, collapse = ", "))
  }

  # Check returnv
  valid_returnv <- c("AGC", "AGB")
  if (!is.character(returnv) || length(returnv) != 1 || !returnv %in% valid_returnv) {
    stop("'returnv' must be either 'AGC' or 'AGB'")
  }

  # Check logical inputs
  if (!is.logical(show_errors) || length(show_errors) != 1) {
    stop("'show_errors' must be a single logical value (TRUE or FALSE)")
  }
  if (!is.logical(interactive) || length(interactive) != 1) {
    stop("'interactive' must be a single logical value (TRUE or FALSE)")
  }
  if (!is.logical(log_scale) || length(log_scale) != 1) {
    stop("'log_scale' must be a single logical value (TRUE or FALSE)")
  }
  if (!is.logical(jitter) || length(jitter) != 1) {
    stop("'jitter' must be a single logical value (TRUE or FALSE)")
  }
  if (!is.numeric(point_size) || length(point_size) != 1 || point_size <= 0) {
    stop("'point_size' must be a single positive numeric value")
  }

  # Check color_scheme
  valid_color_schemes <- c("default", "viridis", "brewer")
  if (!is.character(color_scheme) || length(color_scheme) != 1 || !color_scheme %in% valid_color_schemes) {
    stop("'color_scheme' must be one of: ", paste(valid_color_schemes, collapse = ", "))
  }

  # Check font_size
  if (!is.numeric(font_size) || length(font_size) != 1 || font_size <= 0) {
    stop("'font_size' must be a single positive numeric value")
  }

  # Check size_scale (optional) - convert NA/empty to NULL
  if (is.null(size_scale) || (length(size_scale) == 1 && (is.na(size_scale) || size_scale == "" || tolower(size_scale) == "none"))) {
    size_scale <- NULL
  } else {
    if (!is.character(size_scale) || length(size_scale) != 1) {
      stop("'size_scale' must be NULL, 'none', 'Height', or 'DBH'")
    }
    valid_size_scales <- c("height", "dbh")
    if (!tolower(size_scale) %in% valid_size_scales) {
      stop("'size_scale' must be NULL, 'none', 'Height', or 'DBH'")
    }
    # Normalize to title case
    size_scale <- if (tolower(size_scale) == "height") "Height" else "DBH"
  }

  # Check axis limits (optional) - convert NA/empty to NULL
  if (is.null(x_min) || (length(x_min) == 1 && (is.na(x_min) || x_min == ""))) {
    x_min <- NULL
  } else {
    if (!is.numeric(x_min) || length(x_min) != 1) {
      stop("'x_min' must be a single numeric value or NULL")
    }
  }

  if (is.null(x_max) || (length(x_max) == 1 && (is.na(x_max) || x_max == ""))) {
    x_max <- NULL
  } else {
    if (!is.numeric(x_max) || length(x_max) != 1) {
      stop("'x_max' must be a single numeric value or NULL")
    }
  }

  if (is.null(y_min) || (length(y_min) == 1 && (is.na(y_min) || y_min == ""))) {
    y_min <- NULL
  } else {
    if (!is.numeric(y_min) || length(y_min) != 1) {
      stop("'y_min' must be a single numeric value or NULL")
    }
  }

  if (is.null(y_max) || (length(y_max) == 1 && (is.na(y_max) || y_max == ""))) {
    y_max <- NULL
  } else {
    if (!is.numeric(y_max) || length(y_max) != 1) {
      stop("'y_max' must be a single numeric value or NULL")
    }
  }

  # Check that min < max if both are provided
  if (!is.null(x_min) && !is.null(x_max) && x_min >= x_max) {
    stop("'x_min' must be less than 'x_max'")
  }
  if (!is.null(y_min) && !is.null(y_max) && y_min >= y_max) {
    stop("'y_min' must be less than 'y_max'")
  }

  # Auto-detect data type if returnv doesn't match available columns
  has_AGC <- any(grepl("_C_t$", colnames(results_df)))
  has_AGB <- any(grepl("_B_t$", colnames(results_df)))

  # If returnv doesn't match data, auto-detect
  if (returnv == "AGC" && !has_AGC && has_AGB) {
    returnv <- "AGB"
    warning("Data contains AGB columns but returnv='AGC' was specified. Using AGB instead.")
  } else if (returnv == "AGB" && !has_AGB && has_AGC) {
    returnv <- "AGC"
    warning("Data contains AGC columns but returnv='AGB' was specified. Using AGC instead.")
  }

  # Determine columns
  if (returnv == "AGC") {
    methods_cols <- c("WCC" = "WCC_C_t", "Biomass" = "biomass_C_t",
                      "Allodb" = "allodb_C_t", "Bunce" = "Bunce_C_t")
    err_cols <- c("WCC" = "WCC_C_sig", "Biomass" = "biomass_C_sig",
                  "Allodb" = "allodb_C_sig", "Bunce" = "Bunce_C_sig")
    y_label <- "Carbon (t)"
  } else {
    methods_cols <- c("WCC" = "WCC_B_t", "Biomass" = "biomass_B_t",
                      "Allodb" = "allodb_B_t", "Bunce" = "Bunce_B_t")
    err_cols <- c("WCC" = "WCC_B_sig", "Biomass" = "biomass_B_sig",
                  "Allodb" = "allodb_B_sig", "Bunce" = "Bunce_B_sig")
    y_label <- "Biomass (t)"
  }

  # Check which columns actually exist in the data
  available_method_cols <- methods_cols[methods_cols %in% colnames(results_df)]

  # Identify which methods have no data
  missing_methods <- setdiff(names(methods_cols), names(available_method_cols))

  if (length(available_method_cols) == 0) {
    # Try to provide helpful error message
    if (!has_AGC && !has_AGB) {
      warning("No method columns found in results_df (neither _C_t nor _B_t columns). Available columns: ",
              paste(colnames(results_df), collapse = ", "))
    } else {
      warning("No matching method columns found for returnv='", returnv, "'. Available columns: ",
              paste(colnames(results_df), collapse = ", "),
              ". Try using returnv='", ifelse(has_AGC, "AGC", "AGB"), "' instead.")
    }
    return(NULL)
  }

  # Build long dataframe
  n <- nrow(results_df)
  plot_df_list <- lapply(names(available_method_cols), function(m) {
    col_name <- available_method_cols[m]
    if (col_name %in% colnames(results_df)) {
      data.frame(
        tree = seq_len(n),
        DBH = results_df$dbh,
        Height = results_df$height,
        method = m,
        value = results_df[[col_name]],
        genus = if ("genus" %in% colnames(results_df)) results_df$genus else NA_character_,
        species = if ("species" %in% colnames(results_df)) results_df$species else NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  plot_df <- do.call(rbind, Filter(Negate(is.null), plot_df_list))

  if (is.null(plot_df) || nrow(plot_df) == 0) {
    warning("No data to plot after filtering. Methods with no data: ",
            paste(missing_methods, collapse = ", "))
    return(NULL)
  }

  # Remove rows with all NA values and check which methods have no valid data
  plot_df_before_na <- plot_df
  plot_df <- plot_df[!is.na(plot_df$value), ]

  # Check which methods have no non-NA values
  methods_with_data <- unique(plot_df$method)
  methods_with_no_data <- setdiff(unique(plot_df_before_na$method), methods_with_data)
  all_missing_methods <- c(missing_methods, methods_with_no_data)

  if (nrow(plot_df) == 0) {
    warning("No non-NA values to plot. Methods with no data: ",
            paste(all_missing_methods, collapse = ", "))
    return(NULL)
  }

  # Warn about methods with no data (if any)
  if (length(all_missing_methods) > 0) {
    warning("Some methods have no data and will not be plotted: ",
            paste(all_missing_methods, collapse = ", "))
  }

  # Warn if log scale is selected but data contains values <= 0
  if (log_scale && plot_type != "bar" && plot_type != "tree_index" && plot_type != "density" && plot_type != "residual") {
    if (any(plot_df$value <= 0, na.rm = TRUE)) {
      n_zero_or_neg <- sum(plot_df$value <= 0, na.rm = TRUE)
      warning("Log scale selected but data contains ", n_zero_or_neg,
              " value(s) less than or equal to zero. ",
              "Log scale cannot display zero or negative values. ",
              "Consider using linear scale or filtering out zero/negative values.")
    }
  }

  # Add error bars if available
  # Always create se column (even if show_errors is FALSE) for aggregation purposes
  plot_df$se <- NA_real_
  if (show_errors) {
    available_err_cols <- err_cols[err_cols %in% colnames(results_df)]
    for (m in names(available_err_cols)) {
      if (m %in% plot_df$method) {
        err_vals <- results_df[[available_err_cols[m]]]
        if (!all(is.na(err_vals))) {
          plot_df$se[plot_df$method == m] <- err_vals
        }
      }
    }
    # For log scale, allow ymin to go below 0 (will be handled by scale)
    # For linear scale, ensure ymin >= small positive value
    if (log_scale) {
      plot_df$ymin <- plot_df$value - plot_df$se
      plot_df$ymax <- plot_df$value + plot_df$se
    } else {
      plot_df$ymin <- pmax(plot_df$value - plot_df$se, 1e-6, na.rm = TRUE)
      plot_df$ymax <- plot_df$value + plot_df$se
    }
  } else {
    # Initialize ymin/ymax even when not showing errors (needed for aggregation)
    plot_df$ymin <- NA_real_
    plot_df$ymax <- NA_real_
  }

  # Define color schemes
  if (color_scheme == "viridis") {
    method_colors <- scales::viridis_pal()(length(unique(plot_df$method)))
  } else if (color_scheme == "brewer") {
    method_colors <- RColorBrewer::brewer.pal(max(3, length(unique(plot_df$method))), "Set2")[1:length(unique(plot_df$method))]
  } else {
    # Default colors
    method_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")
  }
  names(method_colors) <- unique(plot_df$method)

  # Create plots
  if (plot_type == "bar") {
    if (length(unique(plot_df$tree)) == 1) {
      p <- ggplot(plot_df, aes(method, value, fill = method)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = method_colors) +
        geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.4, size = font_size * 0.3) +
        labs(x = "Method", y = y_label, title = paste("Above Ground", returnv, "Estimates for Single Tree")) +
        ggplot2::theme(legend.position = "none")
      p <- add_error_bars(p, plot_df, show_errors, width = 0.1)
    } else {
      # Aggregate values
      sum_df <- aggregate(value ~ method, plot_df, sum, na.rm = TRUE)

      # Aggregate errors if se column exists and has values
      if ("se" %in% colnames(plot_df) && !all(is.na(plot_df$se))) {
        se_agg <- aggregate(se ~ method, plot_df, function(x) sqrt(sum(x^2, na.rm = TRUE)), na.action = na.pass)
        sum_df <- merge(sum_df, se_agg, by = "method", all.x = TRUE)
        sum_df$se[is.na(sum_df$se)] <- 0
      } else {
        sum_df$se <- 0
      }

      # For log scale, allow ymin to go below 0
      if (log_scale) {
        sum_df$ymin <- sum_df$value - sum_df$se
        sum_df$ymax <- sum_df$value + sum_df$se
      } else {
        sum_df$ymin <- pmax(sum_df$value - sum_df$se, 1e-6, na.rm = TRUE)
        sum_df$ymax <- sum_df$value + sum_df$se
      }

      p <- ggplot(sum_df, aes(method, value, fill = method)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = method_colors[names(method_colors) %in% sum_df$method]) +
        geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.4, size = font_size * 0.3) +
        labs(x = "Method", y = y_label, title = paste("Total Above Ground", returnv, "Estimates per Method")) +
        ggplot2::theme(legend.position = "none")
      p <- add_error_bars(p, sum_df, show_errors, width = 0.1)
    }
  } else if (plot_type == "dbh") {
    # Set up jitter positions based on data range
    dbh_range <- diff(range(plot_df$DBH, na.rm = TRUE))
    jitter_setup <- setup_jitter_positions(dbh_range, jitter)
    point_position <- jitter_setup$point_position
    error_position <- jitter_setup$error_position
    error_width <- jitter_setup$error_width

    # Build geom_point based on size_scale
    if (is.null(size_scale)) {
      p <- ggplot(plot_df, aes(DBH, value, colour = method)) +
        geom_point(position = point_position, size = point_size)
    } else if (size_scale == "Height") {
      p <- ggplot(plot_df, aes(DBH, value, colour = method, size = Height)) +
        geom_point(position = point_position)
    } else {
      p <- ggplot(plot_df, aes(DBH, value, colour = method, size = DBH)) +
        geom_point(position = point_position)
    }

    p <- p +
      scale_color_manual(values = method_colors) +
      labs(x = "DBH (cm)", y = y_label, title = paste("Above Ground", returnv, "Estimates by DBH"))
    p <- add_error_bars(p, plot_df, show_errors,
                        position = error_position,
                        width = error_width)
  } else if (plot_type == "height") {
    # Set up jitter positions based on data range
    height_range <- diff(range(plot_df$Height, na.rm = TRUE))
    jitter_setup <- setup_jitter_positions(height_range, jitter)
    point_position <- jitter_setup$point_position
    error_position <- jitter_setup$error_position
    error_width <- jitter_setup$error_width

    # Build geom_point based on size_scale
    if (is.null(size_scale)) {
      p <- ggplot(plot_df, aes(Height, value, colour = method)) +
        geom_point(position = point_position, size = point_size)
    } else if (size_scale == "Height") {
      p <- ggplot(plot_df, aes(Height, value, colour = method, size = Height)) +
        geom_point(position = point_position)
    } else {
      p <- ggplot(plot_df, aes(Height, value, colour = method, size = DBH)) +
        geom_point(position = point_position)
    }

    p <- p +
      scale_color_manual(values = method_colors) +
      labs(x = "Height (m)", y = y_label, title = paste("Above Ground", returnv, "Estimates by Height"))
    p <- add_error_bars(p, plot_df, show_errors,
                        position = error_position,
                        width = error_width)
  } else if (plot_type == "tree_index") {
    # Use position_dodge (or jitterdodge if jitter enabled) to separate methods at each tree index
    # Calculate dodge width based on number of methods
    n_methods <- length(unique(plot_df$method))
    dodge_width <- min(0.3, 0.5 / n_methods)

    # Set up position based on jitter parameter
    if (jitter) {
      point_position <- position_jitterdodge(jitter.width = 0.1, dodge.width = dodge_width, seed = 123)
      error_position <- position_dodge(width = dodge_width)
    } else {
      point_position <- position_dodge(width = dodge_width)
      error_position <- position_dodge(width = dodge_width)
    }

    # Build geom_point based on size_scale
    if (is.null(size_scale)) {
      p <- ggplot(plot_df, aes(as.factor(tree), value, colour = method)) +
        geom_point(position = point_position, size = point_size)
    } else if (size_scale == "Height") {
      p <- ggplot(plot_df, aes(as.factor(tree), value, colour = method, size = Height)) +
        geom_point(position = point_position)
    } else {
      p <- ggplot(plot_df, aes(as.factor(tree), value, colour = method, size = DBH)) +
        geom_point(position = point_position)
    }

    p <- p +
      scale_color_manual(values = method_colors) +
      scale_x_discrete(name = "Tree Index") +
      labs(y = y_label, title = paste("Above Ground", returnv, "Estimates Across Allometric Methods by Tree Index"))
    p <- add_error_bars(p, plot_df, show_errors,
                        position = error_position,
                        width = 0.2)
  } else if (plot_type == "density") {
    # Density plot showing distribution of estimates for each method
    p <- ggplot(plot_df, aes(value, colour = method, fill = method)) +
      geom_density(alpha = 0.3) +
      scale_color_manual(values = method_colors) +
      scale_fill_manual(values = method_colors) +
      labs(x = y_label, y = "Density",
           title = paste("Distribution of Above Ground", returnv, "Estimates by Method"))
  } else if (plot_type == "residual") {
    # Residual plot showing differences between methods
    # Calculate mean value across all methods for each tree as reference
    if (length(unique(plot_df$tree)) > 1) {
      # For multiple trees, calculate mean per tree
      tree_means <- aggregate(value ~ tree, plot_df, mean, na.rm = TRUE)
      names(tree_means)[2] <- "mean_value"
      plot_df <- merge(plot_df, tree_means, by = "tree", all.x = TRUE)
      plot_df$residual <- plot_df$value - plot_df$mean_value
      x_var <- "mean_value"
      x_label <- paste("Mean", y_label, "across methods")
    } else {
      # For single tree, use method mean as reference
      method_mean <- mean(plot_df$value, na.rm = TRUE)
      plot_df$residual <- plot_df$value - method_mean
      plot_df$mean_value <- method_mean
      x_var <- "method"
      x_label <- "Method"
    }

    # Determine point aesthetics based on size_scale
    if (is.null(size_scale)) {
      point_aes <- aes()
      point_size_val <- point_size
    } else if (size_scale == "Height") {
      point_aes <- aes(size = Height)
      point_size_val <- NULL
    } else {
      point_aes <- aes(size = DBH)
      point_size_val <- NULL
    }

    if (x_var == "mean_value") {
      # Scatter plot of residuals vs mean value
      p <- ggplot(plot_df, aes(x = .data[["mean_value"]], y = .data[["residual"]], colour = method)) +
        geom_point(mapping = point_aes, size = point_size_val) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        scale_color_manual(values = method_colors) +
        labs(x = x_label, y = paste("Residual (", y_label, ")"),
             title = paste("Residuals from Mean Above Ground", returnv, "by Method")) +
        theme_minimal(base_size = font_size)
    } else {
      # Bar plot of residuals for single tree
      p <- ggplot(plot_df, aes(x = method, y = .data[["residual"]], fill = method)) +
        geom_col(width = 0.6, position = position_dodge()) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        scale_fill_manual(values = method_colors) +
        labs(x = x_label, y = paste("Residual (", y_label, ")"),
             title = paste("Residuals from Mean Above Ground", returnv, "by Method")) +
        ggplot2::theme(legend.position = "none")
    }
  } else {
    return(NULL)
  }

  # Apply axis limits if specified
  x_limits <- NULL
  if ((!is.null(x_min) && !is.na(x_min)) || (!is.null(x_max) && !is.na(x_max))) {
    x_limits <- c(if (!is.null(x_min) && !is.na(x_min)) x_min else -Inf,
                  if (!is.null(x_max) && !is.na(x_max)) x_max else Inf)
    p <- p + xlim(x_limits)
  }

  y_limits <- NULL
  if ((!is.null(y_min) && !is.na(y_min)) || (!is.null(y_max) && !is.na(y_max))) {
    y_limits <- c(if (!is.null(y_min) && !is.na(y_min)) y_min else -Inf,
                  if (!is.null(y_max) && !is.na(y_max)) y_max else Inf)
    p <- p + ylim(y_limits)
  }

  # Apply log scale if requested (only for scatter plots, not bar plots, density, or residual)
  # Bar plots, density, and residual plots should always use linear scale
  if (log_scale && plot_type != "bar" && plot_type != "tree_index" && plot_type != "density" && plot_type != "residual") {
    if (!interactive) {
      p <- p + scale_y_log10()
    }
  }

  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    p_plotly <- plotly::ggplotly(p, tooltip = "text")

    # Helper function to get species name from plot_df
    get_species_name <- function(method_name, tree_idx = NULL, x_val = NULL, y_val = NULL) {
      if (!"genus" %in% colnames(plot_df) || !"species" %in% colnames(plot_df)) {
        return(NULL)
      }

      # Try to match based on method and optionally tree/x/y values
      if (!is.null(tree_idx) && tree_idx %in% plot_df$tree) {
        # For tree_index plots
        matching_rows <- plot_df[plot_df$tree == tree_idx & plot_df$method == method_name, ]
      } else if (!is.null(x_val) && !is.null(y_val)) {
        # For scatter plots, try to match by method and approximate values
        matching_rows <- plot_df[plot_df$method == method_name, ]
        if (nrow(matching_rows) > 0) {
          # Get first matching row
          matching_rows <- matching_rows[1, ]
        }
      } else {
        # For bar/density plots, just match by method
        matching_rows <- plot_df[plot_df$method == method_name, ]
        if (nrow(matching_rows) > 0) {
          matching_rows <- matching_rows[1, ]
        }
      }

      if (nrow(matching_rows) > 0 && !is.na(matching_rows$genus[1]) && !is.na(matching_rows$species[1])) {
        return(paste0(matching_rows$genus[1], " ", matching_rows$species[1]))
      }
      return(NULL)
    }

    # Helper function to create consistent hover text
    # Format: Species (if available), Method, Value, and optionally additional fields
    create_hover_text <- function(species_name = NULL, method_name = "", value = NULL,
                                  value_label = "", additional_fields = NULL) {
      parts <- character()

      # Add species if available
      if (!is.null(species_name) && !is.na(species_name) && species_name != "") {
        parts <- c(parts, paste0("Species: ", species_name))
      }

      # Add method
      if (!is.null(method_name) && method_name != "") {
        parts <- c(parts, paste0("Method: ", method_name))
      }

      # Add value
      if (!is.null(value) && !is.na(value)) {
        parts <- c(parts, paste0("Value: ", sprintf("%.3f", value), " ", value_label))
      }

      # Add additional fields (list of character vectors)
      if (!is.null(additional_fields)) {
        parts <- c(parts, additional_fields)
      }

      paste(parts, collapse = "<br>")
    }

    # Customize hover text to use sprintf for all numeric values
    # Format: Value with 3 dp, DBH/Height with 2 dp
    for (i in seq_along(p_plotly$x$data)) {
      if (!is.null(p_plotly$x$data[[i]]$y) && length(p_plotly$x$data[[i]]$y) > 0) {
        # Get original text if it exists
        original_text <- p_plotly$x$data[[i]]$text
        method_name <- ifelse(is.null(p_plotly$x$data[[i]]$name), "", p_plotly$x$data[[i]]$name)

        # Format numeric values in hover text using sprintf
        if (!is.null(original_text) && length(original_text) > 0) {
          # Format each text element - need to distinguish between Value, DBH, and Height
          formatted_text <- sapply(seq_along(original_text), function(idx) {
            txt <- original_text[idx]
            if (any(is.na(txt)) || txt == "") return(txt)

            # Check if this is a value (y-axis) or DBH/Height (x-axis)
            # For bar plots, we only have y values
            # For scatter plots, we have both x and y

            # Extract value (y) - always 3 dp
            y_val <- if (!is.null(p_plotly$x$data[[i]]$y) && length(p_plotly$x$data[[i]]$y) >= idx) {
              p_plotly$x$data[[i]]$y[idx]
            } else NULL

            # Extract x value (DBH or Height) - 2 dp
            x_val <- if (!is.null(p_plotly$x$data[[i]]$x) && length(p_plotly$x$data[[i]]$x) >= idx) {
              p_plotly$x$data[[i]]$x[idx]
            } else NULL

            # Get species name
            species_name <- get_species_name(method_name, tree_idx = x_val, x_val = x_val, y_val = y_val)
            species_line <- if (!is.null(species_name)) paste0("Species: ", species_name, "<br>") else ""

            # Build formatted text based on plot type using helper function
            if (plot_type == "dbh" && !is.null(x_val) && !is.null(y_val)) {
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label),
                additional_fields = c(paste0("DBH: ", sprintf("%.2f", x_val), " cm"))
              )
            } else if (plot_type == "height" && !is.null(x_val) && !is.null(y_val)) {
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label),
                additional_fields = c(paste0("Height: ", sprintf("%.2f", x_val), " m"))
              )
            } else if (plot_type == "tree_index" && !is.null(y_val)) {
              # For tree_index, we need to get DBH from the data if available
              tree_idx <- if (!is.null(x_val)) x_val else idx
              species_name <- get_species_name(method_name, tree_idx = tree_idx)

              dbh_val <- if (tree_idx %in% plot_df$tree && length(plot_df$DBH[plot_df$tree == tree_idx]) > 0) {
                plot_df$DBH[plot_df$tree == tree_idx][1]
              } else NULL
              height_val <- if (tree_idx %in% plot_df$tree && length(plot_df$Height[plot_df$tree == tree_idx]) > 0) {
                plot_df$Height[plot_df$tree == tree_idx][1]
              } else NULL

              additional_fields <- c(
                paste0("Tree: ", tree_idx),
                if (!is.null(dbh_val)) paste0("DBH: ", sprintf("%.2f", dbh_val), " cm") else NULL,
                if (!is.null(height_val)) paste0("Height: ", sprintf("%.2f", height_val), " m") else NULL
              )

              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label),
                additional_fields = additional_fields
              )
            } else if (plot_type == "bar" && !is.null(y_val)) {
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label)
              )
            } else if (plot_type == "density" && !is.null(y_val)) {
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = y_val,
                value_label = tolower(y_label)
              )
            } else if (plot_type == "residual" && !is.null(y_val)) {
              additional_fields <- if (!is.null(x_val)) {
                c(paste0("Mean Value: ", sprintf("%.3f", x_val), " ", tolower(y_label)),
                  paste0("Residual: ", sprintf("%.3f", y_val), " ", tolower(y_label)))
              } else {
                c(paste0("Residual: ", sprintf("%.3f", y_val), " ", tolower(y_label)))
              }
              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                additional_fields = additional_fields
              )
            } else {
              # Fallback: try to parse and format existing text
              txt
            }
          })
          p_plotly$x$data[[i]]$text <- formatted_text
        } else {
          # Create hover text if it doesn't exist using helper function
          method_name <- ifelse(is.null(p_plotly$x$data[[i]]$name), "", p_plotly$x$data[[i]]$name)

          if (plot_type == "dbh" && !is.null(p_plotly$x$data[[i]]$x) && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name, x_val = p_plotly$x$data[[i]]$x[1], y_val = p_plotly$x$data[[i]]$y[1])
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              value = p_plotly$x$data[[i]]$y[1],
              value_label = tolower(y_label),
              additional_fields = c(paste0("DBH: ", sprintf("%.2f", p_plotly$x$data[[i]]$x[1]), " cm"))
            )
          } else if (plot_type == "height" && !is.null(p_plotly$x$data[[i]]$x) && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name, x_val = p_plotly$x$data[[i]]$x[1], y_val = p_plotly$x$data[[i]]$y[1])
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              value = p_plotly$x$data[[i]]$y[1],
              value_label = tolower(y_label),
              additional_fields = c(paste0("Height: ", sprintf("%.2f", p_plotly$x$data[[i]]$x[1]), " m"))
            )
          } else if (plot_type == "tree_index" && !is.null(p_plotly$x$data[[i]]$y)) {
            # For tree_index, get DBH and Height from plot_df
            tree_vals <- unique(plot_df$tree)
            p_plotly$x$data[[i]]$text <- sapply(seq_along(p_plotly$x$data[[i]]$y), function(idx) {
              tree_idx <- if (!is.null(p_plotly$x$data[[i]]$x) && length(p_plotly$x$data[[i]]$x) >= idx) {
                p_plotly$x$data[[i]]$x[idx]
              } else if (idx <= length(tree_vals)) tree_vals[idx] else idx

              species_name <- get_species_name(method_name, tree_idx = tree_idx)

              dbh_val <- if (tree_idx %in% plot_df$tree && length(plot_df$DBH[plot_df$tree == tree_idx]) > 0) {
                plot_df$DBH[plot_df$tree == tree_idx][1]
              } else NULL
              height_val <- if (tree_idx %in% plot_df$tree && length(plot_df$Height[plot_df$tree == tree_idx]) > 0) {
                plot_df$Height[plot_df$tree == tree_idx][1]
              } else NULL

              additional_fields <- c(
                paste0("Tree: ", tree_idx),
                if (!is.null(dbh_val)) paste0("DBH: ", sprintf("%.2f", dbh_val), " cm") else NULL,
                if (!is.null(height_val)) paste0("Height: ", sprintf("%.2f", height_val), " m") else NULL
              )

              create_hover_text(
                species_name = species_name,
                method_name = method_name,
                value = p_plotly$x$data[[i]]$y[idx],
                value_label = tolower(y_label),
                additional_fields = additional_fields
              )
            })
          } else if (plot_type == "density" && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name)
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              value = p_plotly$x$data[[i]]$y[1],
              value_label = tolower(y_label)
            )
          } else if (plot_type == "residual" && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name)
            additional_fields <- if (!is.null(p_plotly$x$data[[i]]$x)) {
              c(paste0("Mean Value: ", sprintf("%.3f", p_plotly$x$data[[i]]$x[1]), " ", tolower(y_label)),
                paste0("Residual: ", sprintf("%.3f", p_plotly$x$data[[i]]$y[1]), " ", tolower(y_label)))
            } else {
              c(paste0("Residual: ", sprintf("%.3f", p_plotly$x$data[[i]]$y[1]), " ", tolower(y_label)))
            }
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              additional_fields = additional_fields
            )
          } else if (plot_type == "bar" && !is.null(p_plotly$x$data[[i]]$y)) {
            species_name <- get_species_name(method_name)
            p_plotly$x$data[[i]]$text <- create_hover_text(
              species_name = species_name,
              method_name = method_name,
              value = p_plotly$x$data[[i]]$y[1],
              value_label = tolower(y_label)
            )
          }
        }
      }
    }

    # Build axis configurations
    xaxis_config <- list()
    yaxis_config <- list()

    # Format hover text: x-axis (DBH/Height) with 2 dp, y-axis (Value) with 3 dp
    xaxis_config$hoverformat <- ".2f"
    yaxis_config$hoverformat <- ".3f"

    # Only apply log scale to scatter plots, not bar plots, tree_index, density, or residual
    if (log_scale && plot_type != "bar" && plot_type != "tree_index" && plot_type != "density" && plot_type != "residual") {
      yaxis_config$type <- "log"
    }

    # Apply axis limits for plotly (if not already applied via xlim/ylim)
    if (!is.null(x_limits)) {
      xaxis_config$range <- x_limits
    }
    if (!is.null(y_limits)) {
      yaxis_config$range <- y_limits
    }

    # Apply all axis configurations
    p_plotly <- plotly::layout(p_plotly, xaxis = xaxis_config, yaxis = yaxis_config)

    return(p_plotly)
  } else {
    # Apply theme if specified
    if (!is.null(theme)) {
      p <- p + theme(base_size = font_size)
    } else {
      p <- p + ggplot2::theme_minimal(base_size = font_size)
    }
    return(p)
  }
}


#############  Comparison Statistics for Allometry Methods ################
#'
#' @title Calculate comparison statistics for allometry methods
#' @description Calculates summary statistics comparing different allometric methods
#' including mean differences, coefficient of variation, and method agreement metrics
#' @author Isabel Openshaw. I.Openshaw@kew.org
#' @param results_df Data frame with results from allometries function
#' @param returnv Either "AGC" for carbon or "AGB" for biomass
#' @return List with comparison statistics
#' @examples
#' results <- allometries(c("Quercus","Quercus"), c("robur","robur"), c(20, 30), c(10, 15))
#' compare_methods(results, "AGC")
#' @export
#'
compare_methods <- function(results_df, returnv = "AGC") {
  if (returnv == "AGC") {
    method_cols <- c("WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")
    sig_cols <- c("WCC_C_sig", "biomass_C_sig", "allodb_C_sig", "Bunce_C_sig")
    method_names <- c("WCC", "Biomass", "Allodb", "Bunce")
  } else {
    method_cols <- c("WCC_B_t", "biomass_B_t", "allodb_B_t", "Bunce_B_t")
    sig_cols <- c("WCC_B_sig", "biomass_B_sig", "allodb_B_sig", "Bunce_B_sig")
    method_names <- c("WCC", "Biomass", "Allodb", "Bunce")
  }

  # Extract method values
  method_data <- results_df[, intersect(method_cols, colnames(results_df)), drop = FALSE]
  if (ncol(method_data) == 0) {
    return(list(summary_stats = data.frame(Message = "No method data available")))
  }
  colnames(method_data) <- method_names[method_cols %in% colnames(results_df)]

  # Remove rows with all NAs
  valid_rows <- rowSums(!is.na(method_data)) > 0
  method_data <- method_data[valid_rows, , drop = FALSE]

  if (nrow(method_data) == 0) {
    return(list(summary_stats = data.frame(Statistic = "No valid data", Value = NA)))
  }

  # Calculate statistics
  stats_list <- list()

  # Mean values per method
  means <- colMeans(method_data, na.rm = TRUE)
  stats_list$mean <- data.frame(
    Method = names(means),
    Value = means,
    Statistic = "Mean"
  )

  # Standard deviation per method
  sds <- apply(method_data, 2, function(x) sd(x, na.rm = TRUE))
  stats_list$sd <- data.frame(
    Method = names(sds),
    Value = sds,
    Statistic = "SD"
  )

  # Coefficient of variation
  cv <- (sds / means) * 100
  stats_list$cv <- data.frame(
    Method = names(cv),
    Value = cv,
    Statistic = "CV (%)"
  )

  # Mean difference matrix
  n_methods <- length(colnames(method_data))
  method_names_actual <- colnames(method_data)
  diff_matrix <- matrix(NA, nrow = n_methods, ncol = n_methods)
  rownames(diff_matrix) <- colnames(diff_matrix) <- method_names_actual

  for (i in 1:n_methods) {
    for (j in 1:n_methods) {
      if (i != j) {
        diff_vals <- method_data[, i] - method_data[, j]
        diff_matrix[i, j] <- mean(diff_vals, na.rm = TRUE)
      }
    }
  }

  # Correlation matrix
  cor_matrix <- cor(method_data, use = "pairwise.complete.obs")

  # Method agreement (mean absolute difference)
  agreement <- data.frame(
    Method1 = character(),
    Method2 = character(),
    Mean_Abs_Diff = numeric(),
    Correlation = numeric()
  )

  for (i in 1:(n_methods - 1)) {
    for (j in (i + 1):n_methods) {
      diff_vals <- abs(method_data[, i] - method_data[, j])
      agreement <- rbind(agreement, data.frame(
        Method1 = method_names_actual[i],
        Method2 = method_names_actual[j],
        Mean_Abs_Diff = mean(diff_vals, na.rm = TRUE),
        Correlation = cor_matrix[i, j]
      ))
    }
  }

  # Round all statistics to 4 decimal places
  stats_list$mean$Value <- round(stats_list$mean$Value, 4)
  stats_list$sd$Value <- round(stats_list$sd$Value, 4)
  stats_list$cv$Value <- round(stats_list$cv$Value, 4)
  diff_matrix <- round(diff_matrix, 4)
  agreement$Mean_Abs_Diff <- round(agreement$Mean_Abs_Diff, 4)
  agreement$Correlation <- round(agreement$Correlation, 4)

  return(list(
    summary_stats = rbind(stats_list$mean, stats_list$sd, stats_list$cv),
    mean_differences = diff_matrix,
    correlation_matrix = cor_matrix,  # Note: correlation often = 1 for single tree, less useful
    method_agreement = agreement
  ))
}

