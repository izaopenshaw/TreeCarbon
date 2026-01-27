# ==============================================================================
# TreeCarbon - Data Documentation
# ==============================================================================
#
# This file contains roxygen2 documentation for all internal data tables
# used by the TreeCarbon package.
#
# Data categories:
#   - Woodland Carbon Code lookup tables (tariffs, species codes, NSG)
#   - Crown and root biomass parameters
#   - Carbon conversion factors (CVF)
#   - Seedling carbon lookup tables
#   - Method metadata for allometric comparisons
#
# Data sources:
#   - Jenkins, T.A.R., et al. (2018). FC Woodland Carbon Code (v2.0).
#   - Thomas, S.C. & Martin, A.R. (2012). Carbon content of tree tissues.
#   - IPCC (2006). Guidelines for National Greenhouse Gas Inventories.
#
# ==============================================================================

############# WCC Crown and Root Biomass Data ################

#' Crown Biomass Data
#'
#' This dataset contains crown biomass information for various tree species.
#' For use with the crownbiomass (Eq 6 & 7 of the FC's WCC)
#'
#' @format A data frame with 6 columns:
#' \describe{
#'   \item{Species.species}{Name of the species}
#'   \item{Code}{Code for the species}
#'   \item{b1}{First parameter related to biomass calculation}
#'   \item{p}{Second parameter related to biomass calculation}
#'   \item{A}{Parameter A description}
#'   \item{b2}{Second parameter related to biomass calculation}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: #' Carbon
#' Assessment Protocol (v2. 0)." (2018). (Equation 1)
"crown_biomasdf"

#' Nominal Specific Gravity Data
#'
#' This dataset contains the nominal specific gravity for various species.
#'
#' @format A data frame with 4 columns:
#' \describe{
#'   \item{Species}{Species name}
#'   \item{Sp_code}{Species code}
#'   \item{Allocated.Species}{Allocated species group}
#'   \item{Nominal.Specific.Gravity..NSG.}{Nominal specific gravity values}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: #' Carbon
#' Assessment Protocol (v2. 0)." (2018). (Equation 1)
"nominal_specific_gravity"

#' Root Biomass Data
#'
#' This dataset contains root biomass parameters for different tree species.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{Species.species}{Name of the species}
#'   \item{Code}{Code for the species}
#'   \item{b1}{Parameter b1 for biomass calculation}
#'   \item{a}{Parameter a description}
#'   \item{b2}{Parameter b2 for biomass calculation}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: #' Carbon
#' Assessment Protocol (v2. 0)." (2018). (Equation 1)
"root_biomassdf"


############# WCC Seedling Data ################

#' Broadleaf Seedlings Data
#'
#' This dataset contains carbon data for broadleaf seedlings.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{height.cm}{Height of the seedling in cm}
#'   \item{Carbon.kg}{Carbon content in kg}
#'   \item{cat}{Category (broadleaf)}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: #' Carbon
#' Assessment Protocol (v2. 0)." (2018). (Equation 1)
"seedlings_broad"

#' Conifer Seedlings Data
#'
#' This dataset contains carbon data for conifer seedlings.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{height.cm}{Height of the seedling in cm}
#'   \item{Carbon.kg}{Carbon content in kg}
#'   \item{cat}{Category (conifer)}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: #' Carbon
#' Assessment Protocol (v2. 0)." (2018). (Equation 1)
"seedlings_conifer"


############# Species Lookup Tables ################

#' Species Lookup Table
#'
#' This dataset provides a lookup table for species, including common names, Latin names, and other attributes.
#'
#' @format A data frame with 19 columns:
#' \describe{
#'   \item{short}{Short species code}
#'   \item{common_name}{Common name of the species}
#'   \item{latin_name}{Latin name of the species}
#'   \item{type}{Type of species (broadleaf/conifer)}
#'   \item{level}{Classification level}
#'   \item{single}{Single or grouped species}
#'   \item{stand}{Stand data}
#'   \item{NSG}{Nominal specific gravity}
#'   \item{origin}{Species origin}
#'   \item{Crown}{Crown type code}
#'   \item{Root}{Root type code}
#'   \item{notes}{Additional notes}
#'   \item{General.genus}{If the entry is for a genus and not species level}
#'   \item{General.type}{If the entry is for broafleaf or confier}
#'   \item{Genus}{Genus of the species}
#'   \item{Species}{The second term in the bionomial, species}
#'   \item{NSG_sd}{Wood density standard deviation calculated from Zanne et al 2009} # ** TODOs
#'   \item{meanWD}{Wood density from the BIOMASS package}
#'   \item{sdWD}{Wood density standard deviation from the BIOMASS package}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: Carbon Assessment Protocol (v2. 0)." (2018). (Equation 1)
"lookup_df"

#' Coefficient of Variation lookup table
#'
#' This dataset provides a lookup table for values for the coefficient of variation for converting biomass to carbon
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{method}{reference for CVF}
#'   \item{type}{type of tree: broadleaf or conifer}
#'   \item{biome}{biome of study}
#'   \item{CVF}{Percentage of coefficient of variation}
#'   \item{confidence}{confidence of estimate}
#' }
#' @source (1) Thomas, Sean C., and Adam R. Martin. "Carbon content of tree tissues: a synthesis." Forests 3.2 (2012): 332-352.  https://www.mdpi.com/1999-4907/3/2/332.
#' (2) IPCC. Forest lands. Intergovernmental Panel on Climate Change Guidelines for National Greenhouse Gas Inventories; Institute for Global Environmental Strategies (IGES): Hayama,Japan, 2006; Volume 4, p. 83.
#' (3) Matthews, G.A.R. (1993) The Carbon Content of Trees. Forestry Commission Technical Paper 4. Forestry Commission, Edinburgh. 21pp. ISBN: 0-85538-317-8
"CVF_df"

#' Stem Volume Data
#'
#' This dataset contains stem volume data for trees based on diameter at breast height (dbh).
#'
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{dbh..cm.}{Diameter at breast height in cm}
#'   \item{X}{Volume in cubic meters}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: #' Carbon
#' Assessment Protocol (v2. 0)." (2018). (Equation 1)
"stemvoldf"


############# WCC Tariff Parameters ################

#' Tarif to Height Conversion Data
#'
#' This dataset contains conversion parameters from tariff values to tree height
#'  for various species.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{common.name}{Common name of the species}
#'   \item{abbreviation}{Abbreviation for the species}
#'   \item{a1}{Conversion parameter a1}
#'   \item{a2}{Conversion parameter a2}
#'   \item{a3}{Conversion parameter a3}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: #' Carbon
#' Assessment Protocol (v2. 0)." (2018). (Equation 1)
"tarif2heightdf"

#' Broadleaf Tariff Data
#'
#' This dataset contains tariff parameters for broadleaf species.
#'
#' @format A data frame with 6 columns:
#' \describe{
#'   \item{Common.name}{Common name of the species}
#'   \item{abbreviation}{Abbreviation for the species}
#'   \item{a1}{Tariff parameter a1}
#'   \item{a2}{Tariff parameter a2}
#'   \item{a3}{Tariff parameter a3}
#'   \item{a4}{Tariff parameter a4}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: #' Carbon
#' Assessment Protocol (v2. 0)." (2018). (Equation 1)
"tariff_broaddf"

#' Conifer Tariff Data
#'
#' This dataset contains tariff parameters for conifer species.
#'
#' @format A data frame with 4 columns:
#' \describe{
#'   \item{common.name}{Common name of the species}
#'   \item{abbreviation}{Abbreviation for the species}
#'   \item{a1}{Tariff parameter a1}
#'   \item{a2}{Tariff parameter a2}
#'   \item{a3}{Tariff parameter a3}
#' }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code: #' Carbon
#' Assessment Protocol (v2. 0)." (2018). (Equation 1)
"tariff_coniferdf"


############# Alternative Allometry Data (Bunce, Wood Density) ################

#' Bunce equation coefficient data table
#'
#' This dataset contains coefficients for the Bunce allometric equation.
#'
#' @format A data frame with 4 columns:
#' \describe{
#'   \item{X}{Common name of the species}
#'   \item{spcode}{Abbreviation for the species}
#'   \item{a}{Bunce coefficient a}
#'   \item{b}{Bunce coefficient b}
#' }
#' @source Bunce, R. G. H. "Biomass and Production of Trees in a Mixed
#' Deciduous Woodland: I. Girth and Height as Parameters for the Estimation of
#' Tree Dry Weight" (1968)
"buncedf"

#' Wood Density Dataset from Zanne et al.
#'
#' A dataset of wood density measurements compiled by Zanne et al. across various species, continents, and biomes.
#'
#' @format A data frame with 16,468 rows and 11 variables:
#' \describe{
#'   \item{Family}{Character. Plant family.}
#'   \item{Genus}{Character. Plant genus.}
#'   \item{Species}{Character. Plant species.}
#'   \item{Binomial}{Character. Genus + species name.}
#'   \item{Wood.density}{Numeric. Wood density (g/cm³)}
#'   \item{Reference}{Integer. Reference ID for source of data.}
#'   \item{Continent}{Character. Continent where data was collected.}
#'   \item{Biome}{Character. Biome classification.}
#'   \item{Region}{Character. Combined continent and biome description.}
#'   \item{wd_sd}{Numeric. The calculated standard deviation of the wood density
#'   estimates from the available data for that species if there are more than
#'   10 estimates per class.}
#'   \item{wd_global_sd}{Numeric. Global standard deviation of wood density
#'   across all species.}
#' }
#'
#' @source Zanne, A.E., et al. (2009). Global wood density database. Dryad. http://hdl.handle.net/10255/dryad.235
"WD_Zanne"


############# Method Metadata Tables ################

#' Allometric Method Metadata
#'
#' A dataset containing comprehensive metadata for each allometric method
#' supported by the TreeCarbon package. Includes citations, valid ranges,
#' regional applicability, and source information.
#'
#' @format A data frame with 4 rows (one per method) and 16 variables:
#' \describe{
#'   \item{method}{Character. Short method identifier: "WCC", "BIOMASS", "allodb", "Bunce"}
#'   \item{full_name}{Character. Full descriptive name of the method}
#'   \item{citation}{Character. Full citation for the method}
#'   \item{citation_short}{Character. Short citation format (Author, Year)}
#'   \item{doi}{Character. Digital Object Identifier (DOI) if available, NA otherwise}
#'   \item{region}{Character. Geographic region(s) where method is applicable}
#'   \item{biome}{Character. Biome(s) where method is applicable, semicolon-separated}
#'   \item{source_type}{Character. Type of source: "peer-reviewed" or "government protocol"}
#'   \item{dbh_min_cm}{Numeric. Minimum valid DBH in centimetres}
#'   \item{dbh_max_cm}{Numeric. Maximum valid DBH in centimetres}
#'   \item{height_min_m}{Numeric. Minimum valid height in metres, NA if height not used}
#'   \item{height_max_m}{Numeric. Maximum valid height in metres, NA if height not used}
#'   \item{height_required}{Logical. TRUE if height is a required input}
#'   \item{species_specific}{Logical. TRUE if method uses species-specific parameters}
#'   \item{wood_density_required}{Logical. TRUE if wood density is a required input}
#'   \item{uncertainty_method}{Character. Description of how uncertainty is calculated}
#' }
#'
#' @examples
#' # View all method metadata
#' data(method_metadata)
#' method_metadata
#'
#' # Get metadata for a specific method
#' method_metadata[method_metadata$method == "WCC", ]
#'
#' # Check which methods require height
#' method_metadata[method_metadata$height_required, c("method", "full_name")]
#'
#' @seealso \code{\link{method_assumptions}} for detailed assumptions per method
#' @source Package authors' compilation from original method publications
"method_metadata"

#' Allometric Method Assumptions
#'
#' A dataset containing the key assumptions for each allometric method.
#' This table has a one-to-many relationship with \code{method_metadata},
#' with multiple assumptions per method.
#'
#' @format A data frame with 20 rows (5 assumptions per method) and 3 variables:
#' \describe{
#'   \item{method}{Character. Method identifier matching \code{method_metadata$method}}
#'   \item{assumption_order}{Integer. Order of the assumption (1-5)}
#'   \item{assumption}{Character. Description of the assumption}
#' }
#'
#' @examples
#' # View all assumptions
#' data(method_assumptions)
#' method_assumptions
#'
#' # Get assumptions for WCC method
#' method_assumptions[method_assumptions$method == "WCC", ]
#'
#' # Get first assumption for each method
#' method_assumptions[method_assumptions$assumption_order == 1, ]
#'
#' @seealso \code{\link{method_metadata}} for general method information
#' @source Package authors' compilation from original method publications
"method_assumptions"
