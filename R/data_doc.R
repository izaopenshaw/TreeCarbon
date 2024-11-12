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

#' Species Lookup Table
#'
#' This dataset provides a lookup table for species, including common names, Latin names, and other attributes.
#'
#' @format A data frame with 18 columns:
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
#'   \item{General.for.classification}{If the entry is for broafleaf or confier}
#'   \item{Genus}{Genus of the species}
#'   \item{Species1}{First species classification}
#'   \item{Species2}{Second species classification (if applicable)}
#'   \item{Species3}{Third species classification (if applicable)}
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
"stemvol"

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
#' @format A data frame with 5 columns:
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
