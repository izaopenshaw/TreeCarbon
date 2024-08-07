#' Species Lookup Data Frame
#' Lookup table for covariables for equations from the Woodland Carbon Code
#'
#' @format A data frame with 18 variables:
#' \describe{
#'   \item{short}{Species code short}
#'   \item{common_name}{Species common name}
#'   \item{latin_name}{Species latin name}
#'   \item{type}{broadleaf or conifer}
#'   \item{level}{taxanomic rank at which covariables are given for}
#'   \item{single}{Species code single}
#'   \item{stand}{Species code stand}
#'   \item{NSG}{Nominal specific gravity}
#'   \item{origin}{origin}
#'   \item{Crown}{Species code crown}
#'   \item{Root}{Species code root}
#'   \item{notes}{notes}
#'   \item{General.for.genus}{If these are the general for genus covariables}
#'   \item{General.for.classification}{If these are the general for type covariables}
#'   \item{Genus}{Genus extracted from latin name}
#'   \item{Species1}{First word of speicies extracted from latin name}
#'   \item{Species2}{Second word of speicies extracted from latin name}
#'   \item{Species3}{Third word of speicies extracted from latin name}
#'   }
#' @source Jenkins, Thomas AR, et al. "FC Woodland Carbon Code:
#' Carbon Assessment Protocol (v2. 0)." (2018).
#' @name sp_lookupdf
#' @docType data
#' @export
"sp_lookupdf"
