# ==============================================================================
# TreeCarbon - Package Hooks
# ==============================================================================
#
# This file contains package-level hooks and initialization code that runs
# when the package is loaded or attached.
#
# ==============================================================================

#' @importFrom utils packageVersion txtProgressBar setTxtProgressBar
#' @importFrom stats aggregate ave cor density na.pass predict qnorm
NULL

# Suppress R CMD check notes for package data objects
# These are lazy-loaded from data/ and accessed without explicit ::
utils::globalVariables(c(

  # Package data objects
  "buncedf",
  "bunce_co_lookup",
  "CVF_df",
  "WD_Zanne",
  "crown_biomasdf",
  "lookup_df",
  "method_metadata",
  "method_assumptions",
  "root_biomassdf",
  "seedlings_broad",
  "seedlings_conifer",
  "stemvoldf",
  "tarif2heightdf",
  "tariff_broaddf",
  "tariff_coniferdf",
  "nominal_specific_gravity"
))

# Package-level options with defaults
.TreeCarbon_options <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {

  # Set default package options
  .TreeCarbon_options$default_method <- "WCC"
  .TreeCarbon_options$default_re_dbh <- 0.025
  .TreeCarbon_options$default_re_height <- 0.05
  .TreeCarbon_options$verbose <- FALSE

  invisible()
}

.onAttach <- function(libname, pkgname) {
  # Display startup message
  version <- packageVersion(pkgname)

  packageStartupMessage(
    "TreeCarbon v", version, " - Tree Carbon Estimation with Error Propagation\n",
    "Primary method is the UK Woodland Carbon Code (WCC)\n",
    "\n",
    "Required dependencies: BIOMASS and allodb packages\n",
    "If missing, install with:\n",
    "  install.packages('BIOMASS')\n",
    "  remotes::install_github('ropensci/allodb')\n"
  )

  invisible()
}

# Accessor for package options (internal use)
.get_option <- function(name) {
  .TreeCarbon_options[[name]]
}
