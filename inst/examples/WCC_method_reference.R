# ==============================================================================
# WCC Method Reference Table
# ==============================================================================
# See vignette("WCC-methods", package = "TreeCarbon") for full documentation.
#
# --------+----------------------------------------+---------------------------------------+----------------+------------------
#  Method | Functions                              | Inputs                                | Vignette sect. | Example 8 sect.
# --------+----------------------------------------+---------------------------------------+----------------+------------------
#  A      | tariff_vol_area()                      | Felled sample: vol (m³), dbh (cm)     | 2.1, 6         | 5
#         | merchtreevol(), treevol(),             | Then: NSG, species for full pipeline  |                |
#         | woodbiomass(), crownbiomass(),         |                                       |                |
#         | biomass2c()                            |                                       |                |
# --------+----------------------------------------+---------------------------------------+----------------+------------------
#  B      | broadleaf_tariff(), tariffs()          | type="broadleaf", species,            | 2.3, 7, 12.3   | 1
#         | fc_agc(), fc_agc_error()               | dbh (cm), height_timber (m)           |                |
# --------+----------------------------------------+---------------------------------------+----------------+------------------
#  C      | conifer_tariff(), tariffs()            | type="conifer", species,              | 2.2, 7, 12.3   | 1
#         | fc_agc(), fc_agc_error()               | dbh (cm), height (total, m)           |                |
# --------+----------------------------------------+---------------------------------------+----------------+------------------
#  D      | stand_tariff(), calculate_top_height() | stand_height OR dbh+height vectors,   | 2.4, 12.1      | 3
#         | fc_stand_carbon()                      | area_ha, species/name, type           |                |
# --------+----------------------------------------+---------------------------------------+----------------+------------------
#  E      | wcc_process_tally()                    | dbh_class, count; optional:           | 12.2           | 6
#         | wcc_dbh_class_carbon()                 | height, area_ha, species/name, type   |                |
#
# ==============================================================================
# Notes - WCC Protocol Methods (Carbon Assessment Protocol v2.0)
# ==============================================================================
# The TreeCarbon package functions can be used for all WCC methods.
#
# Method A: Fell sample trees - tariff_vol_area(). Step-by-step pipeline from
#   tariff onward (merchtreevol, treevol, woodbiomass, crownbiomass, biomass2c).
# Method B: Measure timber height (broadleaves) - broadleaf_tariff(), or
#   fc_agc()/fc_agc_error() for end-to-end carbon.
# Method C: Measure total height (conifers) - conifer_tariff(), or
#   fc_agc()/fc_agc_error() for end-to-end carbon.
# Method D: Measure top height - stand_tariff(), fc_stand_carbon(). fc_stand_carbon()
#   computes top height and mean DBH internally from tree data.
# Method E: Full tree count - (a) DBH class tallies: wcc_process_tally(),
#   wcc_dbh_class_carbon(); (b) Individual trees measured: fc_agc()/fc_agc_error()
#   for each tree.
#
# Averaging trees (as required in some WCC methods):
#   1. Calculate carbon for each tree individually (fc_agc, fc_agc_error)
#   2. Sum results for totals: total_sigma = sqrt(sum(sig_AGC^2))
#   3. Calculate means by dividing by number of trees
#   4. For uncertainty, propagate errors (see ?wcc_error_propagation)
#   Helper functions: wcc_mean_tariff(), wcc_per_hectare(), wcc_stratify()
#
# Carbon conversion: Matthews2 recommended for WCC (default in fc_agc,
#   fc_agc_error, fc_stand_carbon, wcc_dbh_class_carbon).
#
# SIGNPOSTING: Vignette: vignette("WCC-methods"); Examples: 6 (full workflow),
#   8 (method selection).
# ==============================================================================
