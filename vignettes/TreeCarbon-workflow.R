############ setup ##########
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

# load packages
library(TreeCarbon)
library(ggplot2)

########## single tree with wcc ########################
# A single oak tree
result_wcc <- fc_agc(
  name = "Oak",
  dbh = 45,        # cm
  height = 18,     # m
  type = "broadleaf"
)

print(result_wcc)

############ single wcc rich output ##########
result_rich <- fc_agc(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  rich_output = TRUE
)

# View the complete result
print(result_rich)

############ single wcc with error ####################
result_error <- fc_agc_error(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  re_dbh = 0.025,   # 2.5% relative measurement error
  re_h = 0.05       # 5% height measurement error
)

print(result_error[, c("name", "AGC_WCC_t", "sig_AGC")])

# Calculate 95% confidence interval
agc <- result_error$AGC_WCC_t
sigma <- result_error$sig_AGC
cat(sprintf("\nAGC: %.3f ± %.3f t (95%% CI: [%.3f, %.3f])\n",
            agc, 1.96 * sigma, agc - 1.96*sigma, agc + 1.96*sigma))

############ single tree with bunce ##########
result_bunce <- Bunce(
  name = "Oak",
  dbh = 45,
  re_dbh = 0.025  # Include measurement error
)

print(result_bunce)

############ single tree bunce rich output ##########
result_bunce_rich <- Bunce("Oak", dbh = 45, re_dbh = 0.025, rich_output = TRUE)
print(result_bunce_rich)

############ single tree with BIOMASS ##########
# UK coordinates (Kew Gardens)
coords <- c(-0.2915, 51.4787)

result_biomass <- BIOMASS(
  dbh = 45,
  height = 18,
  genus = "Quercus",
  species = "robur",
  coords = coords
)

print(result_biomass[, c("genus", "species", "dbh", "height",
                         "Wood_Density", "AGB_Biomass_kg")])

# Convert to tonnes for comparison
cat(sprintf("\nAGB: %.3f kg (%.4f t)\n",
            result_biomass$AGB_Biomass_kg,
            result_biomass$AGB_Biomass_kg / 1000))

