# ==================================================
# TreeCarbon: Getting Started Examples
# ==================================================

############ set up ####################
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Install package
#devtools::install_github("izaopenshaw/TreeCarbon", force = TRUE)

# Load package
library(TreeCarbon)

########## Calculate above-ground carbon for an oak tree ##########
# WCC #
result <- fc_agc(
  name = "Oak",
  dbh = 45,        # cm
  height = 18,     # metres
  type = "broadleaf"
)

cat(sprintf("Above-ground Carbon: %.3f tonnes\n", result$AGC_WCC_t))

# Bunce #
result <- Bunce(name = "Oak", dbh = 45)
cat(sprintf("Biomass: %.1f kg (%.3f tonnes)\n",
            result$biomass, result$biomass/1000))

############ Calculate carbon for sample data of multiple trees ##########
# Sample data
trees <- data.frame(species = c("Oak", "Beech", "Ash", "Birch"),
  dbh = c(45, 38, 52, 28),      height = c(18, 22, 24, 15),
  type = rep("broadleaf", 4))

# WCC #
results <- fc_agc(name = trees$species,   dbh = trees$dbh,
                  height = trees$height,  type = trees$type)
cat(sprintf("Total carbon: %.2f tonnes\n", sum(results$AGC_WCC_t, na.rm = TRUE)))

############ Compare carbon calculation for different methods ##########
comparison <- allometries(genus = "Quercus",         species = "robur",
                          dbh = 45,                  height = 18,
                          coords = c(-0.29, 51.48),  returnv = "AGC")

print(comparison[, c("genus", "dbh", "WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")])

############ Calculate carbon with uncertainty for WCC ####################
result <- fc_agc_error(name = "Oak",  dbh = 45,  height = 18, type = "broadleaf",
                       re_dbh = 0.025, re_h = 0.05 ) # 2.5% DBH error, 5% height error

cat(sprintf("Carbon: %.3f ± %.3f tonnes\n", result$AGC_WCC_t, result$sig_AGC))

############ Carbon with rich output for WCC ############
result <- fc_agc(name = "Oak",  dbh = 45,  height = 18,  type = "broadleaf",
  rich_output = TRUE)

print(result)

