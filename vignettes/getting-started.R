############ set up ####################
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(TreeCarbon)

############ single tree with WCC ##########
# Calculate above-ground carbon for an oak tree
result <- fc_agc(
  name = "Oak",
  dbh = 45,        # cm
  height = 18,     # metres
  type = "broadleaf"
)

# View result
cat(sprintf("Above-ground Carbon: %.3f tonnes\n", result$AGC_WCC_t))

## ----bunce--------------------------------------------------------------------
result <- Bunce(name = "Oak", dbh = 45)
cat(sprintf("Biomass: %.1f kg (%.3f tonnes)\n",
            result$biomass, result$biomass/1000))

## ----batch--------------------------------------------------------------------
# Sample data
trees <- data.frame(
  species = c("Oak", "Beech", "Ash", "Birch"),
  dbh = c(45, 38, 52, 28),
  height = c(18, 22, 24, 15),
  type = rep("broadleaf", 4)
)

# Calculate carbon for all trees
results <- fc_agc(
  name = trees$species,
  dbh = trees$dbh,
  height = trees$height,
  type = trees$type
)

# Summary
cat(sprintf("Total carbon: %.2f tonnes\n", sum(results$AGC_WCC_t, na.rm = TRUE)))

## ----compare, eval=requireNamespace("BIOMASS", quietly = TRUE) && requireNamespace("allodb", quietly = TRUE)----
comparison <- allometries(
  genus = "Quercus",
  species = "robur",
  dbh = 45,
  height = 18,
  coords = c(-0.29, 51.48),  # UK coordinates
  returnv = "AGC"
)

# View carbon estimates from each method
print(comparison[, c("genus", "dbh", "WCC_C_t", "biomass_C_t", "allodb_C_t", "Bunce_C_t")])

## ----uncertainty--------------------------------------------------------------
result <- fc_agc_error(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  re_dbh = 0.025,   # 2.5% DBH error
  re_h = 0.05       # 5% height error
)

cat(sprintf("Carbon: %.3f ± %.3f tonnes\n",
            result$AGC_WCC_t, result$sig_AGC))

## ----rich---------------------------------------------------------------------
result <- fc_agc(
  name = "Oak",
  dbh = 45,
  height = 18,
  type = "broadleaf",
  rich_output = TRUE
)

print(result)

