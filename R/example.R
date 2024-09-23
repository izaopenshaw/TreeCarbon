# Install package
#devtools::install_github("izaopenshaw/WoodlandCarbonCode", force = TRUE) # force = TRUE to install updates.

# Load package
#library(WoodlandCarbonCode)

# Example tree metrics
dbh    <- 74 # cm
height <- 24 # m
vol <- 50    # m^3
sigma_DBH <- 10
sigma_H <- 2

spcode <- 'OK'

stand_tariff('OK',height)
stand_tariff('OK',height, 1.5)

tariff_vol_area(vol, dbh)
tariff_vol_area(vol, dbh, sigma_vol = 5, sigma_dbh = 10)

# Conifer tariff
conifer_tariff('SP', height, dbh)
conifer_tariff('SP', height, dbh, sigma_h = 1, sigma_dbh = 10)

# Broadleaf tariff
tariff <- broadleaf_tariff(spcode, height, dbh)
broadleaf_tariff(spcode, height, dbh, sigma_h = sigma_H, sigma_dbh = sigma_DBH)

# Merchantable tree volume
mercvol <- merchtreevol(tariff, DBH)
merchtreevol(tariff, DBH, sigma_dbh = sigma_DBH, sigma_tariff = NA)

# Stem volume
treevol(r$mercvol[i], DBH[i])

# Stem Biomass
woodbiomass(r$stemvol[i], rec$NSG)

# Crown Biomass
crownbiomass(rec$Crown, DBH[i])

# Above ground Biomass
r$stembiomass[i] + r$crownbiomass[i]

# Above ground Carbon
biomass2c(AGB,method=method,type,biome=biome)

# Root Biomass
rootbiomass(rec$Root, DBH[i])
