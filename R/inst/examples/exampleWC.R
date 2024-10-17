
# Install package
#devtools::install_github("izaopenshaw/WoodlandCarbonCode", force = TRUE) # force = TRUE to install updates.

# Load package
#library(WoodlandCarbonCode)



# Example tree metrics
dbh    <- 74 # cm
height <- 24 # m
vol <- 50    # m^3
sigma_dbh <- 10
sigma_h <- 2

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
broadleaf_tariff(spcode, height, dbh, sigma_h = sigma_h, sigma_dbh = sigma_dbh)

# Merchantable tree volume
mercvol <- merchtreevol(tariff, dbh)
merchtreevol(tariff, dbh, sigma_dbh = sigma_dbh, sigma_tariff = NA)



# Test with my data & add to methodology.R
#setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Methodology")
#df <- read.csv("df_7_24.csv")


