# Install package
devtools::install_github("izaopenshaw/WoodlandCarbonCode", force = TRUE) # force = TRUE to install updates.

# Load package
library(WoodlandCarbonCode)

# Example tree metrics
dbh    <- 74 # cm
height <- 24 # m
vol <- 50    # m^3

#==========Tariff Number===========
#==Method A: felled tree
tariff_vol_area(vol, dbh)

#==Method B: broadleaf
# Lookup species code in
data(sp_lookupdf)
rec <- sp_lookupdf[sp_lookupdf$General.for.genus=="Quercus",] # either by general genus
rec <- sp_lookupdf[sp_lookupdf$common_name=="oak",] # or by common name

spcode <- rec$single
btariff <-  broadleaf_tariff(spcode, height, dbh, dbh_sd = 5, height_sd = 1)
mercvol <- merchtreevol(dbh, tariff = btariff[1], dbh_sd = 1, tariff_sd = btariff[2])
stemvolume <-  treevol(mtreevol = mercvol[1], dbh, mtreevol_sd = mercvol[2])

#==Stem Biomass
stembiomass <-  woodbiomass(treevol = stemvolume, nsg = rec$NSG)
stembiomass
#==Crown Biomass
crownbio <-  crownbiomass(rec$Crown, dbh, dbh_sd)
crownbio
#==Total above ground carbon
biomass <- (stembiomass + crownbio)*0.5
biomass <- fc_agc(spcode, dbh, height, method="IPCC1", biome="temperate", "AGC")
biomass


# Test with my data & add to methodology.R
#setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Methodology")
#df <- read.csv("df_7_24.csv")


