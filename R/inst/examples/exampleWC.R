
# Install package
#devtools::install_github("izaopenshaw/WoodlandCarbonCode", force = TRUE) # force = TRUE to install updates.

# Load package
#library(WoodlandCarbonCode)


spcode = "OK"
height = 25
dbh = 15
sig_dbh = 1 #to test
sig_h = NA

spcode = c("OK", "NA", "SP", "NA", "NA", "NA", "SP" , "SP")
height = c(10,   NA,   NA,    10,   NA,   10,   10 , NA)
dbh    = c(20,   NA,   NA,    NA,   10,   10,   NA, 10)

tariffs(spcode, height, dbh, sig_h = sig_h, sig_dbh = sig_dbh)

tariffs(c("SP","SP", "SP", "SP"), c(10,10,10,NA), c(20,20,20, 20), sig_h = 1, sig_dbh = 1)
tariffs(c("SP","SP", "SP", "SP"), c(10,10,10,10), c(20,NA,20, 20), sig_h = 1, sig_dbh = 1)

# Example tree metrics
dbh    <- 74 # cm
height <- 24 # m
vol <- 50    # m^3
sigma_dbh <- 10
sigma_h <- 2
dbhs <- seq(6,100,1)
n <- length(dbhs)
df <- data.frame(dbh = dbhs, height = rep(20,n), name = rep("Quercus robur", n))

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


