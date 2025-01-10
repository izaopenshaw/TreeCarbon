# Install package
#devtools::install_github("izaopenshaw/TreeCarbon", force = TRUE) # force = TRUE to install updates.

# Load package
library(TreeCarbon)

df <- read.csv("R/ins/examples/WCC_eg.csv")

set.seed(101)

# Simulation data set
dbhs <- seq(6,100,1)
n <- length(dbhs)
heights <- rnorm(n, mean = 15, sd = 3)
df <- data.frame(dbh = dbhs, height = heights, name = rep("Quercus robur", n), type = rep("broadleaf", n))
head(df)

#======= Get AGC =======
AGC <- fc_agc(spcode = df$spcode, dbh = df$dbh, height = df$height,
              method = "IPCC2", returnv = "AGC")

#======= Step by Step process =======

# Since height is all greater than 6.5 we don't use the sapling model
range(heights)

# Get species code
spcodes <- lookspcode(df$name , type = df$type, returnv='short')
df$spcode <- spcodes$spcode

# Get tariff number
df$tariff <- tariffs(df$spcode, df$height, df$dbh)

# Merchantable tree volume
df$merchvol <- merchtreevol(df$dbh, df$tariff)

# Stem volume
df$stemvol <- treevol(df$merchvol, dbh = df$dbh)

# Get NSG for Quercus robur
rec <- lookup_df[lookup_df$short == df$spcode[1], ]
df$nsg <- rep(rec$NSG, n)

# Stem Biomass
df$woodbio <- woodbiomass(df$stemvol, df$nsg)

# Crown Biomass
crownbio <- crownbiomass(rep(rec$Crown, n), df$dbh)
df$crownbio <- crownbio$biomass

# Above ground Biomass
df$AGB <- df$woodbio + df$crownbio

# Above ground Carbon
df$AGC <- biomass2c(df$AGB, method='Thomas', df$type)




