#======= Initialise =======
# Install package
devtools::install_github("izaopenshaw/TreeCarbon", force = TRUE) # force = TRUE to install updates

# Load package
library(TreeCarbon)

#======= Example data set =======
setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents")
eg <- read.csv("example_data.csv")

als <- allometries(eg$genus, eg$species, eg$dbh, eg$height, eg$type)

genus <- eg$genus
species <- eg$species
dbh <- eg$dbh
height <- eg$height
type <- eg$type
method ="IPCC2" ; returnv = "AGC" ; region = "Europe"
biome = "temperate" ; coords = c(-0.088837,51.071610) ; re_dbh = 0.05 ; re_h = 0.1; re = 0.025; nsg = NULL; sig_nsg = 0.09413391; checkTaxo = FALSE

#======= Simulation data set =======
dbhs <- seq(6,100,1)
n <- length(dbhs)
set.seed(101)
heights <- rnorm(n, mean = 15, sd = 3)
df <- data.frame(DBH = dbhs, Height = heights, type = rep("broadleaf", n),
                 Genus = rep("Quercus", n), Species = rep("robur", n))
df$name <- paste(df$Genus, df$Species)
head(df)

#======= Get AGC from WCC =======
# Get species code
spcodes <- lookspcode(df$name , type = df$type, returnv = 'short')
df$spcode <- spcodes$spcode

df$AGC_WCC <- fc_agc(spcode = df$spcode, dbh = df$DBH, height = df$Height, method = "Thomas", returnv = "AGC")

#======= Get AGC using BIOMASS and allodb =======
coords <- c(-0.088837,51.071610)
bio <- biomass(df$DBH, df$Height, df$Genus, df$Species, coords, region="Europe", output.all = TRUE)
df$AGB_Biomass_kg <- bio$AGB_Biomass_kg

allo <- allodb(df$DBH, df$Genus, df$Species, coords, output.all = TRUE)
df$AGB_allodb_kg <- allo$AGB_allodb_kg
df$allodb_sigma <- allo$allodb_sigma

df$AGB_Bunce_kg <- bunce(df$spcode, df$DBH)

# Convert to AGC in tonnes
df$AGC_Biomass <- biomass2c(df$AGB_Biomass_kg*0.001, method='Thomas', type=df$type)
df$AGC_allodb <- biomass2c(df$AGB_allodb_kg*0.001, method='Thomas', type=df$type)
df$AGC_Bunce <- biomass2c(df$AGB_Bunce_kg*0.001, method='Thomas', type=df$type)

df <- df[,c(1:3,6,8,12:14)]

#======= Plot =======
library(ggplot2)
library(tidyr)

df_long <- df %>%
  pivot_longer(cols = starts_with("AGC"),    names_to = "AGC_Type",
               values_to = "AGC_Biomass_tonnes")

ggplot(df_long, aes(x = AGC_Biomass_tonnes, y = DBH)) +
  geom_point(aes(colour = AGC_Type), alpha = 0.8) +
  labs(x = "AGC Biomass (tonnes)", color = "Method")

#======= Step by Step process for WCC =======

# Since height is all greater than 6.5 we don't use the sapling model
range(heights)

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


# Run shiny app
TreeCarbo::run_shiny_app()


