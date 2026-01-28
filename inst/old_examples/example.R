# Install package
#devtools::install_github("izaopenshaw/TreeCarbon", force = TRUE) # force = TRUE to install updates.

# Load package
library(TreeCarbon)

# Example tree metrics
dbh    <- 74 # cm
height <- 24 # m
vol <- 50    # m^3
sigma_dbh <- 10
sigma_h <- 2

spcode <- 'OK'

dbh=74
method="IPCC2"
biome="temperate"
returnv ="AGC"
sigma_dbh=10
sigma_h=1

# To access example files
example_path <- system.file("example.R", "WCC_eg.R", package = "WoodlandCarbonCode")
file.edit(example_file)  # Opens the file in a text editor


# Simulation data sets
dbhs <- seq(6,100,1)
heights <- rep(20,95)
n <- length(dbhs)
df <- data.frame(dbh = dbhs, height = heights, name = rep("Quercus robur", n), type = rep("broadleaf", n))
spcodes <- lookspcode(df$name, df$type, returnv = 'single')
sim <- fc_agc_error(spcodes$spcode, df$dbh, df$height)

dbhs <- rep(10, 100, 30)
heights <- seq(1,30,1)
n <- length(dbhs)
df <- data.frame(dbh = dbhs, height = heights, name = rep("Quercus robur", n), type = rep("broadleaf", n))
spcodes <- lookspcode(df$name, df$type, returnv = 'single')
sim <- fc_agc_error(spcodes$spcode, df$dbh, df$height)

plot(sim$height, sim$AGC)
plot(sim$height, sim$sig_AGC)
plot(sim$height, sim$rootbiomass)
plot(sim$height, sim$crownbiomass)

# Our data
df$Classification[df$Classification == "coniferous"] <- "conifer"
sp_codes <- lookspcode(as.character(df$Name), df$Classification, returnv = 'single')
carbon <- fc_agc_error(sp_codes$spcode, df$DBH, df$Height)

df$Classification[df$Classification == "coniferous"] <- "conifer"
sp_codes <- lookspcode(as.character(df$Name), df$Classification, returnv = 'single')
carbon <- fc_agc_error(sp_codes$spcode, df$DBH, df$Height)
colnames(carbon[18]) <- "AGC_WCC"
colnames(carbon[18]) <- "AGC_WCC"

summary <- cbind(df[,c(1,2,5:8,13,20,29,31:37)], carbon[,c(6:19)])

library("ggplot2")
ggplot(summary, aes(x=AGC, y=dbh))+
  geom_point(aes(colour = Habitat, alpha=0.5))
ggplot(summary, aes(x=AGC, y=dbh))+
  geom_point(aes(colour = Classification, alpha=0.5))

# TODO check:
ggplot(summary, aes(x=AGC_WCC, y=AGC))+
  geom_point(aes(colour = Classification, alpha=0.5))

summary$AGC_WCC1 <- summary$AGC
summary$AGC_WCC1[summary$AGC_WCC1 < 0] <- 0

summary$check <- summary$AGC_WCC == round(summary$AGC_WCC1, 10)
ggplot(summary, aes(x=AGC_WCC, y=AGC_WCC1))+
  geom_point(aes(colour = check, alpha=0.5))+
  geom_abline(slope=1, intercept=0)

summary$check <- sapply(1:nrow(summary), function(i) {
  for (dp in 1:10) {
    if (round(summary$AGC_WCC[i], dp) == round(summary$AGC_WCC1[i], dp)) {
      return(dp)
    }
  }
  return(0)
})
summary$check <- as.factor(summary$check)

# Testing
spcode = sp_codes$spcode ; dbh = df$DBH ; height = df$Height
method = "IPCC2"; biome = "temperate"
returnv = "All"; sigma_dbh = 20; sigma_h = 8

####




