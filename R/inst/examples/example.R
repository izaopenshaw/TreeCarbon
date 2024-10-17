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

dbh=74
method="IPCC2"
biome="temperate"
returnv ="AGC"
sigma_dbh=10
sigma_h=1

# To access example files
example_path <- system.file("example.R", "WCC_eg.R", package = "WoodlandCarbonCode")
file.edit(example_file)  # Opens the file in a text editor
