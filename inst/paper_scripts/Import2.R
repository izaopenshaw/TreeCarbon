# ======================= Import packages ==========================
library(ggplot2)
#library(car)
#library(ggrepel)
#library(utils)
#library(stringr)

setwd("C:/git/TreeCarbon")
df_raw <- read.csv("inst/paper_scripts/wakehurst_agb_2.csv", stringsAsFactors = FALSE)

# ======================= Map columns ==========================
# Convert mean of DBHqsm, DBHcyl from m to cm
# df_raw$DBH <- (df_raw$DBHcyl + df_raw$DBHqsm) / 2 * 100

# Check species names
# table(df_raw$name)

# build dataframe
df <- data.frame(
  Species = df_raw$species,
  DBHqsm = as.numeric(df_raw$DBHqsm) * 100,   # m to cm
  DBHcyl = as.numeric(df_raw$DBHcyl) * 100,   # m to cm
  Height = df_raw$TreeHeight,
  Habitat = df_raw$Habitat,
  Plot = df_raw$Plot,
  Classification = df_raw$classification,
  TotalVolume_opt = df_raw$TotalVolume_opt,
  TotalVolume_std = df_raw$TotalVolume_std,
  DBH_Manual = df_raw$dbh_manual,
  treeID = df_raw$tree,
  x_m = df_raw$x_m,
  y_m = df_raw$y_m,
  Name = df_raw$name,
  Genus = df_raw$genus,
  DBH = df_raw$dbh,
  timber_height = df_raw$trunk_height,
  agc = df_raw$agc,
  agc_var = df_raw$agc_var,
  TotalVolume_final = df_raw$TotalVolume_final,
  TotalVolume_final_sd = df_raw$TotalVolume_final_sd,
  stringsAsFactors = FALSE
)

# Filter: DBH >= 10 cm (0.1 m equivalent) & remove NA
# df <- df[!is.na(df$DBH) & df$DBH >= 10, ]

df$Habitat <- as.factor(as.character(df$Habitat))
df$Species <- as.factor(df$Species)

#======================= Plot sizes =======================
# setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/SOF/Plot IDs")
setwd("~/SOF/Plot IDs")
plot_areas <- read.csv("SSSI_Plot_Areas.csv")
plot_areas$perimeter <- 4*sqrt(plot_areas$plot_area_m.2)

# Plot area
plots <- c(5,6,8:12,16,17)
area_ha_broadleaf <- sum(plot_areas$plot_area_ha[plot_areas$Plot_ID %in% plots])
area_ha_conifer <- 0.444184542654192
area_ha_coppice <- 5.1681764938

perimeter_m = c(sum(plot_areas$perimeter[plot_areas$Plot_ID %in% plots]), # broadleaf
                290.077802,  # conifer
                1169.538277) # coppice
