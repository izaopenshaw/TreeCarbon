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
  Genus = df_raw$genus,
  Species = df_raw$species,
  family = df_raw$family,
  Habitat = df_raw$Habitat,
  Plot = df_raw$Plot,
  Classification = df_raw$classification,
  DBHqsm = as.numeric(df_raw$DBHqsm) * 100,   # m to cm
  DBHcyl = as.numeric(df_raw$DBHcyl) * 100,   # m to cm
  DBHqsm_100 = as.numeric(df_raw$DBHqsm_100) * 100,   # m to cm
  DBHcyl_100 = as.numeric(df_raw$DBHcyl_100) * 100,   # m to cm
  DBHsec = as.numeric(df_raw$DBHsec) * 100,   # m to cm
  DBHsec_100 = as.numeric(df_raw$DBHsec_100) * 100,   # m to cm
  DBH = df_raw$dbh,
  DBH_Manual = df_raw$dbh_manual,
  girth_manual = df_raw$girth_manual,
  Height = df_raw$TreeHeight,
  timber_height = df_raw$trunk_height,
  TotalVolume_opt = df_raw$TotalVolume_opt,
  TotalVolume_std = df_raw$TotalVolume_std,
  TotalVolume_opt_100 = df_raw$TotalVolume_opt_100,
  TotalVolume_std_100 = df_raw$TotalVolume_std_100,
  TotalVolume_final = df_raw$TotalVolume_final,
  TotalVolume_final_sd = df_raw$TotalVolume_final_sd,
  Name = df_raw$name,
  wd = df_raw$wd,
  wd_sd = df_raw$wd_sd,
  cf = df_raw$cf,
  wd_contrib = df_raw$wd_contrib,
  agc = df_raw$agc,
  agc_manual = df_raw$agc_manual,
  p_agc = df_raw$p_agc,
  agc_var = df_raw$agc_var,
  agc_var_p = df_raw$agc_var_p,
  h_agc = df_raw$h_agc,
  agc_cumsum = df_raw$agc_cumsum,
  h_agc_p = df_raw$h_agc_p,
  residual = df_raw$residual,
  residual_p = df_raw$residual_p,
  residual_cumsum = df_raw$residual_cumsum,
  tariff = df_raw$tariff,
  mercvol_m.3 = df_raw$mercvol_m.3,
  stemvol_m.3 = df_raw$stemvol_m.3,
  stembiomass_t = df_raw$stembiomass_t,
  crownbiomass_t = df_raw$crownbiomass_t,
  rootbiomass_t = df_raw$rootbiomass_t,
  stringsAsFactors = FALSE
)

df <- data.frame(
  Genus = df_raw$genus,
  Species = df_raw$species,
  family = df_raw$family,
  Habitat = df_raw$Habitat,
  Plot = df_raw$Plot,
  Classification = df_raw$classification,
  DBH = df_raw$dbh,
  DBH_Manual = df_raw$dbh_manual,
  girth_manual = df_raw$girth_manual,
  Height = df_raw$TreeHeight,
  timber_height = df_raw$trunk_height,
  TotalVolume_opt_100 = df_raw$TotalVolume_opt_100,
  TotalVolume_std_100 = df_raw$TotalVolume_std_100,
  Name = df_raw$name,
  wd = df_raw$wd,
  wd_sd = df_raw$wd_sd,
  cf = df_raw$cf,
  agc = df_raw$agc,
  agc_manual = df_raw$agc_manual,
  tariff = df_raw$tariff,
  stembiomass_t = df_raw$stembiomass_t,
  rootbiomass_t = df_raw$rootbiomass_t,
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

habitat_areas <- data.frame(
  Habitat = c("broadleaf", "conifer", "coppice"),
  area_ha = c(sum(plot_areas$plot_area_ha[plot_areas$Plot_ID %in% plots]),
                0.444184542654192,
                5.1681764938),
  perimeter_m = c(sum(plot_areas$perimeter[plot_areas$Plot_ID %in% plots]),
                290.077802,
                1169.538277))

