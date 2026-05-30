# ===== Wakehurst Tree Carbon Calculation WCC ====================================

setwd("C:/git/TreeCarbon")
devtools::load_all()

# ========= Import =================
source("inst/paper_scripts/Import2.R")

trees <- df

print(table(trees$Habitat)) # Habitats
print(sprintf("Total trees: %d", nrow(trees))) # Total trees

# ======== Habitat Area SD =================================================
# Wakehurst Place coordinates
coords <- c(-0.0867, 51.0660)  # Longitude, Latitude
wakehurst_coords <- coords  # used by allometries, BIOMASS, etc.

# Calculate habitat area sd
habitat_areas$sd_area <- c(
  sd_area(plot_areas$perimeter[plot_areas$Plot_ID %in% plots],
          RMSE = 0.5, n_vertices = 4, sum_plots = TRUE),
  sd_area(perimeter = habitat_areas$perimeter_m[c(2:3)], RMSE = 0.5))

print(habitat_areas)

# Plots Names
trees$Plot[trees$Plot == "08-Nov"] <- 8 # Plot 8-Nov  = Plot 8, Plot 11 (adjacent plots)
trees$Plot[trees$Plot == "09-Oct"] <- 9 # Plot 8-Nov  = Plot 9, Plot 10 (adjacent plots)
trees$Plot2 <- NA
trees$Plot2[trees$Plot == "8"] <- 11
trees$Plot2[trees$Plot == "9"] <- 10
trees$Plot <- as.numeric(as.character(trees$Plot))

# ========== Calculate allometric carbon ===============================
genus <- as.character(trees$Genus)
species <- as.character(trees$Species)
dbh <- trees$DBH # "DBHqsm"               "DBHcyl"               "DBHqsm_100"  "DBHcyl_100"           "DBHsec"               "DBHsec_100"
height = trees$Height
type = trees$Classification
height_timber = trees$timber_height
method = "Matthews2"     # Carbon conversion (FC_WCC 6.3: Matthews 1993)
returnv = "AGC"           # Return Above-Ground Carbon
re_dbh = 0.05             # 5% DBH measurement error
re_h = 0.10               # 10% height measurement error
re = 0.025                # 2.5% coefficient error
region = "Europe"

# Skewness
moments::skewness(dbh, na.rm = TRUE)

# ==== Calculate AGB by WCC with manual DBH ====
name <- paste(genus, species)
wcc  <- fc_agc_error(name = name, dbh = dbh, height = height, type = type,
                     height_timber = height_timber, method = method,
                     re_dbh = re_dbh, re_h = re_h, re = re, rich_output = TRUE)

print(wcc) # rich_output
wcc_df <- wcc$detailed_output

manual_dbh <- cbind(trees, wcc_df)
trees <- manual_dbh  # needed by WCC_stand_Wakehurst.R

setwd("~/SOF/Phil's paper")
write.csv(manual_dbh, "manual_dbh.csv")

