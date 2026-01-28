# ======================= Import packages ==========================
library(ggplot2)
library(tidyr)
library(car)
library(ggrepel)
library(utils)
library(dplyr)
library(stringr)

#================= Import Stem maps ==================
setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/data")
conifer <- read.csv("conifer.tree-attributes.csv")
coppice <- read.csv("pearcelands.tree-attributes.csv")
plot5  <- read.csv("sssi-plot5.tree-attributes.csv")
plot6  <- read.csv("sssi-plot6.tree-attributes.csv")
plot8.11  <- read.csv("sssi-plot8-11.tree-attributes.csv")
plot9.10  <- read.csv("sssi-plot9-10.tree-attributes.csv")
plot12 <- read.csv("sssi-plot12.tree-attributes.csv")
plot16.17 <- read.csv("sssi-plot16-17.tree-attributes.csv")

# Create new columns to align datasets
coppice$PLOT <- NA  ;  conifer$PLOT <- NA
plot12$dbh_manual <- plot16.17$dbh_manual <- NA

# Collate broadleaf plot data into one data frame
broadleaf <- rbind(plot5, plot6,  plot8.11, plot9.10, plot12, plot16.17)
rm(plot5, plot6, plot9.10, plot12, plot16.17, plot8.11) # clean the environment

# Create new columns for Habitathttp://127.0.0.1:46853/graphics/plot_zoom_png?width=1168&height=822
conifer$Habitat <- "conifer"       ;    coppice$Habitat <- "coppice"
broadleaf$Habitat <- "broadleaf"   ;    broadleaf$Classification <- "broadleaf"
conifer$Classification <- "conifer";    coppice$Classification <- "broadleaf"

#======================= Import Plot sizes =======================
setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Methodology")
plot_areas <- read.csv("OG_Data/C-plot-area-sd.csv")

# Convert values to numeric
plot_areas[ , -which(names(plot_areas) %in% c("nplot", "habitat"))] <-
  lapply(plot_areas[ , -which(names(plot_areas) %in% c("nplot", "habitat"))], as.numeric)

# Area in hectares
area_ha_broadleaf <- 0.0001*sum(plot_areas$Area[plot_areas$habitat == "broadleaf"])
area_ha_conifer <- 0.0001*sum(plot_areas$Area[plot_areas$nplot == "forest ridge"])
area_ha_coppice <- 0.0001*plot_areas$Area[plot_areas$nplot == "pearceland"]

# ================ Select columns & collate all habitats into one data frame ================
df <-          cbind(conifer$species, conifer$DBHqsm, conifer$DBHcyl, conifer$TreeHeight, conifer$Habitat, conifer$PLOT, conifer$Classification, conifer$TotalVolume_opt_100, conifer$TotalVolume_opt, conifer$TotalVolume_std_100, conifer$TotalVolume_std, conifer$hull_volume, conifer$dbh_manual, conifer$tree, conifer$x_m, conifer$y_m)
df <- rbind(df,cbind(coppice$species, coppice$DBHqsm, coppice$DBHcyl, coppice$TreeHeight, coppice$Habitat, coppice$PLOT, coppice$Classification, coppice$TotalVolume_opt_100, coppice$TotalVolume_opt, coppice$TotalVolume_std_100, coppice$TotalVolume_std, coppice$hull_volume, coppice$dbh_manual, coppice$tree, coppice$x_m, coppice$y_m))
df <- rbind(df,cbind(broadleaf$species, broadleaf$DBHqsm, broadleaf$DBHcyl, broadleaf$TreeHeight, broadleaf$Habitat, broadleaf$PLOT, broadleaf$Classification, broadleaf$TotalVolume_opt_100, broadleaf$TotalVolume_opt, broadleaf$TotalVolume_std_100, broadleaf$TotalVolume_std, broadleaf$hull_volume, broadleaf$dbh_manual, broadleaf$tree, broadleaf$x_m, broadleaf$y_m))
df <- as.data.frame(df)
colnames(df) <- c("Species","DBHqsm", "DBHcyl", "Height","Habitat", "Plot", "Classification","TotalVolume_opt_100","TotalVolume_opt","TotalVolume_std_100","TotalVolume_std","Crown_Area", "DBH_Manual", "treeID", "x_m", "y_m")

#====== Correct classes ======
sapply(df, class)
for (i in c("DBHqsm", "DBHcyl", "Height", "TotalVolume_opt_100", "TotalVolume_std_100", "TotalVolume_opt", "TotalVolume_std", "Crown_Area", "DBH_Manual")) {
  df[[i]] <- as.numeric(df[[i]])
}
df$Habitat  <- as.factor(as.character(df$Habitat))
df$Species  <- as.factor(df$Species)

#====== Additional tidying ======
# Indexes for reference
df$ID <- 1:nrow(df)

# Seperate Genus and Species
df$Name <- trimws(df$Species)
#table(df$Name)
df$Name[df$Name == "hazel"] <- "Corylus avellana"

df <- mutate(df, Genus = word(Name), Species = word(Name,2,-1))

df <- df %>% relocate(Name, Genus)

# Clean classification
df$Classification[df$Genus == "Taxus"] <- "conifer"

