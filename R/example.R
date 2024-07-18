#==========Initialise===========
#remove.packages("WoodlandCarbonCode")
#detach("package:WoodlandCarbonCode", unload = TRUE)
#unlink("C:/Users/ipr10kg/AppData/Local/R/win-library/4.3/00LOCK-WoodlandCarbonCode", recursive = TRUE)

#temp_files <- list.files(tempdir(), full.names = TRUE)
#file.remove(temp_files)

# Install package
#devtools::install_github("izaopenshaw/WoodlandCarbonCode", force = TRUE) # force = TRUE to install updates.

# Load package
#library(WoodlandCarbonCode)

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

# to Lookup species code

# Search genus and species
#for(i in 1:nrow(df)){
#  rec <- sp_lookupdf[sp_lookupdf$latin_name == df$Name[i],]
#  if(nrow(rec) == 0){    # if species is not found, search genus
#    rec <- sp_lookupdf[sp_lookupdf$General.for.genus == df$Genus_corrected[i],] # * look up
#    if(nrow(rec) == 0){   # if genus not found, get general code for broadleaf/conifer
#      rec <- sp_lookupdf[sp_lookupdf$General.for.classification == df$Classification[i],]
#    }
#  }
#}

spcode <- rec$single
tariff <-  broadleaf_tariff(spcode, height, dbh)
tariff

#==Method C: conifer
rec_c <- sp_lookupdf[sp_lookupdf$General.for.classification=="coniferous",]
spcode_c <- rec_c$single
conifer_tariff(spcode_c, height, dbh)

#==Method D: dense and dark stands
stand_tariff(spcode, height)

#===========Volume===========
#==Merchantable tree volume
mercvol <-  merchtreevol(tariff, dbh)
mercvol
#==Stem volume
stemvolume <-  treevol(mercvol, dbh)
stemvolume
#==Stem Biomass
stembiomass <-  woodbiomass(stemvolume, rec$NSG)
stembiomass
#==Crown Biomass
crownbio <-  crownbiomass(rec$Crown, dbh)
crownbio
#==Total above ground carbon
biomass <- (stembiomass + crownbio)*0.5
biomass <- fc_agc(spcode, dbh, height, method="IPCC1", biome="temperate", "AGC")
biomass

AGC <-  fc_agc(spcode, dbh, height, method="IPCC2", biome="temperate", "AGC")
fc_agc(spcode, dbh, height, method="IPCC2", biome="temperate", "All")

AGC <- fc_agc(spcode=c(spcode,spcode),
              DBH= c(75, 76),
              height= c(height, height+0.1),
              method="IPCC2", biome="temperate", "AGC")
all <- fc_agc(spcode=c(spcode,spcode),
              DBH= c(75, 76),
              height= c(23, 25),
              method="IPCC2", biome="temperate", "ALL")


##
# Lookup WCC code for each species
#for(i in 1:nrow(df)){
#  # Search genus and species
#  rec <- sp_lookupdf[sp_lookupdf$latin_name == df$Name[i],]
#  if(nrow(rec) == 0){    # if species is not found, search genus
#    rec <- sp_lookupdf[sp_lookupdf$General.for.genus == df$Genus_corrected[i],] # * look up
#    if(nrow(rec) == 0){   # if genus not found, get general code for broadleaf/conifer
#      rec <- sp_lookupdf[sp_lookupdf$General.for.classification == df$Classification[i],]
#    }
#  }
#  df$code[i] <- rec$short
#}

#for (i in 1:nrow(df)){
#  r <- df[i,]
#  df[i,]$AGC_WCC <- fc_agc(spcode=r$code,dbh=r$DBH,height=r$Height,returnv="AGC")
#}
##

# Root Biomass
root.biomass <-  rootbiomass(rec$Root, dbh)

broad_sap_seedling2C(100)
con_sap_seedling2C(100)

# biomass2c
biomass2c(biomass,method="Matthews1")
biomass2c(biomass,method="Matthews2",type="broadleaf")
biomass2c(biomass,method="IPCC1")
biomass2c(biomass,method="IPCC2",type="broadleaf",biome="temperate", return="error")
biomass2c(biomass,method="Thomas",type="broadleaf",biome="temperate", return="error")

#multiple
#note should work with vectors, but not at the moment
sp <- c('OK',"AH","SP","SS")
d <- c(70,35,50,60)
h <- c(17,20,25,28)
df <- data.frame(sp, d, h)
#empty field to put results
df$carbon_ton <- NaN

fc_agc(spcode,dbh,height,returnv="AGC")

#for (i in 1:nrow(df)){
#  r <- df[i,]
#  df[i,]$carbon_ton <- fc_agc(spcode=r$sp,dbh=r$d,height=r$h,returnv="AGC")
#}

#df <- read.csv("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Methodology/Stem_map_Pearcelands&Conifer.csv")
#df$code <- NA
# * to check spelling first with biomass package

#for(i in 1:nrow(df)){
#  rec <- sp_lookupdf[sp_lookupdf$latin_name == df$Name[i],]

#  if(nrow(rec) == 0){
#    rec <- sp_lookupdf[sp_lookupdf$General.for.genus == df$Genus[i],]
#    if(nrow(rec) == 0){
#      rec <- sp_lookupdf[sp_lookupdf$General.for.classification == df$Classification[i],]
#    }
# }
#  df$code[i] <- rec$short
#}

#itree result
#https://mytree.itreetools.org#/benefits/error=&warning=&creationDate=1652891933748&group=&note=/uuid=c4a556e9-8e19-4846-98e7-569bb8ad15d2&formatted=Richmond+TW9+3AB%2C+UK&entered=Royal+Botanic+Gardens%2C+Richmond+TW9+3AB%2C+UK&city=Washington&county=District+of+Colombia&latitude=51.47884371687695&longitude=-0.2927330131994329&nation=US&stateAbbr=DC&zip=20003&pin=false/uuid=f5b7aebc-00cf-4e29-8c7d-c62e88faec18&common=Northern+red+oak&condition=0&diameter=27.559055118110237&exposure=0&measurementType=true&namingType=true&scientific=Quercus+rubra&species=QURU&type=&trillionTrees=null/uuid=65e83431-d13a-4b5f-bcae-585ccd59e158&direction=-1&distance=-1&proximity=2&vintage=-1/uuid=d3cee964-3d49-4249-b427-d60c9dfd59fa&optIn=false&emailed=true"

###################
# progression of error
# example for volume
# assuming 5% error for diameter and 2 m height
#v <- pro_error_vol(70,2.8,17,2)

d<-60:80
dsd <- h*0.05
h <- 7:27
hsd <- h*0.1

#p <- pro_error_vol(d,dsd,h,hsd)

#examples using oak with 20% vol sd and 4 m3

tc <- pro_error_carbon(4,0.8,0.5747142,0.0708236,0.488,0.003)

