#================= Initialise ==================
setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Methodology")
source("Import.R")
# NA values in broadleaf plot 18 total volume
# do we times TLS values by WD?
# to check DBHs of trees
# to add in Zanis equation
# to check WCC lookup tables are same as my estimates: https://rbgkew-my.sharepoint.com/personal/i_openshaw_kew_org/Documents/Documents/RFiles/WCC
# to add in Bunce, Spruce

detach("TreeCarbon")
remove.packages("TreeCarbon")
devtools::install_github("izaopenshaw/TreeCarbon", force = TRUE) # force = TRUE to install updates
library(TreeCarbon)

#================= Calculate carbon using Biomass.R & allodb.R ==================
coords <- c(-0.088837,51.071610)
df <- biomass(df, coords, region="Europe", output.all = TRUE)
df <- allodb(df, coords, output.all = TRUE)

#df0 <- read.csv('Output_Data/df_06_12_24.csv')

#================= Calculate carbon using WCC ==================
#df$Classification[df$Classification == "coniferous"] <- "conifer"
df$Name <- paste(df$Genus_corrected, df$Species_corrected)
sp_codes <- lookspcode(as.character(df$Name), df$Classification, returnv = 'short')

df$spcode <- sp_codes$spcode
df$match <- sp_codes$matchtype
WCC <- fc_agc_error(sp_codes$spcode, df$DBH, df$Height, returnv = "AGC")

df$AGC_WCC <- WCC$AGC_t ;  df$sd_WCC <- WCC$sig_AGC

df$AGB_Bunce_kg <- bunce(df$spcode, df$DBH)

# Convert from biomass to carbon using the Carbon Volatile Fraction for each method
# TLS:
bio2c <- biomass2c(df$TotalVolume_opt/1000, method='Thomas', type=df$Classification, sig_biomass = df$TotalVolume_std/1000)
df$AGC_TLS <- bio2c$AGC    ;  df$sd_TLS  <- bio2c$sig_AGC
# allodb:
bio2c <- biomass2c(df$AGB_allodb_kg*0.001, method='Thomas', type=df$Classification, sig_biomass = df$allodb_sigma*0.001)
df$AGC_allodb <- bio2c$AGC ;  df$sd_allodb  <- bio2c$sig_AGC
# Biomass:
df$AGC_Biomass <- biomass2c(df$AGB_Biomass_kg*0.001, method='Thomas', type=df$Classification)
df$AGC_Bunce <- biomass2c(df$AGB_Bunce_kg*0.001, method='Thomas', type=df$Classification)

rm(bio2c, sp_codes, WCC)

#==================== Plots ====================
df_long <- df %>%
  pivot_longer(cols = starts_with("AGC"),    names_to = "AGC_Type",
    values_to = "AGC_Biomass_tonnes")

ggplot(df_long, aes(x = AGC_Biomass_tonnes, y = DBH, color = AGC_Type)) +
  geom_point() +
  labs(x = "AGC Biomass (tonnes)", color = "Method")
  #+ylim(c(0,100)) +  xlim(c(0, 10))

# Adjust
df$AGC_TLS[is.na(df$AGC_TLS)] <- 0
df$sd_TLS[is.na(df$sd_TLS)] <- 0
df$AGC_WCC[is.na(df$AGC_WCC)] <- 0
df$sd_WCC[df$sd_WCC < 0] <- 1
options(scipen = 999)

# Compare error bars from TLS
ggplot(df, aes(x = AGC_TLS, y = DBH, colour = Height)) +
  geom_point() +
  geom_errorbar(aes(x = AGC_TLS, y = DBH, xmin = AGC_TLS - sd_TLS, xmax = AGC_TLS + sd_TLS), width = 0.2)
#+  ylim(c(0,100)) +  xlim(c(0, 10))

# and WCC
ggplot(df, aes(x = AGC_WCC, y = DBH, colour = Height)) +
  geom_point() +
  geom_errorbar(aes(x = AGC_WCC, y = DBH, xmin = AGC_WCC - sd_WCC, xmax = AGC_WCC + sd_WCC), width = 0.2)

#=========== Aggregate sums of carbon values ===========
totals <- aggregate(cbind(Biomass = df$AGC_Biomass,      allodb = df$AGC_allodb,
                          WCC = df$AGC_WCC,              TLS = df$AGC_TLS,
                          Bunce = df$AGC_Bunce),
  by = list(Habitat = df$Habitat), FUN = function(x) sum(x, na.rm = TRUE))

# Calculate propagated errors for each carbon type
error_totals <- aggregate(cbind(WCC_sd = df$sd_WCC^2,
                                sd_allodb = df$allodb_sigma^2,
                                sd_TLS = df$sd_TLS^2,
                          sd_TLS1 = df$sd_TLS),
  by = list(Habitat = df$Habitat), FUN = function(x) sqrt(sum(x, na.rm = TRUE))) # Sum of squares, then square root

# Combine the totals and errors into one data frame
totals <- merge(totals, error_totals, by = "Habitat")
#totals$rel <- totals$sd_TLS/totals$TLS
#totals$relW <- totals$WCC_sd/totals$WCC

# Aggregate data columns with explicit conversion
sqrt_with_na <- function(x) {ifelse(is.na(x), NA, sqrt(as.numeric(x)))}
aggregated_columns <- data.frame(Biomass = as.numeric(df$AGC_Biomass), allodb = as.numeric(df$AGC_allodb),
  WCC = as.numeric(df$AGC_WCC), TLS = as.numeric(df$AGC_TLS), Bunce = as.numeric(df$AGC_Bunce),
  WCC_sd = as.numeric(df$sd_WCC)^2, sd_allodb = sqrt_with_na(df$allodb_sigma)^2, sd_TLS = sqrt_with_na(df$sd_TLS)^2)

totals0 <- aggregate(. ~ Habitat, data = cbind(Habitat = df$Habitat, aggregated_columns), FUN = sum, na.action = na.omit)

long <- totals %>% pivot_longer(cols = c(Biomass, allodb, WCC, TLS, Bunce), names_to = "Method",values_to = "Carbon")
long

# ==
totals <- aggregate(cbind(
  Biomass = df$AGC_Biomass,
  allodb = df$AGC_allodb,
  WCC = df$AGC_WCC,
  TLS = df$AGC_TLS,
  Bunce = df$AGC_Bunce
), by = list(Habitat = df$Habitat), FUN = function(x) sum(x, na.rm = TRUE))

error_totals <- aggregate(cbind(
  WCC_sd = df$sd_WCC^2,
  sd_allodb = df$allodb_sigma^2,
  sd_TLS = df$sd_TLS^2
), by = list(Habitat = df$Habitat), FUN = function(x) sqrt(sum(x, na.rm = TRUE)))

totals <- merge(totals, error_totals, by = "Habitat")

aggregated_columns <- data.frame(
  Biomass = as.numeric(df$AGC_Biomass),
  allodb = as.numeric(df$AGC_allodb),
  WCC = as.numeric(df$AGC_WCC),
  TLS = as.numeric(df$AGC_TLS),
  Bunce = as.numeric(df$AGC_Bunce),
  WCC_sd = df$sd_WCC^2,
  sd_allodb = df$allodb_sigma^2,
  sd_TLS = df$sd_TLS^2
)

totals0 <- aggregate(. ~ Habitat,
                     data = cbind(Habitat = df$Habitat, aggregated_columns),
                     FUN = sum, na.action = na.omit)
totals0$WCC_sd <- sqrt(totals0$WCC_sd)
totals0$sd_allodb <- sqrt(totals0$sd_allodb)
totals0$sd_TLS <- sqrt(totals0$sd_TLS)


# Convert back to standard deviations
totals0$WCC_sd <- sqrt(totals0$WCC_sd)
totals0$sd_allodb <- sqrt(totals0$sd_allodb)
totals0$sd_TLS <- sqrt(totals0$sd_TLS)

# ==

#reshape(totals,direction = "long",varying = list(names(totals)[2:4]),v.names = "Value",timevar = "Habitat")
long <- as.data.frame(long)
long$sigma <- sqrt(long$sigma)
long$Carbon_per_ha <- NA

totals$area <- c(area_ha_broadleaf, area_ha_conifer, area_ha_coppice)
totals$TLS_per_H <- totals$TLS / totals$area

# Convert biomass estimates to per unit hectar by dividing by habitat area
for(i in 1:nrow(long)){
  if(long$Habitat[i] == "broadleaf"){
    long$Carbon_per_ha[i] <- long$Carbon[i]/sum(plot_areas$plot_area_ha)
    long$sigma[i] <- long$sigma[i]/sum(plot_areas$plot_area_ha)
  }
  if(long$Habitat[i] == "conifer"){
    long$Carbon_per_ha[i] <- long$Carbon[i]/area_ha_conifer
    long$sigma[i] <- long$sigma[i]/area_ha_conifer
  }
  if(long$Habitat[i] == "coppice"){
    long$Carbon_per_ha[i] <- long$Carbon[i]/area_ha_coppice
    long$sigma[i] <- long$sigma[i]/area_ha_coppice
  }
}
long$Carbon_per_ha <- round(long$Carbon_per_ha,1)
long$sigma <- long$sd_TLS
long$sigma <- ifelse(long$Method == "WCC", long$WCC_sd, long$sigma)
#long$sigma <- ifelse(long$Method == "allodb", long$sd_allodb, long$sigma)

# Carbon per habitat per method Barplot
ggplot(long, aes(y=Carbon_per_ha, x=Habitat))+
  geom_bar(stat="identity", width=0.5, fill="forest green")+
  geom_text(aes(label=Carbon_per_ha), vjust=-0.3, size=3.5)+
  facet_grid(~Method)+
  geom_errorbar(aes(ymin=Carbon_per_ha-sigma, ymax=Carbon_per_ha+sigma), width=.2,position=position_dodge(.9))

# TLS Carbon Barplot
TLS <- long[long$Method=="TLS",]
TLS$Habitat <- as.character(TLS$Habitat)
TLS[4,] <- c(as.factor("meadow"), 0,0,"TLS",0,0)
sapply(TLS, class)
TLS$sigma <- as.numeric(TLS$sigma)
TLS$Carbon_per_ha  <- as.numeric(TLS$Carbon_per_ha)
TLS$Habitat  <- as.factor(TLS$Habitat)

TLS$Habitat <- factor(TLS$Habitat, levels = c("broadleaf", "conifer", "coppice", "meadow"))
TLS$Habitat[4] <- "meadow"
ggplot(TLS, aes(y=Carbon_per_ha, x=Habitat))+
  geom_bar(stat="identity", width=0.5, fill="#56B4E9")+
  geom_text(aes(label=Carbon_per_ha), vjust=-0.3, size=3.5)+
  geom_errorbar(aes(ymin=Carbon_per_ha-sigma, ymax=Carbon_per_ha+sigma), width=.2,position=position_dodge(.9))+
  ylab("Carbon per hectar")

# Wood density
ggplot(df, aes(WD, Wood_Density,label = rownames(df)))+
  geom_text(position=position_jitter(height=0.01))

# Export Data tables
setwd("C:/Users/ipr10kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/Methodology")
#write.csv(long, "long.csv")
write.csv(df, "Output_Data/Output_df_070125.csv")

# Testing a subset
subset <- df[c(1,139,230,560,700,800),]

ggplot(subset, aes(y=AGC_Biomass, x=Name))+
  geom_bar(stat="identity", width=0.5, fill="forest green")+
  geom_text(aes(label=AGC_Biomass), vjust=-0.3, size=3.5)+
  geom_errorbar(aes(ymin=AGC_Biomass-error_biomass, ymax=AGC_Biomass+error_biomass), width=.2,position=position_dodge(.9))


# =============== Plot Total Carbon by Habitat =================

# Plot totals by method by habitat
carbon_data <- data.frame(
  Habitat = df$Habitat,
  Carbon_Type = rep(c("TLS", "Biomass", "allodb", "WCC"), each = nrow(df)),
  Carbon_Value = c(df$AGC_TLS,
                   df$AGC_Biomass,
                   df$AGC_allodb,
                   df$AGC_WCC),
  Sigma = rep(c(df$allodb_sigma, df$allodb_sigma, df$allodb_sigma, df$sd_WCC), each = nrow(df))
)

# Summarize by Habitat and Carbon Type
carbon_summary <- aggregate(Carbon_Value ~ Habitat + Carbon_Type, data = carbon_data, sum, na.rm = TRUE)
carbon_summary$Sigma <- aggregate(Sigma ~ Habitat + Carbon_Type, data = carbon_data, mean, na.rm = TRUE)$Sigma

# Plotting with ggplot2 and facet by Habitat
ggplot(carbon_summary, aes(x = Carbon_Type, y = Carbon_Value, fill = Carbon_Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  #geom_errorbar(aes(ymin = Carbon_Value - Sigma, ymax = Carbon_Value + Sigma), width = 0.2) +
  labs(title = "Total Carbon by Habitat with Error Bars",
       x = "Carbon Type",
       y = "Total Carbon (tonnes)") +
  theme_minimal() +
  facet_wrap(~ Habitat)


#============ QSM Error ============
#write.csv(df, "Output_Data/estimates_25_9_24.csv")
#df1 <- read.csv("Output_Data/estimates_25_9_24.csv")

broadlf <- df[df$Habitat == "broadleaf",]
confr <- df[df$Habitat == "conifer",]
coppc <- df[df$Habitat == "coppice",]

broadlf$PlotArea <- NA
broadlf$PlotArea[broadlf$Plot =="sssi12" ] <- plot_areas$plot_area_ha[plot_areas$Plot_ID == 12]
broadlf$PlotArea[broadlf$Plot =="sssi16-17" ] <- sum(plot_areas$plot_area_ha[plot_areas$Plot_ID == 16 | plot_areas$Plot_ID == 17])
broadlf$PlotArea[broadlf$Plot =="sssi5" ] <- plot_areas$plot_area_ha[plot_areas$Plot_ID == 5]
broadlf$PlotArea[broadlf$Plot =="sssi6" ] <- plot_areas$plot_area_ha[plot_areas$Plot_ID == 6]
broadlf$PlotArea[broadlf$Plot =="sssi8-11" ] <- sum(plot_areas$plot_area_ha[plot_areas$Plot_ID == 8 | plot_areas$Plot_ID == 11])
broadlf$PlotArea[broadlf$Plot =="sssi9-10" ] <- sum(plot_areas$plot_area_ha[plot_areas$Plot_ID == 9 | plot_areas$Plot_ID == 10])

n_iterations <- 10000  # TODO: do values level out?

# Empty list to store total carbon for each habitat
total_carbon_estimates <- list()

# Function to calculate total carbon per hectare using Markov Chain
calculate_total_carbon_per_hectare <- function(sub_df, habitat_area_ha) {
  n_trees <- nrow(sub_df)
  total_carbon_chain <- numeric(n_iterations)

  # Initialize with the sum of AGC_TLS values (mean)
  total_carbon_chain[1] <- sum(sub_df$AGC_TLS) / habitat_area_ha  # Convert to per hectare

  # Run the Markov Chain for the given habitat
  for (i in 2:n_iterations) {
    sampled_carbon <- rnorm(n_trees, mean = sub_df$AGC_TLS, sd = sub_df$sd_TLS)
    new_carbon <- sum(sampled_carbon) / habitat_area_ha  # Convert to per hectare
    # Reject negative total carbon values
    if (new_carbon >= 0) {
      total_carbon_chain[i] <- new_carbon
    } else {
      total_carbon_chain[i] <- total_carbon_chain[i-1]  # Stay in the previous state
    }
  }

  return(total_carbon_chain)
}

total_carbon_estimates$conifer <- calculate_total_carbon_per_hectare(df[df$Habitat == "conifer", ], habitat_area_ha=area_ha_conifer)
total_carbon_estimates$coppice <- calculate_total_carbon_per_hectare(df[df$Habitat == "coppice", ], area_ha_coppice)

calculate_broadleaf_total_carbon <- function(broadlf, area_ha_broadleaf) {

  # Group data by plot and calculate total AGC_TLS and plot area for each plot
  plot_summary <- aggregate(cbind(AGC_TLS = broadlf$AGC_TLS, sd_TLS = broadlf$sd_TLS),
                            by = list(Plot = broadlf$Plot, plot_area_ha = broadlf$PlotArea),
                            FUN = sum)

  n_plots <- nrow(plot_summary)  # Correctly count the number of plots
  total_carbon_chain <- numeric(n_iterations)

  # Initialize with the deterministic total carbon per hectare
  total_carbon_chain[1] <- sum(plot_summary$AGC_TLS) / area_ha_broadleaf  # Total carbon per hectare

  # Run the Markov Chain for the broadleaf habitat
  for (i in 2:n_iterations) {
    # Sample total carbon for each plot
    sampled_carbon <- rnorm(n_plots, mean = plot_summary$AGC_TLS, sd = plot_summary$sd_TLS)
    # Calculate total carbon and scale to the entire habitat area
    new_carbon <- sum(sampled_carbon) / area_ha_broadleaf  # Total carbon per hectare
    # Reject negative total carbon values
    if (!is.na(new_carbon) | new_carbon >= 0) {
      total_carbon_chain[i] <- new_carbon
    } else {
      total_carbon_chain[i] <- total_carbon_chain[i - 1]  # Stay in the previous state
    }
  }

  return(total_carbon_chain)
}

# Calculate total carbon estimates for broadleaf habitat using plot data and matching plot areas
total_carbon_estimates$broadleaf <- calculate_broadleaf_total_carbon(broadlf , area_ha_broadleaf)

# ======== MCMC ========
calculate_broadleaf_total_carbon <- function(broadlf, area_ha_broadleaf, n_iterations = 10000) {

  # Get unique plot IDs and their corresponding areas
  plot_ids <- unique(broadlf$Plot)

  broadlf$PlotArea <- as.factor(broadlf$PlotArea)
  plot_areas_ha <- c(as.numeric(levels(broadlf$PlotArea)))

  # Ensure no missing values in plot_areas_ha, skip plots with missing areas
  valid_plots <- !is.na(plot_areas_ha)
  plot_ids <- plot_ids[valid_plots]
  plot_areas_ha <- plot_areas_ha[valid_plots]

  # Calculate initial total carbon per hectare
  plot_carbon <- tapply(broadlf$AGC_TLS, broadlf$Plot, sum, na.rm = TRUE)
  plot_carbon <- plot_carbon[valid_plots]  # Skip invalid plots

  initial_estimate <- sum(plot_carbon / plot_areas_ha, na.rm = TRUE) * area_ha_broadleaf

  # Initialize storage for total carbon estimates
  total_carbon_chain <- numeric(n_iterations)
  total_carbon_chain[1] <- initial_estimate

  # Run the Markov Chain
  for (i in 2:n_iterations) {
    # Sample carbon for each plot
    sampled_totals <- sapply(plot_ids, function(plot) {
      # Subset data for this plot
      plot_data <- broadlf[broadlf$Plot == plot, ]

      # Skip plots with no data or invalid values
      if (nrow(plot_data) == 0 || any(is.na(plot_data$AGC_TLS) | is.na(plot_data$sd_TLS))) {
        return(NA)  # Skip this plot
      }

      # Sample tree-level carbon estimates for the plot
      sampled_carbon <- rnorm(nrow(plot_data), mean = plot_data$AGC_TLS, sd = plot_data$sd_TLS)

      # Reject invalid samples
      if (any(is.na(sampled_carbon))) {
        return(NA)
      }

      # Return the total carbon for this plot
      sum(sampled_carbon, na.rm = TRUE) / plot_areas_ha[which(plot_ids == plot)]
    })

    # Calculate the new total carbon across all plots
    new_total <- sum(sampled_totals, na.rm = TRUE) * area_ha_broadleaf

    # Check for valid new_total and update the chain
    if (!is.na(new_total) && new_total >= 0) {
      total_carbon_chain[i] <- new_total
    } else {
      total_carbon_chain[i] <- total_carbon_chain[i - 1]  # Stay in the previous state
    }
  }

  return(total_carbon_chain)
}
bootstrap_broadleaf_total_carbon <- function(broadlf, area_ha_broadleaf, n_bootstrap = 1000) {
  plot_ids <- unique(sub_df$Plot)
  n_plots <- length(plot_ids)

  # Store bootstrap results
  bootstrap_totals <- numeric(n_bootstrap)

  for (i in 1:n_bootstrap) {
    # Resample plots with replacement
    resampled_plots <- sample(plot_ids, n_plots, replace = TRUE)

    # Calculate total carbon for the resampled plots
    resampled_carbon <- sapply(resampled_plots, function(plot) {
      # Subset data for this plot
      plot_data <- broadlf[broadlf$Plot == plot, ]

      # Sum carbon and scale by plot area
      plot_area_ha <- plot_data$PlotArea[1]
      sum(plot_data$AGC_TLS) / plot_area_ha
    })

    # Scale to habitat area
    bootstrap_totals[i] <- mean(resampled_carbon) * area_ha_broadleaf
  }

  return(bootstrap_totals)
}

# Run MCMC
broadleaf_total_carbon_mcmc <- calculate_broadleaf_total_carbon(broadlf, area_ha_broadleaf)

# Run Bootstrap
broadleaf_total_carbon_bootstrap <- bootstrap_broadleaf_total_carbon(broadlf, area_ha_broadleaf)

# Summarize results
summary_res <- data.frame(
  Method = c("MCMC", "Bootstrap"),
  Mean_Carbon_per_Ha = c(mean(broadleaf_total_carbon_mcmc), mean(broadleaf_total_carbon_bootstrap, na.rm = TRUE)),
  SD_Carbon_per_Ha = c(sd(broadleaf_total_carbon_mcmc), sd(broadleaf_total_carbon_bootstrap, na.rm = TRUE)),
  Relative_Error = c(sd(broadleaf_total_carbon_mcmc) / mean(broadleaf_total_carbon_mcmc),
                     sd(broadleaf_total_carbon_bootstrap, na.rm = TRUE) / mean(broadleaf_total_carbon_bootstrap, na.rm = TRUE))
)
summary_res

# ========== Summarize the results for carbon per hectare ==========
summary_results_MC <- data.frame(Habitat = c("Conifer", "Coppice", "Broadleaf"),
                                 Mean_Carbon_per_Ha = c(mean(total_carbon_estimates$conifer),
                                                        mean(total_carbon_estimates$coppice),
                                                        mean(total_carbon_estimates$broadleaf)),
                                 SD_Carbon_per_Ha = c(sd(total_carbon_estimates$conifer)*2,
                                                      sd(total_carbon_estimates$coppice)*2,
                                                      sd(total_carbon_estimates$broadleaf)*2))
summary_results_MC$rel <- summary_results_MC$SD_Carbon_per_Ha/summary_results_MC$Mean_Carbon_per_Ha
summary_results_MC

summary <- data.frame(Habitat = c("Conifer", "Coppice", "Broadleaf"),
                              Carbon_per_Ha = c(sum(df[df$Habitat == "conifer", ]$AGC_TLS) / area_ha_conifer,
                                                sum(df[df$Habitat == "coppice", ]$AGC_TLS) / area_ha_coppice,
                                                sum(df[df$Habitat == "broadleaf", ]$AGC_TLS, na.rm = TRUE) / area_ha_broadleaf),
                              SD_Carbon_per_Ha = c(sqrt(sum(df[df$Habitat == "conifer", ]$sd_TLS^2/ area_ha_conifer))*2 ,
                                                   sqrt(sum(df[df$Habitat == "coppice", ]$sd_TLS^2/ area_ha_coppice))*2 ,
                                                   sqrt(sum(df[df$Habitat == "broadleaf", ]$sd_TLS^2/ area_ha_broadleaf, na.rm = TRUE)))*2 )

summary <- data.frame(Habitat = c("Conifer", "Coppice", "Broadleaf"),
                              Carbon_Total = c(sum(df[df$Habitat == "conifer", ]$AGC_TLS) ,
                                                sum(df[df$Habitat == "coppice", ]$AGC_TLS) ,
                                                sum(df[df$Habitat == "broadleaf", ]$AGC_TLS, na.rm = TRUE)),
                              SD_Carbon_Total = c(sqrt(sum(df[df$Habitat == "conifer", ]$sd_TLS^2))*2 ,
                                                   sqrt(sum(df[df$Habitat == "coppice", ]$sd_TLS^2))*2 ,
                                                   sqrt(sum(df[df$Habitat == "broadleaf", ]$sd_TLS^2, na.rm = TRUE)))*2,
                              Carbon_per_Ha = c(sum(df[df$Habitat == "conifer", ]$AGC_TLS) / area_ha_conifer,
                                                sum(df[df$Habitat == "coppice", ]$AGC_TLS) / area_ha_coppice,
                                                sum(df[df$Habitat == "broadleaf", ]$AGC_TLS, na.rm = TRUE) / area_ha_broadleaf),
                              SD_Carbon_per_Ha = NA)

summary$SD_Carbon_per_Ha <- mapply(error_product, a = summary$Carbon_Total, sig_a = summary$SD_Carbon_Total,
                              b = c(area_ha_conifer, area_ha_coppice, area_ha_broadleaf),
                              sig_b = sd.area)

summary$rel <- summary$SD_Carbon_per_Ha/summary$Carbon_per_Ha
summary
print(summary_results_MC)
totals$rel <- totals$sd_TLS/totals$TLS

# standard deviation for measuring the area if measured to the nearest 50cm within 100m plot

sd.area <- function(perimeter, RMSE){ # inputs in m
  (2*RMSE*perimeter)/10000   # outputs in ha
}
areas <- c(area_ha_broadleaf, area_ha_conifer, area_ha_coppice)
# broadleaf's perimeters have just been added together for each plot...
totals$TLS_per_H_sd <- mapply(error_product, a = totals$TLS, sig_a = totals$sd_TLS,
                              b = areas,
                              sig_b = mapply(sd.area, perimeter = perimeter_m, RMSE = 0.5)) # aerial imagery that has a reported accuracy of +/- 0.5m RMSE

totals[,c(1, 5, 9, 10, 13)]


set.seed(123)  # For reproducibility

# Bootstrap function to estimate error propagation
bootstrap_error_propagation <- function(n_iterations, a, sig_a, b, sig_b) {
  # a = TLS, b = area
  results <- numeric(n_iterations)

  for (i in 1:n_iterations) {
    # Bootstrap sample for TLS and area (with replacement)
    sampled_a <- sample(a, size = length(a), replace = TRUE)
    sampled_b <- sample(b, size = length(b), replace = TRUE)

    # Resample the uncertainties (if needed, or assume they are independent)
    sampled_sig_a <- sample(sig_a, size = length(sig_a), replace = TRUE)
    sampled_sig_b <- sample(sig_b, size = length(sig_b), replace = TRUE)

    # Calculate TLS_per_H for the resampled values (same formula as before)
    result <- abs(sampled_a * sampled_b) * sqrt((sampled_sig_a / sampled_a)^2 + (sampled_sig_b / sampled_b)^2)

    # Store the result for this iteration
    results[i] <- mean(result)  # Take the mean of the resampled results
  }

  # Return the standard deviation of the bootstrap results
  return(sd(results))
}

# Run bootstrapping bootstrap_sd
bootstrap_error_propagation()

totals$sd_TLS_ha <- mapply(bootstrap_error_propagation, n_iterations = 1000,
       a = totals$TLS, sig_a = totals$sd_TLS,
       b = areas, sig_b = sd.area(perimeter = perimeter_m, RMSE = 0.5))

totals$rel <- totals$sd_TLS_ha/totals$TLS


