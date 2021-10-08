library(raster)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(prettymapr)
library(viridis)

#set working directory
setwd("C:/github/MIMICS_HiRes")

### Set path to "pool" rasters
pool_path = "Mapping/Map_data_out/pset_RCrk_TSOI1_map_data/SOC_rasters"


####################################################
# Create dataframe with total raster cell sum
####################################################
# Find the raster filenames
pset_pool_rasters <- list.files(pool_path, pattern='\\.tif$')

pset_pool_df <- data.frame(layer=0, sum=0)

for (j in 1:length(pset_pool_rasters)){
  tot <- sum(getValues(raster(paste0(pool_path, "/", pset_pool_rasters[j]))), na.rm = T)
  df <- data.frame(layer=j, sum=tot)
  pset_pool_df <- rbind(pset_pool_df, df)
}


################################################
# Get total SOC stocks for comparisons
################################################
pset_SOC_rasters <- list.files("Mapping/Map_data_out/estSOC", pattern='\\.tif$')

pset_SOC_df <- data.frame(layer=0, sum=0)

for (j in 1:length(pset_SOC_rasters)){
  tot <- sum(getValues(raster(paste0("Mapping/Map_data_out/estSOC/", pset_SOC_rasters[j]))), na.rm = T)
  df <- data.frame(layer=j, sum=tot)
  pset_SOC_df <- rbind(pset_SOC_df, df)
}


################################################
# Calc summary stats
################################################

# Calc mean and std deviation
pool_tot_mean <- mean(pset_pool_df$sum[-1]) *10*0.000001 #convert to kg from kg/m2, since each grid cell is 10 m2
pool_tot_sd <- sd(pset_pool_df$sum[-1]) *10*0.000001     #then from kg to kt

# Calc mean and std deviation
SOC_tot_mean <- mean(pset_SOC_df$sum[-1]) *10*0.000001 #convert to kg from kg/m2, since each grid cell is 10 m2
SOC_tot_sd <- sd(pset_SOC_df$sum[-1]) *10*0.000001     #then from kg to kt

# pool proportion of SOC
pool_prop_df <- data.frame(SOC=pset_SOC_df$sum,
                          pool=pset_pool_df$sum)[-1,]
pool_prop_df$poolprop <- pool_prop_df$pool/pool_prop_df$SOC
  
mean(pool_prop_df$poolprop)
sd(pool_prop_df$poolprop)

#SOC diff, [for scenario rasters]
scen_diff_df <- data.frame(def_SOC=pset_SOC_df$sum,
                           scen_SOC=pset_pool_df$sum)[-1,]
scen_diff_df$diff <- scen_diff_df$scen_SOC-scen_diff_df$def_SOC

mean(scen_diff_df$diff)*10*0.000001 
sd(scen_diff_df$diff)*10*0.000001 



