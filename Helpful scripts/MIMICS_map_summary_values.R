library(tidyverse)
library(furrr)
library(purrr)
library(raster)
library(rgdal)

setwd("C:/github/MIMICS_HiRes")

########################################
# Calculate SOC stock total for RCEW
########################################

# Set path to raster files
soc_raster_path <- "C:/github/MIMICS_HiRes/Figures/Fig4_estSOC_map/"

## Load raster data to get the 
estSOC <- raster(paste0(soc_raster_path,"estSOC_pset_mean.tif"))
plot(estSOC)

# Sum all SOC stocks, upscale to 10 m2 from m2
# Then convert from kg to Tt
sum(getValues(estSOC), na.rm=T)*10 *0.000001#kg in the ~2.39 km2 RCEW


###########################################
# Calculate standard deviation from pset map SOC stock totals
###########################################

# Get raster filenames from local folder
estSOC_rasters <- list.files("C:/github/MIMICS_HiRes/Mapping/Map_data_out/estSOC", pattern='\\.tif$')

# Get cropland polygon
agLand <- readOGR(paste0("C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/RCrk_agLand.shp"))

tot_soc <- NULL

# Loop through SOC pset layers, crop otu agland, and calc total SOC
for(i in 1:length(estSOC_rasters)) {
  estSOC <- raster(paste0("C:/github/MIMICS_HiRes/Mapping/Map_data_out/estSOC/", estSOC_rasters[i]))
  estSOC_crop <- mask(estSOC, agLand, inverse=TRUE)
  tot_soc[i] <- sum(getValues(estSOC_crop), na.rm=T)
}

mean(tot_soc) * 10 *0.000001 #kt C
sd(tot_soc) * 10 *0.000001 #kt C


#####################################
# Calculate aspect class from DEM
#####################################

# Set path to raster files
DEM_raster_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"

# GEt DEM raster
DEM <- raster(paste0(DEM_raster_path,"DEM_extract1.tif"))

# Calculate aspect in degree from DEM
DEM_aspect = terrain(DEM, 'aspect', unit='degrees', neighbors=8)

## Classify aspect
# create a matrix of values that represent the classification ranges
### NSEW ### 
# North = 1
# South = 2
# East = 3
# West = 4
class.m <- c(0, 45, 1,
             45, 135, 3,
             135, 225, 2,  
             225 , 315, 4,
             315, 360, 1)

# reshape the object into a matrix with columns and rows
rcl.m <- matrix(class.m, 
                ncol=3, 
                byrow=TRUE)

# reclassify the raster using the reclass object - rcl.m
RC_aspect_NSEW <- reclassify(DEM_aspect, 
                           rcl.m)

# 1=North, 2=South
plot(RC_aspect_NSEW)

########################
# Save aspect rasters
########################
forcing_rasters_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"

writeRaster(DEM_aspect, paste0(forcing_rasters_path, "RC_Aspect_Deg.tif"), options=c('TFW=YES'))
writeRaster(RC_aspect_NSEW, paste0(forcing_rasters_path, "RC_Aspect_NSEW.tif"), options=c('TFW=YES'))



####################################################
# Calculate stocks by aspect class, north vs south
####################################################

# Stack rasters
SOC_aspect_stack <- stack(mask(estSOC, agLand, inverse=TRUE), 
                          mask(RC_aspect_NSEW, agLand, inverse=TRUE),
                          mask(DEM, agLand, inverse=TRUE)) 

# Create new raster with SOC stocks only for north slopes, all other cell = 0
N_SOC <- overlay(x = SOC_aspect_stack[[1]], y = SOC_aspect_stack[[2]], fun = function(x, y) ifelse(y == 1, x, NA))

# Create new raster with SOC stocks only for south slopes, all other cell = 0
S_SOC <- overlay(x = SOC_aspect_stack[[1]], y = SOC_aspect_stack[[2]], fun = function(x, y) ifelse(y == 2, x, NA))

# Calculate mean concentration by slope
N_SOC_mn <- mean(getValues(N_SOC), na.rm=T)
S_SOC_mn <- mean(getValues(S_SOC), na.rm=T)


### Calculate for elevation above 1600 m
N_1600_SOC <- overlay(x = SOC_aspect_stack[[1]], y = SOC_aspect_stack[[2]], z = SOC_aspect_stack[[3]], 
                      fun = function(x, y, z) ifelse(y == 1 & z > 1600, x, NA))

S_1600_SOC <- overlay(x = SOC_aspect_stack[[1]], y = SOC_aspect_stack[[2]], z = SOC_aspect_stack[[3]], 
                      fun = function(x, y, z) ifelse(y == 2 & z > 1600, x, NA))

# Calculate mean concentration by slope above 1600 m
N_1600_SOC_mn <- mean(getValues(N_1600_SOC), na.rm=T)
S_1600_SOC_mn <- mean(getValues(S_1600_SOC), na.rm=T)

N_1600_SOC_mn/S_1600_SOC_mn

