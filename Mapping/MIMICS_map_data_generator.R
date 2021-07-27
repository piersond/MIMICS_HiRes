######################################################
# Create MIMICS forcing dataset from raster layers
######################################################
library(raster)
library(rgdal)
library(tidyverse)

#set working drive
setwd("C:/github/MIMICS_HiRes")

# Set path to raster files
raster_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"

# Load raster data
estGPP <- raster(paste0(raster_path,"MSAVI_estGPP.tif"))
estTSOI <- raster(paste0(raster_path,"tsoi_est2.tif"))
estCLAY <- raster(paste0(raster_path,"RCrk_estClay.tif"))
estLIGN <- raster(paste0(raster_path,"RCrk_estLigN.tif"))

#smaller test areas
# raster_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/specs/"
# estGPP <- raster(paste0(raster_path,"MSAVI_estGPP_spc.tif"))
# estTSOI <- raster(paste0(raster_path,"tsoi_est2_spc.tif"))
# estCLAY <- raster(paste0(raster_path,"RCrk_estClay_spc.tif"))
# estLIGN <- raster(paste0(raster_path,"RCrk_estLigN_spc.tif"))

raster_n <- ncell(estGPP)

raster_frc <- data.frame(Site=seq(1,raster_n),
                         pGPP=as.numeric(getValues(estGPP)),
                         TSOI=as.numeric(getValues(estTSOI)),
                         CLAY=as.numeric(getValues(estCLAY)),
                         lig_N=as.numeric(getValues(estLIGN)))

# Remove rows if NA exists in data columns
raster_frc_nona <- na.omit(raster_frc)

raster_frc_nona$index <- seq(1,nrow(raster_frc_nona),1)

#Write output
saveRDS(raster_frc_nona, "Mapping/Map_data/MIMICS_map_forcing_data.rds")

