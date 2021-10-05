library(tidyverse)
library(furrr)
library(purrr)
library(raster)
library(rgdal)

setwd("C:/github/MIMICS_HiRes")

# Load map data (output from MIMICS_soil_C_mapper.R)
MIM_Map_data_full <- readRDS("Mapping/Map_data_out/MIMICS_map_data_full_pset1.rds")


##################################################
# Load MIMICS forcing data rasters
#################################################

# Set path to raster files
raster_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"

# Load raster data
estGPP <- raster(paste0(raster_path,"MSAVI_estGPP.tif"))
estTSOI <- raster(paste0(raster_path,"tsoi_est2.tif"))
estCLAY <- raster(paste0(raster_path,"RCrk_estClay.tif"))
estLIGN <- raster(paste0(raster_path,"RCrk_estLigN.tif"))


raster_n <- ncell(estGPP)

raster_frc <- data.frame(Site=seq(1,raster_n),
                         pGPP=as.numeric(getValues(estGPP)),
                         TSOI=as.numeric(getValues(estTSOI)),
                         CLAY=as.numeric(getValues(estCLAY)),
                         lig_N=as.numeric(getValues(estLIGN)))


#################################################
# Fill in NA rows for new raster data
#################################################
MIM_Map_data_full <- merge(raster_frc, MIM_Map_data_full, by.x = "Site",
                           by.y = "Site", all.x = TRUE, all.y = FALSE)


#################################################
# Build rasters from map data
#################################################

## copies raster construct and meta from estGPP, refill with MIMICS output values
estSOC <- setValues(estGPP, MIM_Map_data_full$MIMSOC)
estMIMLIT <- setValues(estGPP, MIM_Map_data_full$MIMLIT)
estMIMMIC <- setValues(estGPP, MIM_Map_data_full$MIMMIC)
estLITm <- setValues(estGPP, MIM_Map_data_full$LITm)
estLITs <- setValues(estGPP, MIM_Map_data_full$LITs)
estMICr <- setValues(estGPP, MIM_Map_data_full$MICr)
estMICk <- setValues(estGPP, MIM_Map_data_full$MICK)
estMIMCO <- setValues(estGPP, MIM_Map_data_full$MIM_CO)
estSOMa <- setValues(estGPP, MIM_Map_data_full$SOMa)
estSOMc <- setValues(estGPP, MIM_Map_data_full$SOMc)
estSOMp <- setValues(estGPP, MIM_Map_data_full$SOMp)

#################################################
# Plot rasters
#################################################

## MIMICS output maps
plot(estSOC)
plot(estMIMLIT)
plot(estMIMMIC)
plot(estLITm)
plot(estLITs)
plot(estMICr)
plot(estMICk)
plot(estMIMCO)
plot(estSOMa)
plot(estSOMc)
plot(estSOMp)

## MIMICS forcing data maps
plot(estGPP)
plot(estTSOI)
plot(estCLAY)
plot(estLIGN)

#################################################
# Save rasters
#################################################
save_path <- "C:/local_temp/MIMICS_raster_dump"

writeRaster(estSOC, paste0(save_path,"estSOC.tif"), options=c('TFW=YES'))
writeRaster(estMIMLIT, paste0(save_path, "estMIMLIT.tif"), options=c('TFW=YES'))
writeRaster(estMIMMIC, paste0(save_path, "estMIMMIC.tif"), options=c('TFW=YES'))
writeRaster(estLITm, paste0(save_path, "estLITm.tif"), options=c('TFW=YES'))
writeRaster(estLITs, paste0(save_path, "estLITs.tif"), options=c('TFW=YES'))
writeRaster(estMICr, paste0(save_path, "estMICr.tif"), options=c('TFW=YES'))
writeRaster(estMICk, paste0(save_path, "estMICK.tif"), options=c('TFW=YES'))
writeRaster(estMIMCO, paste0(save_path, "estMIMCO.tif"), options=c('TFW=YES'))
writeRaster(estSOMa, paste0(save_path, "estSOMa.tif"), options=c('TFW=YES'))
writeRaster(estSOMc, paste0(save_path, "estSOMc.tif"), options=c('TFW=YES'))
writeRaster(estSOMp, paste0(save_path, "estSOMp.tif"), options=c('TFW=YES'))

