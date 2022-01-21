library(raster)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(prettymapr)
library(viridis)

#set working directory
setwd("C:/github/MIMICS_HiRes/Figures")

# Set path to raster files
raster_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"

# Load raster data
GEP <- raster(paste0(raster_path,"MSAVI_estGPP.tif"))
MAST <- raster(paste0(raster_path,"tsoi_est2.tif"))
CLAY <- raster(paste0(raster_path,"RCrk_estClay.tif"))
LIGN <- raster(paste0(raster_path,"RCrk_estLigN.tif"))

#Crop out agland
agLand <- readOGR(paste0(raster_path,"RCrk_agLand.shp"))

# GEP <- mask(GEP, agLand, inverse=TRUE)
# MAST <- mask(MAST, agLand, inverse=TRUE)
# CLAY <- mask(CLAY, agLand, inverse=TRUE)
# LIGN <- mask(LIGN, agLand, inverse=TRUE)

#create a color palette
pal <- colorRampPalette(c("#c32222",
                          "#f5ff00",
                          "#25f100",
                          "#00d298",
                          "#004fe0",
                          "#5a2dfd"))
### GEP ###
# Setup to save GEP map plot as png
png(file="C:/github/MIMICS_HiRes/Figures/S1_MIMICS_forcing_maps/GEP_map.png", width=4000, height=6000, units="px", res=600)

#overlay raster
plot(GEP,axes=FALSE, box=FALSE,
     main = "GEP", 
     add = FALSE,
     col = viridis(100, direction = -1),
     alpha = 1)

# Add agLand shapefile overlay
plot(agLand,
     add = TRUE,
     col= "grey50",
     border = "grey50")

#Add scale bar & north arrow
addscalebar(plotunit = 'm', widthhint = 0.3, lwd = 1, padin = c(0, 2.5), label.cex = 0.9)
addnortharrow(scale = 0.6, text.col = 'black', cols = c('black', 'black'))

# End save 
dev.off()


### MAST ###
# Setup to save GEP map plot as png
png(file="C:/github/MIMICS_HiRes/Figures/S1_MIMICS_forcing_maps/MAST_map.png", width=4000, height=6000, units="px", res=600)

#overlay raster
plot(MAST,axes=FALSE, box=FALSE,
     main = "MAST", 
     add = FALSE,
     col = viridis(100, direction = 1, option='B'),
     alpha = 1)

# Add agLand shapefile overlay
plot(agLand,
     add = TRUE,
     col= "grey50",
     border = "grey50")

#Add scale bar & north arrow
addscalebar(plotunit = 'm', widthhint = 0.3, lwd = 1, padin = c(0, 2.5), label.cex = 0.9)
addnortharrow(scale = 0.6, text.col = 'black', cols = c('black', 'black'))

# End save 
dev.off()


### CLAY ###
# Setup to save GEP map plot as png
png(file="C:/github/MIMICS_HiRes/Figures/S1_MIMICS_forcing_maps/CLAY_map.png", width=4000, height=6000, units="px", res=600)

#overlay raster
plot(CLAY, axes=FALSE, box=FALSE,
     main = "MAST", 
     add = FALSE,
     col = viridis(100, direction = -1, option='F'),
     alpha = 1)

# Add agLand shapefile overlay
plot(agLand,
     add = TRUE,
     col= "grey50",
     border = "grey50")

#Add scale bar & north arrow
addscalebar(plotunit = 'm', widthhint = 0.3, lwd = 1, padin = c(0, 2.5), label.cex = 0.9)
addnortharrow(scale = 0.6, text.col = 'black', cols = c('black', 'black'))

# End save 
dev.off()


### LIGN ###
# Setup to save GEP map plot as png
png(file="C:/github/MIMICS_HiRes/Figures/S1_MIMICS_forcing_maps/LIGN_map.png", width=4000, height=6000, units="px", res=600)

#overlay raster
plot(LIGN, axes=FALSE, box=FALSE,
     main = "MAST", 
     add = FALSE,
     col = viridis(100, direction = 1, option='G'),
     alpha = 1)

# Add agLand shapefile overlay
plot(agLand,
     add = TRUE,
     col= "grey50",
     border = "grey50")

#Add scale bar & north arrow
addscalebar(plotunit = 'm', widthhint = 0.3, lwd = 1, padin = c(0, 2.5), label.cex = 0.9)
addnortharrow(scale = 0.6, text.col = 'black', cols = c('black', 'black'))

# End save 
dev.off()
