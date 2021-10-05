library(raster)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(prettymapr)
library(viridis)

#set working directory
setwd("C:/github/MIMICS_HiRes")

# Set path to raster files
raster_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"

# Load base raster data
estGPP <- raster(paste0(raster_path,"MSAVI_estGPP.tif"))
estTSOI <- raster(paste0(raster_path,"tsoi_est2.tif"))
RC_hilsh <- raster(paste0(raster_path,"RC_hillsh.tif"))
DEM <- raster(paste0(raster_path,"DEM_extract1.tif"))

# Load sample points
cal_pts <- read.csv("Figures/Fig1_Cal_vs_Val/cal_coords.csv")
val_pts <- read.csv("Figures/Fig1_Cal_vs_Val/val_coords.csv")

############################################################
# Create estLIT summary stats maps
############################################################

# Create raster stack from individual pset rasters
estLIT_rasters <- list.files("Mapping/Map_data_out/estLIT", pattern='\\.tif$')
estLIT_stack <- stack(paste0("Mapping/Map_data_out/estLIT/", estLIT_rasters))

# Calulate map stats
estLIT_mean <- calc(estLIT_stack, fun = mean)
estLIT_sd <- calc(estLIT_stack, fun = sd)

# Plot to see if it worked :)
plot(estLIT_mean)
plot(estLIT_sd)


#############################################
# Remove agland from maps
#############################################

# Load cropland shapefile
agLand <- readOGR(paste0(raster_path,"RCrk_agLand.shp"))

estLIT_mean_crop <- mask(estLIT_mean, agLand, inverse=TRUE)
estLIT_sd_crop <- mask(estLIT_sd, agLand, inverse=TRUE)
#estLIT_range_crop <- mask(estLIT_range, agLand, inverse=TRUE)

plot(estLIT_mean_crop)
plot(estLIT_sd_crop)
#plot(estLIT_range_crop)

###################################
# Save rasters
###################################
save_path <- "Figures/Fig5_C_pool_maps/"

writeRaster(estLIT_mean_crop, paste0(save_path, "estLIT_pset_mean.tif"), options=c('TFW=YES'))
writeRaster(estLIT_sd_crop, paste0(save_path, "estLIT_pset_sd.tif"), options=c('TFW=YES'))

###################################
# Create mean LIT map
###################################

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig5_C pool_maps/estLIT_mn_map_R.png", width=4000, height=6000, units="px", res=600)

# Bin evertything above a specific value
estLIT_mean_crop2 <- estLIT_mean_crop
estLIT_mean_crop2[estLIT_mean_crop > 2] <- 2

#overlay raster
plot(estLIT_mean_crop2, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     col = viridis(100, option = "G", direction = -1),
     alpha = 1,     
     axis.args=list(cex.axis=1.5),
     legend.width=2)

# Add agLand shapefile overlay
plot(agLand,
     add = TRUE,
     col= "grey50",
     border = "grey50")

#Add scale bar & north arrow
addscalebar(plotunit = 'm', widthhint = 0.3, lwd = 1, padin = c(3.3, 0), label.cex = 0.9)
addnortharrow(scale = 0.6, text.col = 'black', cols = c('black', 'black'))

# Close jpeg save
dev.off()


###################################
# Create sd LIT map
###################################

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig5_C pool_maps/estLIT_sd_map_R.png", width=4000, height=6000, units="px", res=600)

# Bin evertything above a specific value
estLIT_sd_crop2 <- estLIT_sd_crop
estLIT_sd_crop2[estLIT_sd_crop > 1.4] <- 1.4

#overlay raster
plot(estLIT_sd_crop2, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     col = viridis(100, option = "A", direction = -1),
     alpha = 1,     
     axis.args=list(cex.axis=1.5),
     legend.width=2)

# Add agLand shapefile overlay
plot(agLand,
     add = TRUE,
     col= "grey50",
     border = "grey50")

#Add scale bar & north arrow
addscalebar(plotunit = 'm', widthhint = 0.3, lwd = 1, padin = c(3.3, 0), label.cex = 0.9)
addnortharrow(scale = 0.6, text.col = 'black', cols = c('black', 'black'))

# Close jpeg save
dev.off()





