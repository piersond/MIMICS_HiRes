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
# Create estMIC summary stats maps
############################################################

# Create raster stack from individual pset rasters
estMIC_rasters <- list.files("Mapping/Map_data_out/estMIC", pattern='\\.tif$')
estMIC_stack <- stack(paste0("Mapping/Map_data_out/estMIC/", estMIC_rasters))

# Calulate map stats
estMIC_mean <- calc(estMIC_stack, fun = mean)
estMIC_sd <- calc(estMIC_stack, fun = sd)
#estMIC_max <- calc(estMIC_stack, fun = max)
#estMIC_min <- calc(estMIC_stack, fun = min)
#estMIC_range <- estMIC_max-estMIC_min

# Plot to see if it worked :)
plot(estMIC_mean)
plot(estMIC_sd)
plot(estMIC_range)

#############################################
# Remove agland from maps
#############################################

# Load cropland shapefile
agLand <- readOGR(paste0(raster_path,"RCrk_agLand.shp"))

estMIC_mean_crop <- mask(estMIC_mean, agLand, inverse=TRUE)
estMIC_sd_crop <- mask(estMIC_sd, agLand, inverse=TRUE)
#estMIC_range_crop <- mask(estMIC_range, agLand, inverse=TRUE)

plot(estMIC_mean_crop)
plot(estMIC_sd_crop)
#plot(estMIC_range_crop)


###################################
# Save rasters
###################################
save_path <- "Figures/Fig5_C_pool_maps/"

writeRaster(estMIC_mean_crop, paste0(save_path, "estMIC_pset_mean.tif"), options=c('TFW=YES'))
writeRaster(estMIC_sd_crop, paste0(save_path, "estMIC_pset_sd.tif"), options=c('TFW=YES'))


###################################
# Create mean MIC map
###################################

#create a color palette
# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig5_C pool_maps/estMIC_mn_map_R.png", width=4000, height=6000, units="px", res=600)

# Bin evertything above a specific value
estMIC_mean_crop2 <- estMIC_mean_crop
estMIC_mean_crop2[estMIC_mean_crop > 0.28] <- 0.28

#overlay raster
plot(estMIC_mean_crop2, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     col = viridis(100, option = "F", direction = -1),
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
# Create sd MIC map
###################################

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig5_C pool_maps/estMIC_sd_map_R.png", width=4000, height=6000, units="px", res=600)

# Bin evertything above a specific value
estMIC_sd_crop2 <- estMIC_sd_crop
estMIC_sd_crop2[estMIC_sd_crop > 0.16] <- 0.18

#overlay raster
plot(estMIC_sd_crop2, axes=FALSE, box=FALSE,
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





