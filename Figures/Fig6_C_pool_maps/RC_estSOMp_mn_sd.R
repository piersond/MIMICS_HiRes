library(raster)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(prettymapr)
library(viridis)

#set working directory
setwd("C:/github/MISOMpS_HiRes")

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
# Create estSOMp summary stats maps
############################################################

# Create raster stack from individual pset rasters
estSOMp_rasters <- list.files("Mapping/Map_data_out/estSOMp", pattern='\\.tif$')
estSOMp_stack <- stack(paste0("Mapping/Map_data_out/estSOMp/", estSOMp_rasters))

# Calulate map stats
estSOMp_mean <- calc(estSOMp_stack, fun = mean)
estSOMp_sd <- calc(estSOMp_stack, fun = sd)
#estSOMp_max <- calc(estSOMp_stack, fun = max)
#estSOMp_min <- calc(estSOMp_stack, fun = min)
#estSOMp_range <- estSOMp_max-estSOMp_min

# Plot to see if it worked :)
plot(estSOMp_mean)
plot(estSOMp_sd)
#plot(estSOMp_range)

#############################################
# Remove agland from maps
#############################################

# Load cropland shapefile
agLand <- readOGR(paste0(raster_path,"RCrk_agLand.shp"))

estSOMp_mean_crop <- mask(estSOMp_mean, agLand, inverse=TRUE)
estSOMp_sd_crop <- mask(estSOMp_sd, agLand, inverse=TRUE)
#estSOMp_range_crop <- mask(estSOMp_range, agLand, inverse=TRUE)

plot(estSOMp_mean_crop)
plot(estSOMp_sd_crop)
#plot(estSOMp_range_crop)


###################################
# Save rasters
###################################
save_path <- "Figures/Fig5_C_pool_maps/"

writeRaster(estSOMp_mean_crop, paste0(save_path, "estSOMp_pset_mean.tif"), options=c('TFW=YES'))
writeRaster(estSOMp_sd_crop, paste0(save_path, "estSOMp_pset_sd.tif"), options=c('TFW=YES'))


###################################
# Create mean SOMp map
###################################

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig5_C pool_maps/estSOMp_mn_map_R.png", width=4000, height=6000, units="px", res=600)

# Bin evertything above a specific value
estSOMp_mean_crop2 <- estSOMp_mean_crop
estSOMp_mean_crop2[estSOMp_mean_crop > 10] <- 10

#overlay raster
plot(estSOMp_mean_crop2, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     col = viridis(100, option = "D", direction = 1),
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
# Create sd SOMp map
###################################

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig5_C pool_maps/estSOMp_sd_map_R.png", width=4000, height=6000, units="px", res=600)

# Bin evertything above a specific value
estSOMp_sd_crop2 <- estSOMp_sd_crop
estSOMp_sd_crop2[estSOMp_sd_crop > 3] <- 3

#overlay raster
plot(estSOMp_sd_crop2, axes=FALSE, box=FALSE,
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





