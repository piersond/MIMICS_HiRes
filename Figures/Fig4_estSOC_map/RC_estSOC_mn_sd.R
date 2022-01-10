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
# Create estSOC summary stats maps
############################################################

# Create raster stack from individual pset rasters
estSOC_rasters <- list.files("Mapping/Map_data_out/estSOC", pattern='\\.tif$')
estSOC_stack <- stack(paste0("Mapping/Map_data_out/estSOC/", estSOC_rasters))

# Calulate map stats
estSOC_mean <- calc(estSOC_stack, fun = mean)
estSOC_sd <- calc(estSOC_stack, fun = sd)
estSOC_max <- calc(estSOC_stack, fun = max)
estSOC_min <- calc(estSOC_stack, fun = min)

estSOC_range <- estSOC_max-estSOC_min

# Plot to see if it worked :)
plot(estSOC_mean)
plot(estSOC_sd)
plot(estSOC_range)

#############################################
# Remove agland from maps
#############################################

# Load cropland shapefile
agLand <- readOGR(paste0(raster_path,"RCrk_agLand.shp"))

estSOC_mean_crop <- mask(estSOC_mean, agLand, inverse=TRUE)
estSOC_sd_crop <- mask(estSOC_sd, agLand, inverse=TRUE)
estSOC_range_crop <- mask(estSOC_range, agLand, inverse=TRUE)

plot(estSOC_mean_crop)
plot(estSOC_sd_crop)
plot(estSOC_range_crop)


###################################
# Save map rasters
###################################
save_path <- "Figures/Fig4_estSOC_map/"

writeRaster(estSOC_mean_crop, paste0(save_path, "estSOC_pset_mean.tif"), options=c('TFW=YES'))
writeRaster(estSOC_sd_crop, paste0(save_path, "estSOC_pset_sd.tif"), options=c('TFW=YES'))


###################################
# Create mean SOC map
###################################

#create a color palette
pal <- colorRampPalette(c("#c32222",
                          "#f5ff00",
                          "#25f100",
                          "#00d298",
                          "#004fe0",
                          "#5a2dfd"))

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig4_estSOC_map/estSOC_mn_map_R.png", width=4000, height=6000, units="px", res=600)

#plot hillshade first
# plot(RC_hilsh, axes=FALSE, box=FALSE,
#      main = "",
#      col = grey(1:100/100),
#      legend = FALSE,
#      alpha = 0.7)

#overlay raster
plot(estSOC_mean_crop, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     col = viridis(100, option = "D", direction = -1), #pal(100),
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
# Create sd SOC map
###################################

#create a color palette
pal2 <- colorRampPalette(c("#43ccec",
                          "#eaed00",
                          "#ffac37",
                          "#fd1d1d",
                          "#833ab4",
                          "#3a4bb4"))

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig4_estSOC_map/estSOC_sd_map_R.png", width=4000, height=6000, units="px", res=600)

# Bin evertything above a specific value
estSOC_sd_crop2 <- estSOC_sd_crop
estSOC_sd_crop2[estSOC_sd_crop > 2.5] <- 2.6

#overlay raster
plot(estSOC_sd_crop2, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     col = viridis(100, option = "A", direction = -1),#pal2(100),
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





