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
estTSOI <- raster(paste0(raster_path,"MSAVI_estGPP.tif"))

############################################
# Load default ensemble mean estSOC map
############################################
def_estSOC <- raster("Figures/Fig4_estSOC_map/estSOC_pset_mean.tif")

############################################################
# Create summary stats map
############################################################

# Create raster stack from individual pset rasters
estSOC_TSOI_rasters <- list.files("Mapping/Map_data_out/pset_RCrk_TSOI1_map_data/SOC_rasters", pattern='\\.tif$')
estSOC_rasters <- list.files("Mapping/Map_data_out/estSOC", pattern='\\.tif$')

###Build stack of ensemble pair difference rasters
e1_diff <- raster(paste0("Mapping/Map_data_out/pset_RCrk_TSOI1_map_data/SOC_rasters/",estSOC_TSOI_rasters[1])) -
  raster(paste0("Mapping/Map_data_out/estSOC/",estSOC_rasters[1]))

# Start stack
estSOC_diff_stack <- stack(e1_diff) 

for (i in 2:length(estSOC_rasters)) {
  r_diff <- raster(paste0("Mapping/Map_data_out/pset_RCrk_TSOI1_map_data/SOC_rasters/",estSOC_TSOI_rasters[i])) -
              raster(paste0("Mapping/Map_data_out/estSOC/",estSOC_rasters[i]))
  #Add to stack
  estSOC_diff_stack <- addLayer(estSOC_diff_stack, r_diff)
}

# Calulate map stats
estSOC_diff_mean <- calc(estSOC_diff_stack, fun = mean)
estSOC_diff_sd <- calc(estSOC_diff_stack, fun = sd)
estSOC_diff_min <- calc(estSOC_diff_stack, fun = min)
estSOC_diff_max <- calc(estSOC_diff_stack, fun = max)


# Plot to see if it worked :)
plot(estSOC_diff_mean)
plot(estSOC_diff_sd)

est_SOC_diff_range <- estSOC_diff_max - estSOC_diff_min
est_SOC_diff_range2 <- est_SOC_diff_range
#est_SOC_diff_range2[est_SOC_diff_range > 1.5] <- 0

plot(mask(est_SOC_diff_range2, agLand, inverse=TRUE), axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     #breaks=seq(-0.1, 4, length.out=100),
     col = viridis(100, option = "D", direction = -1))


#############################################
# Remove agland from maps
#############################################

# Load cropland shapefile
agLand <- readOGR(paste0(raster_path,"RCrk_agLand.shp"))

estSOC_diff_mean_crop <- mask(estSOC_diff_mean, agLand, inverse=TRUE)
estSOC_diff_sd_crop <- mask(estSOC_diff_sd, agLand, inverse=TRUE)
#estSOC_range_crop <- mask(estSOC_range, agLand, inverse=TRUE)

plot(estSOC_diff_mean_crop)
plot(estSOC_diff_sd_crop)

###################################
# Save rasters
###################################
save_path <- "Figures/Fig6_Scenario maps/"

writeRaster(estSOC_diff_mean_crop, paste0(save_path, "scenTSOI1_diff_mean.tif"), options=c('TFW=YES'))
writeRaster(estSOC_diff_sd_crop, paste0(save_path, "scenTSOI1_diff_sd.tif"), options=c('TFW=YES'))


###################################
# Create mean difference SOC map
###################################

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig6_Scenario maps/TSOI1_estSOC_pair-diff_mn_map.png", width=4000, height=6000, units="px", res=600)

plot(density(na.omit(getValues(estSOC_diff_mean_crop))))

estSOC_diff_mean_crop2 <- estSOC_diff_mean_crop
estSOC_diff_mean_crop2[estSOC_diff_mean_crop > 0] <- 0
estSOC_diff_mean_crop2[estSOC_diff_mean_crop < -0.6] <- -0.6

#create a color scale
pal <- colorRampPalette(c("#313695",
                          #"#abd9e9",
                          "#ffffbf",
                          "#fdae61"))

pal2 <- colorRampPalette(c("#d7301f",
                           "#df65b0",
                           "#ffffca"
                           ))
#overlay raster
plot(estSOC_diff_mean_crop2, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     #breaks=seq(0, 1, length.out=100),
     #col = viridis(100, option = "D", direction = -1), #pal(100),
     col = pal2(600),
     alpha = 1,
     legend.width=2,
     legend.shrink=0.3)

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

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig6_Scenario maps/TSOI1_estSOC_pair-diff_sd_map.png", width=4000, height=6000, units="px", res=600)

estSOC_diff_sd_crop2 <- estSOC_diff_sd_crop
estSOC_diff_sd_crop2[estSOC_diff_sd_crop > 0.5] <- 0.5

#overlay raster
plot(estSOC_diff_sd_crop2, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     breaks=seq(0, 0.5, length.out=500),
     col = viridis(500, option = "A", direction = -1), #pal(100),
     alpha = 1,
     axis.args=list(cex.axis=1.5),
     legend.width=2,
     legend.shrink=0.8)

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


