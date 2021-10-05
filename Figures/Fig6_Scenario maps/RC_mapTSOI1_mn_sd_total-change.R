library(raster)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(prettymapr)

#set working directory
setwd("C:/github/MIMICS_HiRes")

# Set path to raster files
raster_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"

# Load base raster data
estGPP <- raster(paste0(raster_path,"MSAVI_estGPP.tif"))

############################################
# Load default map
############################################
def_estSOC <- raster("Figures/Fig4_estSOC_map/estSOC_pset_mean.tif")

############################################################
# Create summary stats map
############################################################

# Create raster stack from individual pset rasters
estSOC_rasters <- list.files("Mapping/Map_data_out/pset_RCrk_TSOI1_map_data/SOC_rasters", pattern='\\.tif$')
estSOC_stack <- stack(paste0("Mapping/Map_data_out/pset_RCrk_TSOI1_map_data/SOC_rasters/", estSOC_rasters))

# Calc difference from default map
estSOC_diff_stack <- estSOC_stack - def_estSOC

# Calulate map stats
estSOC_diff_mean <- calc(estSOC_diff_stack, fun = mean)
estSOC_diff_sd <- calc(estSOC_diff_stack, fun = sd)

# Plot to see if it worked :)
plot(estSOC_diff_mean)
plot(estSOC_diff_sd)


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
# Create mean difference SOC map
###################################

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig6_Scenario maps/TSOI1_estSOC_diff_mn_map.png", width=4000, height=6000, units="px", res=600)

estSOC_diff_mean_crop2 <- estSOC_diff_mean_crop
estSOC_diff_mean_crop2[estSOC_diff_mean_crop > 0.5] <- 0.5
estSOC_diff_mean_crop2[estSOC_diff_mean_crop < -0.5] <- -0.5

#overlay raster
plot(estSOC_diff_mean_crop2, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     breaks=seq(-0.6, 1, length.out=1000),
     col = viridis(1000, option = "D", direction = -1), #pal(100),
     alpha = 1,
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

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig6_Scenario maps/TSOI1_estSOC_diff_sd_map.png", width=4000, height=6000, units="px", res=600)

estSOC_diff_sd_crop2 <- estSOC_diff_sd_crop
estSOC_diff_sd_crop2[estSOC_diff_sd_crop > 2.5] <- 2.5

#overlay raster
plot(estSOC_diff_sd_crop2, axes=FALSE, box=FALSE,
     main = "",
     add = FALSE,
     col = viridis(1000, option = "A", direction = -1), #pal(100),
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





