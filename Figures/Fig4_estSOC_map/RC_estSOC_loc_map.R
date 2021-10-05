library(raster)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(prettymapr)

#set working directory
setwd("C:/github/MIMICS_HiRes/Figures")

# Set path to raster files
raster_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"

# Load raster data
estGPP <- raster(paste0(raster_path,"MSAVI_estGPP.tif"))
estTSOI <- raster(paste0(raster_path,"tsoi_est2.tif"))
RC_hilsh <- raster(paste0(raster_path,"RC_hillsh.tif"))
DEM <- raster(paste0(raster_path,"DEM_extract1.tif"))
SOC <- raster("C:/local_temp/MC-PSET-MAPS/rasters/estSOC_pset2.tif")

# Load sample points
cal_pts <- read.csv("Fig1_Cal_vs_Val/cal_coords.csv")
val_pts <- read.csv("Fig1_Cal_vs_Val/val_coords.csv")

#create a color palette
pal <- colorRampPalette(c("#c32222",
                          "#f5ff00",
                          "#25f100",
                          "#00d298",
                          "#004fe0",
                          "#5a2dfd"))

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig6_estSOC_map/estSOC_map_R.png", width=4000, height=6000, units="px", res=600)

#plot hillshade first
plot(RC_hilsh, axes=FALSE, box=FALSE,
    main = "",
    col = grey(1:100/100),
    legend = FALSE, 
    alpha = 0)

#overlay raster
plot(SOC,axes=FALSE, box=FALSE,
     main = "", 
     add = FALSE,
     col = pal(100),
     alpha = 1)

#Add scale bar & north arrow
addscalebar(plotunit = 'm', widthhint = 0.3, lwd = 1, padin = c(3.3, 0), label.cex = 0.9)
addnortharrow(scale = 0.6, text.col = 'black', cols = c('black', 'black'))

# Close jpeg save
dev.off()

