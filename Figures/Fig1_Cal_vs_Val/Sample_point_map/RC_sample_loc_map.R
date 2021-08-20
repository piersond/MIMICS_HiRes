library(raster)
library(rgdal)
library(RColorBrewer)
library(ggplot2)

#set working directory
setwd("C:/github/MIMICS_HiRes/Figures/Fig1_Cal_vs_Val")

# Set path to raster files
raster_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"

# Load raster data
estGPP <- raster(paste0(raster_path,"MSAVI_estGPP.tif"))
estTSOI <- raster(paste0(raster_path,"tsoi_est2.tif"))
RC_hilsh <- raster(paste0(raster_path,"RC_hillsh.tif"))
DEM <- raster(paste0(raster_path,"DEM_extract1.tif"))

# Load sample points
cal_pts <- read.csv("cal_coords.csv")
val_pts <- read.csv("val_coords.csv")

#create a color palette
pal <- colorRampPalette(c("#025d00","#e5f700","#ff9e00","#ffffff"))

# Setup to save plot as png
png(file="Cal_Val_map.png", width=4000, height=6000, units="px", res=600)

#plot hillshade first
plot(RC_hilsh,
     main = "Reynolds Creek\nExperimental Watershed",
     col = grey(1:100/100),
     legend = FALSE)

#overlay raster
plot(DEM,
     main = "RASTER TITLE", 
     add = TRUE,
     col = pal(100),
     alpha = .4)

points(cal_pts[,3:4],
       col = alpha("#555555", 0.6), 
       bg = alpha("#008B8B", 0.6), 
       pch = 21,
       cex=1.5) 

points(val_pts[,3:4],
       col = alpha("#555555", 0.6), 
       bg = alpha("#FF4500", 0.6), 
       pch = 22,
       cex=1.5) 

# Close jpeg save
dev.off()

