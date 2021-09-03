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
RC_hilsh <- raster(paste0(raster_path,"RC_hillsh.tif"))
SOMp <- raster("C:/local_temp/MC-PSET-MAPS/rasters/MIMSOMp/estSOMp_pset2.tif")

#create a color palette
pal <- colorRampPalette(c("#080d61",
                         "#19ff49",
                          "#fff72a",
                          "#c40000"))

# Setup to save plot as png
png(file="C:/github/MIMICS_HiRes/Figures/Fig5_C pool_maps/SOMp_map_R.png", width=4000, height=6000, units="px", res=600)

#plot hillshade first
plot(RC_hilsh, axes=FALSE, box=FALSE,
    main = "",
    col = grey(1:100/100),
    legend = FALSE, 
    alpha = 0)

#overlay raster
plot(SOMp,axes=FALSE, box=FALSE,
     main = "", 
     add = FALSE,
     col = pal(100),
     alpha = 1)

#Add scale bar & north arrow
addscalebar(plotunit = 'm', widthhint = 0.3, lwd = 1, padin = c(3.3, 0), label.cex = 0.9)
addnortharrow(scale = 0.6, text.col = 'black', cols = c('black', 'black'))

# Close jpeg save
dev.off()

