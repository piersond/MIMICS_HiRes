library(raster)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(prettymapr)
library(viridis)
library(dplyr)

#set working directory
setwd("C:/github/MIMICS_HiRes")

################### 
# Load Rasters
###################
forcing_rasters_path <- "C:/Users/Derek/Google Drive/RCrk/GIS/MIMICS_rasters/"
GPP <- raster(paste0(forcing_rasters_path,"MSAVI_estGPP.tif"))
TSOI <- raster(paste0(forcing_rasters_path,"tsoi_est2.tif"))
CLAY <- raster(paste0(forcing_rasters_path,"RCrk_estClay.tif"))
LIGN <- raster(paste0(forcing_rasters_path,"RCrk_estLigN.tif"))
DEM <- raster(paste0(forcing_rasters_path,"DEM_extract1.tif"))
ASPECT <- raster(paste0(forcing_rasters_path,"RC_Aspect_Deg.tif"))
NSEW <- raster(paste0(forcing_rasters_path,"RC_Aspect_NSEW.tif"))

#MIMICS rasters
SOC <- raster("Figures/Fig4_estSOC_map/estSOC_pset_mean.tif")
LIT <- raster("Figures/Fig5_C_pool_maps/estLIT_pset_mean.tif")
MIC <- raster("Figures/Fig5_C_pool_maps/estMIC_pset_mean.tif")
SOMp <- raster("Figures/Fig5_C_pool_maps/estSOMp_pset_mean.tif")


#######################
# Litter plots
#######################
LIT_plot_df <- na.omit(data.frame(LIT = getValues(LIT),
                      ASPECT = getValues(ASPECT),
                      NSEW = getValues(NSEW),
                      DEM = getValues(DEM),
                      GPP = getValues(GPP) + 400,
                      TSOI = getValues(TSOI)))

### Plot 1
p1 <- ggplot(LIT_plot_df,
       aes(y=LIT, x=ASPECT)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Terrain Aspect") +
  ylab("MIMICS Litter C (kg m-2)") +
  scale_fill_viridis(option = "D", direction = 1)

### Plot 2
p2 <- ggplot(LIT_plot_df,
       aes(y=TSOI, x=ASPECT)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Terrain Aspect") +
  ylab("Mean Annual Soil Temperature (deg C)") +
  scale_fill_viridis(option = "D", direction = 1)

### Plot 3
p3 <- ggplot(LIT_plot_df,
       aes(y=LIT, x=TSOI)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  ylab("MIMICS Litter C (kg m-2)") +
  xlab("Mean Annual Soil Temperature (deg C)") +
  scale_fill_viridis(option = "D", direction = 1)

### Plot 4
p4 <- ggplot(LIT_plot_df,
       aes(y=LIT, x=GPP/2)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Estimated ANPP") +
  ylab("MIMICS Litter C (kg m-2)") +
  scale_fill_viridis(option = "D", direction = 1)

png("litC_plots.png", width=6, height=5, units="in", res=600)

ggarrange(ncol=2,
          nrow=2,
          p1, p4,
          p2, p3)

dev.off()

#######################
# MIC-C plots
#######################
MIC_plot_df <- na.omit(data.frame(MIC = getValues(MIC),
                                  ASPECT = getValues(ASPECT),
                                  NSEW = getValues(NSEW),
                                  DEM = getValues(DEM),
                                  GPP = getValues(GPP) + 400,
                                  TSOI = getValues(TSOI),
                                  LIGN = getValues(LIGN),
                                  CLAY = getValues(CLAY)))

### Plot 1
p1 <- ggplot(MIC_plot_df,
             aes(y=MIC, x=ASPECT)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Terrain Aspect") +
  ylab("MIMICS Microbial-C (kg m-2)") +
  scale_fill_viridis(option = "B", direction = 1)

### Plot 2
p2 <- ggplot(MIC_plot_df,
             aes(y=MIC, x=as.character(round(LIGN,1)))) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  #geom_violin() +
  theme_bw() +
  xlab("Lignin:N") +
  ylab("MIMICS Microbial-C (kg m-2)") +
  scale_fill_viridis(option = "B", direction = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### Plot 3
p3 <- ggplot(MIC_plot_df,
             aes(y=MIC, x=TSOI)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  ylab("MIMICS Microbial-C (kg m-2)") +
  xlab("Mean Annual Soil Temperature (deg C)") +
  scale_fill_viridis(option = "B", direction = 1)

### Plot 4
p4 <- ggplot(MIC_plot_df,
             aes(y=MIC, x=GPP/2)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Estimated ANPP") +
  ylab("MIMICS Microbial-C (kg m-2)") +
  scale_fill_viridis(option = "B", direction = 1)

png("micC_plots.png", width=6, height=5, units="in", res=600)

ggarrange(ncol=2,
          nrow=2,
          p1, p4,
          p2, p3)

dev.off()


#######################
# SOMp plots
#######################
SOMp_plot_df <- na.omit(data.frame(SOMp = getValues(SOMp),
                                  ASPECT = getValues(ASPECT),
                                  NSEW = getValues(NSEW),
                                  DEM = getValues(DEM),
                                  GPP = getValues(GPP) + 400,
                                  TSOI = getValues(TSOI),
                                  LIGN = getValues(LIGN),
                                  CLAY = getValues(CLAY)))

### Plot 1
p1 <- ggplot(SOMp_plot_df,
             aes(y=SOMp, x=ASPECT)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Terrain Aspect") +
  ylab("MIMICS SOMp C (kg m-2)") +
  scale_fill_viridis(option = "G", direction = 1)

### Plot 2
p2 <- ggplot(SOMp_plot_df,
             aes(y=SOMp, x=CLAY)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Clay Content (%)") +
  ylab("MIMICS SOMp C (kg m-2)") +
  scale_fill_viridis(option = "G", direction = 1)

### Plot 3
p3 <- ggplot(SOMp_plot_df,
             aes(y=SOMp, x=TSOI)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  ylab("MIMICS SOMp C (kg m-2)") +
  xlab("Mean Annual Soil Temperature (deg C)") +
  scale_fill_viridis(option = "G", direction = 1)

### Plot 4
p4 <- ggplot(SOMp_plot_df,
             aes(y=SOMp, x=GPP/2)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Estimated ANPP") +
  ylab("MIMICS SOMp C (kg m-2)") +
  scale_fill_viridis(option = "G", direction = 1)

png("SOMp_plots.png", width=6, height=5, units="in", res=600)

ggarrange(ncol=2,
          nrow=2,
          p1, p4,
          p2, p3)

dev.off()
