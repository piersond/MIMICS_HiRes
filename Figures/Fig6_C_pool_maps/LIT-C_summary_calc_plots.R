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
                                  TSOI = getValues(TSOI),
                                  SOMp = getValues(SOMp),
                                  SOC = getValues(SOC)))

mean_GPP <- mean(LIT_plot_df$GPP)

### Plot 1
p1 <- ggplot(LIT_plot_df,
             aes(y=LIT, x=ASPECT)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Terrain Aspect") +
  ylab("MIMICS Litter C (kg m-2)") +
  scale_fill_viridis(option = "D", direction = 1) +
  ggtitle("Reynolds Creek Experimental Watershed") +
  ylim(0,5)

### Plot 2
p2 <- ggplot(LIT_plot_df %>% filter(GPP > mean(GPP)),
             aes(y=LIT, x=ASPECT)) + 
  geom_bin2d(bins=200) +
  #geom_point(alpha=0.5) + 
  theme_bw() +
  xlab("Terrain Aspect") +
  ylab("MIMICS Litter C (kg m-2)") +
  scale_fill_viridis(option = "D", direction = 1) +
  ggtitle("Areas with above average GPP (>555 g C/m-2)") +
  ylim(0,5)


png("litC_by_aspect.png", width=6, height=5, units="in", res=600)

ggarrange(ncol=2,
          nrow=1,
          p1, p2)

dev.off()


#Calc diff in aspect LIT-C
north_LIT <- LIT_plot_df %>% filter(ASPECT > 315) %>% rbind(LIT_plot_df %>% filter(ASPECT < 45))
south_LIT <- LIT_plot_df %>% filter(ASPECT < 225) %>% filter(ASPECT > 135)

mean(north_LIT$LIT)/mean(south_LIT$LIT)


#Calc how much of SOC is litter C
sum(LIT_plot_df$LIT)/sum(LIT_plot_df$SOC)
sum(north_LIT$LIT)/sum(north_LIT$SOC)
sum(south_LIT$LIT)/sum(south_LIT$SOC)

#Calc SOMp / SOC
sum(LIT_plot_df$SOMp)/sum(LIT_plot_df$SOC)

