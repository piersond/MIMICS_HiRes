library(ggplot2)
library(rayshader)
library(dplyr)
library(plot3D)

setwd("C:/github/MIMICS_HiRes")
param_sets <- read.csv("Mapping/Map_data_in/RC_MIM_param_combos_RMSE_less2.csv", as.is=T)

gg1 <- ggplot(param_sets, aes(x=CUE_x, y=Tau_x, color=desorb_x)) + 
        geom_point(size=2) +
        geom_title("Test plot")

# gg1 %>% plot_gg(height=3,
#                 width=3.5,
#                 multicore=TRUE,
#                 pointcontract =0.7,
#                 soliddepth = -200)

scatter3D(param_sets$CUE_x,param_sets$Tau_x,param_sets$desorb_x,box=TRUE,pch=16,bty="b2",axes=TRUE,label=TRUE, nticks=5, ticktype="detailed",theta=40, phi=40, xlab="X-val", ylab="Y-val", zlab="Z-val", main="3D scatter plot")
