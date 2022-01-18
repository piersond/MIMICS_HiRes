library(tidyverse)
library(ggplot2)
library(psych)
#source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")

setwd("C:/github/MIMICS_HiRes")

############################
# Load and prep best-fit parameter data
############################

best_parms <- read.csv("MC/Output/RC_MIM_param_combos_RMSE_less2.csv")
best_parms <- best_parms[,3:10]

# Scale each parameter from 0-1 based on MC parameter range limits
####################################

prm_spc <- best_parms

# Scale parameter values to 0-1 based on random parameter
# proposal range used for the MC simulation
prm_spc$Vslope_spc <- (prm_spc$Vslope_x-0.4)/(4-0.4)
prm_spc$Vint_spc <- (prm_spc$Vint_x-0.3)/(3-0.3)
prm_spc$Kslope_spc <- (prm_spc$Kslope_x-0.4)/(4-0.4)
prm_spc$Kint_spc <- (prm_spc$Kint_x-0.3)/(3-0.3)
prm_spc$Tau_spc <- (prm_spc$Tau_x-0.3)/(3-0.3)
prm_spc$CUE_spc <- (prm_spc$CUE_x-0.2)/(2-0.2)
prm_spc$desorb_spc <- (prm_spc$desorb_x-0.001)/(0.3-0.001)
prm_spc$fPHYS_spc <- (prm_spc$fPHYS_x-0.01)/(4-0.01)

#### PLOT MATRIX ####
pairs_pan_data <- prm_spc[,9:16] 
colnames(pairs_pan_data) <- c("Vslope", "Vint", "Kslope", "Kint", "Tau", "MGE", "D", "fp" )

### Save plot
png(file="Figures/MC_param_space.png", width=3000, height=3000, units="px", res=600)

# Make plot
pairs.panels(pairs_pan_data, 
             method = "pearson", # correlation method
             hist.col = "#999999",
             smooth = FALSE, #Show loess fit line
             density = TRUE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             scale = FALSE,
             pch = 16,
             cex.cor = 1.1,
             cex = 0.8,
             cex.axis = 1,
             xlim=c(0,1),
             ylim=c(0,1),
             breaks = 8,
             rug = FALSE,
             smoother = FALSE,
             stars = FALSE
)

#Finish save 
dev.off()
