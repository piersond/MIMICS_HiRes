library(tidyverse)
library(ggplot2)
library(Metrics)
source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")

setwd("C:/github/MIMICS_HiRes")

############################
# Load and prep MC data
############################

# Find for MC files in Output folder
filenames <- list.files(path="MC/Output/",pattern=".*rds")

### Load MC output data RDS
MC <- readRDS(paste0("MC/Output/",filenames[1]))

# Calc summary statistics
MC_stats <- MC %>% group_by(run_num) %>% summarize(RMSE = rmse(SOC, MIMSOC),
                                                   MICpropSOC = mean(MIMMIC)/mean(MIMSOC),
                                                   LITpropSOC = mean(MIMLIT)/mean(MIMSOC),
                                                   MIM_CO_mn = mean(MIM_CO),
                                                   SOMpTO = mean(1/(desorb*24*365)))
MC_all <- MC %>% left_join(MC_stats)

#Filter MC runs by microbe and litter pool ranges
MCf <- MC_all %>% filter(between(MICpropSOC, 0.01, 0.08)) %>%
  filter(between(LITpropSOC, 0.05, 0.50)) %>%
  filter(between(MIM_CO_mn, 0.01, 100)) %>%
  filter(between(SOMpTO, 20, 500))  ### CHANGE LOWER BOUND TO 50 ###

#Take the top 1% of runs based on RMSE
MCf <- MCf %>% arrange(RMSE)
MCf <- MCf[1:(nrow(MCf)/100)*51,]  #<--If MC data includes all sampling locs, multiply by # of sampling locs in dataset

#Create dataframe of the just the parameter values and RMSE
best_parms <- MCf[,25:32]
best_parms <- best_parms[!duplicated(parms),] # Remove duplicate rows

####################################
# Matrix plot
####################################
pairs(best_parms,
      col = alpha("blue", 0.3),                                         # Change color
      pch = 16,                                            # Change shape of points
      cex=1.5,
      labels = c("Vslope", "Vint", "Kslope", "Kint", "Tau", "CUE", "Desorb", "fPHYS"),                  # Change labels of diagonal
      main = "MIMICS Monte Carlo Top 1% Lowest RMSE",
      upper.panel = NULL)




####################################
# Matrix plot 2
# Scale each parameter from 0-1 based on MC parameter range limits
####################################

prm_spc <- best_parms

# Scale parameter values to 0-1 based on random parameter
# proposal range used for the MC simulation
prm_spc$Vslope_spc <- (prm_spc$Vslope_x-0.8)/(5-0.8)
prm_spc$Vint_spc <- (prm_spc$Vint_x-0.8)/(5-0.8)
prm_spc$Kslope_spc <- (prm_spc$Kslope_x-0.8)/(5-0.8)
prm_spc$Kint_spc <- (prm_spc$Kint_x-0.8)/(5-0.8)
prm_spc$Tau_spc <- (prm_spc$Tau_x-0.3)/(2-0.3)
prm_spc$CUE_spc <- (prm_spc$CUE_x-0.3)/(2-0.3)
prm_spc$desorb_spc <- (prm_spc$desorb_x-0.3)/(3-0.3)
prm_spc$fPHYS_spc <- (prm_spc$fPHYS_x-0.3)/(3-0.3)

pairs(prm_spc[,9:16],
      col = alpha("blue", 0.3),                                         # Change color
      pch = 16,                                            # Change shape of points
      cex=1.5,
      labels = c("Vslope", "Vint", "Kslope", "Kint", "Tau", "CUE", "Desorb", "fPHYS"),                  # Change labels of diagonal
      main = "MIMICS Monte Carlo Top 1% Lowest RMSE",
      upper.panel = NULL,
      xlim=c(0,1),
      ylim=c(0,1),
      breaks=c(0,1))

####################################
# Matrix plot 3
# Scale each parameter from 0-1 based on MC parameter range limits
####################################

library(psych)
pairs_pan_data <- prm_spc[,9:16] 
colnames(pairs_pan_data) <- c("Vslope", "Vint", "Kslope", "Kint", "Tau", "MGE", "D", "fp" )

### Save plot
png(file="Figures/Fig4_MC_param_spc/MC_param_space.png", width=3000, height=3000, units="px", res=600)

#Create the plot
matrix_p3 <- pairs.panels(pairs_pan_data,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             smooth = FALSE, #Show loess fit line
             density = TRUE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             scale = TRUE,
             pch = 16,
             cex.cor = 2,
             cex = 0.5,
             xlim=c(0,1),
             ylim=c(0,1))
dev.off()






