# Script used to split Reynolds Creek data into 3 equal 
# parts for 3-fold cross validation

# Stratified sampling used to ensure 3 data parts contain 
#  similar representations of SOC across the Reynolds Creek 
#  landscape

library(dplyr)

# Set working drive
setwd("C:/github/MIMICS_HiRes")

# Load the Reynolds Creek dataset
rcdf <- read.csv("RCrk_Modelling_Data/RCrk_SOC_Cal+Val.csv", as.is=T)
rcdf$pGPP <- rcdf$pGPP + 400

#Create unique ID for each sample
rcdf$ID <- paste0("ID", seq(1, nrow(rcdf), 1))

# Create bins for sampling stratification based on productivity
rcdf <- rcdf %>% mutate(bin = ntile(pGPP, n=5))

# Create samples stratified by bin number
strat_sample <- rcdf %>%
  group_by(bin) %>%
  sample_n(size=6)

strat_sample2 <- rcdf %>%
  filter(!ID %in% strat_sample$ID) %>%
  group_by(bin) %>%
  sample_n(size=6)

strat_sample3 <- rcdf %>%
  filter(!ID %in% strat_sample$ID) %>%
  filter(!ID %in% strat_sample2$ID)


# pGPP distribution plots
pGPP_d1 <- density(strat_sample$pGPP, adjust = 1.3) 
pGPP_d2 <- density(strat_sample2$pGPP, adjust = 1.3) 
pGPP_d3 <- density(strat_sample3$pGPP, adjust = 1.3) 

plot(pGPP_d3, col=3, main="pGPP Distributions")
lines(pGPP_d2, col=2)
lines(pGPP_d1, col=1)


# TSOI distribution plots
TSOI_d1 <- density(strat_sample$TSOI, adjust = 1.3) 
TSOI_d2 <- density(strat_sample2$TSOI, adjust = 1.3) 
TSOI_d3 <- density(strat_sample3$TSOI, adjust = 1.3) 

plot(TSOI_d3, col=3, main="TSOI Distributions")
lines(TSOI_d2, col=2)
lines(TSOI_d1, col=1)


# CLAY distribution plots
CLAY_d1 <- density(strat_sample$CLAY, adjust = 1.3) 
CLAY_d2 <- density(strat_sample2$CLAY, adjust = 1.3) 
CLAY_d3 <- density(strat_sample3$CLAY, adjust = 1.3) 

plot(CLAY_d3, col=3, main="CLAY Distributions")
lines(CLAY_d2, col=2)
lines(CLAY_d1, col=1)


# lig_N distribution plots
lig_N_d1 <- density(strat_sample$lig_N, adjust = 1.3) 
lig_N_d2 <- density(strat_sample2$lig_N, adjust = 1.3) 
lig_N_d3 <- density(strat_sample3$lig_N, adjust = 1.3) 

plot(lig_N_d1, col=1, main="lig_N Distributions")
lines(lig_N_d2, col=2)
lines(lig_N_d3, col=3)


### Save the distribution groups
strat_sample$cv_group <- 1
strat_sample2$cv_group <- 2
strat_sample3$cv_group <- 3

out_df <- rbind(strat_sample, strat_sample2, strat_sample3)
write.csv(out_df, "MC_cross-val_data.csv")

