## Set working drive
#setwd("C:/github/MIMICS_HiRes")
setwd("C:/github/MIMICS_HiRes")

#Libraries
library(rootSolve)
#library(boot)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(Metrics) 
library(parallel)
library(furrr)
library(purrr)
#library(grid)
#library(gridExtra)

#bring in RXEQ function
source("MIMICS_ftns/RXEQ_ftn.R")

########################################
# Set MIMICS default parameters
########################################
Vslope  <- rep(0.063, 6)
Vint    <- rep(5.47, 6)
aV      <- rep(0.000000075, 6)
Kslope  <- rep(0.02, 6)
Kint    <- rep(3.19, 6)
aK      <- rep(0.15625, 6)
vMOD    <- c(2, 0.4, 2, 0.6, 0.6, 0.4)
kMOD    <- c(8, 2, 4, 2, 4, 6)
KO      <- c(6, 6)
CUE     <- c(0.5, 0.25, 0.7, 0.35)
tau_r   <- c(0.00052, 0.3)
tau_K   <- c(0.00024, 0.1)
Tau_MOD <- c(100, 0.6, 1.3, 3.5)
Tau_MULT <- 1
fPHYS_r <- c(0, 0)
fPHYS_K <- c(0, 0)
fCHEM_r <- c(0.1, -3, 1)
fCHEM_K <- c(0.3, -3, 1)
fSOM_p  <- c(0.000015, -1.5)
PHYS_scalar <- c(2, -2, NA, NA, NA, NA)
FI      <- c(0.05, 0.05)
fmet_p <- c(1, 0.85, 0.013)
depth <- 5 ###
h2y        <- 24*365
MICROtoECO <- depth * 1e4 * 1e-3  # mgC/cm3 to g/m2

#Set default multipliers
Tau_MULT = 1
desorb_MULT = 1
fPHYS_MULT = 1
VMAX_MULT = 1
KM_MULT = 1

########################################
# Apply parameter multipliers
########################################
Vslope = Vslope * 2.632015
Vint = Vint * 0.7244883
Kslope = Kslope * 1.567581
Kint = Kint * 1.4903838
# CUE = CUE * 1
# Tau_MULT = 1
# desorb_MULT = 2.3635554
# fPHYS_MULT = 2.0716163
#VMAX_MULT = 1
#KM_MULT = 1

###########################################
# MIMICS single point function
###########################################
MIMICS1 <- function(df){
  
  ### Setup a var to collect run notes
  note <- ""
  
  ### Bring in lig:N forcing data
  lig_N <- df$lig_N
  
  ###########################################################
  ### Set fMET equation
  ###########################################################
  ## Option A: Defualt fMET equation using lig:N values
  #fMET <- fmet_p[1] * (fmet_p[2] - fmet_p[3] * lig_N) 
  
  ## Option B: LTER "SHORTCUT" fMET value (average from LiDET)
  #fMET <- 0.3846423
  
  ## Option C: 
  lig    <- df$LIG/100
  Nnew   <- 1/df$CN/2.5                  	                       # N in litter additions
  fMET  <- fmet_p[1] * (fmet_p[2] - fmet_p[3] * lig / Nnew) 
  
  ###########################################################
  
  ###Bring in forcing data
  ANPP       <- df$ANPP/2
  fCLAY      <- df$CLAY/100
  TSOI       <- df$MAT
  
  ############################################################
  # MIMICS MODEL CODE STARTS HERE
  ############################################################
  
  # Calc litter input rate
  EST_LIT <- (ANPP / (365*24)) #* 1e3 / 1e4
  #print(EST_LIT)# gC/m2/h (from gC/m2/y) then mgC/cm2/h(from gC/m2/h) 
  
  # ------------ caclulate parameters ---------------
  Vmax     <- exp(TSOI * Vslope + Vint) * aV 
  Km       <- exp(TSOI * Kslope + Kint) * aK
  
  #ANPP strongly correlated with MAP
  Tau_MOD1 <- sqrt(ANPP/Tau_MOD[1])         
  Tau_MOD2 <- Tau_MOD[4]                        
  Tau_MOD1[Tau_MOD1 < Tau_MOD[2]] <- Tau_MOD[2]
  Tau_MOD1[Tau_MOD1 > Tau_MOD[3]] <- Tau_MOD[3] 
  tau <- c(tau_r[1]*exp(tau_r[2]*fMET), 
           tau_K[1]*exp(tau_K[2]*fMET))   
  tau <- tau * Tau_MOD1 * Tau_MOD2 * Tau_MULT 
  
  fPHYS    <- c(fPHYS_r[1] * exp(fPHYS_r[2]*fCLAY), 
                fPHYS_K[1] * exp(fPHYS_K[2]*fCLAY)) 	            
  fCHEM    <- c(fCHEM_r[1] * exp(fCHEM_r[2]*fMET) * fCHEM_r[3], 
                fCHEM_K[1] * exp(fCHEM_K[2]*fMET) * fCHEM_K[3]) 	
  fAVAI    <- 1 - (fPHYS + fCHEM)
  desorb   <- fSOM_p[1] * exp(fSOM_p[2]*(fCLAY))                  
  
  desorb <- desorb * desorb_MULT
  fPHYS <- fPHYS * fPHYS_MULT
  
  pSCALAR  <- PHYS_scalar[1] * exp(PHYS_scalar[2]*(sqrt(fCLAY)))  #Scalar for texture effects on SOMp
  v_MOD    <- vMOD  
  k_MOD    <- kMOD 
  k_MOD[3] <- k_MOD[3] * pSCALAR    
  k_MOD[6] <- k_MOD[6] * pSCALAR    
  
  VMAX     <- Vmax * v_MOD 
  KM       <- Km / k_MOD
  
  #MC scalars on VMAX and Km
  VMAX <- VMAX * VMAX_MULT
  KM <- KM * KM_MULT
  
  LITmin  <- rep(NA, dim=4)
  MICtrn  <- rep(NA, dim=6)
  SOMmin  <- rep(NA, dim=2)
  DEsorb  <- rep(NA, dim=1)
  OXIDAT  <- rep(NA, dim=1)
  
  
  nday   <- 200
  day    <- 1
  
  MIMout <- data.frame(SITE = rep(df$SITE, nday),
                       DAY = rep(NA, nday),
                       LITm = rep(NA, nday),
                       LITs = rep(NA, nday),
                       MICr = rep(NA, nday),
                       MICK = rep(NA, nday),
                       SOMp = rep(NA, nday),
                       SOMc = rep(NA, nday),
                       SOMa = rep(NA, nday),
                       CO2_MICr = rep(NA, nday),
                       CO2_MICK = rep(NA, nday))
  
  
  #initialize pools and fluxes
  I        <- rep(0,2)
  LIT_1    <- 100   
  LIT_2    <- 100
  MIC_1    <- 0.2 #min start (default) = 0.01
  MIC_2    <- 0.02 #min start (default) = 0.01
  SOM_1    <- 0
  SOM_2    <- 0
  SOM_3    <- 0
  CO2_1    <- 0
  CO2_2    <- 0
  
  # Loop over days
  for (d in 1:nday)  {
    for (h in 1:24)   {
      
      ## Set global parameters to allow pass to stode function
      .GlobalEnv$VMAX <- VMAX
      .GlobalEnv$KM <- KM
      .GlobalEnv$fPHYS <- fPHYS
      .GlobalEnv$fCHEM <- fCHEM
      .GlobalEnv$fAVAI <- fAVAI
      .GlobalEnv$I <- I
      .GlobalEnv$tau <- tau
      .GlobalEnv$LITmin <- LITmin
      .GlobalEnv$SOMmin <- SOMmin
      .GlobalEnv$MICtrn <- MICtrn
      .GlobalEnv$desorb <- desorb
      .GlobalEnv$DEsorb <- DEsorb
      .GlobalEnv$OXIDAT <- OXIDAT
      
      UPpars  <- c( I = I, VMAX = VMAX, KM = KM, CUE = CUE, 
                    fPHYS = fPHYS, fCHEM = fCHEM, fAVAI = fAVAI, FI = FI, 
                    tau   = tau, LITmin = LITmin, SOMmin = SOMmin, MICtrn = MICtrn, 
                    desorb= desorb, DEsorb = DEsorb, OXIDAT = OXIDAT, KO = KO)
      UPy     <- c( LIT_1 = LIT_1, LIT_2 = LIT_2, 
                    MIC_1 = MIC_1, MIC_2 = MIC_2, 
                    SOM_1 = SOM_1, SOM_2 = SOM_2, SOM_3 = SOM_3 )
      update <- RXEQ(y = UPy, pars = UPpars)
      
      LIT_1  <- LIT_1 + update[[1]][1]
      LIT_2  <- LIT_2 + update[[1]][2]
      MIC_1  <- MIC_1 + update[[1]][3]
      MIC_2  <- MIC_2 + update[[1]][4]
      SOM_1  <- SOM_1 + update[[1]][5]
      SOM_2  <- SOM_2 + update[[1]][6]
      SOM_3  <- SOM_3 + update[[1]][7]
      CO2_1  <- CO2_1 + update[[1]][8]
      CO2_2  <- CO2_2 + update[[1]][9]
      remove(UPpars, UPy, update)
      
      #write out daily results
      if (h == 24) {
        MIMout$DAY[d] <- d
        MIMout$LITm[d] <- LIT_1
        MIMout$LITs[d] <- LIT_2
        MIMout$MICr[d] <- MIC_1
        MIMout$MICK[d] <- MIC_2
        MIMout$SOMp[d] <- SOM_1
        MIMout$SOMc[d] <- SOM_2
        MIMout$SOMa[d] <- SOM_3
        MIMout$CO2_MICr[d] <- CO2_1
        MIMout$CO2_MICK[d] <- CO2_2
      }	   						#close daily results counter
    }							#close hour loop
  }		

  return(MIMout)
}

#####################
# Example use of 
#####################

# ##############################################
# #single point run
# ##############################################
# df <- data.frame(SITE = 'HARV',
#                    ANPP = 744,
#                    MAT = 25,#7.1,
#                    CLAY = 15,
#                    LIG = 21,
#                    N = 1.02,
#                    CN = 49.01960784)
# 
# MIMout <- MIMICS1(df[1,])


# ##############################################
# # Full forcing dataset run
# ##############################################
# data <- data <- read.csv("RCrk_Modelling_Data/LTER_SITE_1.csv", as.is=T)

# MIMrun <- data %>% split(1:nrow(data)) %>% map(~ MIMICS1(df=.)) %>% bind_rows()
# MIMrun <- data %>% cbind(MIMrun %>% select(-Site, -TSOI))


############
# Plots
###########

# Litter mass
# plot_LIT <- ggplot(MIMout, aes(y=LITs, x=DAY, color="Structural")) + geom_line(size=1) +
#   geom_line(aes(y=LITm, x=DAY, color="Metabolic"), size=1) +
#   theme_bw() +
#   ylab("Litter mass remaining (%)") +
#   xlab("Incubation Time (days)") +
#   labs(color = "Litter Pool")
# 
# # SOM & MIC pools
# plot_SOM_MIC <- ggplot(MIMout, aes(SOMc, x=DAY, color="SOMc")) + geom_line(size=1) +
#   geom_line(aes(y=SOMa, x=DAY, color="SOMa"), size=1) +
#   geom_line(aes(y=MICr, x=DAY, color="MIC-r"), size=1) +
#   geom_line(aes(y=MICK, x=DAY, color="MIC-K"), size=1) +
#   theme_bw() +
#   ylab("Microbial and soil C") +
#   xlab("Incubation Time (days)") +
#   labs(color = "C Pool") +
#   ylim(0, 3)
# 
# # CO2 fraction
# plot_CO2 <- ggplot(MIMout, aes(y=rowSums(MIMout[,10:11])/rowSums(MIMout[,3:11]),
#                    x=DAY, color="CO2-C")) + geom_line(size=1) +
#   theme_bw() +
#   ylab("CO2 (fraction of initial") +
#   xlab("Incubation Time (days)") +
#   labs(color = "C Pool")
# 
# 
# # Build a panel plot
# ggarrange(plot_LIT, plot_SOM_MIC, plot_CO2,
#           nrow=3,
#           ncol=1)

