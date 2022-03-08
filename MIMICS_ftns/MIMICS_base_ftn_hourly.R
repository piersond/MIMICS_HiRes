## Set working drive
setwd("C:/github/MIMICS_HiRes")

#Libraries
library(rootSolve)
library(boot)
library(ggplot2)
library(tidyverse)
library(Metrics) 
library(parallel)
library(furrr)
library(purrr)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggpubr)

#bring in RXEQ function
source("MIMICS_ftns/RXEQ_ftn.R")

########################################
# Set MIMICS default parameters
########################################
Vslope  <- rep(0.063, 6)
Vint    <- rep(5.47, 6)
aV      <- rep(0.000008, 6)  
Kslope  <- rep(c(0.025, 0.035, 0.025),2)
Kint    <- rep(3.19, 6)
aK      <- rep(10, 6)
vMOD    <- c(10, 2, 10, 3, 3, 2)
kMOD    <- c(8, 2, 4, 2, 4, 6)
KO      <- c(6, 6)
CUE     <- c(0.55, 0.25, 0.75, 0.35)
tau_r   <- c(0.00052, 0.3)
tau_K   <- c(0.00024, 0.1)
Tau_MOD <- c(100, 0.8, 1.2, 2)
Tau_MULT <- 1
fPHYS_r <- c(0.3, 1.3)
fPHYS_K <- c(0.2, 0.8)
fCHEM_r <- c(0.1, -3, 1)
fCHEM_K <- c(0.3, -3, 1)
fSOM_p  <- c(0.000015, -1.5)
PHYS_scalar <- c(2, -2, NA, NA, NA, NA)
FI      <- c(0.05, 0.05)
fmet_p <- c(1, 0.85, 0.013)
depth <- 30 ###
h2y        <- 24*365
MICROtoECO <- depth * 1e4 * 1e-3  # mgC/cm3 to g/m2

#Set default multipliers
Tau_MULT = 1
desorb_MULT = 1
fPHYS_MULT = 1


########################################
# Apply parameter multipliers
########################################
# Vslope = Vslope * 1.693578
# Vint = Vint * 0.633318
# Kslope = Kslope * 1.782366
# Kint = Kint * 0.3609913
# CUE = CUE * 1
# Tau_MULT = 1
# desorb_MULT = 2.3635554
# fPHYS_MULT = 2.0716163

###########################################
# MIMICS single point function
###########################################
MIMICS_hr <- function(df, ndays) {
  
  #DEBUG
  #df <- data[1,]
  
  
  # Set run length
  nday <- ndays
  
  ### Bring in lig:N forcing data
  lig_N <- df$lig_N
  
  ###########################################################
  ### Set fMET equation
  ###########################################################
  ## Option A: Defualt fMET equation using lig:N values
  #fMET <- fmet_p[1] * (fmet_p[2] - fmet_p[3] * lig_N) 
  
  ## Option B: LTER "SHORTCUT" fMET value (average from LiDET)
  fMET <- 0.3846423
  
  ###########################################################
  
  ###Bring in forcing ANPP value
  ANPP       <- df$pGPP
  
  ## Modify as necessary to approximate ANPP
  # e.g. ANPP ~ GPP from MSAVI +400, then divided by 2 
  ANPP <-  (ANPP+400)/2
  
  ### Bring in TSOI value
  TSOI       <- df$TSOI
  
  ### Bring in TSOI value
  fCLAY       <- df$CLAY/100 
  
  ############################################################
  # MIMICS MODEL CODE STARTS HERE
  ############################################################
  
  # Calc litter input rate
  EST_LIT <- (ANPP / (365*24)) * 1e3 / 1e4
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
  
  
  
  ############################################################
  # MIMICS MODEL RUN STARTS HERE
  ############################################################

  # Open matrices to store model output
  LITmin  <- rep(NA, dim=4)
  MICtrn  <- rep(NA, dim=6)
  SOMmin  <- rep(NA, dim=2)
  DEsorb  <- rep(NA, dim=1)
  OXIDAT  <- rep(NA, dim=1)
  
  # Create dataframe to store model output
  MIMout <- data.frame(SITE = rep("Site1", nday),
                       DAY = rep(NA, nday),
                       LITm = rep(NA, nday),
                       LITs = rep(NA, nday),
                       MICr = rep(NA, nday),
                       MICK = rep(NA, nday),
                       SOMp = rep(NA, nday),
                       SOMc = rep(NA, nday),
                       SOMa = rep(NA, nday),
                       CO2_MICr = rep(NA, nday),
                       CO2_MICK = rep(NA, nday),
                       dLITm = rep(NA, nday),
                       dLITs = rep(NA, nday),
                       dMICr = rep(NA, nday),
                       dMICK = rep(NA, nday),
                       dSOMp = rep(NA, nday),
                       dSOMc = rep(NA, nday),
                       dSOMa = rep(NA, nday),
                       dCO2_MICr = rep(NA, nday),
                       dCO2_MICK = rep(NA, nday))
  
  # Initialize model pools and fluxes (same as sandbox)
  I       <- array(NA, dim=2)             
  I[1]    <- (EST_LIT / depth) * fMET     
  I[2]    <- (EST_LIT / depth) * (1-fMET)
  lit     <- I   
  mic     <- I  
  som     <- rep(NA, 3) 
  som[1]  <- I[1]
  som[2]  <- I[2]
  som[3]  <- I[1] 
  LITmin  <- rep(NA, dim=4)
  MICtrn  <- c(NA,NA,NA,NA,NA,NA)
  SOMmin  <- rep(NA, dim=2)
  DEsorb  <- rep(NA, dim=1)
  OXIDAT  <- rep(NA, dim=1)
  
  # Set initial pool values
  LIT_1  <- lit[1]
  LIT_2  <- lit[2]
  MIC_1  <- mic[1]
  MIC_2  <- mic[2]
  SOM_1  <- som[1]
  SOM_2  <- som[2]
  SOM_3  <- som[3]
  CO2_1  <- 0 
  CO2_2  <- 0
  
  ## Set global parameters to pass to RXEQ function
  #DEBUG: Double check this works correctly when run outside of the loop
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
  
  # Create vector of parameter values
  # tpars <- c(I = I, VMAX = VMAX, KM = KM, CUE = CUE, 
  #            fPHYS = fPHYS, fCHEM = fCHEM, fAVAI = fAVAI, FI = FI, 
  #            tau   = tau, LITmin = LITmin, SOMmin = SOMmin, MICtrn = MICtrn, 
  #            desorb= desorb, DEsorb = DEsorb, OXIDAT = OXIDAT, KO = KO)
  
  ### BEGIN MODEL LOOP ###
  
  # Set up progress bar
  pbar <- txtProgressBar(min = 0, max = nday, style = 3)
  
  # Loop over 24 hours for specified number of days 
  for (d in 1:nday)  {
    for (h in 1:24)   {
    
      # Get model output from RXEQ ftn
      update <- RXEQ(y = c(LIT_1 = LIT_1, LIT_2 = LIT_2, 
                           MIC_1 = MIC_1, MIC_2 = MIC_2, 
                           SOM_1 = SOM_1, SOM_2 = SOM_2, 
                           SOM_3 = SOM_3),
                     pars = c(I = I, VMAX = VMAX, KM = KM, CUE = CUE, 
                              fPHYS = fPHYS, fCHEM = fCHEM, fAVAI = fAVAI, FI = FI, 
                              tau   = tau, LITmin = LITmin, SOMmin = SOMmin, MICtrn = MICtrn, 
                              desorb= desorb, DEsorb = DEsorb, OXIDAT = OXIDAT, KO = KO))
      
      # Update C pools
      LIT_1  <- LIT_1 + update[[1]][1]
      LIT_2  <- LIT_2 + update[[1]][2]
      MIC_1  <- MIC_1 + update[[1]][3]
      MIC_2  <- MIC_2 + update[[1]][4]
      SOM_1  <- SOM_1 + update[[1]][5]
      SOM_2  <- SOM_2 + update[[1]][6]
      SOM_3  <- SOM_3 + update[[1]][7]
      CO2_1  <- CO2_1 + update[[1]][8]
      CO2_2  <- CO2_2 + update[[1]][9]
      #remove(UPpars, UPy, update)
      
      # Store daily output
      if(h == 24)  {
        # Store delta
        MIMout$DAY[d] <- d
        MIMout$dLITm[d] <- update[[1]][1]
        MIMout$dLITs[d] <- update[[1]][2]
        MIMout$dMICr[d] <- update[[1]][3]
        MIMout$dMICK[d] <- update[[1]][4]
        MIMout$dSOMp[d] <- update[[1]][5]
        MIMout$dSOMc[d] <- update[[1]][6]
        MIMout$dSOMa[d] <- update[[1]][7]
        MIMout$dCO2_MICr[d] <- update[[1]][8]
        MIMout$dCO2_MICK[d] <- update[[1]][9]
        
        # Store daily pool size
        MIMout$LITm[d] <- LIT_1
        MIMout$LITs[d] <- LIT_2
        MIMout$MICr[d] <- MIC_1
        MIMout$MICK[d] <- MIC_2
        MIMout$SOMp[d] <- SOM_1
        MIMout$SOMc[d] <- SOM_2
        MIMout$SOMa[d] <- SOM_3
        MIMout$CO2_MICr[d] <- CO2_1
        MIMout$CO2_MICK[d] <- CO2_2
        
        setTxtProgressBar(pbar, d)
        #print(d)
        }	
    }	
    } 
  # Return daily model output as datatable
  return(MIMout)
  
} #close MIMICS_24 ftn

#####################
# Example use of 
#####################

##############################################
#single point run
##############################################
data <- data.frame(Site = "Site1",
                   pGPP = 1.39,
                   TSOI = 10.6,
                   CLAY = 20,
                   lig_N = 11)

MIMout <- MIMICS_hr(data[1,], 100000)

##############################################
# Full forcing dataset run
##############################################
# data <- data <- read.csv("RCrk_Modelling_Data/LTER_SITE_1.csv", as.is=T)

# MIMout_single <- MIMICS1(data[1,])

# MIMrun <- data %>% split(1:nrow(data)) %>% map(~ MIMICS1(df=.)) %>% bind_rows()
# MIMrun <- data %>% cbind(MIMrun %>% select(-Site, -TSOI))


############
# Plots
###########

# Litter mass
plot_LIT <- ggplot(MIMout, aes(y=LITs, x=DAY, color="Structural")) + geom_line(size=1) +
  geom_line(aes(y=LITm, x=DAY, color="Metabolic"), size=1) +
  theme_bw() +
  ylab("Litter C") +
  xlab("Time (days)") +
  labs(color = "Litter Pool")

# SOM & MIC pools
plot_SOM <- ggplot(MIMout, aes(y=SOMc, x=DAY, color="SOMc")) + geom_line(size=1) +
  geom_line(aes(y=SOMa, x=DAY, color="SOMa"), size=1) +
  geom_line(aes(y=SOMp, x=DAY, color="SOMp"), size=1) +
  theme_bw() +
  ylab("SOM") +
  xlab("Time (days)") +
  labs(color = "SOC Pool")

# SOM & MIC pools
plot_MIC <- ggplot(MIMout, aes(y=MICK, x=DAY, color="MIC-K")) + geom_line(size=1) +
  geom_line(aes(y=MICr, x=DAY, color="MIC-r"), size=1) +
  theme_bw() +
  ylab("Microbial C") +
  xlab("Time (days)") +
  labs(color = "MIC-C Pool")

# CO2 fraction
plot_CO2 <- ggplot(MIMout, aes(y=CO2_MICK,
                               x=DAY, color="dMIC-K")) + geom_line(size=1) +
  geom_line(aes(y=CO2_MICr, x=DAY, color="dMIC-r"), size=1) +
  theme_bw() +
  ylab("Respiration Total (CO2-C)") +
  xlab("Time (days)") +
  labs(color = "MIC")



# # Build a panel plot
# ggarrange(plot_LIT, plot_SOM, plot_MIC, plot_CO2,
#          nrow=4,
#          ncol=1)

####### Rate plots ################
# Litter mass
plot_dLIT <- ggplot(MIMout, aes(y=dLITs, x=DAY, color="Structural")) + geom_line(size=1) +
  geom_line(aes(y=dLITm, x=DAY, color="Metabolic"), size=1) +
  theme_bw() +
  ylab("Litter C") +
  xlab("Time (days)") +
  labs(color = "Litter Pool")

# SOM & MIC pools
plot_dSOM <- ggplot(MIMout, aes(y=dSOMc, x=DAY, color="SOMc")) + geom_line(size=1) +
  geom_line(aes(y=dSOMa, x=DAY, color="SOMa"), size=1) +
  geom_line(aes(y=dSOMp, x=DAY, color="SOMp"), size=1) +
  theme_bw() +
  ylab("SOM") +
  xlab("Time (days)") +
  labs(color = "SOC Pool")

# SOM & MIC pools
plot_dMIC <- ggplot(MIMout, aes(y=dMICK, x=DAY, color="dMIC-K")) + geom_line(size=1) +
  geom_line(aes(y=dMICr, x=DAY, color="dMIC-r"), size=1) +
  theme_bw() +
  ylab("Microbial C") +
  xlab("Time (days)") +
  labs(color = "MIC-C Pool")

# CO2 fraction
plot_dCO2 <- ggplot(MIMout, aes(y=dCO2_MICK,
                                x=DAY, color="MIC-K")) + geom_line(size=1) +
  geom_line(aes(y=dCO2_MICr, x=DAY, color="MIC-r"), size=1) +
  theme_bw() +
  ylab("Respiration (CO2-C d-1)") +
  xlab("Time (days)") +
  labs(color = "MIC")



# Build a panel plot
ggarrange(plot_LIT, plot_dLIT, 
          plot_SOM, plot_dSOM,
          plot_MIC, plot_dMIC, 
          plot_CO2, plot_dCO2,
          nrow=4,
          ncol=2)

