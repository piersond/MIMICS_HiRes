
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

#bring in RXEQ function
source("MIMICS_ftns/RXEQ_ftn.R")

### "Jitter step"
#Ftn to re-run stode ftn if solve is unsuccessful (mostly necessary to counter MICk crash to 0)
stode_jitter <- function(stode_y = Ty, stode_time = 1e6, stode_fun = RXEQ, stode_parms = Tpars, stode_pos = TRUE, run_i = 0) {
  success <- FALSE
  while (!success) {
    run_i <- run_i + 1
    #do something
    test  <- stode(y = stode_y, time = stode_time, fun = stode_fun, parms = stode_parms, positive = stode_pos)
    tbl <- as.numeric(test[[1]])
    
    # Repeat stode ftn if the r or K microbial pools crash below 1e-10
    success <- tbl[3] > 1e-10 & tbl[4] > 1e-10

    if(!success) {
      #Add 1% on to Ty$mic2 if no success
      stode_y['MIC_2'] = stode_y['MIC_2'] * 1.01
      #print(stode_y['MIC_2'])
    }
    
    if(run_i > 3) {
      success <- TRUE
      #print(paste0("Jitter cap: ", as.character(run_i)))
      }
  }
  
  return(c(test,run_i))
}


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
MICROtoECO <- depth * 1e4 * 1e-3         # mgC/cm3 to g/m2

########################################
# Apply parameter multipliers
########################################
Vslope = Vslope * 1
Vint = Vint * 1
Kslope = Kslope * 1
Kint = Kint * 1
CUE = CUE * 1
Tau_MULT = 1
desorb_MULT = 1
fPHYS_MULT = 1

###########################################
# MIMICS single point function
###########################################
MIMICS1 <- function(df){

  #run notes
  note <- ""
  print(df$Site)
  
  
  ###set LIGNIN:N to fMET value
  lig_N <- df$lig_N
  #fMET <- fmet_p[1] * (fmet_p[2] - fmet_p[3] * lig_N) 
  #LTER SHORTCUT FMET
  fMET <- 0.3846423
  
  #if lignin:N data missing, use specified value
 # if(lig_N == 0) {
 #    fMET <- 0.6979
    #print("Using default fMET")
#    } #<--sagebrush litter 
   
  ###set ANPP value
  ANPP       <- (df$pGPP+400)/2
  print(ANPP)
  #prevent negative ANPP
 # if(ANPP < 1){
 #   ANPP <- 1.19999
#    note <- "Fixing ANPP < 1"
#  }
  
  ### Set CLAY value
  fCLAY      <- df$CLAY/100
  # Prevent clay < 3%
 # if(fCLAY < 0.02) {
#    fCLAY <- 0.02
    #print("Low clay value set to 2%")
#  }
  
  ### Set TSOI value
  TSOI       <- df$TSOI
  
  # Calc litter input rate
  EST_LIT <- (ANPP / (365*24)) * 1e3 / 1e4
  print(EST_LIT)# gC/m2/h (from gC/m2/y) then mgC/cm2/h(from gC/m2/h) 
  
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
  print(desorb)
  
  
  pSCALAR  <- PHYS_scalar[1] * exp(PHYS_scalar[2]*(sqrt(fCLAY)))  #Scalar for texture effects on SOMp
  v_MOD    <- vMOD  
  k_MOD    <- kMOD 
  k_MOD[3] <- k_MOD[3] * pSCALAR    
  k_MOD[6] <- k_MOD[6] * pSCALAR    
  
  VMAX     <- Vmax * v_MOD 
  KM       <- Km / k_MOD
  
  #initialize pools
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
  
  #Calculate RXEQ pools  
  Tpars <- c( I = I, VMAX = VMAX, KM = KM, CUE = CUE, 
              fPHYS = fPHYS, fCHEM = fCHEM, fAVAI = fAVAI, FI = FI, 
              tau = tau, LITmin = LITmin, SOMmin = SOMmin, MICtrn = MICtrn, 
              desorb = desorb, DEsorb = DEsorb, OXIDAT = OXIDAT, KO = KO)
  Ty    <- c( LIT_1 = lit[1], LIT_2 = lit[2], 
              MIC_1 = mic[1], MIC_2 = mic[2], 
              SOM_1 = som[1], SOM_2 = som[2], SOM_3 = som[3])
  
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
  
  
  #test  <- stode(y = Ty, time = 1e6, fun = RXEQ, parms = Tpars, positive = TRUE)
  test  <- stode_jitter(stode_y = Ty, stode_time = 1e6, stode_fun = RXEQ, stode_parms = Tpars, stode_pos = TRUE)
  
  
  ###Return MIMICS output 
  #table[i,2:8] <- as.numeric(test[[1]])
  MIMLIT    <- (test[[1]][[1]]+test[[1]][[2]])  * depth *1e4 / 1e6 #convert kgC/m2 from mgC/cm3 (0-30 cm) 
  MIMMIC    <- (test[[1]][[3]]+test[[1]][[4]])  * depth *1e4 / 1e6
  MIM_CO    <-  test[[1]][[3]]/test[[1]][[4]]
  MIMSOC    <- sum(test[[1]])  * depth *1e4 / 1e6   
  
  table <- as.numeric(test[[1]])
  
  MIMout <- data.frame(Site = df$Site,
                       fCLAY = fCLAY,
                       TSOI = TSOI,
                       ANPP = ANPP,
                       LIGN = lig_N,
                       EST_LIT = EST_LIT,
                       MIMSOC = MIMSOC,
                       MIMMIC = MIMMIC,
                       MIMLIT = MIMLIT,
                       MIM_CO = MIM_CO,
                       LITm = table[1] * depth *1e4 / 1e6, #convert kgC/m2 from mgC/cm3 (0-30 cm) 
                       LITs = table[2] * depth *1e4 / 1e6,
                       MICr = table[3] * depth *1e4 / 1e6,
                       MICK = table[4] * depth *1e4 / 1e6,
                       SOMp = table[5] * depth *1e4 / 1e6,
                       SOMc = table[6] * depth *1e4 / 1e6,
                       SOMa = table[7] * depth *1e4 / 1e6,
                       JITn = test[[2]],
                       DEBUG = note
                      )
  #Reset global parameters from last run
  # .GlobalEnv$VMAX <- NA
  # .GlobalEnv$KM <- NA
  # .GlobalEnv$fPHYS <- NA
  # .GlobalEnv$fCHEM <- NA
  # .GlobalEnv$fAVAI <- NA
  # .GlobalEnv$I <- NA
  # .GlobalEnv$tau <- NA
  # .GlobalEnv$LITmin <- NA
  # .GlobalEnv$SOMmin <- NA
  # .GlobalEnv$MICtrn <- NA
  # .GlobalEnv$desorb <- NA
  # .GlobalEnv$DEsorb <- NA
  # .GlobalEnv$OXIDAT <- NA

  #remove global variables set for stode ftn
  #rm(I, VMAX, KM, fPHYS, fCHEM, fAVAI, tau, LITmin, SOMmin, MICtrn, desorb, DEsorb, OXIDAT)
  
  return(MIMout)
  }

#####################
# Example use
#####################

# ### single point run
# ##############################################
# data <- data.frame(Site = 1,
#                    pGPP = 1.39,
#                    TSOI = 10.6,
#                    CLAY = 20,
#                    lig_N = 11)
# 
# MIMout_single <- MIMICS1(data[1,])
# 
# 
# ### full forcing dataset run
# ##############################################
data <- data <- read.csv("RCrk_Modelling_Data/LTER_SITE_1.csv", as.is=T)

MIMout_single <- MIMICS1(data[1,])

MIMrun <- data %>% split(1:nrow(data)) %>% map(~ MIMICS1(df=.)) %>% bind_rows()
MIMrun <- data %>% cbind(MIMrun %>% select(-Site, -TSOI))


### Plot SOC vs MIMSOC
################################################
library(ggplot2)
library(Metrics)

plot_data <- MIMrun

r2_test <- cor.test(MIMrun$SOC, MIMrun$MIMSOC)
r_val <- round(as.numeric(unlist(r2_test ['estimate'])),2)
lb2 <- paste("R^2 == ", r_val)

rmse <- round(rmse(MIMrun$SOC, MIMrun$MIMSOC),2) 

ggplot(plot_data, aes(x=MIMSOC, y=SOC, color=ANPP)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=0.8) + 
  geom_text(aes(label=paste0(Site)),hjust=-0.2, vjust=0.2) +
  annotate("text", label = lb2, x = 4, y = 8.5, size = 6, colour = "black", parse=T) + 
  annotate("text", label = paste0("RMSE = ", rmse), x = 4, y = 7.4, size = 6, colour = "black") 

