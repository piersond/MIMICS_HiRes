## Set working drive
#setwd("C:/github/MIMICS_HiRes")

# Bring in MIMICS ftn
source("MIMICS_ftns/MIMICS_base_ftn.R")

###################################################
# Set MIMICS initial parameters (default values)
###################################################
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
CUE_MULT <- 1
tau_r   <- c(0.00052, 0.3)
tau_K   <- c(0.00024, 0.1)
Tau_MOD <- c(100, 0.8, 1.2, 2)
Tau_MULT <- 1
fPHYS_r <- c(0.3, 1.3)
fPHYS_K <- c(0.2, 0.8)
fPHYS_MULT <- 1
fCHEM_r <- c(0.1, -3, 1)
fCHEM_K <- c(0.3, -3, 1)
fSOM_p  <- c(0.000015, -1.5)
PHYS_scalar <- c(2, -2)
desorb_MULT <- 1
FI      <- c(0.05, 0.05)
fmet_p <- c(1, 0.85, 0.013)
depth <- 30
h2y        <- 24*365
MICROtoECO <- depth * 1e4 * 1e-3         # mgC/cm3 to g/m2

# Store default parameters for brute force reference
Vslope_default <- rep(0.063, 6)
Vint_default <- rep(5.47, 6)
Kslope_default <- rep(c(0.025, 0.035, 0.025),2)
Kint_default <- rep(3.19, 6)
Tau_MULT_default <- 1
CUE_default <- c(0.55, 0.25, 0.75, 0.35)
CUE_MULT_default <- 1
desorb_MULT_default <- 1
fPHYS_MULT_default <- 1


########################################
# Set fMET equation
########################################
#fMET_calc  <- fmet_p[1] * (fmet_p[2] - fmet_p[3] * data$lig_N)   
#fMET       <- fMET_calc  


###########################################
# MIMICS repeat run function
###########################################

MIMrepeat <- function(forcing_df, rparams, output_type = "summary") {
  
  # Set global model parameters
  .GlobalEnv$Vslope = Vslope_default * rparams$Vslope_x[1]
  .GlobalEnv$Vint = Vint_default * rparams$Vint_x[1]
  .GlobalEnv$Kslope = Kslope_default * rparams$Kslope_x[1]
  .GlobalEnv$Kint = Kint_default * rparams$Kint_x[1]
  .GlobalEnv$Tau_MULT = Tau_MULT_default * rparams$Tau_x[1]
  .GlobalEnv$CUE = CUE_default * rparams$CUE_x[1]
  .GlobalEnv$desorb_MULT = desorb_MULT_default * rparams$desorb_x[1]
  .GlobalEnv$fPHYS_MULT = fPHYS_MULT_default * rparams$fPHYS_x[1]
  
  #full run of forcing data csv
  MIMrun <- forcing_df %>% split(1:nrow(forcing_df)) %>% map(MIMICS1) %>% bind_rows() 
  
  #Optional combine MIMout with forcing data
  MIMrun <- forcing_df %>% cbind(MIMrun %>% select(MIMSOC, MIMMIC, MIMLIT, MIM_CO, LITm, LITs, MICr, MICK, SOMa, SOMc, SOMp, desorb, JITn, DEBUG))
  
  #add run number
  MIMrun$run_num <- rparams$run_num[1]
  
  
  ######################################
  # Selection of output data type
  ######################################
  if(output_type == "summary") {
    # Option 1: Values for each site location in the forcing dataset
    
    
    ############################################
    # Collect summary stats from MIMrun
    ############################################
    # Calculate correlation between field SOC and MIMSOC
    r2_test <- cor.test(MIMrun$SOC, MIMrun$MIMSOC)
    r_val <- round(as.numeric(unlist(r2_test ['estimate'])),3)
    
    #SOC mean & stdev
    MSOC_mn <- mean(MIMrun$MIMSOC, na.rm=T)
    MSOC_sd <- sd(MIMrun$MIMSOC, na.rm=T)
    
    #MIC mean & stdev
    MMIC_mn <- mean(MIMrun$MIMMIC, na.rm=T)
    MMIC_sd <- sd(MIMrun$MIMMIC, na.rm=T)    
    
    #LIT mean & stdev
    MLIT_mn <- mean(MIMrun$MIMLIT, na.rm=T)
    MLIT_sd <- sd(MIMrun$MIMLIT, na.rm=T)   
    
    #Pool mean $ stdev
    LITm_mn_calc <- mean(MIMrun$LITm, na.rm=T)
    LITm_sd_calc <- sd(MIMrun$LITm, na.rm=T)
    
    LITs_mn_calc <- mean(MIMrun$LITs, na.rm=T)
    LITs_sd_calc <- sd(MIMrun$LITs, na.rm=T)
    
    MICr_mn_calc <- mean(MIMrun$MICr, na.rm=T)
    MICr_sd_calc <- sd(MIMrun$MICr, na.rm=T)
    
    MICK_mn_calc <- mean(MIMrun$MICK, na.rm=T)
    MICK_sd_calc <- sd(MIMrun$MICK, na.rm=T)   
    
    SOMa_mn_calc <- mean(MIMrun$SOMa, na.rm=T)
    SOMa_sd_calc <- sd(MIMrun$SOMa, na.rm=T)      
    
    SOMc_mn_calc <- mean(MIMrun$SOMc, na.rm=T)
    SOMc_sd_calc <- sd(MIMrun$SOMc, na.rm=T)   
    
    SOMp_mn_calc <- mean(MIMrun$SOMp, na.rm=T)
    SOMp_sd_calc <- sd(MIMrun$SOMp, na.rm=T)   
    
    # Calculate MIC pool size relative to SOC
    MICpropSOC_mn <- mean(MIMrun$MIMMIC/MIMrun$MIMSOC, na.rm = T)
    
    # Calculate LIT pool size relative to SOC
    LITpropSOC_mn <- mean(MIMrun$MIMLIT/MIMrun$MIMSOC, na.rm = T)
    
    #Get mean MIM_CO
    MIMCO_mn <- mean(MIMrun$MIM_CO, na.rm=T)
    
    #Get mean LITs/LITm
    LITr_mn <- mean((as.numeric(MIMrun$LITs)/as.numeric(MIMrun$LITm)), na.rm = T)
    
    #Calculate average desorb value
    SOMpTO_mn <- 1/(mean(MIMrun$desorb, na.rm = T)* 24 * 365)
    
    # Calculate average residual value
    resid_avg <- mean(MIMrun$SOC - MIMrun$MIMSOC, na.rm = T)
    resid_sd <- sd(MIMrun$SOC - MIMrun$MIMSOC, na.rm = T)
    
    #RMSE
    RMSE <- rmse(MIMrun$SOC, MIMrun$MIMSOC)
    
    #slope fit
    mdl <- lm(MIMrun$SOC~MIMrun$MIMSOC)
    mdl_smry <- summary(mdl)
    slope_fit <- round(mdl_smry$coefficients[2, 1],2)
    
    MIMOUT <- data.frame(
      #Model fit stats
      r2 = r_val,
      RMSE = RMSE,
      slope = slope_fit,
      resid_avg = resid_avg,
      resid_sd = resid_sd,
      
      #MIMICS pools
      MIMSOC_mn = MSOC_mn,
      MIMSOC_sd = MSOC_sd,
      MIMMIC_mn = MMIC_mn,
      MIMMIC_sd = MMIC_sd,
      MIMLIT_mn = MLIT_mn,
      MIMLIT_sd = MLIT_sd,
      
      MICpropSOC = MICpropSOC_mn,
      LITpropSOC = LITpropSOC_mn,
      MIM_CO_mn = MIMCO_mn,
      LIT_RATIO_mn = LITr_mn,
      
      SOMpTO = SOMpTO_mn,
      
      #More pools
      LITm_mn = LITm_mn_calc,
      LITm_sd = LITm_sd_calc,
      
      LITs_mn = LITs_mn_calc,
      LITs_sd = LITs_sd_calc,
      
      MICr_mn = MICr_mn_calc,
      MICr_sd = MICr_sd_calc,
      
      MICK_mn = MICK_mn_calc,
      MICK_sd = MICK_sd_calc, 
      
      SOMa_mn = SOMa_mn_calc,
      SOMa_sd = SOMa_sd_calc,    
      
      SOMc_mn = SOMc_mn_calc,
      SOMc_sd = SOMc_sd_calc,  
      
      SOMp_mn = SOMp_mn_calc,
      SOMp_sd = SOMp_sd_calc, 
      
      #Parameter multipliers
      Vslope_x = rparams$Vslope_x[1],
      Vint_x = rparams$Vint_x[1],
      Kslope_x = rparams$Kslope_x[1],
      Kint_x = rparams$Kint_x[1],
      Tau_x = rparams$Tau_x[1],
      CUE_x = rparams$CUE_x[1],
      desorb_x = rparams$desorb_x[1],
      fPHYS_x = rparams$fPHYS_x[1],
      
      #run info
      run_num = rparams$run_num[1],
      
      #debug info
      jitr_mn = mean(as.numeric(MIMrun$JITn), na.rm = T)
    )
    return(MIMOUT)
    
    
  } else if(output_type == "all") {
    # Option 2: Summary statistics for the run
    return(MIMrun)
  } else {
    print("Set function parameter 'output_type' to either 'summary' or 'all'")
  }
  
  
  
  
}

#####################
# Example use
#####################

# bring in forcing data
data <- read.csv("RCrk_Modelling_Data/RCrk_SOC_calibration.csv", as.is=T)

test_params <- data.frame(Vslope_x = 1.5382,
                          Vint_x = 1.8601,
                          Kslope_x = 0.8204,
                          Kint_x = 1.7086,
                          Tau_x = 0.8446,
                          CUE_x = 0.9113,
                          desorb_x = 1.7790,
                          fPHYS_x = 0.9690,
                          run_num = 1)

test_output_summary <- MIMrepeat(forcing_df = data, rparams = test_params, output_type = "summary")
test_output_all <- MIMrepeat(forcing_df = data, rparams = test_params, output_type = "all")
