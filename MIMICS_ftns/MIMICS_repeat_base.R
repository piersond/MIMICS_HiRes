## Set working drive
setwd("C:/github/MIMICS_HiRes")

# Bring in MIMICS ftn
source("MIMICS_ftns/MIMICS_base_ftn.R")

###################################################
# Set MIMICS initial parameters (default values)
###################################################
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
CUE_MULT <- 1
tau_r   <- c(0.00052, 0.3)
tau_K   <- c(0.00024, 0.1)
Tau_MOD <- c(100, 0.6, 1.3, 3.5)
Tau_MULT <- 1
fPHYS_r <- c(0, 0)
fPHYS_K <- c(0, 0)
fPHYS_MULT <- 1
fCHEM_r <- c(0.1, -3, 1)
fCHEM_K <- c(0.3, -3, 1)
fSOM_p  <- c(0.000015, -1.5)
PHYS_scalar <- c(2, -2, NA, NA, NA, NA)
desorb_MULT <- 1
FI      <- c(0.05, 0.05)
fmet_p <- c(1, 0.85, 0.013)
depth <- 5 ###
h2y        <- 24*365
MICROtoECO <- depth * 1e4 * 1e-3  # mgC/cm3 to g/m2

# Store default parameters for brute force reference
Vslope_default <- rep(0.063, 6)
Vint_default <- rep(5.47, 6)
Kslope_default <- rep(0.02, 6)
Kint_default <- rep(3.19, 6)
CUE_default <- c(0.5, 0.25, 0.7, 0.35)
CUE_MULT_default <- 1
Tau_MULT_default <- 1
desorb_MULT_default <- 1
fPHYS_MULT_default <- 1


###########################################
# MIMICS repeat run function
###########################################

MIMrepeat <- function(forcing_df, rparams) {
  
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
  #MIMrun <- forcing_df %>% cbind(MIMrun)
  
  #add run number
  MIMrun$run_num <- rparams$run_num[1]
  
  
  ######################################
  # Selection of output data type
  ######################################
  return(MIMrun)
    
}


#####################
# Example use
#####################

# Test data
ex_data1 <- data.frame(SITE = 'HARV',
                 ANPP = 744,
                 MAT = 25,
                 CLAY = 15, 
                 LIG = 21,
                 N = 1.02, 
                 CN = 49.01960784)

ex_data2 <- data.frame(SITE = c('HARV', 'Test'),
                 ANPP = c(744, 500),
                 MAT = c(25, 25),
                 CLAY = c(15, 10),
                 LIG = c(21,23),
                 N = c(1.02, 1.02),
                 CN = c(49.01960784, 49))

ex_params <- data.frame(Vslope_x = 1.5382,
                          Vint_x = 1.8601,
                          Kslope_x = 0.8204,
                          Kint_x = 1.7086,
                          Tau_x = 0.8446,
                          CUE_x = 0.9113,
                          desorb_x = 1.7790,
                          fPHYS_x = 0.9690,
                          run_num = 1)

ex_output <- MIMrepeat(forcing_df = ex_data1, rparams = ex_params)
