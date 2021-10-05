library(tidyverse)
library(furrr)
library(purrr)

# Set working drive if not running on HPC 
if(.Platform$OS.type == "unix") {
} else {
  setwd("C:/github/MIMICS_HiRes")  
}

# Bring in MIMICS ftn
source("MIMICS_ftns/MIMICS_base_ftn.R")

# Load map forcing data (output from MIMICS_map_data_generator.R)
frc_data <- readRDS("Mapping/Map_data_in/MIMICS_map_forcing_data.rds")

##################################################
# Impose environmental change scenario
##################################################

# +10% GPP
frc_data$pGPP <- frc_data$pGPP + abs(frc_data$pGPP)*0.1

# +1 C TSOI
#frc_data$TSOI <- frc_data$TSOI+1

########################################
# Apply parameter multipliers
#######################################

# Load a set of parameters for MIMICS
param_sets <- read.csv("Mapping/Map_data_in/RC_MIM_param_combos_RMSE_less2.csv", as.is=T)
param_sets$pID <- seq(1,nrow(param_sets),1)

for(i in 1:nrow(param_sets)) {
  
  # Manually select parameter set
  pset_num <- i  #<-- enter number for desired parameter set (row #)
  pset <- param_sets[pset_num,]

  #####################################
  ### Setting of chosen parameters
  #####################################
  
  # Set parameter defaults
  Vslope_d  = rep(0.063, 6)
  Vint_d    = rep(5.47, 6)
  Kslope_d  = rep(c(0.025, 0.035, 0.025),2)
  Kint_d    = rep(3.19, 6)
  CUE_d     = c(0.55, 0.25, 0.75, 0.35)
  Tau_MULT_d = 1
  Tau_MULT_d = 1
  desorb_MULT_d = 1
  fPHYS_MULT_d = 1
  
  # Set parameters
  Vslope = Vslope_d * pset$Vslope_x
  Vint = Vint_d * pset$Vint_x
  Kslope = Kslope_d * pset$Kslope_x
  Kint = Kint_d * pset$Kint_x
  CUE = CUE_d * pset$CUE_x
  Tau_MULT = pset$Tau_x
  desorb_MULT = pset$desorb_x
  fPHYS_MULT = pset$fPHYS_x
  
  
  ###################################################
  # Start up MIMICS run using parallel processing
  ###################################################
  
  # Set number of cores to use
  no_cores <- availableCores() - 2
  plan(multicore, gc = TRUE, workers = no_cores)
  
  # Run MIMICS! (track time required)
  
  MIM_Map_data <- frc_data %>% split(1:nrow(frc_data)) %>%
                  future_map(MIMICS1, .progress=TRUE) %>% bind_rows() 
  
  # Release CPU cores
  plan(sequential)
  nbrOfWorkers()
  
  
  #################################################
  # Save map output
  #################################################
  
  #Write raw map output
  #saveRDS(MIM_Map_data, paste0("Mapping/Map_data_out/MIMICS_map_data_pset", pset_num, ".rds"))
  
  # join to raster frac data frame
  MIM_Map_data_full <- merge(frc_data, MIM_Map_data, by.x = "Site",
                             by.y = "Site", all.x = TRUE, all.y = FALSE)
  #save full map dataset
  saveRDS(MIM_Map_data_full, paste0("Mapping/Map_data_out/MIMICS_map_GPP10_pset", pset_num, ".rds"))
  
  
  ### clean up env
  rm(MIM_Map_data)
  rm(MIM_Map_data_full)
  
  # Clean up memory
  gc()
    
}  

