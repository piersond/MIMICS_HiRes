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

######################################
# Run a scenario, or comment out
######################################
#frc_data$TSOI <- frc_data$TSOI+1
#frc_data$pGPP <- frc_data$pGPP*1.05

#DEBUG: Run a small piece of the map for testing purposes
#frc_data <- frc_data[1:3000,]


########################################
# Apply parameter multipliers
#######################################
### Use option 1 or 2, not both (comment out code for option not used)

##########################################
# OPTION 1
### Manually set parameters for MIMICS run
# pset <- data.frame(
#           id = 38489, #set an ID# for tracking 
#           Vslope_x = 2.891815932,
#           Vint_x = 1.767005898,
#           Kslope_x = 1.003510074,
#           Kint_x = 2.3084668,
#           CUE_x = 0.978538555,
#           Tau_x = 1.025889712,
#           desorb_x = 1.891280193,
#           fPHYS_x = 2.74531248
# )


###########################################

###########################################
# OPTION 2
### If making many maps from sets of MIMICS parameters

# Load a set of parameters for MIMICS
param_sets <- read.csv("Mapping/Map_data_in/RC_MIM_param_combos_RMSE_less2.csv", as.is=T)
param_sets$pID <- seq(1,nrow(param_sets),1)

# Manually select parameter set
pset_num <- 2  #<-- enter number for desired parameter set (row #)
pset <- param_sets[pset_num,]
###########################################


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
#no_cores <- availableCores() - 2
no_cores <- 5
plan(multicore, gc = TRUE, workers = no_cores)

# Run MIMICS! (track time required)
start_time <- Sys.time()
MIM_Map_data <- frc_data %>% split(1:nrow(frc_data)) %>% future_map(MIMICS1, .progress=TRUE) %>% bind_rows() 
Sys.time() - start_time

# Release CPU cores
plan(sequential)
nbrOfWorkers()

# Clean up memory
gc()


#################################################
# Save map output
#################################################

#Write raw map output
saveRDS(MIM_Map_data, paste0("Mapping/Map_data_out/MIMICS_map_data_pset", pset_num, ".rds"))

# join to raster frac data frame
MIM_Map_data_full <- merge(frc_data, MIM_Map_data, by.x = "Site",
                           by.y = "Site", all.x = TRUE, all.y = FALSE)
#save full map dataset
saveRDS(MIM_Map_data_full, paste0("Mapping/Map_data_out/MIMICS_map_data_full_pset", pset_num, ".rds"))


