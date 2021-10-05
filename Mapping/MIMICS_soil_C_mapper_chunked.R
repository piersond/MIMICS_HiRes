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

# Double check that data has no rows with NA values
map_data <- frc_data %>% drop_na()

######################################
# Run a scenario, or comment out
######################################
#frc_data$TSOI <- frc_data$TSOI+1
#frc_data$pGPP <- frc_data$pGPP*1.05

### DEBUG: Run a small piece of the map for testing purposes
#map_data <- map_data[1:5000,]


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
pset_num <- 1  #<-- enter number for desired parameter set (row #)
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

############################################################################
#Set up in a for loop to reduce the memory load and save output chunks
############################################################################
chunck_size <- 10000

### DEBUG: Run a specific chunk
#for(chunk in 49:49) {

for(chunk in 1:as.integer(nrow(map_data)/chunck_size)) {
  
    chunk_start_row <- chunck_size*(chunk-1)+1
    
    chunk_end_row <- if(chunk == as.integer(nrow(map_data)/chunck_size)) {
      nrow(map_data)
    } else {
      chunck_size*chunk
    }
    
  map_chunk <- map_data[chunk_start_row:chunk_end_row,]

  ######################
  # Check if data already exist
  if(file.exists(paste0("Mapping/Map_data_out/MIMICS_map_data_pset", pset_num, "_chunk",chunk, ".rds"))) {
    print(paste0("Data exists for chunk ", chunk))
    next
  } else {
    print(paste0("Running chunk ",chunk))
  }

  
  ###################################################
  # Start up MIMICS run using parallel processing
  ###################################################
  
  # Set number of cores to use
  no_cores <- availableCores() - 2
  plan(multicore, gc = TRUE, workers = no_cores)
  
  # Run MIMICS! (track time required)
  start_time <- Sys.time()
  MIM_Map_data <- map_chunk %>% split(1:nrow(map_chunk)) %>%  future_map(MIMICS1, .progress=FALSE) %>% bind_rows() 
  
  # Release CPU cores
  plan(sequential)
  nbrOfWorkers()
  
  # Clean up memory
  gc()


#################################################
# Save map output
#################################################

#Write raw map output
saveRDS(MIM_Map_data, paste0("Mapping/Map_data_out/MIMICS_map_data_pset", pset_num, "_chunk",chunk, ".rds"))

#end of for loop for chunk
}






