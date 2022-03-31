### MIMICS MC

#setwd("C:/github/MIMICS_HiRes")

########################################
# Load MIMICS data and ftns from Brute Forcing script
########################################
source("MIMICS_ftns/MIMICS_repeat_base.R")


########################################
# Load forcing data
########################################
data <- read.csv("MC/Cross-Validation/MC_cross-val_data.csv", as.is=T)

# Select subsample to remove
data <- data %>% filter(cv_group != 2)

# Trim out data columns not required for the MC run 
## Save on dataframe size and we can add the back later

data <- data %>% select(Site, lat, long, SOC, CLAY, pGPP, TSOI, lig_N)


####################################
# Use the brute force MIMICS ftn
####################################

# Set desired number of random parameter runs
MIM_runs <- 500000

### Create random parameter dataframe
## Parameter range informed by range observed over 10+ MCMC analysis results
rand_params <- data.frame(Vslope_x = runif(MIM_runs, 0.4, 4),  
                          Vint_x = runif(MIM_runs, 0.3, 3),  
                          Kslope_x = runif(MIM_runs, 0.4, 4),  
                          Kint_x = runif(MIM_runs, 0.3, 3),  
                          Tau_x = runif(MIM_runs, 0.3, 3),  
                          CUE_x = runif(MIM_runs, 0.2, 2),  
                          desorb_x = runif(MIM_runs, 0.001, 0.3),  
                          fPHYS_x = runif(MIM_runs, 0.01, 4)  
                          )

rand_params$run_num <- seq(1,MIM_runs,1)

# Set number of cores to use
no_cores <- availableCores() - 1
#plan(multicore, gc = FALSE, workers = no_cores)
plan(multicore, gc = FALSE, workers = no_cores)

# Run MIMICS!
print(paste0("Starting ", MIM_runs, " runs"))
print(paste0("Start time: ", Sys.time()))

start_time <- Sys.time()
MC_MIMICS <- rand_params %>% split(1:nrow(rand_params)) %>% future_map(~ MIMrepeat(forcing_df = data, rparams = ., output_type = "summary"), .progress=TRUE) %>% bind_rows()
print(paste0("Task time: ", Sys.time() - start_time))

# Release CPU cores
plan(sequential)
nbrOfWorkers()

# Clean up memory
gc()


## Join parameters to MIMICS output table
MC_MIMICS <- MC_MIMICS %>% left_join(rand_params)


##########################################
# Save MC output data
##########################################
saveRDS(MC_MIMICS, paste0("MC/Cross-Validation/", "MC_MIMICS_output_ssamp12", as.character(MIM_runs), "_", format(Sys.time(), "%Y%m%d_%H%M%S_"),  ".rds"))




