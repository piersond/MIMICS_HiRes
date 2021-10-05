# Set working drive if not running on HPC 
if(.Platform$OS.type == "unix") {
} else {
  setwd("C:/github/MIMICS_HiRes")  
}

########################################
# Load MIMICS data and ftns from Brute Forcing script
########################################
source("MIMICS_ftns/MIMICS_repeat_base.R")


########################################
# Load forcing data and parameter set
########################################
data <- read.csv("RCrk_Modelling_Data/RCrk_SOC_Cal+Val.csv", as.is=T)

# Trim out data columns not required for the MC run 
## Save on dataframe size and add any needed columns back after MIMICS run
data <- data %>% select(Site, lat, long, SOC, CLAY, pGPP, TSOI, lig_N)

# Load parameter set
params_raw <- read.csv("MC/Output/RC_MIM_param_combos_RMSE_less2.csv")
params_raw$run_num <- seq(1,nrow(params),1)

# Select only the MIMICS parameter columns
params <- params_raw %>% select(run_num, Vslope_x, Vint_x, Kslope_x, Kint_x, CUE_x, Tau_x, desorb_x, fPHYS_x)



#######################
# Run MIMICS
#######################


# Set number of cores to use
no_cores <- availableCores() - 1
plan(multicore, gc = FALSE, workers = no_cores)

# Run MIMICS!
print(paste0("Start time: ", Sys.time()))

start_time <- Sys.time()
MIMruns <- params %>% split(1:nrow(params)) %>% future_map(~ MIMrepeat(forcing_df = data, rparams = ., output_type = "all"), .progress=TRUE) %>% bind_rows()
print(paste0("Task time: ", Sys.time() - start_time))

# Release CPU cores
plan(sequential)
nbrOfWorkers()

# Clean up memory
gc()


## Join parameters to MIMICS output table
MIMruns_full <- MIMruns %>% left_join(params_raw)


##########################################
# Save MC output data
##########################################
saveRDS(MIMruns_full, paste0("MIM_pset_runs_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".rds"))




