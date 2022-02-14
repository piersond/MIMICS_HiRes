### MIMICS MC

setwd("C:/github/MIMICS_HiRes")

########################################
# Load MIMICS data and ftns from Brute Forcing script
########################################
source("MIMICS_ftns/MIMICS_repeat_base.R")


########################################
# Load forcing data
########################################
#data <- read.csv("RCrk_Modelling_Data/RCrk_SOC_calibration.csv", as.is=T)

ex_data <- data.frame(SITE = 'HARV',
                      ANPP = 744,
                      MAT = 25,
                      CLAY = 15, 
                      LIG = 21,
                      N = 1.02, 
                      CN = 49.01960784)

# Trim out data columns not required for the MC run 
## Save on dataframe size and we can add the back later

#data <- data %>% select(Site, lat, long, SOC, CLAY, pGPP, estCLAY, TSOI, lig_N)


####################################
# Use the brute force MIMICS ftn
####################################

# Set desired number of random parameter runs
MIM_runs <- 10

### Create random parameter dataframe
## Parameter range informed by range observed over 10+ MCMC analysis results
rand_params <- data.frame(Vslope_x = 1, #runif(MIM_runs, 0.4, 4),  
                          Vint_x = 1, #runif(MIM_runs, 0.3, 3),  
                          Kslope_x = 1, #runif(MIM_runs, 0.4, 4),  
                          Kint_x = 1, #runif(MIM_runs, 0.3, 3),  
                          Tau_x = 1, #runif(MIM_runs, 0.3, 3),  
                          CUE_x = 1, #runif(MIM_runs, 0.5, 1.5),  
                          desorb_x = 1, #runif(MIM_runs, 0.001, 0.3),  
                          fPHYS_x = 1, #runif(MIM_runs, 0.4, 4) 
                          VMAX_x = runif(MIM_runs, 0.25, 4), 
                          KM_x = runif(MIM_runs, 0.25, 4) 
                          )

rand_params$run_num <- seq(1,MIM_runs,1)

# Set number of cores to use
no_cores <- availableCores() - 1
plan(multicore, gc = FALSE, workers = no_cores)

# Run MIMICS!

print(paste0("Starting ", MIM_runs, " runs"))
print(paste0("Start time: ", Sys.time()))

start_time <- Sys.time()
MC_MIMICS <- rand_params %>% split(1:nrow(rand_params)) %>% future_map(~ MIMrepeat(forcing_df = ex_data, rparams = .), .progress=TRUE) %>% bind_rows()
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
#saveRDS(MC_MIMICS, paste0("MC/Output/", "MC_MIMICS_data-r", as.character(MIM_runs), "_", format(Sys.time(), "%Y%m%d_%H%M%S_"),  ".rds"))


####

MC_summary <- MC_MIMICS %>% filter(DAY == 200)
MC_summary$CO2_frac_tot <- round(rowSums(MIMout[,10:11])[nrow(MIMout)]/rowSums(MIMout[,3:11])[nrow(MIMout)], 4)

                            

