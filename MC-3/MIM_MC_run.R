### MIMICS MC

#setwd("C:/local_temp/ISU_HPC/071921")

# Bring in MIMICS ftn
source("param_bruteForcing2.R")

#################################################

####################################
# Using the brute force MIMICS ftn
####################################

# Set desired number of random parameter runs
MIM_runs <- 100

#Create random parameter dataframe
rand_params <- data.frame(Vslope_x = runif(MIM_runs, 0.8, 3),  #Start w/ (0.8,3.0)
                          Vint_x = runif(MIM_runs, 0.8, 3),  #Start w/ (0.8, 3)
                          Kslope_x = runif(MIM_runs, 0.8, 3),  #Start w/ (0.8, 3)
                          Kint_x = runif(MIM_runs, 0.8, 3),  #Start w/ (0.8, 3)
                          Tau_x = runif(MIM_runs, 0.3, 2),  #Start w/ (0.3, 2)
                          CUE_x = runif(MIM_runs, 0.3, 2),  #Start w/ (0.3, 2)
                          desorb_x = runif(MIM_runs, 0.3, 3),  #start w/ (0.3, 3)
                          fPHYS_x = runif(MIM_runs, 0.3, 3)  #Start w/ (0.3, 3)
                          )

rand_params$run_num <- seq(1,MIM_runs,1)

# Set number of cores to use
no_cores <- availableCores() - 1
plan(multicore, gc = FALSE, workers = no_cores)

# Run MIMICS!

print(paste0("Starting ", MIM_runs, " runs"))
print(paste0("Start time: ", Sys.time()))

start_time <- Sys.time()
BruteMIM <- rand_params %>% split(1:nrow(rand_params)) %>% future_map(~ MIMbrute(forcing_df = data, rparams = .), .progress=TRUE) %>% bind_rows()
print(paste0("Task time: ", Sys.time() - start_time))

# Release CPU cores
plan(sequential)
nbrOfWorkers()

# Clean up memory
gc()


## Join parameters to MIMICS output table
BruteMIM <- BruteMIM %>% left_join(rand_params)


##########################################
# Save MC output data
##########################################
#write.csv(BruteMIM, paste0("MC_output/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "BruteMIM-", as.character(MIM_runs), ".csv"))
saveRDS(BruteMIM, paste0("MC_output/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "BruteMIM-", as.character(MIM_runs), ".rds"))


# Close R
#quit(save="no")


