
## Set working drive
# FYI: Packages are loaded in MIMICS_base_ftn.R

# For local run
setwd("C:/github/MIMICS_HiRes")

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

########################################
# Load MIMICS data and ftns from Brute Forcing script
########################################
source("MIMICS_ftns/MIMICS_repeat_base.R")


########################################
# Set allowable min/max range for each MIMICS parameter
## Values are multipliers of default parameters in MIMICS sandbox
########################################

p_rng <- data.frame(Parameter = c("Vslope", "Vint", "Kslope", "Kint", "Tau", "CUE", "desorb", "fPHYS", "VMAX", "KM"),
                    P_min = c(0.5, 0.5, 0.5, 0.5, 0.1, 0.5, 0.005, 0.1, 0.25, 0.25),
                    P_max = c(4, 4, 4, 4, 2, 2, 0.4, 2, 4, 4))

########################################
# ### Allow multi-core use (not sure this is helpful for a loop)
# Set number of cores to use
########################################

# Set number of CPU cores to use
nbr_cores <- detectCores(all.tests = FALSE, logical = TRUE)-1

plan(multicore, gc = FALSE, workers = nbr_cores)

# Begin track time required
start_time <- Sys.time()

########################################
### Run MCMC
################################

#set initial loop values (aka initial piors)
curr_p <- data.frame(Vslope_x = 1,   
                     Vint_x = 1,
                     Kslope_x = 1,
                     Kint_x = 1,
                     Tau_x = 1,
                     CUE_x = 1,
                     desorb_x = 1,
                     fPHYS_x = 1,
                     VMAX_x=1,
                     KM_x=1,
                     run_num=NA)

#Set number of iterations (3 trials are nested within each run)
MIM_runs <- 1000
MIM_MC  <- NULL

# Send progress statement to console
print(paste0("Running ", as.character(MIM_runs), " MC iterations"))


#Run MCMC loop
for(i in 1:MIM_runs) {

  print(paste0("Running proposal set #", as.character(i)))
  
  #Set new parameter value
  test_p <- curr_p
  
  #Get random parameters to test, in groups
  test_p[1,1] <- 1 #runif(1, p_rng[1,2], p_rng[1,3]) #Vslope
  test_p[1,2] <- 1 #runif(1, p_rng[2,2], p_rng[2,3]) #Vint
  test_p[1,3] <- 1 #runif(1, p_rng[3,2], p_rng[3,3]) #Kslope
  test_p[1,4] <- 1 #runif(1, p_rng[4,2], p_rng[4,3]) #Kint
  test_p[1,5] <- 1 #runif(1, p_rng[5,2], p_rng[5,3]) #Tau
  test_p[1,6] <- 1 #runif(1, p_rng[6,2], p_rng[6,3]) #CUE 
  test_p[1,7] <- 1 #runif(1, p_rng[7,2], p_rng[7,3]) #desorb
  test_p[1,8] <- 1 #runif(1, p_rng[8,2], p_rng[8,3]) #fPHYS
  test_p[1,9] <- runif(1, p_rng[9,2], p_rng[9,3]) #VMAX
  test_p[1,10] <- runif(1, p_rng[10,2], p_rng[10,3]) #KM
  
  #Run MIMICS ftn with test parameters
  MIMout <- MIMrepeat(forcing_df = ex_data, rparams = test_p)
  
  MIMout_all <- cbind(test_p[1, 9:10], MIMout)
  
  # Export MCMC data
  MIM_MC <- rbind(MIM_MC, MIMout_all)
}

#Print time required
Sys.time() - start_time

# Release CPU cores
plan(sequential)
nbrOfWorkers()
gc()

### QUICK DATA CHECK
MIM_MC_SMRY <- MIM_MC %>% filter(DAY == 200)
MIM_MC_SMRY$CO2_frac_tot <- round(rowSums(MIM_MC_SMRY[,12:13])/rowSums(MIM_MC_SMRY[,5:13]), 4)


plot(MIM_MC_SMRY$VMAX, MIM_MC_SMRY$CO2_frac_tot)

