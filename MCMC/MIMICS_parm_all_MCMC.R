
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
# Create dataframe to store MCMC steps
########################################
MCMC_out <- data.frame(i=0,
                     iter=0,
                     Vslope_x=1,
                     Vint_x=1,
                     Kslope_x=1,
                     Kint_x=1,
                     Tau_x=1,
                     CUE_x=1,
                     desorb_x=1,
                     fPHYS_x=1,
                     VMAX_x=1,
                     KM_x=1,
                     CO2_frac_tot = 0, 
                     cost = 0.5,
                     improve=0)

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

# Set initial cost value (RMSE value to improve from)
curr_cost <- 0.5 #RMSE value to improve upon

#Set trackers
iters_wo_improve = 0

#Set number of iterations (3 trials are nested within each run)
MIM_runs <- 100

# Send progress statement to console
print(paste0("Running ", as.character(MIM_runs), " MCMC iterations"))


#Run MCMC loop
for(i in 1:MIM_runs) {
  
  #DEBUG
  #i <- 1
  
  print(paste0("Running proposal set #", as.character(i)))
  
  #Set new parameter value
  test_p <- curr_p
  
  # Set target for CO2 total # <-- Build on this later
  CO2_tot_target <- 0.1 # <-- Get this from site data spreadsheet later
  
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
  
  #log parameter updates in dataframe
  iter_out <- data.frame(i=i,
                         iter = i, 
                         Vslope_x=test_p[1],
                         Vint_x=test_p[2],
                         Kslope_x=test_p[3],
                         Kint_x=test_p[4],
                         Tau_x=test_p[5],
                         CUE_x=test_p[6],
                         desorb_x=test_p[7],
                         fPHYS_x=test_p[8],
                         VMAX_x=test_p[9],
                         KM_x=test_p[10],
                         
                         #Cost ftn pieces
                         CO2_frac_tot = round(rowSums(MIMout[,10:11])[nrow(MIMout)]/rowSums(MIMout[,3:11])[nrow(MIMout)], 4),
                         cost = abs(round(rowSums(MIMout[,10:11])[nrow(MIMout)]/rowSums(MIMout[,3:11])[nrow(MIMout)], 4) - CO2_tot_target), 
                         
                         improve=0)
  
  #Make decision based on cost outcome

  cost <- abs(iter_out$CO2_frac_tot - CO2_tot_target)
  if(!is.nan(iter_out$CO2_frac_tot) & cost < curr_cost) {
    
    #Update targets
    curr_p <- test_p
    curr_cost <- cost
    iter_out$improve <- 1
    iters_wo_improve <- 0
    
    # Print to console
    print(paste0("MINIMIZED COST TO ", curr_cost))
    
    ## Walk proposal distributions 
    # ONLY USEFUL IF COMPUTATIONAL POWER IS LIMITED, comment out if not
    #######################################################################
    # Set walk rate
    walk_rt = 1.05 + (iters_wo_improve/100) 
    
    # Set max walk rate
    if(walk_rt > 3){
      walk_rt <- 3
    }
    
    # Set the parameter range min to the current value divided by
    # this number, and the max to the current value multiplied
    # by this number
    
    # New proposal distributions
    ####################################
    # p_rng[1,2] <- iter_out$Vslope_x / walk_rt # V_slope min
    # p_rng[1,3] <- iter_out$Vslope_x +(iter_out$Vslope_x-(iter_out$Vslope_x/walk_rt)) # V_slope max
    # 
    # p_rng[2,2] <- iter_out$Vint_x / walk_rt # V_int min
    # p_rng[2,3] <- iter_out$Vint_x +(iter_out$Vint_x-(iter_out$Vint_x/walk_rt)) # V_int max
    # 
    # p_rng[3,2] <- iter_out$Kslope_x / walk_rt # K_slope min
    # p_rng[3,3] <- iter_out$Kslope_x +(iter_out$Kslope_x-(iter_out$Kslope_x/walk_rt)) # K_slope max
    # 
    # p_rng[3,2] <- iter_out$Kint_x / walk_rt # K_int min
    # p_rng[3,3] <- iter_out$Kint_x +(iter_out$Kint_x-(iter_out$Kint_x/walk_rt)) # K_int max
    # 
    # p_rng[5,2] <- iter_out$Tau_x / walk_rt # Tau min
    # p_rng[5,3] <- iter_out$Tau_x +(iter_out$Tau_x-(iter_out$Tau_x/walk_rt)) # Tau max
    # 
    # p_rng[6,2] <- iter_out$CUE_x / walk_rt # CUE min
    # p_rng[6,3] <- iter_out$CUE_x +(iter_out$CUE_x-(iter_out$CUE_x/walk_rt)) # CUE max
    # 
    # p_rng[7,2] <- iter_out$desorb_x / walk_rt # desorb min
    # p_rng[7,3] <- iter_out$desorb_x +(iter_out$desorb_x-(iter_out$desorb_x/walk_rt)) # desorb max
    # 
    # p_rng[8,2] <- iter_out$fPHYS_x / walk_rt # fPHYS min
    # p_rng[8,3] <- iter_out$fPHYS_x +(iter_out$fPHYS_x-(iter_out$fPHYS_x/walk_rt)) # fPHYS max

    p_rng[9,2] <- iter_out$VMAX_x / walk_rt # VMAX min
    p_rng[9,3] <- iter_out$VMAX_x +(iter_out$VMAX_x-(iter_out$VMAX_x/walk_rt)) # VMAX max
    
    print(p_rng[9,2])
    print(p_rng[9,3])
    
    
    p_rng[10,2] <- iter_out$KM_x / walk_rt # KM min
    p_rng[10,3] <- iter_out$KM_x +(iter_out$KM_x-(iter_out$KM_x/walk_rt)) # KM max    
    
    
  } else {
    #update tracker for number of iterations without improvement
    iters_wo_improve <- iters_wo_improve + 1
    
    # Slowly tighten or expand distributions when, over many iterations,
    # no RMSE improvement is found
    #######################################################
    # Would such a process be an improvement?
    
  }
  
  # Export MCMC data
  MCMC_out <- rbind(MCMC_out, iter_out)
    
  
}

#Print time required
Sys.time() - start_time

# Release CPU cores
plan(sequential)
nbrOfWorkers()


#######################
# Plot MCMC walk
#######################
# Set R to show digits to 6 decimal places
options(digits=5)
options(scipen=10000)

pCOST <- ggplot(MCMC_out, aes(x=iter, y=cost)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="dark red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="black", size=3) + theme_minimal() +theme(legend.position = "none") +
          labs(title="TARGET: Total respired CO2 equal to 10% of total C", subtitle=paste0("Final diff from target = ", round(min(MCMC_out$cost), 5)))
pVMAX <- ggplot(MCMC_out, aes(x=iter, y=VMAX_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="dark blue", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="black", size=3) + theme_minimal() +theme(legend.position = "none") +
          labs(title="VMAX multiplier", subtitle=paste0("Solution VMAX multiplier = ", MCMC_out %>% arrange(-improve, cost) %>% select(VMAX_x) %>%  .$VMAX_x[1] %>% unique() %>% round(6)))
pKM <- ggplot(MCMC_out, aes(x=iter, y=KM_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="dark green", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="black", size=3) + theme_minimal() +theme(legend.position = "none") +
          labs(title="KM multiplier", subtitle=paste0("Solution KM multiplier = ", MCMC_out %>% arrange(-improve, cost) %>% select(KM_x) %>%  .$KM_x[1] %>% unique() %>% round(6)))

ggarrange(pCOST, pVMAX, pKM, ncol = 1)


#######################
# Export MCMC run data
#######################
#write.csv(MCMC_out, paste0("MCMC/Output/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "MIM_MCMC_pCombos-", as.character(MIM_runs), ".csv"))

