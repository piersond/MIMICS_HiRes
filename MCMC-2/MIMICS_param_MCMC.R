
## Set working drive
# FYI: Packages are loaded in MIMICS_base_ftn.R

# For local run
#setwd("C:/github/MIMICS_HiRes")

# For ISU HPC run
#setwd('home/derekpierson/MIMICS_HiRes')

########################################
# Load forcing data
########################################
data <- read.csv("RCrk_Modelling_Data/RCrk_SOC_calibration.csv", as.is=T)


########################################
# Load MIMICS data and ftns from Brute Forcing script
########################################
source("MIMICS_ftns/MIMICS_repeat_base.R")


########################################
# Set allowable min/max range for each MIMICS parameter
########################################

p_rng <- data.frame(Parameter = c("Vslope", "Vint", "Kslope", "Kint", "Tau", "CUE", "desorb", "fPHYS"),
                    P_min = c(0.5, 0.5, 0.5, 0.5, 0.1, 0.1, 0.1, 0.1),
                    P_max = c(4, 4, 4, 4, 3, 3, 3, 3))

########################################
# Create dataframe to store MCMC steps
########################################
MCMC_out <- data.frame(i=0,
                     iter=1,
                     Vslope_x=1,
                     Vint_x=1,
                     Kslope_x=1,
                     Kint_x=1,
                     Tau_x=1,
                     CUE_x=1,
                     desorb_x=1,
                     fPHYS_x=1,
                     slope=0,
                     r2=0,
                     RMSE=3.5,
                     MICpropSOC=0,
                     LITpropSOC=0,
                     improve=1)

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

#set initial loop values
curr_p <- data.frame(Vslope_x = 1,   
                     Vint_x = 1,
                     Kslope_x = 1,
                     Kint_x = 1,
                     Tau_x = 1,
                     CUE_x = 1,
                     desorb_x = 1,
                     fPHYS_x = 1)

# Set initial cost value (RMSE value to improve from)
curr_cost <- 3.5

#Set number of iterations (3 trials are nested within each run)
MIM_runs <- 5000

# Send progress statement to console
print(paste0("Running ", as.character(MIM_runs), " MCMC iterations"))

#Run MCMC loop
for(i in 1:MIM_runs) {
  
  print(paste0("Running set #", as.character(i)))
  
  for(j in 1:3) {
    
    #Set new parameter value
    test_p <- curr_p
    
    #Get random parameters to test, in groups
    if(j == 1) {
      test_p[1,1] <- runif(1, p_rng[1,2], p_rng[1,3]) #Vslope
      test_p[1,2] <- runif(1, p_rng[2,2], p_rng[2,3]) #Vint
      test_p[1,3] <- runif(1, p_rng[3,2], p_rng[3,3]) #kslope
      
      #Set Kint based on observed relationship to Vint 
      test_p[1,4] <- test_p[1,2] * 0.57
    }
    
    
    if(j == 2) {
      test_p[1,5] <- runif(1, p_rng[5,2], p_rng[5,3]) #Tau
      test_p[1,6] <- runif(1, p_rng[6,2], p_rng[6,3]) #CUE 
    }
    
    
    if(j == 3) {
      test_p[1,7] <- runif(1, p_rng[7,2], p_rng[7,3]) #desorb
      test_p[1,8] <- runif(1, p_rng[8,2], p_rng[8,3]) #fPHYS
    }
    
    #Run MIMICS ftn with test parameters
    MIMout <- MIMbrute(forcing_df = data, rparams = test_p)
    
    #log parameter updates in dataframe
    iter_out <- data.frame(i=i,
                           iter = ((i-1)*3) + j, 
                           Vslope_x=test_p[1],
                           Vint_x=test_p[2],
                           Kslope_x=test_p[3],
                           Kint_x=test_p[4],
                           Tau_x=test_p[5],
                           CUE_x=test_p[6],
                           desorb_x=test_p[7],
                           fPHYS_x=test_p[8],
                           slope=MIMout$slope,
                           r2=MIMout$r2,
                           RMSE=MIMout$RMSE,
                           MICpropSOC=MIMout$MICpropSOC,
                           LITpropSOC=MIMout$LITpropSOC,
                           improve=0)
    
    
    #Make decision based on cost outcome
    if(MIMout$RMSE < curr_cost &&
       MIMout$MICpropSOC > 0.005 &&
       MIMout$MICpropSOC < 0.08 &&
       MIMout$LITpropSOC > 0.01 &&
       MIMout$LITpropSOC < 0.5) {
      
        curr_p <- test_p
        curr_cost <- MIMout$RMSE
        iter_out$improve <- 1
    }
        
      MCMC_out <- rbind(MCMC_out, iter_out)
    
  }
}

#Print time required
Sys.time() - start_time

# Release CPU cores
plan(sequential)
nbrOfWorkers()


#######################
# Plot MCMC walk
#######################

#pRMSE <- ggplot(MCMC_out, aes(x=iter, y=RMSE)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
#pr2 <- ggplot(MCMC_out, aes(x=iter, y=r2)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
#pTau_x <- ggplot(MCMC_out, aes(x=iter, y=Tau_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
#pCUE_x <-ggplot(MCMC_out, aes(x=iter, y=CUE_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
#pDesorb_x <- ggplot(MCMC_out, aes(x=iter, y=desorb_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
#pFPHYS_x <- ggplot(MCMC_out, aes(x=iter, y=fPHYS_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
#pVslope_x <- ggplot(MCMC_out, aes(x=iter, y=Vslope_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
#pVint_x <- ggplot(MCMC_out, aes(x=iter, y=Vint_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
#pKslope_x <- ggplot(MCMC_out, aes(x=iter, y=Kslope_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
#pKint_x <- ggplot(MCMC_out, aes(x=iter, y=Kint_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")

#grid.arrange(pRMSE, pr2, pTau_x, pCUE_x, pDesorb_x, pFPHYS_x, pVslope_x, pVint_x, pKslope_x, ncol = 2)

#######################
# Export MCMC run data
#######################
write.csv(MCMC_out, paste0("MCMC-2/Output/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "MIM_MCMC_pCombos-", as.character(MIM_runs), ".csv"))

