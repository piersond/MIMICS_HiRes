
# For local run
setwd("C:/github/MIMICS_HiRes")

source("MIMICS_ftns/MIMICS_base_ftn.R")

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
## Values are multipliers of default parameters in MIMICS sandbox
########################################

p_rng <- data.frame(Parameter = c("Vslope", "Vint", "Kslope", "Kint", "Tau", "CUE", "desorb", "fPHYS"),
                    P_min = c(0.5, 0.5, 0.5, 0.5, 0.1, 0.5, 0.005, 0.1),
                    P_max = c(4, 4, 4, 4, 2, 2, 0.4, 2))

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
                       desorb_x=0.17,
                       fPHYS_x=0.22,
                       slope=0,
                       r2=0,
                       RMSE=3.5,
                       MICpropSOC=0,
                       LITpropSOC=0,
                       MIM_CO_Avg=0,
                       SOMpTOvAvg=0,
                       improve=1)

########################################
# ### Allow multi-core use (not sure this is helpful for a loop)
# Set number of cores to use
########################################

# Set number of CPU cores to use
nbr_cores <- detectCores(all.tests = FALSE, logical = TRUE)-2

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
                     desorb_x = 0.17,
                     fPHYS_x = 0.22,
                     run_num=NA)

# Set initial cost value (RMSE value to improve from)
curr_cost <- 3.5 #RMSE value to improve upon

#Set trackers
iters_wo_improve = 0

#Set number of iterations for each parameter proposal 
MIM_runs <- 30 # Number of MCMC iterations to run, each with 8 parameter proposals
# Min ~20, max ~100, depending on how long you want to run the MCMC

# Send progress statement to console
print(paste0("Running ", as.character(MIM_runs), " MCMC iterations"))

###DEBUG

#Run MCMC loop
for(i in 1:MIM_runs) {
  
  print(paste0("Running proposal set #", as.character(i)))
  
  for(j in 1:8) {
    
    print(paste0("  Trial p", as.character(j), "..."))
    
    #Set new parameter value
    test_p <- curr_p
    
    #Get random parameters to test, in groups
    if(j == 1) {test_p[1,1] <- runif(1, p_rng[1,2], p_rng[1,3])} #Vslope
    if(j == 2) {test_p[1,2] <- runif(1, p_rng[2,2], p_rng[2,3])} #Vint
    if(j == 3) {test_p[1,3] <- runif(1, p_rng[3,2], p_rng[3,3])} #Kslope
    if(j == 4) {test_p[1,4] <- runif(1, p_rng[4,2], p_rng[4,3])} #Kint
    if(j == 5) {test_p[1,5] <- runif(1, p_rng[5,2], p_rng[5,3])} #Tau
    if(j == 6) {test_p[1,6] <- runif(1, p_rng[6,2], p_rng[6,3])} #CUE 
    if(j == 7) {test_p[1,7] <- runif(1, p_rng[7,2], p_rng[7,3])} #desorb
    if(j == 8) {test_p[1,8] <- runif(1, p_rng[8,2], p_rng[8,3])} #fPHYS
    
    
    #Run MIMICS ftn with test parameters
    MIMout <- MIMrepeat(forcing_df = data, rparams = test_p)
    
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
                           r=MIMout$r,
                           RMSE=MIMout$RMSE,
                           MICpropSOC=MIMout$MICpropSOC,
                           LITpropSOC=MIMout$LITpropSOC,
                           MIM_CO_Avg=MIMout$MIM_CO_mn,
                           SOMpTOvAvg=MIMout$SOMpTO,
                           improve=0)
    
    #Make decision based on cost outcome
    if(MIMout$RMSE < curr_cost &&
       MIMout$MICpropSOC > 0.01 &&
       MIMout$MICpropSOC < 0.08 &&
       MIMout$LITpropSOC > 0.05 &&
       MIMout$LITpropSOC < 0.50 &&
       MIMout$MIM_CO_mn > 0.01 &&
       MIMout$MIM_CO_mn < 100 &&
       MIMout$SOMpTO > 50 &&
       MIMout$SOMpTO < 1000) 
    {
      
      #Update targets
      curr_p <- test_p
      curr_cost <- MIMout$RMSE
      iter_out$improve <- 1
      iters_wo_improve <- 0
      
      # Print to console
      print("-----------------------------")
      print(paste0("IMPROVED RMSE TO ", round(MIMout$RMSE,2)))
      print(paste0("  slope =  ", round(MIMout$slope,2)))
      print(paste0("  r =  ", round(MIMout$r,2)))
      print("-----------------------------")
      
      ## Walk proposal distributions 
      # ONLY USEFUL IF COMPUTATIONAL POWER IS LIMITED, comment out if not
      #######################################################################
      # Set walk rate
      walk_rt = 2 # Set the parameter range min to be the current value divided by
      # this number, and the max to the current value multiplied
      # by this number
      
      # New proposal distributions
      ####################################
      p_rng[1,2] <- iter_out$Vslope_x / walk_rt # V_slope min
      p_rng[1,3] <- iter_out$Vslope_x +(iter_out$Vslope_x-(iter_out$Vslope_x/walk_rt)) # V_slope max
      
      p_rng[2,2] <- iter_out$Vint_x / walk_rt # V_int min
      p_rng[2,3] <- iter_out$Vint_x +(iter_out$Vint_x-(iter_out$Vint_x/walk_rt)) # V_int max
      
      p_rng[3,2] <- iter_out$Kslope_x / walk_rt # K_slope min
      p_rng[3,3] <- iter_out$Kslope_x +(iter_out$Kslope_x-(iter_out$Kslope_x/walk_rt)) # K_slope max
      
      p_rng[3,2] <- iter_out$Kint_x / walk_rt # K_int min
      p_rng[3,3] <- iter_out$Kint_x +(iter_out$Kint_x-(iter_out$Kint_x/walk_rt)) # K_int max
      
      p_rng[5,2] <- iter_out$Tau_x / walk_rt # Tau min
      p_rng[5,3] <- iter_out$Tau_x +(iter_out$Tau_x-(iter_out$Tau_x/walk_rt)) # Tau max
      
      p_rng[6,2] <- iter_out$CUE_x / walk_rt # CUE min
      p_rng[6,3] <- iter_out$CUE_x +(iter_out$CUE_x-(iter_out$CUE_x/walk_rt)) # CUE max
      
      p_rng[7,2] <- iter_out$desorb_x / walk_rt # desorb min
      p_rng[7,3] <- iter_out$desorb_x +(iter_out$desorb_x-(iter_out$desorb_x/walk_rt)) # desorb max
      
      p_rng[8,2] <- iter_out$fPHYS_x / walk_rt # fPHYS min
      p_rng[8,3] <- iter_out$fPHYS_x +(iter_out$fPHYS_x-(iter_out$fPHYS_x/walk_rt)) # fPHYS max
      
    } else {
      #update tracker for number of iterations without improvement
      iters_wo_improve <- iters_wo_improve + 1
      
      # Slowly tighten or expand distributions when, over many iterations,
      # no RMSE improvement is found
      #######################################################
      # Would such a process be an improvement?
      
      
      # Exit if fail to improve after n rounds...
      
    }
    
    # Export MCMC data
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
library(ggplot2)
library(gridExtra)
library(ggpubr)


pRMSE <- ggplot(MCMC_out, aes(x=iter, y=RMSE)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + ylim(0, 5) + theme_minimal() +theme(legend.position = "none")
pr <- ggplot(MCMC_out, aes(x=iter, y=r)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + ylim(0.1, 1) + theme_minimal() +theme(legend.position = "none")
pTau_x <- ggplot(MCMC_out, aes(x=iter, y=Tau_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pCUE_x <-ggplot(MCMC_out, aes(x=iter, y=CUE_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pDesorb_x <- ggplot(MCMC_out, aes(x=iter, y=desorb_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pFPHYS_x <- ggplot(MCMC_out, aes(x=iter, y=fPHYS_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pVslope_x <- ggplot(MCMC_out, aes(x=iter, y=Vslope_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pVint_x <- ggplot(MCMC_out, aes(x=iter, y=Vint_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pKslope_x <- ggplot(MCMC_out, aes(x=iter, y=Kslope_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pKint_x <- ggplot(MCMC_out, aes(x=iter, y=Kint_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")

walk_plot <- grid.arrange(pRMSE, pr2, pTau_x, pCUE_x, pDesorb_x, pFPHYS_x, pVslope_x, pVint_x, pKslope_x, ncol = 2)

#save plot
ggsave(file=paste0("MCMC/Output/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "MIM_MCMC_pCombos-", as.character(MIM_runs),"_walk_plot", ".jpeg"), 
       plot=walk_plot,
       width=10,
       height=8)

#######################
# Export MCMC run data
#######################
write.csv(MCMC_out, paste0("MCMC/Output/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "MIM_MCMC_pCombos-", as.character(MIM_runs), ".csv"))

