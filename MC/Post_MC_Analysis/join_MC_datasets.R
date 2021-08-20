library(dplyr)

setwd("C:/github/MIMICS_HiRes/MC")

### Load all MCMC output csv files from directory
filenames <- list.files(path="Output/",pattern=".*rds")

## Create list of data frame names without the ".csv" part 
names <-paste0("MC", seq(1,length(filenames)))



### Load all files
for(i in 1:length(names)){
  data_in <- readRDS(paste0("Output/",filenames[i]))
  
  #Change the run numbers to be unique
  ## All datasets need to have same number of runs
  
  #Save original run_num
  data_in$run_num_org <- data_in$run_num
  
  #Set run_num to be sum of all run num
  data_in$run_num <- data_in$run_num + ((i * max(data_in$run_num)) - max(data_in$run_num))
  
  #Add tracker for 
  data_in$ID = paste0("Run ", as.character(i))

  if(i == 1) {
    MC <- data_in  
  } else {
    MC <- rbind(MC, data_in)
  }
}

#Save the compiled MC dataset
saveRDS(MC, paste0("Output/", "MC_runs_compiled_", format(Sys.time(), "%Y%m%d"), ".rds"))


