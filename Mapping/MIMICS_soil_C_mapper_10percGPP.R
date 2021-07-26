library(tidyverse)
library(furrr)
library(purrr)

#setwd("C:/local_temp/ISU_HPC/052321_manMAP")

# Bring in MIMICS ftn
source("MIMICS_ftn2_wjitter.R")

# Set forcing data

frc_data <- readRDS("RC_map_forcing_data.rds")

#Scenario
#frc_data$TSOI <- frc_data$TSOI+1
frc_data$pGPP <- frc_data$pGPP + (abs(frc_data$pGPP)*0.1)

#DEBUG
#frc_data <- frc_data[1:10000,]


########################################
# Apply rate curve brute force multipliers
#######################################

pset <- readRDS("MIMICS_MC_parms_for_maps.rds")
pset$pID <- seq(1,nrow(pset),1)

#Manually select parameter set
#Completed 1, 
i = 2

#Set parameter defaults
Vslope_d  = rep(0.063, 6)
Vint_d    = rep(5.47, 6)
Kslope_d  = rep(c(0.025, 0.035, 0.025),2)
Kint_d    = rep(3.19, 6)
CUE_d     = c(0.55, 0.25, 0.75, 0.35)
Tau_MULT_d = 1
Tau_MULT_d = 1
desorb_MULT_d = 1
fPHYS_MULT_d = 1

#Set parameters
Vslope = Vslope_d * pset$Vslope_x[i]
Vint = Vint_d * pset$Vint_x[i]
Kslope = Kslope_d * pset$Kslope_x[i]
Kint = Kint_d * pset$Kint_x[i]
CUE = CUE_d * pset$CUE_x[i]
Tau_MULT = pset$Tau_x[i]
desorb_MULT = pset$desorb_x[i]
fPHYS_MULT = pset$fPHYS_x[i]


########################################
# Set up MIMICS run
#######################################

# Set number of cores to use
#no_cores <- availableCores() - 2
no_cores <- 5
plan(multicore, gc = TRUE, workers = no_cores)

# Run MIMICS! (track time required)
start_time <- Sys.time()
MIMdata <- frc_data %>% split(1:nrow(frc_data)) %>% future_map(MIMICS1, .progress=TRUE) %>% bind_rows() 
print(Sys.time() - start_time)

# Release CPU cores
plan(sequential)
nbrOfWorkers()

# Clean up memory
gc()

#Write output
saveRDS(MIMdata, paste0("MAP_output/MIMICS_RC_MAP_pset-2_10percGPP.rds"))


#################################################
# Build raster from MIMdata
#################################################

# join to raster frac data frame
#MIMdata_full <- merge(raster_frc, MIMdata, by.x = "Site",
#      by.y = "Site", all.x = TRUE, all.y = FALSE)

#save map dataframe
#write.csv(MIMdata_full, "C:/local_temp/MIMICS_RC_MAP_data.csv")


# # copy raster construct and meta from estGPP, refill with MIMSOC values
# estSOC <- setValues(estGPP, MIMdata_full$MIMSOC)
# estMIMLIT <- setValues(estGPP, MIMdata_full$MIMLIT)
# estMIMMIC <- setValues(estGPP, MIMdata_full$MIMMIC)
# estLITm <- setValues(estGPP, MIMdata_full$LITm)
# estLITs <- setValues(estGPP, MIMdata_full$LITs)
# estMICr <- setValues(estGPP, MIMdata_full$MICr)
# estMICk <- setValues(estGPP, MIMdata_full$MICK)
# estMIMCO <- setValues(estGPP, MIMdata_full$MIM_CO)
# estSOMa <- setValues(estGPP, MIMdata_full$SOMa)
# estSOMc <- setValues(estGPP, MIMdata_full$SOMc)
# estSOMp <- setValues(estGPP, MIMdata_full$SOMp)

#show map
# plot(estSOC)
# plot(estMIMLIT)
# plot(estMIMMIC)
# plot(estLITm)
# plot(estLITs)
# plot(estMICr)
# plot(estMICk)
# plot(estMIMCO)
# plot(estSOMa)
# plot(estSOMc)
# plot(estSOMp)

#MIMICS input maps
# plot(estGPP)
# plot(estTSOI)
# plot(estCLAY)
# plot(estLIGN)

#save raster
# writeRaster(estSOC,'C:/local_temp/MIMICS_raster_dump/estSOC_040621.tif',options=c('TFW=YES'))
# writeRaster(estMIMLIT,'C:/local_temp/MIMICS_raster_dump/estMIMLIT_040621.tif',options=c('TFW=YES'))
# writeRaster(estMIMMIC,'C:/local_temp/MIMICS_raster_dump/estMIMMIC_040621.tif',options=c('TFW=YES'))
# writeRaster(estLITm,'C:/local_temp/MIMICS_raster_dump/estLITm_040621.tif',options=c('TFW=YES'))
# writeRaster(estLITs,'C:/local_temp/MIMICS_raster_dump/estLITs_040621.tif',options=c('TFW=YES'))
# writeRaster(estMICr,'C:/local_temp/MIMICS_raster_dump/estMICr_040621.tif',options=c('TFW=YES'))
# writeRaster(estMICk,'C:/local_temp/MIMICS_raster_dump/estMICK_040621.tif',options=c('TFW=YES'))
# writeRaster(estMIMCO,'C:/local_temp/MIMICS_raster_dump/estMIMCO_040621.tif',options=c('TFW=YES'))
# writeRaster(estSOMa,'C:/local_temp/MIMICS_raster_dump/estSOMa_040621.tif',options=c('TFW=YES'))
# writeRaster(estSOMc,'C:/local_temp/MIMICS_raster_dump/estSOMc_040621.tif',options=c('TFW=YES'))
# writeRaster(estSOMp,'C:/local_temp/MIMICS_raster_dump/estSOMp_040621.tif',options=c('TFW=YES'))











