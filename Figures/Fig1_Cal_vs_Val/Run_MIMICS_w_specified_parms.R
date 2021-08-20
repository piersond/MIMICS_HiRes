# Generate MIMICS results

setwd("C:/github/MIMICS_HiRes")

source("MIMICS_ftns/MIMICS_base_ftn.R")

# Bring in parameters
parm_df <- read.csv("MC/Output/best_MC_Pcombo_20210819.csv")

# Set parameter values
Vslope = Vslope * parm_df$Vslope_x
Vint = Vint * parm_df$Vint_x
Kslope = Kslope * parm_df$Kslope_x
Kint = Kint * parm_df$Kint_x
CUE = CUE * parm_df$CUE_x
Tau_MULT = parm_df$Tau_x
desorb_MULT = parm_df$desorb_x
fPHYS_MULT = parm_df$fPHYS_x

##############################################
# Full forcing dataset run
##############################################
data <- read.csv("RCrk_Modelling_Data/RCrk_SOC_validation.csv", as.is=T)

MIMrun <- data %>% split(1:nrow(data)) %>% map(~ MIMICS1(df=.)) %>% bind_rows()
MIMrun <- data %>% cbind(MIMrun %>% select(-Site, -TSOI))

# Save MIMICS run data
#write.csv(MIMrun, "Figures/Fig1_Cal_vs_Val/MIMrun_VAL_bestP.csv")

