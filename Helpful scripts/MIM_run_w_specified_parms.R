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
data <- read.csv("RCrk_Modelling_Data/RCrk_SOC_Cal+Val.csv", as.is=T)

MIMrun <- data %>% split(1:nrow(data)) %>% map(~ MIMICS1(df=.)) %>% bind_rows()
MIMrun <- data %>% cbind(MIMrun %>% select(-Site, -TSOI))

# Save MIMICS run data
#write.csv(MIMrun, "Figures/Fig1_Cal_vs_Val/MIMrun_VAL_bestP.csv")

##############################################
# Plot MIMSOC vs SOC
##############################################

library(ggplot2)
library(Metrics)

plot_data <- MIMrun
colnames(plot_data)[1] <- "Set"

#calc SOMp turnover time
plot_data$desorb_yr <- plot_data$desorb*24*365
plot_data$SOMpTO <- plot_data$SOMp/plot_data$desorb_yr

r2_test <- cor.test(MIMrun$SOC, MIMrun$MIMSOC)
r_val <- round(as.numeric(unlist(r2_test ['estimate'])),2)
lb2 <- paste("R^2 == ", r_val)

rmse <- round(rmse(MIMrun$SOC, MIMrun$MIMSOC),2)

ggplot(plot_data, aes(x=MIMSOC, y=SOC, color=Set)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=0.8) +
  #geom_text(aes(label=paste0(Site)),hjust=-0.2, vjust=0.2) +
  annotate("text", label = lb2, x = 2, y = 8.5, size = 6, colour = "black", parse=T) +
  annotate("text", label = paste0("RMSE = ", rmse), x = 2, y = 7.4, size = 6, colour = "black") +
  ylim(0,10) + xlim(0,10)

