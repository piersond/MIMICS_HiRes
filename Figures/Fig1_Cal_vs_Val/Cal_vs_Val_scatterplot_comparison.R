library(ggplot2)
library(Metrics)
library(dplyr)
library(ggpubr)
source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")

setwd("C:/github/MIMICS_HiRes/Figures/Fig1_Cal_vs_Val")


#####################################
# Calibration data plot
#####################################

CAL_data <- read.csv("MIMrun_CAL_bestP.csv")
Default_CAL_data <- read.csv("MIMrun_CAL_default.csv")

## Plot labels
# Calibration data r2, RMSE, resids
r2_test <- cor.test(CAL_data$SOC, CAL_data$MIMSOC)
r_val <- sprintf("%.02f",round(as.numeric(unlist(r2_test ['estimate'])),2))
plot_rmse <- round(rmse(CAL_data$SOC, CAL_data$MIMSOC),2) 

# Default data r2, RMSE, resids
def_r2_test <- cor.test(Default_CAL_data$SOC, Default_CAL_data$MIMSOC)
def_r_val <- sprintf("%.02f",round(as.numeric(unlist(def_r2_test ['estimate'])),2))
def_plot_rmse <- sprintf("%.02f",round(rmse(Default_CAL_data$SOC, Default_CAL_data$MIMSOC),2)) 


#create calibration data plot
calp <-ggplot(CAL_data, aes(x=MIMSOC, y=SOC)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+

  geom_point(data = Default_CAL_data, aes(x=MIMSOC, y=SOC), size=3, alpha=1, pch=16, color="grey60") +
  annotate("text", label = paste0("r = ", def_r_val), x = 11.45, y = 3.5, size = 5, colour = "grey40", parse=F) + 
  annotate("text", label = paste0("RMSE = ", def_plot_rmse), x = 11.79, y = 2.3, size = 5, colour = "grey40") +

  geom_point(size=4, alpha=1, color="#000000", bg="#008B8B", pch=21) +
  annotate("text", label = paste0("r = ", r_val), x = 2.84, y = 13.5, size = 5, colour = "black", parse=F) + 
  annotate("text", label = paste0("RMSE = ", plot_rmse), x = 3.29, y = 12.3, size = 5, colour = "black") +
  
  my_theme +
  ggtitle("Calibration Data (n=51)") +#\nNicholas Patton\n(n=51)") +
  xlim(0,14) +
  ylim(0,14) +
  xlab("MIMICS SOC Stock (kg/m2)") +
  ylab("Field SOC Stock (kg/m2)") +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))
calp


#####################################
# Validation data plot
#####################################

VAL_data <- read.csv("MIMrun_VAL_bestP.csv", as.is=T)
Default_VAL_data <- read.csv("MIMrun_VAL_default.csv")

## Plot labels
# Validation r2, RMSE, resids
VAL_r2_test <- cor.test(VAL_data$SOC_avg, VAL_data$MIMSOC)
VAL_r_val <- round(as.numeric(unlist(VAL_r2_test ['estimate'])),2)
VAL_plot_rmse <- round(rmse(VAL_data$SOC_avg, VAL_data$MIMSOC),2) 

# Validation r2, RMSE, resids
def_VAL_r2_test <- cor.test(Default_VAL_data$SOC_avg, Default_VAL_data$MIMSOC)
def_VAL_r_val <- round(as.numeric(unlist(def_VAL_r2_test ['estimate'])),2)
def_VAL_plot_rmse <- round(rmse(Default_VAL_data$SOC_avg, Default_VAL_data$MIMSOC),2) 



#create plot
valp <- ggplot(VAL_data, aes(x=MIMSOC, y=SOC_avg)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  
  geom_point(data = Default_VAL_data, aes(x=MIMSOC, y=SOC_avg), size=3, alpha=1, pch=15, color="grey60") +
  annotate("text", label = paste0("r = ", def_VAL_r_val), x = 11.45, y = 3.5, size = 5, colour = "grey40", parse=F) + 
  annotate("text", label = paste0("RMSE = ", def_VAL_plot_rmse), x = 11.79, y = 2.3, size = 5, colour = "grey40") +
  
  geom_point(size=4, alpha=1, color="#000000", bg="#FF4500", pch=22) +
  annotate("text", label = paste0("r = ", VAL_r_val), x = 2.84, y = 13.5, size = 5, colour = "black", parse=F) + 
  annotate("text", label = paste0("RMSE = ", VAL_plot_rmse), x = 3.29, y = 12.3, size = 5, colour = "black") +
  
  my_theme +
  ggtitle("Validation Data (n=38)") +#\nRyan Will, Mark Seyfried & Anna Radke\n(n=38)") +
  xlim(0,14) +
  ylim(0,14) +
  xlab("MIMICS SOC Stock (kg/m2)") +
  ylab("Field SOC Stock (kg/m2)") +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))
valp

#######################################
# Save a two panel plot
#######################################

png(file="Calvs_Defvs_Val_plot.png", width=3000, height=6000, units="px", res=600)

ggarrange(calp, valp, 
          #labels = c("CALIBRATION", "VALIDATION"),
          ncol = 1, nrow = 2,
          common.legend = TRUE,
          legend="bottom")
dev.off()
