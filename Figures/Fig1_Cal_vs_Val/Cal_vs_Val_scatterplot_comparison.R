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

## Plot labels
# r2, RMSE, resids
r2_test <- cor.test(CAL_data$SOC, CAL_data$MIMSOC)
r_val <- round(as.numeric(unlist(r2_test ['estimate'])),2)

plot_rmse <- round(rmse(CAL_data$SOC, CAL_data$MIMSOC),2) 

CAL_data$resid <- CAL_data$SOC-CAL_data$MIMSOC

lb1 <- paste("R^2 == ", r_val)

#create calibration data plot
calp <-ggplot(CAL_data, aes(x=MIMSOC, y=SOC)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=0.8, color="#000000", bg="#008B8B", pch=21) +
  annotate("text", label = lb1, x = 2.5, y = 13.5, size = 5, colour = "black", parse=T) + 
  annotate("text", label = paste0("RMSE = ", plot_rmse), x = 3.29, y = 12.3, size = 5, colour = "black") +
  #annotate("text", label = paste0("MC run# ", run_to_plot), x = 13, y = 0, size = 4, colour = "black") +
  my_theme +
  ggtitle("Calibration Data (n=51)") +#\nNicholas Patton\n(n=51)") +
  xlim(0,14) +
  ylim(0,14) +
  xlab("MIMICS SOC Stock (kg/m2)") +
  ylab("Field SOC Stock (kg/m2)") +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))

#####################################
# Validation data plot
#####################################

VAL_data <- read.csv("MIMrun_VAL_bestP.csv", as.is=T)

## Plot labels
# r2, RMSE, resids
VAL_r2_test <- cor.test(VAL_data$SOC_avg, VAL_data$MIMSOC)
VAL_r_val <- round(as.numeric(unlist(VAL_r2_test ['estimate'])),2)

VAL_plot_rmse <- round(rmse(VAL_data$SOC_avg, VAL_data$MIMSOC),2) 

VAL_data$resid <- VAL_data$SOC-VAL_data$MIMSOC

lb2 <- paste("R^2 == ", VAL_r_val)

#create plot
valp <- ggplot(VAL_data, aes(x=MIMSOC, y=SOC_avg)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=0.8, color="#000000", bg="#FF4500", pch=22) + 
  #geom_text(aes(label=paste0(RWGROUP, " - ", Site)),hjust=-0.2, vjust=0.2) +
  annotate("text", label = lb2, x = 2.68, y = 13.5, size = 5, colour = "black", parse=T) + 
  annotate("text", label = paste0("RMSE = ", VAL_plot_rmse), x = 3.29, y = 12.3, size = 5, colour = "black") +
  #annotate("text", label = paste0("MC run# ", run_to_val), x = 13, y = 0, size = 4, colour = "black") +
  my_theme +
  ggtitle("Validation Data (n=38)") +#\nRyan Will, Mark Seyfried & Anna Radke\n(n=38)") +
  xlim(0,14) +
  ylim(0,14) +
  xlab("MIMICS SOC Stock (kg/m2)") +
  ylab("Field SOC Stock (kg/m2)") +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))

#######################################
# Save a two panel plot
#######################################

png(file="Cal_Val_plot.png", width=3000, height=6000, units="px", res=600)

ggarrange(calp, valp, 
          #labels = c("CALIBRATION", "VALIDATION"),
          ncol = 1, nrow = 2,
          common.legend = TRUE,
          legend="bottom")
dev.off()
