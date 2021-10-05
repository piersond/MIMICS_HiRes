library(ggplot2)
library(dplyr)
library(ggpubr)

source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")
my_theme <- my_theme + theme(plot.margin = unit(c(1, 1.5, 1, 1), "lines"))


setwd("C:/github/MIMICS_HiRes/Figures")

# Load data
CAL_data <- read.csv("Fig1_Cal_vs_Val/MIMrun_CAL_bestP.csv")
VAL_data <- read.csv("Fig1_Cal_vs_Val/MIMrun_VAL_bestP.csv")

CAL_data$set <- "CAL"
VAL_data$set <- "VAL"

ALL_data <- rbind(CAL_data %>% select(set, TSOI, CLAY, pGPP, lig_N, MIMSOC, MIMLIT, MIMMIC, SOMpTOv, SOMp, SOMc, SOMa, MICr, MICK), 
                  VAL_data %>% select(set, TSOI, CLAY, pGPP, lig_N, MIMSOC, MIMLIT, MIMMIC, SOMpTOv, SOMp, SOMc, SOMa, MICr, MICK))


# MCMC bound plots
##########################

### MIMSOC/SOC
p1 <- ggplot(ALL_data, aes(x=MIMMIC, y=MIMSOC)) + geom_point(aes(shape=set, color=set, fill=set), alpha=0.7) +
  my_theme + theme(legend.position = "right") +
  scale_shape_manual(values=c(21, 22), labels=c("Calibration", "Validation"), name="") +
  scale_fill_manual(values = c("#008B8B", "#FF4500"), labels=c("Calibration", "Validation"), name="") +
  scale_color_manual(values = c("#008B8B", "#FF4500"), labels=c("Calibration", "Validation"), name="") +
  ylab("Soil Organic C (kg/m2)") +
  xlab("Soil Microbial C (kg/m2)") +
  geom_abline(intercept = 0, slope = 100, linetype = "dashed") +
  geom_abline(intercept = 0, slope = 500, linetype = "dashed") 

### MIMLIT/SOC
p2 <- ggplot(ALL_data, aes(x=MIMLIT, y=MIMSOC)) + geom_point(aes(shape=set, color=set, fill=set), alpha=0.7) +
  my_theme + theme(legend.position = "right") +
  scale_shape_manual(values=c(21, 22), labels=c("Calibration", "Validation"), name="") +
  scale_fill_manual(values = c("#008B8B", "#FF4500"), labels=c("Calibration", "Validation"), name="") +
  scale_color_manual(values = c("#008B8B", "#FF4500"), labels=c("Calibration", "Validation"), name="") +
  ylab("Soil Organic C (kg/m2)") +
  xlab("Litter C (kg/m2)") +
  geom_abline(intercept = 0, slope = 5, linetype = "dashed") +
  geom_abline(intercept = 0, slope = 40, linetype = "dashed") +
  scale_x_continuous(limits=c(0,1.5), expand = c(0, 0)) + 
  scale_y_continuous(limits=c(0,12), expand = c(0, 0))

### MIM_CO
p3 <- ggplot(ALL_data, aes(x=MIMMIC, y=MICK)) + geom_point(aes(fill="MICK", color="MICK"), alpha=0.7) +
  geom_point(aes(y=MICr, fill="MICr", color="MICr"), alpha=0.7) +
  my_theme + theme(legend.position = "right") +
  scale_fill_manual(values = c("#00308F", "#C6011F"), labels=c("MIC-K", "MIC-r"), name="") +
  scale_color_manual(values = c("#00308F", "#C6011F"), labels=c("MIC-K", "MIC-r"), name="") +
  xlab("Total Microbial C (kg/m2)") +
  ylab("Microbial C (kg/m2)") +
  geom_abline(intercept = 0, slope = 0.9, linetype = "dashed") + #+
  geom_abline(intercept = 0, slope = 0.1, linetype = "dashed")# +
  #scale_x_continuous(limits=c(0,0.012), expand = c(0, 0)) + 
  #scale_y_continuous(limits=c(0,0.08), expand = c(0, 0))
p3

#########################
# Save panel plot
#########################
png(file="Fig2_QA_plots/MIM_QA_plots1.png", width=3000, height=1000, units="px", res=300)

ggarrange(p1, p2, p3, 
          ncol = 3, nrow = 1,
          common.legend = FALSE,
          widths = c(1, 1, 1),
          legend="bottom")
dev.off()




#ggplot(ALL_data, aes(x=SOMp, y=MIMMIC, color=CLAY)) + geom_point() 
#ggplot(ALL_data, aes(x=SOMp, y=MIMSOC, color=CLAY)) + geom_point() 
#ggplot(ALL_data, aes(x=CLAY, y=SOMp, color=TSOI)) + geom_point() 



### SOMp metric
b1 <- ggplot(ALL_data, aes(x=set, y=SOMpTOv, color=set)) + my_theme +
  geom_boxplot() + geom_point(alpha=0.3) +
  scale_color_manual(values = c("#008B8B", "#FF4500"), name="") +
  xlab("")+
  ylab("fSOMp Turnover (kg C/ m2/ yr)")

b2 <- ggplot(ALL_data, aes(x=set, y=SOMp/MIMSOC, color=set)) + my_theme +
  geom_boxplot() + geom_point(alpha=0.3) +
  scale_color_manual(values = c("#008B8B", "#FF4500"), name="") +
  xlab("")+
  ylab("fSOMp")

# ggplot(ALL_data, aes(x=set, y=MICr/MICK, color=set)) + my_theme +
#   geom_boxplot() + geom_point(alpha=0.3) +
#   xlab("")+
#   ylab("MICr/MICk")

# ggplot(ALL_data, aes(x=set, y=SOMc/MIMSOC, color=set)) + my_theme +
#   geom_boxplot() + geom_point(alpha=0.3) +
#   xlab("")+
#   ylab("fSOMc")


#########################
# Save panel plot
#########################
png(file="Fig2_QA_plots/MIM_QA_plots2.png", width=3000, height=2500, units="px", res=300)

ggarrange(b1, p1, p2, b2, p3,
          ncol = 3, nrow = 2,
          common.legend = FALSE,
          widths = c(0.5, 1, 1),
          legend="bottom")
dev.off()
