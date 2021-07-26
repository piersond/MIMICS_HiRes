library(ggplot2)
library(Metrics)
library(dplyr)
library(ggpubr)
source("plot_theme.R")

setwd("C:/local_temp/ISU_HPC/072221")

HPCdata_raw <- readRDS("MC_output/20210723_054915_BruteMIM-1e+05.rds")
HPCdata <- HPCdata_raw
#HPCdata <- BruteMIM1k

#Filter data
#HPCdata <- HPCdata %>% filter(sample_col == "Nicholas R. Patton")


#Remove outliers
#HPCdata <- HPCdata %>% filter(Site != "S3")
#HPCdata <- HPCdata %>% filter(Site != "S25")
#HPCdata <- HPCdata %>% filter(Site != "S43")
#HPCdata <- HPCdata %>% filter(Site != "S48")
HPCdata <- HPCdata %>% filter(Site != "S71")
HPCdata <- HPCdata %>% filter(Site != "S75")



# Calc RMSE for all runs
stats <- HPCdata %>% group_by(run_num) %>% summarize(RMSE = rmse(SOC, MIMSOC),
                                                     MICtoSOC = mean(MIMMIC/MIMSOC),
                                                     LITtoSOC = mean(MIMLIT/MIMSOC))
stats$RMSE <- round(stats$RMSE,3)


HPCdata <- HPCdata %>% left_join(stats)

### Filter by MIMMIC and LIT
HPCdata <- HPCdata %>% filter(MICtoSOC < 0.1) %>%
  filter(MICtoSOC > 0.0075) %>%
  filter(LITtoSOC > 0.03)


###Filter for runs with RMSE < X
HPCdata <- HPCdata %>% filter(RMSE < 1.9)

#Calc correlations
corr_data <- data.frame(run_num=NA, r2 = NA)

for(i in 1:length(unique(HPCdata$run_num))) {
  df <- HPCdata %>% filter(run_num == unique(HPCdata$run_num)[i])
  r2_test <- cor.test(df$SOC, df$MIMSOC)
  r_val <- round(as.numeric(unlist(r2_test ['estimate'])),3)
  corr_data <- rbind(corr_data, data.frame(run_num=unique(HPCdata$run_num)[i], r2 = r_val))
}

HPCdata <- HPCdata %>% left_join(corr_data)




### Based on stats, plot data for specific run number
run_to_plot <- 38489

#Select run data for plot
plot_data <- HPCdata %>% filter(run_num == run_to_plot)

#Remove outliers
#plot_data <- plot_data %>% filter(Site != "S3")
#plot_data <- plot_data %>% filter(Site != "S25")
#plot_data <- plot_data %>% filter(Site != "S43")
#plot_data <- plot_data %>% filter(Site != "S48")
plot_data <- plot_data %>% filter(Site != "S71")
plot_data <- plot_data %>% filter(Site != "S75")


## Plot labels
# r2, RMSE, resids
r2_test <- cor.test(plot_data$SOC, plot_data$MIMSOC)
r_val <- round(as.numeric(unlist(r2_test ['estimate'])),2)

plot_rmse <- round(rmse(plot_data$SOC, plot_data$MIMSOC),2) 

plot_data$resid <- plot_data$SOC-plot_data$MIMSOC

lb1 <- paste("R^2 == ", r_val)

#create plot
calp <-ggplot(plot_data, aes(x=MIMSOC, y=SOC, color=pGPP+400)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=0.8) + 
  annotate("text", label = lb1, x = 1.5, y = 10.5, size = 6, colour = "black", parse=T) + 
  annotate("text", label = paste0("RMSE = ", plot_rmse), x = 1.5, y = 9.4, size = 6, colour = "black") +
  annotate("text", label = paste0("MC run# ", run_to_plot), x = 13, y = 0, size = 4, colour = "black") +
  my_theme +
  ggtitle("Calibration Data\nNicholas Patton\n(n=51)") +
  xlim(0,14) +
  ylim(0,14) +
  xlab("MIMICS SOC Stock (kg/m2)") +
  ylab("Field SOC Stock (kg/m2)") +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))

#write.csv(plot_data, "MIMout_pset38489_072321.csv")

#############################################################################
### Run parameter set against calibration dataset
#############################################################################
source("MIMICS_ftn2_wjitter.R")

#Pick MC run to use parameter values from
run_to_val <- 38489

run_params <- HPCdata %>% filter(run_num == run_to_val) %>% 
                          select(Vslope_x, Vint_x, Kslope_x, Kint_x, CUE_x, Tau_x, desorb_x, fPHYS_x) %>%
                          unique()

#set parameter values (defualt value * param-set multiplier)
Vslope = rep(0.063, 6) * run_params$Vslope_x
Vint = rep(5.47, 6) * run_params$Vint_x
Kslope = rep(c(0.025, 0.035, 0.025),2) * run_params$Kslope_x
Kint = rep(3.19, 6) * run_params$Kint_x
CUE = c(0.55, 0.25, 0.75, 0.35) * run_params$CUE_x
Tau_MULT = run_params$Tau_x
desorb_MULT = run_params$desorb_x
fPHYS_MULT = run_params$fPHYS_x

### full dataset run
#setwd("C:/local_temp/ISU_HPC/072221")
VALdata <- read.csv("RC_RyanWill_SOCstocks_avg_VALDATA.csv", as.is=T)

#Filter Validation datasets
#VALdata <- VALdata %>% filter(sample_col == "Ryan Will")
#VALdata <- VALdata %>% filter(sample_col != "Anna Radke")

#Outlier removal
VALdata <- VALdata %>% filter(Site != "S139")
VALdata <- VALdata %>% filter(Site != "S140")

#VALdata <- VALdata %>% filter(Site != "S56")
#VALdata <- VALdata %>% filter(Site != "S20")


MIMrun <- VALdata %>% split(1:nrow(VALdata)) %>% map(~ MIMICS1(df=.)) %>% bind_rows()
MIMrun <- VALdata %>% cbind(MIMrun %>% select(-Site, -TSOI))

VALplot_data <- MIMrun

## Plot labels
# r2, RMSE, resids
VAL_r2_test <- cor.test(VALplot_data$SOC_avg, VALplot_data$MIMSOC)
VAL_r_val <- round(as.numeric(unlist(VAL_r2_test ['estimate'])),2)

VAL_plot_rmse <- round(rmse(VALplot_data$SOC_avg, VALplot_data$MIMSOC),2) 

VALplot_data$resid <- VALplot_data$SOC-VALplot_data$MIMSOC

lb2 <- paste("R^2 == ", VAL_r_val)

#create plot
valp <- ggplot(VALplot_data, aes(x=MIMSOC, y=SOC_avg, color=pGPP+400)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=0.8) + 
  #geom_text(aes(label=paste0(RWGROUP, " - ", Site)),hjust=-0.2, vjust=0.2) +
  annotate("text", label = lb2, x = 1.5, y = 10.5, size = 6, colour = "black", parse=T) + 
  annotate("text", label = paste0("RMSE = ", VAL_plot_rmse), x = 1.5, y = 9.4, size = 6, colour = "black") +
  annotate("text", label = paste0("MC run# ", run_to_val), x = 13, y = 0, size = 4, colour = "black") +
  my_theme +
  ggtitle("Validation Data\nRyan Will, Mark Seyfried & Anna Radke\n(n=38)") +
  xlim(0,14) +
  ylim(0,14) +
  xlab("MIMICS SOC Stock (kg/m2)") +
  ylab("Field SOC Stock (kg/m2)") +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))


#Make a two panel plot
ggarrange(calp, valp, 
          #labels = c("CALIBRATION", "VALIDATION"),
          ncol = 2, nrow = 1,
          common.legend = TRUE,
          legend="bottom")

