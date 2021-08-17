library(ggplot2)
library(Metrics)
library(dplyr)

setwd("C:/github/MIMICS_HiRes")
source("Figures/plot_theme.R")

####################
# Load MC data
####################
MCdata <- readRDS("MC/Output/MC_MIMICS_data-r5000_20210817_125704_.rds")

##########################################################
# Calculate RMSE and R2 for each run in the MC dataset
##########################################################
# Calc RMSE for all runs
stats <- MCdata %>% group_by(run_num) %>% summarize(RMSE = rmse(SOC, MIMSOC),
                                                     MICtoSOC = mean(MIMMIC/MIMSOC),
                                                     LITtoSOC = mean(MIMLIT/MIMSOC))
stats$RMSE <- round(stats$RMSE,3)

#Calc correlations
# corr_data <- data.frame(run_num=NA, r2 = NA)
# 
# for(i in 1:length(unique(MCdata$run_num))) {
#   df <- MCdata %>% filter(run_num == i)
#   r2_test <- cor.test(df$SOC, df$MIMSOC)
#   r_val <- round(as.numeric(unlist(r2_test ['estimate'])),3)
#   corr_data <- rbind(corr_data, data.frame(run_num=i, r2 = r_val))
# }
# 
# stats <- stats %>% left_join(corr_data)

### Filter by MIMMIC and LIT
best_fit <- stats %>% filter(MICtoSOC < 0.08) %>%
                        filter(MICtoSOC > 0.05) %>%
                        filter(LITtoSOC > 0.01)


#Plot SOC vs MIMSOC
################################################################################
### Based on stats in "best-fit" dataframe, plot data for specific run number
run_to_plot <- 34
################################################################################

plot_data <- MCdata %>% filter(run_num == run_to_plot)

#plot_data <- plot_data %>% filter(Site != "S3")
#plot_data <- plot_data %>% filter(Site != "S25")
#plot_data <- plot_data %>% filter(Site != "S43")
#plot_data <- plot_data %>% filter(Site != "S48")

## Plot stats text
# r2, RMSE, resids
r2_test <- cor.test(plot_data$SOC, plot_data$MIMSOC)
r_val <- round(as.numeric(unlist(r2_test ['estimate'])),2)

plot_rmse <- round(rmse(plot_data$SOC, plot_data$MIMSOC),2) 

plot_data$resid <- plot_data$SOC-plot_data$MIMSOC

lb1 <- paste("R^2 == ", r_val)

#create plot
ggplot(plot_data, aes(x=MIMSOC, y=SOC, color=pGPP+400)) + geom_point(size=3, alpha=1) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  annotate("text", label = lb1, x = 1.5, y = 11.5, size = 6, colour = "black", parse=T) + 
  annotate("text", label = paste0("RMSE = ", plot_rmse), x = 1.5, y = 10, size = 6, colour = "black") +
  my_theme +
  xlim(0,15) +
  ylim(0,15) +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))

