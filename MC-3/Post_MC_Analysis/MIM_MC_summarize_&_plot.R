library(ggplot2)
library(Metrics)
library(dplyr)
source("plot_theme.R")

setwd("C:/local_temp/ISU_HPC/071921")

HPCdata <- readRDS("MC_output/20210719_150209_BruteMIM-1e+05.rds")

# Calc RMSE for all runs
stats <- HPCdata %>% group_by(run_num) %>% summarize(RMSE = rmse(SOC, MIMSOC),
                                                     MICtoSOC = mean(MIMMIC/MIMSOC),
                                                     LITtoSOC = mean(MIMLIT/MIMSOC))
stats$RMSE <- round(stats$RMSE,3)

#Calc correlations
corr_data <- data.frame(run_num=NA, r2 = NA)

for(i in 1:length(unique(HPCdata$run_num))) {
  df <- HPCdata %>% filter(run_num == i)
  r2_test <- cor.test(df$SOC, df$MIMSOC)
  r_val <- round(as.numeric(unlist(r2_test ['estimate'])),3)
  corr_data <- rbind(corr_data, data.frame(run_num=i, r2 = r_val))
}

stats <- stats %>% left_join(corr_data)

### Filter by MIMMIC and LIT
best_fit <- stats %>% filter(MICtoSOC < 0.8) %>%
                        filter(MICtoSOC > 0.05) %>%
                        filter(LITtoSOC > 0.01)


### Based on stats, plot data for specific run number
run_to_plot <- 43507

plot_data <- HPCdata %>% filter(run_num == run_to_plot)

## Plot labels
# r2, RMSE, resids
r2_test <- cor.test(plot_data$SOC, plot_data$MIMSOC)
r_val <- round(as.numeric(unlist(r2_test ['estimate'])),2)

plot_rmse <- round(rmse(plot_data$SOC, plot_data$MIMSOC),2) 

plot_data$resid <- plot_data$SOC-plot_data$MIMSOC

lb1 <- paste("R^2 == ", r_val)

#create plot
ggplot(plot_data, aes(x=MIMSOC, y=SOC, color=pGPP+400)) + geom_point(size=3, alpha=1) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  annotate("text", label = lb1, x = 1.5, y = 8, size = 6, colour = "black", parse=T) + 
  annotate("text", label = paste0("RMSE = ", plot_rmse), x = 1.5, y = 7.4, size = 6, colour = "black") +
  my_theme +
  xlim(0,8.2) +
  ylim(0,8.2) +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))



#Misc
plot(plot_data$pGPP, plot_data$resid)

mean(plot_data$SOC)
mean(plot_data$MIMSOC)
mean(plot_data$resid)
