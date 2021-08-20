library(ggplot2)
library(Metrics)
library(dplyr)

setwd("C:/github/MIMICS_HiRes")
source("Figures/plot_theme.R")

####################
# Load MC data
####################
MCdata <- readRDS("MC/Output/MC_runs_compiled20210819.rds")

##########################################################
# Calculate RMSE and R2 for each run in the MC dataset
##########################################################
## If missing, add SOMpTOv
MCdata$SOMpTOv <- 1/(MCdata$desorb* 24 * 365)

# Calc RMSE for all runs
stats <- MCdata %>% group_by(run_num) %>% summarize(RMSE = rmse(SOC, MIMSOC),
                                                    MICtoSOC = mean(MIMMIC/MIMSOC),
                                                    LITtoSOC = mean(MIMLIT/MIMSOC),
                                                    MIM_CO_Avg = mean(MIM_CO),
                                                    SOMpTOvAvg = mean(SOMpTOv),
                                                    desorbx_mn = mean(desorb_x))

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
best_fit <- stats %>% filter(MICtoSOC > 0.005) %>%
                      filter(MICtoSOC < 0.08) %>%
                      filter(LITtoSOC > 0.01) %>%
                      filter(LITtoSOC < 0.50) %>%
                      filter(MIM_CO_Avg > 0.01) %>%
                      filter(MIM_CO_Avg < 100) %>%
                      filter(SOMpTOvAvg > 10) %>%
                      filter(SOMpTOvAvg < 1000)
  

#Plot SOC vs MIMSOC
################################################################################
### Based on stats in "best-fit" dataframe, plot data for specific run number
run_to_plot <- 35366
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


################################
# Save best-fit parameters
################################

# # Best parameter combo
# best_run <- 35366
# 
# best_pcombo <- MCdata %>% filter(run_num == best_run) %>%
#                   select(run_num, Vslope_x, Vint_x, Kslope_x, Kint_x,
#                          CUE_x, Tau_x, desorb_x, fPHYS_x) %>%
#                   unique()
# 
# write.csv(best_pcombo, paste0("MC/Output/", "best_MC_Pcombo_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names = F)
# 
# 
# # Set of best parameter combos with RMSE with 10% of lowest RMSE
# best_pcombos <- best_fit %>% filter(RMSE < (min(RMSE)*1.1)) %>%
#                   left_join(MCdata) %>% na.omit(RMSE)
# 
# write.csv(best_pcombos, paste0("MC/Output/", "best_set_of_MC_Pcombos_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names = F)





