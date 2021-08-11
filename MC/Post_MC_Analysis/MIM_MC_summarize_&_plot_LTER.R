library(ggplot2)
library(Metrics)
library(dplyr)

setwd("C:/github/MIMICS_HiRes")
source("Figures/plot_theme.R")

####################
# Load MC data
####################
MCdata <- readRDS("MC/Output/LTER_MC_data-r5e+05_20210804_094544.rds")


#Add SOMp turnover time 
MCdata$SOMpTOv <- 1/(MCdata$desorb*24*365)  

##########################################################
# Calculate RMSE and R2 for each run in the MC dataset
##########################################################
# Calc RMSE for all runs
stats <- MCdata %>% group_by(run_num) %>% summarize(RMSE = rmse(SOC, MIMSOC),
                                                     MICtoSOC = mean(MIMMIC/MIMSOC),
                                                     LITtoSOC = mean(MIMLIT/MIMSOC),
                                                    MIM_CO_Avg = mean(MIM_CO),
                                                    SOMpTOvAvg = mean(SOMpTOv))
stats$RMSE <- round(stats$RMSE,3)

#Calc correlations
# corr_data <- data.frame(run_num=NA, r2 = NA)

# for(i in 1:length(unique(MCdata$run_num))) {
#   df <- MCdata %>% filter(run_num == i)
#   r2_test <- cor.test(df$SOC, df$MIMSOC)
#   r_val <- round(as.numeric(unlist(r2_test ['estimate'])),3)
#   corr_data <- rbind(corr_data, data.frame(run_num=i, r2 = r_val))
# }

# stats <- stats %>% left_join(corr_data)

### Filter by MIMMIC and LIT
best_fit <- stats %>% filter(MICtoSOC > 0.005) %>%
                        filter(MICtoSOC < 0.08) %>%
                        filter(LITtoSOC > 0.01) %>%
                        filter(LITtoSOC < 0.50) %>%
                        filter(MIM_CO_Avg > 0.01) %>%
                        filter(MIM_CO_Avg < 100) %>%
                        filter(SOMpTOvAvg > 30) %>%
                        filter(SOMpTOvAvg < 1000)

df <- left_join(best_fit, MCdata)

df_all <- left_join(stats, MCdata)

#tighten up the filters
best_fit2 <- stats %>% filter(MICtoSOC > 0.01) %>%
  filter(MICtoSOC < 0.03) %>%
  filter(LITtoSOC > 0.05) %>%
  filter(LITtoSOC < 0.30) %>%
  filter(MIM_CO_Avg > 0.01) %>%
  filter(MIM_CO_Avg < 100) %>%
  filter(SOMpTOvAvg > 100) %>%
  filter(SOMpTOvAvg < 1000)




#Plot SOC vs MIMSOC
################################################################################
### Based on stats in "best-fit" dataframe, plot data for specific run number
run_to_plot <- 132741
################################################################################

plot_data <- MCdata %>% filter(run_num == run_to_plot)

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
  annotate("text", label = lb1, x = 1.5, y = 8, size = 6, colour = "black", parse=T) + 
  annotate("text", label = paste0("RMSE = ", plot_rmse), x = 1.5, y = 7.4, size = 6, colour = "black") +
  my_theme +
  xlim(0,10) +
  ylim(0,10) +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))

#Sandbox LTER plot
ggplot(plot_data, aes(x=MIMSOC, y=SOC, color=pGPP+400)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=0.8) +
  geom_text(aes(label=paste0(Site)),hjust=-0.2, vjust=0.2) +
  annotate("text", label = lb1, x = 2, y = 8.5, size = 6, colour = "black", parse=T) +
  annotate("text", label = paste0("RMSE = ", plot_rmse), x = 2, y = 7.4, size = 6, colour = "black") +
  ylim(0,10) + xlim(0,10) +
  labs(color = "ANPP")


# Parameter distributions
filt_df <- df %>% filter(RMSE < 2)

ggplot(df, aes(x=Vslope_x)) + geom_density(color="blue", size=1.5) + 
  geom_density(data=dist_df, aes(x=Vslope_x), color="red", size=1.5) + 
  theme_bw()

#Hex plots
ggplot(filt_df, aes(x=Vslope_x, y=SOMpTOvAvg)) + geom_hex()## + ylim(1.4,2) 
ggplot(filt_df, aes(x=Vint_x, y=SOMpTOvAvg)) + geom_hex()# + ylim(1.4,2) 
ggplot(filt_df, aes(x=Kslope_x, y=SOMpTOvAvg)) + geom_hex()# + ylim(1.4,2) 
ggplot(filt_df, aes(x=Kint_x, y=SOMpTOvAvg)) + geom_hex()# + ylim(1.4,2) 
ggplot(filt_df, aes(x=Tau_x, y=LITtoSOC)) + geom_hex()# + ylim(1.4,2) 
ggplot(filt_df, aes(x=CUE_x, y=Tau_x)) + geom_hex()# + ylim(1.4,2) 
ggplot(filt_df, aes(x=desorb_x, y=SOMpTOvAvg)) + geom_hex()# + ylim(1.4,2) 
ggplot(filt_df, aes(x=fPHYS_x, y=SOMpTOvAvg)) + geom_hex()# + ylim(1.4,2) 

#LIT-C vs SOM-C
ggplot(plot_data, aes(x=MIMLIT, y=lig_N, color=pGPP+400)) + geom_point() +
  geom_text(aes(label=paste0(Site)),hjust=-0.2, vjust=0.2) 
