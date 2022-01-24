library(tidyverse)
library(ggplot2)
library(Metrics)
library(ggpubr)
source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")

setwd("C:/github/MIMICS_HiRes")

### Point plots of estimate space

# Setup plot formatting
library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1.8, 1.98))
my_theme <- my_theme + theme(axis.line = element_line(colour = "black"), axis.text.x.top = element_blank(), axis.title.x.top = element_blank())
rmx_x_axis <- theme(axis.text.x = element_blank())

# Get the data
MCf_best <- readRDS("Figures/Fig3_Estimate_spc/MIM_pset_runs_20210903_125039.rds")
MCf_stats <- MCf_best %>% group_by(run_num) %>% summarize(RMSE = rmse(SOC, MIMSOC))
MCf_best <- MCf_best %>% left_join(MCf_stats) %>% arrange(RMSE)

#Isolate the best run by lowest RMSE
top_p <- MCf_best %>% filter(RMSE == min(RMSE))

# Make the plots
p1 <- ggplot(MCf_best, aes(x=SOC,y=MIMSOC)) + geom_point(color="grey50", alpha=1, size=1.5) + 
  geom_point(data=top_p, aes(x=SOC,y=MIMSOC), color="black", alpha=1, size=1.5) + 
  my_theme + geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  rmx_x_axis +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  ylab(expression(paste("MIMICS SOC (kg ", m^-2, ")")))+
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis(labels = NULL))
  
p2 <- ggplot(MCf_best, aes(x=SOC,y=MIMMIC)) + geom_point(color="grey50", alpha=1, size=1.5) + 
  geom_point(data=top_p, aes(x=SOC,y=MIMMIC), color="black", alpha=1, size=1.5) + 
  my_theme + 
  ylab(expression(paste("MIMICS Microbial C (kg ", m^-2, ")"))) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis(labels = NULL)) +
  xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")")))

p3 <- ggplot(MCf_best, aes(x=SOC,y=MIMLIT)) + geom_point(color="grey50", alpha=1, size=1.5) + 
  geom_point(data=top_p, aes(x=SOC,y=MIMLIT), color="black", alpha=1, size=1.5) + 
  my_theme +   rmx_x_axis +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis(labels = NULL)) +
  scale_y_continuous(limits = c(0, 3.3), breaks = seq(0, 3, by = 0.5)) +
  xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")"))) +
  ylab(expression(paste("MIMICS Litter C (kg ", m^-2, ")")))

p4 <- ggplot(MCf_best, aes(x=SOC,y=SOMp)) + geom_point(color="grey50", alpha=1, size=1.5) + 
  geom_point(data=top_p, aes(x=SOC,y=SOMp), color="black", alpha=1, size=1.5) + 
  my_theme + rmx_x_axis +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis()) +
  xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")"))) +
  ylab(expression(paste("MIMICS Protected SOC (kg ", m^-2, ")")))

p5 <- ggplot(MCf_best, aes(x=SOC,y=MIM_CO)) + geom_point(color="grey50", alpha=1, size=1.5) + 
  geom_point(data=top_p, aes(x=SOC,y=MIM_CO), color="black", alpha=1, size=1.5) + 
  my_theme + 
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis()) +
  xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")"))) +
  ylab(expression(paste("MICr:MICK C Pool Size")))

p6 <- ggplot(MCf_best, aes(x=SOC,y=1/(desorb*24*365))) + geom_point(color="grey50", alpha=1, size=1.5) + 
  geom_point(data=top_p, aes(x=SOC,y=1/(desorb*24*365)), color="black", alpha=1, size=1.5) + 
  my_theme + rmx_x_axis +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis()) +
  xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")"))) +
  ylab(expression(paste("SOMp Turnover (yr)")))


png(file="Figures/Fig3_Estimate_Space3.png", width=4000, height=6000, units="px", res=600)

ggarrange(p1 + rremove("xlab"),
          p4 + rremove("xlab"),
          p3 + rremove("xlab"),
          p6 + rremove("xlab"),
          p2,
          p5,
          labels = c("A", "B", "C", "D", "E", "F"),
          heights = c(1, 1),
          widths = c(1,1),
          nrow=3,
          ncol=2,
          vjust = 2.3,
          hjust = c(-5.9, -5.5, -5.5, -5.5, -6, -6.5),
          align = "hv",
          legend = "none")
dev.off()

# Save data
#write.csv(MCf_best, "MIMout_for_ensemble.csv")

### Calculate summary stats for results section
# Create function to use for different pools

calc_pool_var <- function(sites, MIM_pool) {
  pool_var_stats <- data.frame(Site=sites, pool=MIM_pool) %>% group_by(Site) %>%
                               summarize(pool_max = max(pool),
                                         pool_min = min(pool),
                                         maxmin_ratio = pool_max/pool_min) 
  
  pool_var_stats %>% summarize(pool_ratio_mn = mean(maxmin_ratio),
              pool_ratio_sd = sd(maxmin_ratio))
}


# THESE VALUES USED IN RESULTS SECTION
calc_pool_var(MCf_best$Site, MCf_best$SOMp)
calc_pool_var(MCf_best$Site, MCf_best$MIMLIT)
calc_pool_var(MCf_best$Site, MCf_best$MIMMIC/MCf_best$MIMSOC)

####### MIM_CO avg and std dev
MCf_best %>% filter(MIM_CO > 0.28) %>% filter(MIM_CO < 4) %>%
            #group by sampling location, then get mean and std dev of pool
             group_by(Site) %>% summarize(mn = mean(MIM_CO),
                                          sd = sd(MIM_CO)) %>%
             #take the overall mean across all the sampling location means
             summarize(all_mn = mean(mn),
                       all_sd = sd(mn))
  
###########################################

####### MIMMIC/MIMSOC avg and std dev
MCf_best %>% 
  #group by sampling location, then get mean and std dev of pool
  group_by(Site) %>% summarize(mn = mean(MIMMIC/MIMSOC),
                               sd = sd(MIMMIC/MIMSOC)) %>%
  #take the overall mean across all the sampling location means
  summarize(all_mn = mean(mn),
            all_sd = sd(mn))

###########################################

 


