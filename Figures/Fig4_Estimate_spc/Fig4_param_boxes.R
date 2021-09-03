library(tidyverse)
library(ggplot2)
library(Metrics)
my_theme <- theme_minimal() + 
  theme(panel.spacing.x=unit(0.5, "lines"),panel.spacing.y=unit(1, "lines")) +
  theme(legend.position="bottom") +
  theme(strip.text = element_text(size = 12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=12)) +
  theme(legend.text=element_text(color='grey20',size=12)) +
  theme(legend.title=element_text(color='grey20',size=12)) +
  theme(panel.background = element_blank()) + 
  theme(strip.background = element_blank()) +
  theme(strip.placement = "outside")# +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

setwd("C:/github/MIMICS_HiRes")

############################
# Load and prep MC data
############################

# Find for MC files in Output folder
filenames <- list.files(path="MC/Output/",pattern=".*rds")

### Load MC output data RDS
MC <- readRDS(paste0("MC/Output/",filenames[1]))

# Calc summary statistics
MC_stats <- MC %>% group_by(run_num) %>% summarize(RMSE = rmse(SOC, MIMSOC),
                                                   MICpropSOC = mean(MIMMIC)/mean(MIMSOC),
                                                   LITpropSOC = mean(MIMLIT)/mean(MIMSOC),
                                                   MIM_CO_mn = mean(MIM_CO),
                                                   SOMpTO = mean(1/(desorb*24*365)))
MC_all <- MC %>% left_join(MC_stats)

#Filter MC runs by microbe and litter pool ranges
MCf <- MC_all %>% filter(between(MICpropSOC, 0.01, 0.08)) %>%
  filter(between(LITpropSOC, 0.05, 0.50)) %>%
  filter(between(MIM_CO_mn, 0.01, 100)) %>%
  filter(between(SOMpTO, 20, 500))  ### CHANGE LOWER BOUND TO 50 ###

#Take the top 1% of runs based on RMSE
MCf <- MCf %>% arrange(RMSE)
MCf <- MCf[1:(51*50),]

library(reshape2)
df <- melt(MCf[,25:32])

ggplot(df, aes(x=variable, y=value)) + geom_boxplot(width=0.5) + 
    geom_point(color="dark red", alpha=0.008) +
    my_theme +
    scale_x_discrete(labels=c("Vslope_x" = "Vslope", 
                              "Vint_x" = "Vint",
                              "Kslope_x" = "Kslope",
                              "Kint_x" = "Kint",
                              "Tau_x" = "Tau",
                              "CUE_x" = "CUE",
                              "desorb_x" = "desorb",
                              "fPHYS_x" = "fPHYS")) +
    ylab("Factor of\n default value") +
    xlab("Model Parameter")

ggsave("Figures/Fig4_Param_effects/fig4_param_boxplots.png",
         plot = last_plot(),
         width = 8,
         height = 2,
         dpi = 600)

 