library(tidyverse)
library(ggpubr)

setwd("C:/github/MIMICS_HiRes/MC/Cross-Validation")
source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")

mc1 <- readRDS("MC_MIMICS_output_ssamp12_500k.rds")
mc2 <- readRDS("MC_MIMICS_output_ssamp13_500k.rds")
mc3 <- readRDS("MC_MIMICS_output_ssamp23_500k.rds")
best_parms <- read.csv("C:/github/MIMICS_HiRes/MC/Output/RC_MIM_param_combos_RMSE_less2.csv")

MIM_plaus_filter <- function(MIMout) {
  
  df <- MIMout %>%
    filter(MICpropSOC > 0.01) %>%
    filter(MICpropSOC < 0.08) %>%
    filter(LITpropSOC > 0.05) %>%
    filter(LITpropSOC < 0.50) %>%
    filter(MIM_CO_mn > 0.01) %>%
    filter(MIM_CO_mn < 100) %>%
    filter(SOMpTO > 50) %>%
    filter(SOMpTO < 1000) 
  
  return(df)
}

mc1p <- MIM_plaus_filter(mc1) %>% filter(RMSE < 2)
mc2p <- MIM_plaus_filter(mc2) %>% filter(RMSE < 2)
mc3p <- MIM_plaus_filter(mc3) %>% filter(RMSE < 2)
mc0p <- MIM_plaus_filter(mc0) %>% filter(RMSE < 2)

mc123_distplot_param <- function(param) {
  
  ggplot(mc1p, aes_string(x=param)) + geom_density(color = "Dark Green", size=1) +
    geom_density(data = mc2p, aes_string(x=param), color="Dark Blue", size=1) +
    geom_density(data = mc3p, aes_string(x=param), color="Dark Red", size=1) +
    geom_density(data = best_parms, aes_string(x=param), color="grey70", size=1, linetype="dashed") + 
    ggtitle(param) +
    xlab("") +
    theme_minimal() +
    my_theme
  
}

p1 <- mc123_distplot_param("Vslope_x") 
p2 <- mc123_distplot_param("Vint_x")
p3 <- mc123_distplot_param("Kslope_x") 
p4 <- mc123_distplot_param("Kint_x") 
p5 <- mc123_distplot_param("Tau_x") 
p6 <- mc123_distplot_param("CUE_x") 
p7 <- mc123_distplot_param("desorb_x") 
p8 <- mc123_distplot_param("fPHYS_x") 

panelp <- ggarrange(p1, p2,
                    p3, p4,
                    p5, p6,
                    p7, p8,
                    ncol=2,
                    nrow=4)

panelp <- annotate_figure(panelp,
                          top = text_grob("MIMICS Hi-Res Monte Carlo Parameter Optimization \n 3-fold Cross Validation - Parameter Distributions", color = "Black", face = "bold", size = 14),
                          bottom = text_grob("Green n = 15, Red n = 11, Blue n = 45, Grey n = 30", color = "grey10",
                                             hjust = 1, x = 0.65, face = "italic", size = 16))

### Save plot
png(file="MC_3fold_param_dist_wBest.png", width=1000, height=1500, units="px", res=100)
panelp
dev.off()