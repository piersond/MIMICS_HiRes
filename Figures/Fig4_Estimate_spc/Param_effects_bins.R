library(tidyverse)
library(ggplot2)
library(Metrics)

my_theme <- theme_bw() + 
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
   theme(strip.placement = "outside") +
   theme(text = element_text(size = 16))  

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
MCf <- MCf[1:(51*12),]

# Create SOC bins
MCf$Bin <- "0-2"
MCf$Bin[MCf$SOC > 2 & MCf$SOC < 4] <- "2-4"
MCf$Bin[MCf$SOC > 4 & MCf$SOC < 6] <- "4-6"
MCf$Bin[MCf$SOC > 6 & MCf$SOC < 8] <- "6-8"
MCf$Bin[MCf$SOC > 8] <- "> 8"

# Set order
MCf$Bin<- factor(MCf$Bin, levels = c("0-2", 
                                     "2-4", 
                                     "4-6",
                                     "6-8",
                                     "> 8"))

p2 <- ggplot(MCf, aes(x=Bin, y=SOC-MIMSOC)) + geom_boxplot() +
  geom_point(alpha=0.1) +
  my_theme +
  ylab("SOC Estimate\n Residual Error (kg/m2)") +
  xlab("")

p3 <- ggplot(MCf, aes(x=Bin, y=MIMMIC)) + geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.05) +
  my_theme +
  ylab("Microbial C (kg/m2)") +
  xlab("")

p4 <- ggplot(MCf, aes(x=Bin, y=MIMLIT)) + geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.05) +
  my_theme +
  ylab("Litter C (kg/m2)") +
  xlab("Soil Organic Carbon (kg/m2)")

p5 <- ggplot(MCf, aes(x=Bin, y=SOMp)) + geom_boxplot(outlier.shape = NA) +
  geom_point(alpha=0.05) +
  my_theme +
  ylab("Protected SOC (kg/m2)") +
  xlab("Soil Organic Carbon (kg/m2)")

library(reshape2)
df <- melt(MCf[,25:32])

p1 <- ggplot(df, aes(x=variable, y=value)) + geom_boxplot(width=0.5) + 
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
p1

library(ggpubr)

png(file="Figures/Fig4_Param_effects/Fig4_param_effects_R.png", width=3000, height=4000, units="px", res=300)

ggarrange(p1,
          ggarrange(p2, p3, ncol = 2, labels = c("", ""), align = "h",widths = c(1,1)), 
          ggarrange(p4, p5, ncol = 2, labels = c("", ""), align = "h",widths = c(1,1)), 
          nrow = 3, 
          heights = c(0.5, 1, 1),
          labels = "") 

dev.off()

