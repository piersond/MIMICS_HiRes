library(dplyr)
library(ggplot2)
library(ggpubr)

setwd("C:/github/MIMICS_HiRes/Figures/Figure 8")
source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")

###### TSOI PLOT
######################

tsoi <- read.csv("MAST_obs_est.csv", as.is=T)
colnames(tsoi) <- c('obs', 'est')

cor_tsoi <- cor.test(tsoi$est, tsoi$obs)
r_tsoi <- sprintf("%.02f",round(as.numeric(unlist(cor_tsoi['estimate'])),2))

lmTSOI = lm(obs~est, data = tsoi) #Create the linear regression
summary(lmTSOI)

tsoi_plot <- ggplot(tsoi, aes(x=est, y=obs)) + 
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=1) +
  annotate("text", label = paste0("r = ", r_tsoi), x = 3.5, y = 13.5, size = 5, colour = "black", parse=F) + 
  annotate("text", label = paste0("y = ", round(lmTSOI$coefficients[2],2), "x + ", round(lmTSOI$coefficients[1],2)), x = 4.5, y = 12.3, size = 5, colour = "black") +
  annotate("text", label = paste0("n = ", nrow(tsoi)), x = 13, y = 3, size = 5, colour = "black", parse=F) +
  geom_abline(intercept=lmTSOI$coefficients[1] , slope=lmTSOI$coefficients[2], size=1, alpha=0.3, linetype = "dashed") +
  my_theme +
  ggtitle("A") +
  xlab("Predicted mean annual soil temperature (\u00B0C)") +
  ylab("Observed mean annual soil temperature (\u00B0C)") +
  scale_x_continuous(breaks = c(4,8,12,14), limits = c(2.5, 14)) +
  scale_y_continuous(breaks = c(4,8,12,14), limits = c(2.5, 14)) +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))
tsoi_plot


###### MSAVI GPP PLOT
#######################################
gpp <- read.csv("MSAVI_GPP.csv", as.is=T)
colnames(gpp) <- c('obs', 'est')

cor_gpp <- cor.test(gpp$est, gpp$obs)
r_gpp <- sprintf("%.02f",round(as.numeric(unlist(cor_gpp['estimate'])),2))

lmGPP = lm(obs~est, data = gpp) #Create the linear regression
summary(lmGPP)

gpp_plot <- ggplot(gpp, aes(x=est, y=obs)) + 
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=1) +
  annotate("text", label = paste0("r = ", r_gpp), x = 300, y = 950, size = 5, colour = "black", parse=F) + 
  annotate("text", label = paste0("y = ", round(lmGPP$coefficients[2],2), "x + ", round(lmGPP$coefficients[1],2)), x = 340, y = 870, size = 5, colour = "black") +
  annotate("text", label = paste0("n = ", nrow(gpp)), x = 900, y = 250, size = 5, colour = "black", parse=F) +
  geom_abline(intercept=lmGPP$coefficients[1] , slope=lmGPP$coefficients[2], size=1, alpha=0.3, linetype = "dashed") +
  my_theme +
  ggtitle("B") +
  xlab(expression(paste("Predicted gross ecosystem productivity (gC ",m^-2," ",yr^-1,")"))) +
  ylab(expression(paste("Observed gross ecosystem productivity (gC ",m^-2," ",yr^-1,")"))) +
  scale_x_continuous(breaks = c(200,400,600,800,1000), limits = c(200, 1000)) +
  scale_y_continuous(breaks = c(200,400,600,800,1000), limits = c(200, 1000)) +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))
gpp_plot

### Join and save plots
#######################

png(file="TSOI_&_GPP_corr_plots.png", width=6000, height=3000, units="px", res=600)
ggarrange(tsoi_plot,
          gpp_plot)
dev.off()


