library(dplyr)
library(ggplot2)
library(ggpubr)

setwd("C:/github/MIMICS_HiRes/Figures/Figure 8/MSAVI to GPP")
source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")

###### data PLOT
######################

data <- read.csv("MSAVI-GPP_relationship.csv", as.is=T, skip=1)

cor_data <- cor.test(data$MSAVI2, data$GEP)
r_data <- sprintf("%.02f",round(as.numeric(unlist(cor_data['estimate'])),2))

lmdata = lm(GEP~MSAVI2, data = data) #Create the linear regression
summary(lmdata)

data_plot <- ggplot(data, aes(x=MSAVI2, y=GEP)) + 
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_point(size=4, alpha=1) +
  annotate("text", label = paste0("r = ", r_data), x = 0.02, y = 900, size = 5, colour = "black", parse=F) + 
  annotate("text", label = paste0("y = ", round(lmdata$coefficients[2],2), "x + ", round(lmdata$coefficients[1],2)), x = 0.05, y = 850, size = 5, colour = "black") +
  annotate("text", label = paste0("n = ", nrow(data)), x = 0.015, y = 800, size = 5, colour = "black", parse=F) +
  geom_abline(intercept=lmdata$coefficients[1] , slope=lmdata$coefficients[2], size=1, alpha=0.3, linetype = "dashed") +
  my_theme +
  ggtitle("Relationship between MSAVI2 and Gross Ecosystem Productivty for the \nReynolds Creek Experimental Watershed") +
  #xlab("Predicted mean annual soil temperature (\u00B0C)") +
  #ylab("GEPerved mean annual soil temperature (\u00B0C)") +
  scale_x_continuous(breaks = c(0,0.1,0.2,0.3, 0.4, 0.5), limits = c(0, 0.5)) +
  scale_y_continuous(breaks = c(0,250,500,750,1000), limits = c(0, 1000)) +
  #scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))
data_plot
