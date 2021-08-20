library(tidyverse)
library(grid)
library(gridExtra)
library(scales) #<-- for plot colors
library(ggpubr)
library(ggthemes)
library(ggplot2)

setwd("C:/github/MIMICS_HiRes/MCMC")

### Load all MCMC output csv files from directory
filenames <- list.files(path="Output/",pattern=".*csv")

## Create list of data frame names without the ".csv" part 
names <-paste0("MCMC", seq(1,length(filenames)))

### Load all files
for(i in 1:length(names)){
  data_in <- read.csv(paste0("Output/",filenames[i]), as.is=T)
  
  ### Optional: Cut down number of iterations
  #data_in <- data_in %>% filter(iter <= 10000) %>% arrange(iter)
  
  #Add end row
  data_improve_steps <- data_in %>% filter(improve == 1)
  data_in <- rbind(data_in, data_improve_steps[nrow(data_improve_steps),])
  data_in$iter[nrow(data_in)] <- nrow(data_in)
  
  data_in$ID = paste0("Run ", as.character(i))
  #assign(names[i], MC_data)
  if(i == 1) {
    MCMC <- data_in  
  } else {
    MCMC <- rbind(MCMC, data_in)
  }
}

### Filter MCMC data to only include steps that improved RMSE
MCMC <- MCMC %>% filter(improve == 1)


############################################
### Create plot of parameter MCMC walks
############################################

colourCount = length(names)

### Change column names for plotting
MCMC <- MCMC %>% rename(Iteration = iter)

#Set plot theme
my_theme <- theme_bw() + 
  theme(panel.spacing.x=unit(1.5, "lines"),panel.spacing.y=unit(3, "lines")) +
  theme(legend.position="none") +
  theme(strip.text = element_text(size = 12)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(text = element_text(size=12)) +
  theme(legend.text=element_text(color='grey20',size=12)) +
  theme(legend.title=element_text(color='grey20',size=12)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())#+
  #theme(panel.background = element_blank()) + 
  #theme(strip.background = element_blank()) +
  #theme(strip.placement = "outside") +
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#set color ramp
colfunc <-colorRampPalette(c("#13a5d5","#0094f7","#374482"))
pcols <- colfunc(length(names))

#set color ramp for RMSE and r2
colfunc2 <-colorRampPalette(c("#d54813","#f73c00","#850000"))
pcols2 <- colfunc2(length(names))

pRMSE <- ggplot(MCMC, aes(x=Iteration, y=RMSE, colour=factor(ID))) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols2 ,guide = guide_legend(nrow=2))# + ylim(1,3.5) 

pr2 <- ggplot(MCMC, aes(x=Iteration, y=r2, color=ID)) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols2 ,guide = guide_legend(nrow=2))# + ylim(0.5, 1) 

pTau_x <- ggplot(MCMC, aes(x=Iteration, y=Tau_x, color=ID)) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols ,guide = guide_legend(nrow=2))# + ylim(0.5, 2) 

pCUE_x <-ggplot(MCMC, aes(x=Iteration, y=CUE_x, color=ID)) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols ,guide = guide_legend(nrow=2))# + ylim(0.5, 2) 

pDesorb_x <- ggplot(MCMC, aes(x=Iteration, y=desorb_x, color=ID)) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols ,guide = guide_legend(nrow=2))# + ylim(0.1, 3) 

pFPHYS_x <- ggplot(MCMC, aes(x=Iteration, y=fPHYS_x, color=ID)) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols ,guide = guide_legend(nrow=2))# + ylim(0.1, 3) 

pVslope_x <- ggplot(MCMC, aes(x=Iteration, y=Vslope_x, color=ID)) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols ,guide = guide_legend(nrow=2))# + ylim(0.5, 4) 

pVint_x <- ggplot(MCMC, aes(x=Iteration, y=Vint_x, color=ID)) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols ,guide = guide_legend(nrow=2))# + ylim(0.5, 4) 

pKslope_x <- ggplot(MCMC, aes(x=Iteration, y=Kslope_x, color=ID)) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols ,guide = guide_legend(nrow=2))# + ylim(0.5, 4) 

pKint_x <- ggplot(MCMC, aes(x=Iteration, y=Kint_x, color=ID)) + geom_line(alpha=0.5, size=1.3) + geom_point(size=1.5, alpha=0.8, shape=16) + my_theme +
  scale_color_manual(values = pcols ,guide = guide_legend(nrow=2))# + ylim(0.5, 4) 

# Put all plotsa together in a matrix
mplot <- ggarrange(pRMSE, pr2, pTau_x, pCUE_x, pDesorb_x, pFPHYS_x, pVslope_x, pVint_x, pKslope_x,  ncol=2, nrow=5, common.legend = TRUE, legend="none")
mplot

### Save matrix plot
ggsave(plot=mplot, filename = paste0("Post_MCMC_Analysis/Plots/MCMC_results_matrix", format(Sys.time(), "%Y%m%d_%H%M%S_"),".jpeg"), width = 6, height = 10 , dpi = 1000)



# Alternate plot code

MCMC_out <- read.csv(paste0("Output/",filenames[1]), as.is=T)

pRMSE <- ggplot(MCMC_out, aes(x=iter, y=RMSE)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none") +ylim(1,5)
pr2 <- ggplot(MCMC_out, aes(x=iter, y=r2)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pTau_x <- ggplot(MCMC_out, aes(x=iter, y=Tau_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pCUE_x <-ggplot(MCMC_out, aes(x=iter, y=CUE_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pDesorb_x <- ggplot(MCMC_out, aes(x=iter, y=desorb_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pFPHYS_x <- ggplot(MCMC_out, aes(x=iter, y=fPHYS_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pVslope_x <- ggplot(MCMC_out, aes(x=iter, y=Vslope_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pVint_x <- ggplot(MCMC_out, aes(x=iter, y=Vint_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pKslope_x <- ggplot(MCMC_out, aes(x=iter, y=Kslope_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")
pKint_x <- ggplot(MCMC_out, aes(x=iter, y=Kint_x)) + geom_line(color="grey50", alpha=0.5) + geom_point(size=3, color="grey50", alpha=0.5)  + geom_line(data=MCMC_out %>% filter(improve > 0), color="red", size=1) + geom_point(data=MCMC_out %>% filter(improve > 0), color="red", size=4) + theme_minimal() +theme(legend.position = "none")

grid.arrange(pRMSE, pr2, pTau_x, pCUE_x, pDesorb_x, pFPHYS_x, pVslope_x, pVint_x, pKslope_x, ncol = 2)


         