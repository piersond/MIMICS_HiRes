library(tidyverse)
library(ggplot2)
library(Metrics)
library(ggpubr)
source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")

setwd("C:/github/MIMICS_HiRes")

############################
# Load and prep MC data
############################

# Find for MC files in Output folder
filenames <- list.files(path="MC/Output/",pattern=".*rds")

### Load MC output data RDS
### Load and combine MC output data RDS
MC <- readRDS(paste0("MC/Output/","MC_MIMICS_data-r5e+05_20210825_171028_.rds"))
#MC2 <- readRDS(paste0("MC/Output/","MC_MIMICS_data-r5e+05_20210825_175404_.rds"))
#MC2$run_num <- MC2$run_num + nrow(MC1) 
#MC <- rbind(MC, MC2)

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
  filter(between(SOMpTO, 70, 400))  ### CHANGE LOWER BOUND TO 50 ###

#Take the top 1% of runs based on RMSE
MCf <- MCf %>% arrange(RMSE)
MCf_best <- MCf %>% filter(RMSE < 2)#MCf[1:(51*10),]  #<--If MC data includes all sampling locs, multiply by # of sampling locs in dataset

# Save lowest RMSE parameter set
#write.csv(unique(MCf_best[,24:33]), "RC_MIM_param_combos_RMSE_less2.csv")

#Take a look at RMSE distribution of best combinations
check_RMSE <- as.data.frame(unique(MCf_best$RMSE))
ggplot(MCf_best, aes(y=RMSE)) + geom_boxplot()

#Create dataframe of the just the parameter values and RMSE
best_parms <- MCf_best[,25:32]


str(best_parms)

####################################
# Matrix plot
####################################
pairs(best_parms,
      col = alpha("blue", 0.3),                                         # Change color
      pch = 16,                                            # Change shape of points
      cex=1.5,
      labels = c("Vslope", "Vint", "Kslope", "Kint", "Tau", "CUE", "Desorb", "fPHYS"),                  # Change labels of diagonal
      main = "MIMICS Monte Carlo Top 1% Lowest RMSE",
      upper.panel = NULL)

library(GGally)
ggcorr(best_parms,
       method = c("everything", "pearson"))


####################################
# Matrix plot 2
# Scale each parameter from 0-1 based on MC parameter range limits
####################################

prm_spc <- best_parms

#save a copy of the "best" parameters
#write.csv(prm_spc, "best_parms.csv")

# Scale parameter values to 0-1 based on random parameter
# proposal range used for the MC simulation
prm_spc$Vslope_spc <- (prm_spc$Vslope_x-0.4)/(4-0.4)
prm_spc$Vint_spc <- (prm_spc$Vint_x-0.3)/(3-0.3)
prm_spc$Kslope_spc <- (prm_spc$Kslope_x-0.4)/(4-0.4)
prm_spc$Kint_spc <- (prm_spc$Kint_x-0.3)/(3-0.3)
prm_spc$Tau_spc <- (prm_spc$Tau_x-0.3)/(3-0.3)
prm_spc$CUE_spc <- (prm_spc$CUE_x-0.2)/(2-0.2)
prm_spc$desorb_spc <- (prm_spc$desorb_x-0.001)/(0.3-0.001)
prm_spc$fPHYS_spc <- (prm_spc$fPHYS_x-0.01)/(4-0.01)

pairs(prm_spc[,9:16],
      col = alpha("blue", 0.3),                                         # Change color
      pch = 16,                                            # Change shape of points
      cex=1.5,
      labels = c("Vslope", "Vint", "Kslope", "Kint", "Tau", "CUE", "Desorb", "fPHYS"),                  # Change labels of diagonal
      main = "MIMICS Monte Carlo Top 1% Lowest RMSE",
      upper.panel = NULL,
      xlim=c(0,1),
      ylim=c(0,1),
      breaks=c(0,1))

####################################
# Matrix plot 3
# Scale each parameter from 0-1 based on MC parameter range limits
####################################

library(psych)
pairs_pan_data <- prm_spc[,9:16] 
colnames(pairs_pan_data) <- c("Vslope", "Vint", "Kslope", "Kint", "Tau", "MGE", "D", "fp" )

### Save plot
png(file="Figures/Fig4_MC_param_spc/MC_param_space.png", width=3000, height=3000, units="px", res=600)

#Create the plot
matrix_p3 <- pairs.panels(pairs_pan_data,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             smooth = FALSE, #Show loess fit line
             density = TRUE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             scale = TRUE,
             pch = 16,
             cex.cor = 2,
             cex = 1,
             xlim=c(0,1),
             ylim=c(0,1))
dev.off()


#Plot SOC vs MIMSOC
################################################################################
### Based on stats in "best-fit" dataframe, plot data for specific run number
run_to_plot <- 367791
################################################################################

plot_data <- MCf %>% filter(run_num == run_to_plot)

plot_data <- plot_data %>% filter(Site != "S48")

## Plot stats text
# r2, RMSE, resids
r2_test <- cor.test(plot_data$SOC, plot_data$MIMSOC)
r_val <- round(as.numeric(unlist(r2_test ['estimate'])),2)

plot_rmse <- round(rmse(plot_data$SOC, plot_data$MIMSOC),2) 

plot_data$resid <- plot_data$SOC-plot_data$MIMSOC

lb1 <- paste("R^2 == ", r_val)

#create plot
ggplot(plot_data, aes(x=MIMSOC, y=SOC, color=pGPP+400, label=Site)) + geom_point(size=3, alpha=1) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  geom_text(hjust = 0, nudge_x = 0.05) +
  annotate("text", label = lb1, x = 1.5, y = 11.5, size = 6, colour = "black", parse=T) + 
  annotate("text", label = paste0("RMSE = ", plot_rmse), x = 1.5, y = 10, size = 6, colour = "black") +
  my_theme +
  xlim(0,15) +
  ylim(0,15) +
  scale_colour_gradient(low = "orange", high = "dark green", na.value = NA, name="GPP ") +
  theme(legend.key.size = unit(2,"line"))


########################################################
# Parm space fancy plot
########################################################
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    stat_density2d(aes(fill=..density..), geom="tile", contour = FALSE) +
    scale_fill_gradientn(colours=c("white", "yellow", "green", "dark blue")) +
    geom_point(size=0.5, alpha=0.7)
}

p1 <- ggpairs(best_parms, lower=list(continuous=my_fn))

# Correlation matrix plot
p2 <- ggcorr(best_parms, label = TRUE, label_round = 2)

# Collect info from ggplot
g2 <- ggplotGrob(p2)
colors <- g2$grobs[[6]]$children[[3]]$gp$fill

p <- 8

# Change background color to tiles in the upper triangular matrix of plots 
idx <- 1
for (k1 in 1:(p-1)) {
  for (k2 in (k1+1):p) {
    plt <- getPlot(p1,k1,k2) +
      theme(panel.background = element_rect(fill = colors[idx], color="white"),
            panel.grid.major = element_line(color=colors[idx]))
    p1 <- putPlot(p1,plt,k1,k2)
    idx <- idx+1
  }
}
print(p1)


### Point plots of estimate space
library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1.8, 1.98))
my_theme <- my_theme + theme(axis.line = element_line(colour = "black"), axis.text.x.top = element_blank(), axis.title.x.top = element_blank())
rmx_x_axis <- theme(axis.text.x = element_blank())

MCf_best <- MCf_best %>% arrange(-RMSE)

p1 <- ggplot(MCf_best, aes(x=SOC,y=MIMSOC, color=RMSE)) + geom_point(color="black", alpha=1, size=1.5) + 
        my_theme + geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        rmx_x_axis +
        scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
        ylab(expression(paste("MIMICS SOC (kg ", m^-2, ")")))+
        scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis(labels = NULL))
  
p2 <- ggplot(MCf_best, aes(x=SOC,y=MIMMIC)) + geom_point(color="black", alpha=1, size=1.5) + 
        my_theme + 
        ylab(expression(paste("MIMICS Microbial C (kg ", m^-2, ")"))) +
        scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis(labels = NULL)) +
        xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")")))

p3 <- ggplot(MCf_best, aes(x=SOC,y=MIMLIT)) + geom_point(color="black", alpha=1, size=1.5) + 
        my_theme +
        scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis(labels = NULL)) +
        scale_y_continuous(limits = c(0, 3.3), breaks = seq(0, 3, by = 0.5)) +
        xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")"))) +
        ylab(expression(paste("MIMICS Litter C (kg ", m^-2, ")")))

p4 <- ggplot(MCf_best, aes(x=SOC,y=SOMp)) + geom_point(color="black", alpha=1, size=1.5) + 
        my_theme + rmx_x_axis +
        scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
        scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis()) +
        xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")"))) +
        ylab(expression(paste("MIMICS Protected SOC (kg ", m^-2, ")")))

png(file="Figures/Fig4_Estimate_Space.png", width=5000, height=4000, units="px", res=600)

ggarrange(p1 + rremove("xlab"),
          p4 + rremove("xlab"),
          p3,
          p2,
          labels = c("A", "B", "C", "D"),
          heights = c(1, 1),
          widths = c(1,1),
          nrow=2,
          ncol=2,
          vjust = 2.3,
          hjust = c(-5.9, -5.5, -5.5, -5.5),
          align = "hv",
          legend = "none")
#dev.off()

#####################################
# for Will: SOMp/SOM
#####################################
library(viridis)

qa_df <- MCf_best %>% filter(RMSE < 1.85)
qa_df2 <- unique(qa_df[,25:37])

MCf_best <- MCf_best %>% arrange(-RMSE)
p5 <- ggplot(MCf_best %>% filter(RMSE < 1.85), aes(x=SOC,y=SOMp/MIMSOC*100, group=RMSE, color=RMSE)) + 
  geom_line(alpha=0.8, size=1.5) +
  geom_point(alpha=1, size=2.5) + 
  my_theme + rmx_x_axis + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis()) +
  xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")"))) +
  ylab(expression(paste("MIMICS SOMp:SOM (%)"))) +
  theme(legend.key.width= unit(1, "inch")) +
  #scale_color_viridis(option = "D")+
  #scale_fill_viridis() +
  labs(title = "RCrk MIMICS SOMp:SOM",
         subtitle = "n = 2 parameter combinations (RMSE < 1.85)",
         caption = "Produced from RCrk calibration dataset, MCx500k")
p5


########
# Plot SOMp:SOM by parameters
########
MCf_best$SOMpSOM <- (MCf_best$SOMp/MCf_best$MIMSOC)*100

df3 <- MCf_best %>% group_by(run_num) %>% summarize(SOMpSOM = mean(SOMpSOM),
                                                    LITrSOM = mean(MIMLIT/MIMSOC),
                                                    MICrSOM = mean(MIMMIC/MIMSOC),
                                                    SOMpTO = mean(SOMpTO),
                                                    Vslope_x = mean(Vslope_x),
                                                    Vint_x = mean(Vint_x),
                                                    Kslope_x = mean(Kslope_x),
                                                    Kint_x = mean(Kint_x),
                                                    CUE_x = mean(CUE_x),
                                                    Tau_x = mean(Tau_x),
                                                    desorb_x = mean(desorb_x),
                                                    fPHYS_x = mean(fPHYS_x))

matrix_p3 <- pairs.panels(df3[,c(2:5)],
                          method = "pearson", # correlation method
                          hist.col = "#00AFBB",
                          smooth = FALSE, #Show loess fit line
                          density = TRUE,  # show density plots
                          ellipses = FALSE, # show correlation ellipses
                          scale = TRUE,
                          pch = 16,
                          cex.cor = 2,
                          cex = 1)

