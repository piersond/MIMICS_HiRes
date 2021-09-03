### MIMICS MC

setwd("C:/github/MIMICS_HiRes")

########################################
# Load MIMICS data and ftns from Brute Forcing script
########################################
source("MIMICS_ftns/MIMICS_repeat_base.R")


########################################
# Load forcing data
########################################
data <- read.csv("RCrk_Modelling_Data/RCrk_SOC_calibration.csv", as.is=T)

# Trim out data columns not required for the MC run 
## Save on dataframe size and we can add the back later

data <- data %>% select(Site, lat, long, SOC, CLAY, pGPP, estCLAY, TSOI, lig_N)


####################################
# Use the brute force MIMICS ftn
####################################


# Get best parameter set
best_parms <- read.csv("Figures/Fig8_scenario_estimate_space/best_parms.csv", as.is=T)
best_parms <- unique(best_parms)[,1:8]

best_parms$run_num <- seq(1,nrow(best_parms),1)

# Set number of cores to use
no_cores <- availableCores() - 1
plan(multicore, gc = FALSE, workers = no_cores)

# Run MIMICS!
print(paste0("Start time: ", Sys.time()))

start_time <- Sys.time()
MC_MIMICS <- best_parms%>% split(1:nrow(best_parms)) %>% future_map(~ MIMrepeat(forcing_df = data, rparams = ., output_type = "all"), .progress=TRUE) %>% bind_rows()
print(paste0("Task time: ", Sys.time() - start_time))

# Release CPU cores
plan(sequential)
nbrOfWorkers()

# Clean up memory
gc()


## Join parameters to MIMICS output table
MC_MIMICS <- MC_MIMICS %>% left_join(best_parms)


##########################################
# Save MC output data
##########################################
#saveRDS(MC_MIMICS, paste0("MC/Output/", "MC_MIMICS_best_parms", "_", format(Sys.time(), "%Y%m%d_%H%M%S_"),  ".rds"))

# Collect MC_MIMICS dataframes from different runs
#scen5GEP <- MC_MIMICS
scen5GEP$set <- "+5% GPP"

#cal <- MC_MIMICS
cal$set <- "CAL"

MIM_all_out <- rbind(cal, scen5GEP) 

#############################################
# Plot estimate space
#############################################
source("C:/github/MIMICS_HiRes/Figures/plot_theme.R")

### Point plots of estimate space
library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_color_manual(values = c("dark red", "black"))
my_theme <- my_theme + theme(axis.line = element_line(colour = "black"), axis.text.x.top = element_blank(), axis.title.x.top = element_blank())
rmx_x_axis <- theme(axis.text.x = element_blank())

MCf_best <- MIM_all_out#%>% arrange(-RMSE)

p1 <- ggplot(MCf_best, aes(x=SOC,y=MIMSOC, color=set)) + geom_point(alpha=1, size=1.5) + 
  my_theme + geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  rmx_x_axis + sc +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 14, by = 2)) +
  ylab(expression(paste("MIMICS SOC (kg ", m^-2, ")")))+
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis(labels = NULL))
p1

p2 <- ggplot(MCf_best, aes(x=SOC,y=MIMMIC, color=set)) + geom_point(alpha=1, size=1.5) + 
  my_theme + sc +
  ylab(expression(paste("MIMICS Microbial C (kg ", m^-2, ")"))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 0.8, by = 0.2)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis(labels = NULL)) +
  xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")")))
p2

p3 <- ggplot(MCf_best, aes(x=SOC,y=MIMLIT, color=set)) + geom_point(alpha=1, size=1.5) + 
  my_theme + sc +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis(labels = NULL)) +
  scale_y_continuous(limits = c(0, 3.3), breaks = seq(0, 3, by = 0.5)) +
  xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")"))) +
  ylab(expression(paste("MIMICS Litter C (kg ", m^-2, ")")))
p3

p4 <- ggplot(MCf_best, aes(x=SOC,y=SOMp, color=set)) + geom_point(alpha=1, size=1.5) + 
  my_theme + rmx_x_axis + sc +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 2), sec.axis = dup_axis()) +
  xlab(expression(paste("Observed Total SOC (kg ", m^-2, ")"))) +
  ylab(expression(paste("MIMICS Protected SOC (kg ", m^-2, ")")))
p4

png(file="Figures/Fig4_Scen5GEP_Estimate_Space.png", width=5000, height=4000, units="px", res=600)

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
          common.legend = T)
dev.off()




