
#set working drive with scripts and database
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load SoDaH analyte avg ftn (brings in the three required ftns)
source("get_analyte_avgs_ftn.R")

## Load SoDaH database
# Get SoDaH .RDS from local path
dt1 <- readRDS("RC_database_current.rds")

# Run ftn to fet analyte avgs for all locations and depths, 
# as well as avgs to the specified depth
out <- get_analyte_avgs(target_database = dt1,
                 target_analyte_to_avg = "lyr_soc_stock", 
                 target_depth_cm = 30)

#Save data
write.csv(avg_toDepth_lyr_soc_stock_clean, "RC_SOC_Stocks_to_30cm_fix.csv")


#ggplot(avg_toDepth_lyr_soc_stock_clean, aes(x=analyte_avg, y=analyte_tot, color=sample_collector)) + geom_point() +
#  geom_abline(intercept = 0, slope = 1, linetype = "dashed")


