library(dplyr)

# Set directory with map chunk files
setwd("C:/github/MIMICS_HiRes/Mapping/Map_data_out")

# Find all files from a specific named set
files <- list.files(pattern = "\\pset1")

### One line way to combine chunks
# all_data <- do.call('rbind', lapply(list.files(pattern = "pset1", full.names = TRUE), readRDS))


### Loop way to combine chunks
all_data <- NULL
for(i in 1:length(files)) {
  print(i)
  df_in <- readRDS(files[i])
  all_data <- rbind(all_data, df_in)
}

