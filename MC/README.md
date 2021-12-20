**MIM_MC_run.R** runs MIMICS for a given calibration dataset using randomly generated parameter combinations for the model. 

Random parameters are chosen based on a defined multiplier range of the defualt parameter value (e.g. Vslope is confined within 0.4-4x the default value). 

The default number of random parameter combinations to run is 500,000, but may be adjusted based on need and computing power.

Script is currently written for running in and HPC environemnt. If running locally, a working drive may need to be set.

Required packages are loaded in the sourced script MIMICS_base_ftn.R, which is itself sourced from MIMICS_repeat_base.R. These scripts may be found in the MIMICS_ftns folder.

The **Post_MC_Analysis folder** contains scripts used for summarizing and plotting the output from the MIM_MC_run.R script.
