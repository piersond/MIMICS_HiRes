The scripts here may be used to find a parameterization for MIMICS given a specific calibration dataset. The Markov Chain Monte Carlo algorithm may generate proposals in three ways: 

**All:** Each proposal contains a new value for each of the 8 parameters.

**Group:** Each proposal changes the values for only 1 of 3 groups of parameters. Parameter groups are based on if the parameter pertains to microbial catabolism, anabolism, or protected SOM turnover.

**Single:** Sequential cycle where only one parameter value is changed in each proposal.

Proposals are selected if a lower RMSE between estimates and observations of SOC stocks is found. Model output must also fit within defined range of values for pool sizes, SOMp turnover, etc.

See methods section in manuscript for further details.
