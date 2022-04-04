The scripts here may be used to find a parameterization for MIMICS given a specific calibration dataset. The Markov Chain Monte Carlo algorithm may generate proposals in three ways: 

**All:** Each proposal contains a new value for each of the 8 parameters.

**Group:** Each proposal changes the values for only 1 of 3 groups of parameters. Parameter groups are based on if the parameter pertains to microbial catabolism, anabolism, or protected SOM turnover.

**Single:** Sequential cycle where only one parameter value is changed in each proposal.

Proposals are selected if a lower RMSE between estimates and observations of SOC stocks is found. Model output must also fit within defined range of values for pool sizes, SOMp turnover, etc.

See methods section in manuscript for further details.

#### MCMC method info from manuscript prior to final revision:

The flow of C through the MIMICS model is controlled by a number of rate parameters (Table 1, Fig. 1; Wieder et al. 2015). These parameters determine microbial kinetics (via temperature sensitive Vmax and Kes) that determine decomposition rates, microbial growth efficiency, and mineral reactivity (as approximated by soil texture). We employed a Hamiltonian Markov Chain Monte Carlo (MCMC) algorithm (Betancourt 2019) to determine the best fit values for eight of the parameters that have the strongest control over litter, microbial biomass, and soil C pools. 
Initially, prior distributions for the MCMC were given a mean and standard deviation equal to the parameter values provided by Wieder et al. 2015. After using the initial MCMC algorithm for extensive observation of the viable parameter proposal space associated with the Reynolds Creek calibration data, we chose to set minimum and maximum limits on the parameter proposals (i.e. the scaling factor proposal range, see Table 1). Prior distribution means were then selected at random from within these limits. To improve the proposal sampling range while also conserving computational demands, we made a few other modifications to the proposal selection process. The standard deviation of the prior proposal distribution was set to initially equal 20% of the mean. Then, but after each successive rejected proposal, the distribution standard deviation was set to increase by 0.1%. Further, the MCMC walk rate was also used to confine 90% of proposals within a factor of 2 from the current accepted value, while the other 10% of proposals had a random chance to propose any value within the parameter limits. Less than 10,000 individual parameter proposals (80,000 total) were typically required to reach a solution. The MCMC was considered complete after 10,000 proposal iterations without improvement in the cost function.
The cost function for the MCMC method included a set of binary filters to confirm proposed model parameterizations produced plausible average values across the calibration dataset for microbial C (1-3% of the total SOC), the relative abundance of microbial r- vs. K-strategist (0.1-10), litter C stocks (10-40% of total SOC), and the turnover time of C in the protected SOM pool (75-400 years). If the binary filter conditions were met, the proposed parameter combination was selected if the root mean square error (RMSE) between field and estimated SOC stocks was reduced relative to the previously accepted parameterization. 
In practice, the best-fit parameterizations determined over multiple MCMC runs were not consistent (Fig. S1). We initially assumed this may be due to the prior distributions for the MCMC algorithm. However, attempts at adjusting the prior distributions, reducing the proposal walk rate, and limiting the proposal range for each parameter, yielded similar inconsistent results, suggesting rather that substantial equifinality exists in the model construct. 



