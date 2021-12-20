# MIMICS_HiRes

---


## Overview
*Repository supports manuscript submitted to Nature Communications for publication. Link to manuscript will be provided following official publication.*

Process-based models, such as the Microbial Mineral Carbon Stabilization (MIMICS) model, are widely used for estimating soil carbon (C) stocks and projecting soil C responses to changing conditions. However, the complexity of model parameterization presents a common roadblock for users of the model. Here, I've addressed this problem by creating a machine learning algorithm for parameterization of the MIMICS model. With the required forcing and calibration data, users may run the algorithm to find the parameters that best align model estimates of bulk SOC with field measures, while also ensuring underlying soil C pools fit within known or expected ranges. 

The repository also includes scripted spatial extrapolation algorithms to produce continuous-spatial maps of MIMICS estimates of soil C, litter C, microbial C, and protected C based on input rasters pertianing to the required model forcing data. The scripts are also capable of mapping the projected response of soil C stocks and the underlying C pools to changes in environmental conditions.  

The parameterization algorithm is best run on a high performance computing (HPC) platform. However, the computing power required is not overwhelming to the extent that the routine cannot be run on most modern desktop computers.

</br>

#### Required data for running the MIMICS model:
1) Annual net primary productivity (ANPP).
2) Mean annual soil temperature (TSOI).
3) Soil clay content (CLAY).
4) Dominant litter type lignin:N ratio (LIG_N).
5) Field measures of SOC stocks also required for model calibration.

*Point-based data is used for model parameterization.* 
*Continuous raster data is required for spatial estimates and projections.*

</br>

## Instructions

**Each folders contains script descriptions and instructions. Scripts also include many useful comments.**

**Figures:** Scripts and images used for associated manuscript figures. </br>

**Helpful scripts:** Miscellaneous scripts used for MIMICS runs, mapping and summary statistics. <br/>

**MC:** Script for running a Monte Carlo simulation with randomized MIMICS parameters.<br/>

**MCMC:** Scripts for running a Markov Chain Monte Carlo to find the best-fit MIMICS parameters for a given set of calibration data.<br/>

**MIMICS_ftns:** Functions used to run MIMICS. Model matches the published model code [here](https://github.com/wwieder/MIMICS/tree/sandbox). <br/>

**Mapping:** Scripts for the creating continuous maps of MIMICS estimates from raster forcing data.</br>

**RCrk_Data_Aggregation:** Scripts used to collect data for modeling and mapping SOC stocks across the Reynolds Creek Experimental Watershed and Critical Zone Observatory (RCEW-CZO).</br>

**RCrk_Modelling_Data:** Specific forcing data .csv files used for original project and associated publication.</br>

</br>

## General steps for reproducing analyses and MIMICS model products

### 1. Collect forcing data for the  model and coressponding field measures of 

  * Harmonized soil data was originally downloaded from the Reynolds Creek Experimental Watershed and Critical Zone Observatory Database at http://data.reynoldscreekczo.org/shiny/RCSoDaH/. The database (.rds) was then processed in three steps (R scripts) to aggregate the available data into single values pertaining to the 0-30 cm stocks of SOC. See RCrk_Data_Aggregation folder.

### 2. Find "best-fit" model parameters using a Markov Chain Monte Carlo algorithm.

 * Useful for obtaining a single best-fit parameterization. See MCMC folder Readme for instructions.
 * Note: MCMC algorithm has been found to return different results in separate runs using the same forcing data (equifinality). See manuscript. 
 * Note: Using 31 cores on the Idaho State University HPC platform, and a calibration dataset with ~50 points, it takes ~2 hours to run the MCMC for 5000x3 iterations.

### 3. Analyze parameterization space (i.e. parametric uncertainty) using a Monte Carlo simulation.

 * Useful for determining the how many viable model parameterizations exist for specific calibration dataset. See MC folder for further instructions. 

### 4. Create spatial maps from the MIMICS model 

 * Once a suitable model parameterization is found, continuous rasters coinciding with the calibration dataset can be used to generate continuous MIMICS estimates and projections. See Mapping folder for additional information.



   
