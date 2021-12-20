# MIMICS_HiRes

---



## Overview
Process-based models, such as the Microbial Mineral Carbon Stabilization (MIMICS) model, are widely used for estimating soil carbon (C) stocks and projecting soil C responses to changing conditions. However, the complexity of model parameterization presents a common roadblock for users of the model. Here, I've addressed this problem by creating a machine learning algorithm for parameterization of the MIMICS model. With the required forcing and calibration data, users may run the algorithm to find the parameters that best align model estimates of bulk SOC with field measures, while also ensuring underlying soil C pools fit within known or expected ranges. 

The repository also includes scripted spatial extrapolation algorithms to produce continuous-spatial maps of MIMICS estimates of soil C, litter C, microbial C, and protected C based on input rasters pertianing to the required model forcing data. The scripts are also capable of mapping the projected response of soil C stocks and the underlying C pools to changes in environmental conditions.  

The parameterization algorithm is best run on a high performance computing (HPC) platform. However, the computing power required is not overwhelming to the extent that the routine cannot be run on most modern desktop computers.

</br>

#### Required forcing data for the MIMICS model:
1) Annual net primary productivity (ANPP)
2) Mean annual soil temperature (TSOI)
3) Soil clay content
4) Dominant litter type lignin:N ratio

</br>

## General steps for reproducing analyses and MIMICS model products

### 1. Generate dataset of 0-30 cm soil C stocks.

  * Harmonized soil data was originally downloaded from the Reynolds Creek Experimental Watershed and Critical Zone Observatory Database at http://data.reynoldscreekczo.org/shiny/RCSoDaH/. The database (.rds) was then processed in three steps (R scripts) to aggregate the available data into single values pertaining to the 0-30 cm stocks of SOC.

### 2. Find "best-fit" model parameters using a MCMC approach

 * Note: Using 31 cores on the ISU Supercomputer, and a calibration dataset with ~50 points, it takes ~2 hours to run the MCMC for 5000*3 iterations

### 3. Find "best-fit" model parameters using a MC approach

### 4. Using best-fit parameterization, create spatial maps from the MIMICS model 

   
