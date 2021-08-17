# MIMICS_HiRes

---

## Overview
Process base models are the most viable method for estimating soil C stocks and projecting soil C responses to changing conditions. However, while process model constructs are intentionally simplified versions of real world dynamics, the complexity of model parameterization presents a barrier for wider use by scientific and land management communities. Here, we've addressed that problem by creating a machine learning algorithm for parameterization of the MIMICS model, which has been folded into an automated routine. With the required data input from a user, the routine can parameterize MIMICS, fitting not only bulk SOC to field measures, but also the underlying soil C pools to known or expected ranges. (The parameterization routine is best run on a high performance computing platform, however the computing power required is not overwhelming to the extent that the routine cannot be run on most modern desktop computers). 

We’ve packaged the parameterization algorithm with a spatial data processing routine that is capable of producing spatial estimate maps of 0-30 cm soil C, litter C, microbial C, and protected C down to resolutions of 10 m2. The routine can also project and map the sensitivity of soil C stocks and the underlying C pools to changes in environmental conditions.  

## Basic steps for reproducing analyses and MIMICS model products

### 1. Generate dataset of 0-30 cm soil C stocks.

  * Harmonized soil data was originally downloaded from the Reynolds Creek Experimental Watershed and Critical Zone Observatory Database at http://data.reynoldscreekczo.org/shiny/RCSoDaH/. The database (.rds) was then processed in three steps (R scripts) to aggregate the available data into single values pertaining to the 0-30 cm stocks of SOC.

### 2. Find "best-fit" model parameters using a MCMC approach

 * Note: Using 31 cores on the ISU Supercomputer, and a calibration dataset with ~50 points, it takes ~2 hours to run the MCMC for 5000*3 iterations

### 3. Find "best-fit" model parameters using a MC approach

### 4. Using best-fit parameterization, create spatial maps from the MIMICS model 

   
