# MIMICS_HiRes

Scripts and data stored in this repository support the following publication:

## Overview
....

## Basic steps for reproducing analyses and MIMICS model products

### 1. Generate dataset of 0-30 cm soil C stocks.

  * Harmonized soil data was originally downloaded from the Reynolds Creek Experimental Watershed and Critical Zone Observatory Database at http://data.reynoldscreekczo.org/shiny/RCSoDaH/. The database (.rds) was then processed in three steps (R scripts) to aggregate the available data into single values pertaining to the 0-30 cm stocks of SOC.

### 2. Find "best-fit" model parameters using a MCMC approach

 * Note: Using 31 cores on the ISU Supercomputer, and a calibration dataset with ~50 points, it takes ~2 hours to run the MCMC for 5000*3 iterations

### 3. Find "best-fit" model parameters using a MC approach

### 4. Using best-fit parameterization, create spatial maps from the MIMICS model 

   
