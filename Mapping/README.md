**MIMICS_map_builder.R:** Creates rasters for MIMICS output from raw model output.

**MIMICS_map_data_generator.R:** Converts raster forcing data into a .csv file that can be used to run MIMICS.

**MIMICS_soil_C_mapper.R:** Runs MIMICS for each row in the forcing data input generated from the MIMICS_map_data_generator.R script.

**MIMICS_soil_C_mapper_chunked.R:** Same as MIMICS_soil_C_mapper.R, but the process is completed in small chunks to avoid losing data is the MIMICS_soil_C_mapper.R crashes. Chunks can be recombined into one dataset using Helpful_scripts/MIM_map_chunk_combine.R
