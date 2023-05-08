# Name: Saeesh Mangwani
# Date: 2020-05-05
# Description: A script that reads and prepares the raster rainfall layers and adds their data to pipe database

# ==== Loading libraries ====
library(tidyverse)
library(sf)
library(raster)

# Reading raster files for all 8 rainfall scenarios under consideration
rain_15yravg <-  raster("data/rasters/rain_15yravg.tif")
rain_30yravg <-  raster("data/rasters/rain_30yravg.tif")
rcp26_2035 <-  raster("data/rasters/rcp26_2035.tif")
rcp26_2050 <- raster("data/rasters/rcp26_2050.tif")
rcp45_2035 <- raster("data/rasters/rcp45_2035.tif")
rcp45_2050 <- raster("data/rasters/rcp45_2050.tif")
rcp85_2035 <- raster("data/rasters/rcp85_2035.tif")
rcp85_2050 <- raster("data/rasters/rcp85_2050.tif")

# Reading the relevant vector files - a line layer containing the pipes to which the rainfall data needs to be added, as well as a point dataset of
# all the vertices that make up the lines in the pipes database. Only point data can be used (efficiently) to extract data from raster files, which is
# why it is used
pipes_sdb <- read_sf("data/pipes_latlong.shp")
pipe_vertices <- read_sf("data/pipe_vertices_latlong.shp") %>% dplyr::select(ORIG_FID)

# Extracting the rainfall values from each of the rainfall rasters that correspond to each of the points in the vertice file, and storing these
# rainfall values as attributes of each vertice. At the end of this block of code, the vertice layer has 8 new columns which each contain the rainfall
# (historical or projected) at that point under each of these different scenarios
# 15 and 30 year historical averages
pipe_vertices$rain_15yravg <- raster::extract(rain_15yravg, pipe_vertices)
pipe_vertices$rain_30yravg <- raster::extract(rain_30yravg, pipe_vertices)
# RCP 2.6 projections
pipe_vertices$rcp26_2035 <- raster::extract(rcp26_2035, pipe_vertices)
pipe_vertices$rcp26_2050 <- raster::extract(rcp26_2050, pipe_vertices)
# RCP 4.5 projections
pipe_vertices$rcp45_2035 <- raster::extract(rcp45_2035, pipe_vertices)
pipe_vertices$rcp45_2050 <- raster::extract(rcp45_2050, pipe_vertices)
# RCP 8.5 projections
pipe_vertices$rcp85_2035 <- raster::extract(rcp85_2035, pipe_vertices)
pipe_vertices$rcp85_2050 <- raster::extract(rcp85_2050, pipe_vertices)

# Grouping the vertices using the IDs of the lines they were originially part of, taking the *mean* of all the rainfall values associated with each
# point within a line group, thus resulting a single meaned rainfall value per line, for each scenario
rain_15yravg <- pipe_vertices %>% group_by(ORIG_FID) %>% summarise(rain_15yravg = mean(rain_15yravg)) %>% as_tibble() %>% select(-geometry)
rain_30yravg <- pipe_vertices %>% group_by(ORIG_FID) %>% summarise(rain_30yravg = mean(rain_30yravg)) %>% as_tibble() %>% select(-geometry)

rcp26_2035 <- pipe_vertices %>% group_by(ORIG_FID) %>% summarise(rcp26_2035 = mean(rcp26_2035)) %>% as_tibble() %>% select(-geometry)
rcp26_2050 <- pipe_vertices %>% group_by(ORIG_FID) %>% summarise(rcp26_2050 = mean(rcp26_2050)) %>% as_tibble() %>% select(-geometry)

rcp45_2035 <- pipe_vertices %>% group_by(ORIG_FID) %>% summarise(rcp45_2035 = mean(rcp45_2035)) %>% as_tibble() %>% select(-geometry)
rcp45_2050 <- pipe_vertices %>% group_by(ORIG_FID) %>% summarise(rcp45_2050 = mean(rcp45_2050)) %>% as_tibble() %>% select(-geometry)

rcp85_2035 <- pipe_vertices %>% group_by(ORIG_FID) %>% summarise(rcp85_2035 = mean(rcp85_2035)) %>% as_tibble() %>% select(-geometry)
rcp85_2050 <- pipe_vertices %>% group_by(ORIG_FID) %>% summarise(rcp85_2050 = mean(rcp85_2050)) %>% as_tibble() %>% select(-geometry)

# Randomly selecting pipes and plotting them between the original pipes dataset and the generated dataset to visually check that the grouping has
# occured correctly - This verification is commented out because it was done as a check before the datasets (once this has been done, they can no
# longer be plotted using the following functions)

# pipes_sdb %>% select(OBJECTID) %>% filter(OBJECTID == 131) %>% plot() rain15_yr %>%
# select(ORIG_FID) %>% filter(ORIG_FID == 131) %>% plot()
# pipes_sdb %>% select(OBJECTID) %>% filter(OBJECTID == 12) %>% plot() 
# rain15_yr %>% select(ORIG_FID) %>% filter(ORIG_FID == 12) %>% plot()

# Joining all of these datasets to the original line dataset using the original Object ID, adding all these raster attributes to the full pipes
# database
pipes_sdb_final <- pipes_sdb %>% 
  inner_join(rain_15yravg, by = c("OBJECTID" = "ORIG_FID")) %>% 
  inner_join(rain_30yravg, by = c("OBJECTID" = "ORIG_FID")) %>% 
  inner_join(rcp26_2035, by = c("OBJECTID" = "ORIG_FID")) %>% 
  inner_join(rcp26_2050, by = c("OBJECTID" = "ORIG_FID")) %>% 
  inner_join(rcp45_2035, by = c("OBJECTID" = "ORIG_FID")) %>% 
  inner_join(rcp45_2050, by = c("OBJECTID" = "ORIG_FID")) %>% 
  inner_join(rcp85_2035, by = c("OBJECTID" = "ORIG_FID")) %>% 
  inner_join(rcp85_2050, by = c("OBJECTID" = "ORIG_FID"))

# Writing this final pipes sdb as a shapefile - this is the data that will be used for the dashboard
st_write(pipes_sdb_final, "data/pipes_final.shp")
# Writing it also as a tibble to make data analysis easier later
write_csv(pipes_sdb_final %>% as_tibble() %>% select(-geometry), "data/pipes_final.csv")

# Now that the files have been saved to the directory, removing unnecessary objects and garbage collecting
rm(rain_15yravg, rain_30yravg, rcp26_2035, rcp26_2050, rcp45_2035, rcp45_2050, rcp85_2035, rcp85_2050, pipe_vertices, pipes_final)
gc()
