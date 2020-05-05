# Name: Saeesh Mangwani
# Date: 2020-05-05
# Description: A script that reads and prepares the raster rainfall layers and adds their data to pipe database

# ==== Loading libraries ====
# A family of fundamental packages that assist with the manipulation, tidying and visualization of data in R
library(tidyverse)
library(sf)
library(raster)
library(tmap)

# Reading raster files
rain_15yravg <-  raster("data/rasters/rain_15yravg.tif")
glimpse(rain_15yravg)

# Adding data from each as variables into the pipes database
pipes_sdb$rain_15yravg <- raster::extract(rain_15yravg, pipes_sdb)
