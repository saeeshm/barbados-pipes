# Name: Saeesh Mangwani
# Date: 2020-05-02
# Description: A script that handles the cleaning and manipulation of the spatial datasets needed for the shiny app

# ==== Loading libraries ====
# A family of fundamental packages that assist with the manipulation, tidying and visualization of data in R
library(tidyverse)
library(sf)
library(tmap)
library(readr)

# Reading data
pipes <- read_csv("data/final_db.csv")
bwa_districts <- read_sf("data/Bwa_districts.shp")
pipes_sdb <- read_sf("data/Final_Database_2020.shp")
bb_parish <- read_sf("data/BB_parishes.shp")

# 