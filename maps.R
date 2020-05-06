# Name: Saeesh Mangwani
# Date: 2020-05-02
# Description: A script that handles the cleaning and manipulation of the spatial datasets needed for the shiny app

# ==== Loading libraries ====
# A family of fundamental packages that assist with the manipulation, tidying and visualization of data in R
library(tidyverse)
library(sf)
library(raster)
library(tmap)
library(readr)
library(leaflet)

# Reading data as sf objects
bwa_districts <- read_sf("data/Bwa_districts_latlong.shp")
pipes <- read_sf("data/pipes_final.shp")
bb_parish <- read_sf("data/BB_parishes_latlong.shp")

# Calling the colorFactor function that can return a 'pal' function which generates a color palette based on an input length
pal <- colorFactor(
  palette = "viridis",
  domain = bwa_districts$OBJECTID
)

# Creating a basemap using BWA districts as divisions
district_base <- leaflet(bwa_districts) %>% 
  addPolygons(color = ~pal(OBJECTID), 
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0("<b>Name: </b>",
                             bwa_districts$DISTR_NAME,
                             "<br>",
                             "<b>Area: </b>",
                             round(bwa_districts$Shape_Area, 2), " sqr m")
  )
# Creating a basemap using parishes as divisions
parish_base <- leaflet(bb_parish) %>% 
  addPolygons(color = ~pal(OBJECTID), 
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 2,
                                                  bringToFront = TRUE),
              popup = paste0("<b>Name: </b>",
                             bb_parish$NAME,
                             "<br>",
                             "<b>Population (In 1990): </b>",
                             bb_parish$POP_1990,
                             "<br>",
                             "<b>Area: </b>",
                             round(bb_parish$Shape_Area, 2), " sqr m")
  )

leaflet(pipes_sdb) %>% addPolylines()
