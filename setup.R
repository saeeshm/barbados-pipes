# Name: Saeesh Mangwani
# Date: 2020-06-08
# Description: A script that contains all the helper functions required for implementing the app

# ==== Loading libraries ====
# A family of fundamental packages that assist with the manipulation, tidying and visualization of data in R
library(tidyverse)
library(sf)

# ==== LOADING DATA ====
# Getting all the relevant basemaps and objects from the map script, and adding UID variables to ease querying on the map at a later stage

# Boundary files --------
bwa_districts <- read_sf("www/bwa_districts_latlong.shp") %>% mutate(UID = paste0("bwa",OBJECTID))
bb_parish <- read_sf("www/bb_parishes_latlong.shp") %>% mutate(UID = paste0("parish",OBJECTID))

# Pipe data --------

# Spatial
pipes <- read_sf("www/pipes_final.shp") %>% 
  # Selecting only relevant variables
  dplyr::select(c(OBJECTID = "OBJECTI", Subdistrict = "Sbdstrc", Diameter = "Diametr", Length = "Lngth_M", 
                  Pressure = "Prssr_A", CNX_Density = "CNXDnst", Soil_Type = "Sol_Typ", "Era", "Landuse", Landuse_Bin = "Lands_D",
                  rain_15yravg = "rn_15yr", rain_30yravg = "rn_30yr", rcp26_2035 = "r26_203", rcp26_2050 = "r26_205",
                  rcp45_2050 = "r45_203", rcp45_2035 = "r45_205", rcp85_2035 = "r85_203", rcp85_2050 = "r85_205")) %>% 
  dplyr::mutate(UID = paste0("pipe", OBJECTID))

# Non-spatial
# pipes_csv <- read_csv("www/pipes_final.csv") %>%
#   # selecting only the relevant variables (those related to the rainfall scenario)
#   dplyr::select(OBJECTID, Diameter, Landuse = "Landuse_Du", Pressure = "Pressure_A", rain_15yravg, rain_30yravg,
#                 rcp26_2035, rcp26_2050, rcp45_2035, rcp45_2050, rcp85_2035, rcp85_2050) %>%
#   # Adding a month variable and setting it to a default value of one for now. This will be manipulated later.
#   dplyr::mutate(month = 1) %>%
#   dplyr::mutate(UID = paste0("pipe", OBJECTID))

# ==== DEFINING GLOBAL VARIABLES ====

# A helper global variable to store the text description of the background layers based on the hover interaction. Defaults to an empty string
textOut <- character(0)
# A variable that stores whether or not the mouseout value is valid. Used when querying the base boundary layers for data, in the "properties" box in
# the UI
mouseout_valid <- 1
mousover <- NULL
mouseout <- NULL

# Colour Palettes: --------

# A discrete color palette for the basemaps
pal_disc <- colorFactor(palette = "viridis", domain = bwa_districts$OBJECTID)

# Another bin-based colour palette to categorize pipe risk
pal_bin <- colorBin(palette = c("#F1F1F1", "#FFFFB8", "#ED9000", "#E80000"), domain = c(0,4), bins = 4, pretty = F, reverse = F)

# An inverted palette for use with the legend, since there seems to be some problem
pal_bin_invert <- colorBin(palette = c("#F1F1F1", "#FFFFB8", "#ED9000", "#E80000"), domain = c(0,4), bins = 4, pretty = F, reverse = T)

# Defining a vector of rainfall values with names that can allow the user to select a rainfall scenario
vars <- c(
  "15-year average monthly rainfall" = "rain_15yravg",
  "30-year average monthly rainfall" = "rain_30yravg",
  "Projected average monthly rainfall in 2035, RCP 2.6" = "rcp26_2035",
  "Projected average monthly rainfall in 2035, RCP 4.5" = "rcp45_2035",
  "Projected average monthly rainfall in 2035, RCP 8.5" = "rcp85_2035",
  "Projected average monthly rainfall in 2050, RCP 2.6" = "rcp26_2050",
  "Projected average monthly rainfall in 2050, RCP 4.5" = "rcp45_2050",
  "Projected average monthly rainfall in 2050, RCP 8.5" = "rcp85_2050"
)

# ==== DEFINING HELPER FUNCTIONS ====

# A function to transform numeric risk scores into categorical risk scores
get_risk_category <- function(num){
  case_when(
    num < 1 ~ "Low",
    (num >= 1) & (num < 2) ~ "Medium",
    (num >= 2) & (num < 3) ~ "High",
    num >= 3 ~ "Peak"
  )
}

# A function that checks which layer a mouseover ID belongs to and formats the appropriate HTML container for describing it
getFeatureInfo <- function(uid){
  text <- character(0)
  if(str_detect(uid, "bwa")){
    feature <- bwa_districts[bwa_districts$UID == uid,]
    text <- paste0("<b>District Name: </b>", feature$DISTR_NAME, "<br>", "<b>Area: </b>", round(feature$Shape_Area, 2), " sqr m")
  }else{
    feature <- bb_parish[bb_parish$UID == uid,]
    text <- paste0("<b>District Name: </b>", feature$NAME, "<br>", "<b>Area: </b>", round(feature$Shape_Area, 2), " sqr m")
  }
  return(text)
}
