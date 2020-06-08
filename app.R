# Name: Saeesh Mangwani
# Date: 2020-05-07
# Description: A shiny web application visualizing risk in the municipal system on the island of Barbados

# ==== Loading libraries ====
library(shiny)
library(tidyverse)
library(sf)
library(readr)
library(leaflet)
source("fuzzy.R")

# Getting all the relevant basemaps and objects from the map script
# Boundary files --------
bwa_districts <- read_sf("www/bwa_districts_latlong.shp") %>% mutate(UID = paste0("bwa",OBJECTID))
bb_parish <- read_sf("www/bb_parishes_latlong.shp") %>% mutate(UID = paste0("bwa",OBJECTID))

# Pipe data --------
# Spatial
pipes <- read_sf("www/pipes_final.shp") %>% 
    # Selecting only relevant variables
    dplyr::select(c(OBJECTID = "OBJECTI", Subdistrict = "Sbdstrc", Diameter = "Diametr", Length = "Lngth_M", 
                    Pressure = "Prssr_A", CNX_Density = "CNXDnst", Soil_Type = "Sol_Typ", "Era", "Landuse", 
                    rain_15yravg = "rn_15yr", rain_30yravg = "rn_30yr", rcp26_2035 = "r26_203", rcp26_2050 = "r26_205",
                    rcp45_2050 = "r45_203", rcp45_2035 = "r45_205", rcp85_2035 = "r85_203", rcp85_2050 = "r85_205"))
# Non-spatial
pipes_csv <- read_csv("www/pipes_final.csv") %>% 
    # selecting only the relevant variables (those related to the rainfall scenario)
    dplyr::select(OBJECTID, Diameter, Landuse = "Landuse_Du", Pressure = "Pressure_A", rain_15yravg, rain_30yravg, 
                  rcp26_2035, rcp26_2050, rcp45_2035, rcp45_2050, rcp85_2035, rcp85_2050) %>% 
    # Adding a month variable and setting it to a default value of one for now. This will be manipulated later.
    dplyr::mutate(month = 1)

# ==== Creating color palettes for maps ====

# A discrete color palette for the basemaps
pal_disc <- colorFactor(palette = "viridis", domain = bwa_districts$OBJECTID)

# Another bin-based colour palette to categorize pipe risk
pal_bin <- colorBin(palette = c("#F1F1F1", "#FFFFB8", "#ED9000", "#E80000"), domain = c(0,4), bins = 4, pretty = F, reverse = F)

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

# A function to transform numeric risk scores into categorical risk scores
get_risk_category <- function(num){
    case_when(
        num < 1 ~ "Low",
        (num >= 1) & (num < 2) ~ "Medium",
        (num >= 2) & (num < 3) ~ "High",
        num >= 3 ~ "Peak"
    )
}

# Define UI for application - using a navbar page
ui <- navbarPage(
    title = "Vizualizing risk in the barbados pipe network",
    id = "nav",
    # The first tab which draws the map using Parishes
    tabPanel("Parishes", 
             div(class = "outer",
                 # Calling the styles.css stylesheet
                 tags$head(includeCSS("styles.css")),
                 # Adding the font catamaran from google fonts
                 tags$link(rel = "stylesheet", href="http://fonts.googleapis.com/css?family=Catamaran"),
                 
                 # Setting the output of the map to cover the whole screen using the leaflet output command
                 leafletOutput("parish_map", height = "100%"))
             ),
    # The second tab, which draws the map using BWA districts as boundaries
    tabPanel("BWA Districts", 
             div(class = "outer",
                 # Including the same stylesheets, map options and panel options
                 tags$head(includeCSS("styles.css")),
                 tags$link(rel = "stylesheet", href="http://fonts.googleapis.com/css?family=Catamaran"),
                 
                 leafletOutput("bwa_map", height = "100%"))
             ),
    
    # Creating a floating panel that contains the filtering options we want
    absolutePanel(id = "filters", class = "panel panel-default filters", draggable = F, top = 60, left = 10, 
                  right = "auto", bottom = "auto", width = "20%", height = "auto", fixed = T,
                  # The title
                  h2("Filtering Parameters"),
                  
                  # Selection filter for soil type
                  selectInput("soil", "Soil type", unique(pipes$Soil_Type), multiple = T),
                  
                  # Subdistrict
                  # selectInput("subdistrict", "Sub-District", unique(pipes$Subdistrict), multiple = F),
                  
                  # Era
                  selectInput("era", "Era built", unique(pipes$Era), multiple = T),
                  
                  # Landuse
                  checkboxGroupInput("landuse", "Landuse", unique(pipes$Landuse), inline = T, width = "auto"),
                  
                  # Length (as a range slider)
                  sliderInput("length", "Pipe length (meters)", 
                              min = floor(range(pipes$Length)[1]), 
                              max = ceiling(range(pipes$Length)[2]),
                              # Setting the default range to be bewteen 1/4 and 3/4 of the total range
                              value = (c((ceiling(range(pipes$Length)[2]) - floor(range(pipes$Length)[1]))/4, 
                                         ((ceiling(range(pipes$Length)[2]) - floor(range(pipes$Length)[1]))/4) * 3)),
                              dragRange = T),
                  
                  # Diameter (as a range slider)
                  sliderInput("diameter", "Diameter (inches)",
                              min = floor(range(pipes$Diameter)[1]), 
                              max = ceiling(range(pipes$Diameter)[2]),
                              # Setting the default value range to be bewteen 1/4 and 3/4 of the total range
                              value = (c((ceiling(range(pipes$Diameter)[2]) - floor(range(pipes$Diameter)[1]))/4, 
                                         ((ceiling(range(pipes$Diameter)[2]) - floor(range(pipes$Diameter)[1]))/4) * 3)),
                              dragRange = T)
                  ),
    # Creating a floating panel that contains the climate scenario options we want
    absolutePanel(id = "rainfall", class = "panel panel-default rainfall", fixed = TRUE,
                  draggable = FALSE, top = 60, left = "auto", right = 10, bottom = "auto",
                  width = "20%", height = "auto",
                  
                  selectInput("rain", "Rainfall scenario", vars, selected = "rain_15yravg"),
                  sliderInput(inputId = "month", label = "Month of the year", min = 1, max = 12, value = 6)
                  ),
    absolutePanel(id = "boundaries", class = "panel panel-default boundaries", fixed = TRUE,
                  draggable = F, top = 160, left = "auto", right = 10, bottom = "auto", width = "20%", height = "auto",
                  
                  textOutput("properties")
                  )
    )

# Define server logic required to draw the map and implement filter conditions
server <- function(input, output) {
    
    # Drawing the base-map using the parishes as base
    output$parish_map <- renderLeaflet({
        leaflet(bb_parish %>% arrange(OBJECTID), options = leafletOptions(zoomControl = FALSE)) %>% 
            # htmlwidgets::onRender("function(el, x) {
            #     L.control.zoom({ position: 'bottomright' }).addTo(this)
            # }") %>%
            # Adding the background tileset
            addProviderTiles(providers$Esri.WorldShadedRelief) %>% 
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.4)) %>% 
            # Adding the boundary polygons and applying the appropriate colour palette so that each one is coloured uniquely.
            addPolygons(layerId = bb_parish %>% arrange(OBJECTID) %>% pull(UID),
                        group = "parish",
                        color = "#1A1A1A", 
                        fillColor = ~pal_disc(bb_parish$OBJECTID),
                        weight = 0.8, 
                        smoothFactor = 0.5,
                        opacity = 0.8, 
                        fillOpacity = 0.5) %>%
            # Adding the legend for the risk scores
            addLegend(position = "bottomright", 
                      pal = pal_bin, 
                      values = 1:4,
                      title = "Burst risk",
                      opacity = 0.9,
                      labFormat = function(type, cuts, p) {  # Here's the trick
                          paste0(c("Peak", "High", "Medium", "Low"))
                      })
    })
    
    # The map using the bwa districts as base
    output$bwa_map <- renderLeaflet({
        leaflet(bwa_districts %>% arrange(OBJECTID), options = leafletOptions(zoomControl = FALSE)) %>% 
            # Adding the background tileset
            addProviderTiles(providers$Esri.WorldShadedRelief) %>% 
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.4)) %>% 
            # Adding the boundary polygons and applying the appropriate colour palette so that each one is coloured uniquely.
            addPolygons(layerId = bwa_districts %>% arrange(OBJECTID) %>% pull(UID),
                        color = "#1A1A1A", 
                        fillColor = ~pal_disc(bwa_districts$OBJECTID),
                        weight = 0.8, 
                        smoothFactor = 0.5,
                        opacity = 0.8, 
                        fillOpacity = 0.5) %>%
            # Adding the legend for the risk scores
            addLegend(position = "bottomright", 
                      pal = pal_bin, 
                      values = 0:4,
                      title = "Burst Risk",
                      opacity = 1.0,
                      labFormat = function(type, cuts, p) {  # Here's the trick
                          paste0(c("Peak", "High", "Medium", "Low"))
                      })
    })
    
    # Creating a reactive object to store the outputs of the the filtering selectors, to only select the pipes that are actually requested
    map_filter <- reactive({
        pipes %>% 
            filter(Soil_Type %in% if_else(is.null(input$soil), 
                                          list(unique(pipes$Soil_Type)), 
                                          list(input$soil), 
                                          list(unique(pipes$Soil_Type)))[[1]]
            ) %>% 
            filter(Era %in% if_else(is.null(input$era),
                                    list(unique(pipes$Era)),
                                    list(input$era),
                                    list(unique(pipes$Era)))[[1]]
            ) %>%
            filter(Landuse %in% if_else(is.null(input$landuse),
                                        list(unique(pipes$Landuse)),
                                        list(input$landuse),
                                        list(unique(pipes$Landuse)))[[1]]
            ) %>% 
            filter((Length >= input$length[1]) & (Length <= input$length[2])) %>%
            filter(Diameter >= input$diameter[1] & Diameter <= input$diameter[2])
    })
    
    # Using the inputs from the climate scenario panel to calculate the correct risk scores and storing these in a reactive object.Note that we
    # calculated for the entire dataset every time the user asks for a new scenario
    risk <- reactive({
        pipes_csv %>% 
            rowwise() %>%
            mutate(risk = evalfis(matrix(c(Diameter, Landuse, input$month, Pressure, get(input$rain)), 1, 5), fis)[1]) %>% 
            select(OBJECTID, risk)
    })
    
    # Adding an observe event for the parish map that waits for changes to the risk variables
    observe({
        # Storing the filtered pipes from the reactive object in a temporary variable 
        temp <- map_filter()
        # Assigning a column for risk, by taking the values from the risk reactive objective. First filtering out only those risk scores that are in
        # the new filtered set and attaching them to the dataset
        temp$risk <- risk() %>% 
            filter(OBJECTID %in% temp$OBJECTID) %>% 
            pull(risk)
        # When triggered by a change in the risk calculation or a change in the filter, it removes the old pipes layer and redraws a new one on top
        leafletProxy("parish_map", data = temp) %>%
            clearGroup("pipe_parish") %>%
            addPolylines(group = "pipe_parish",
                         color = ~pal_bin(risk),
                         weight = 4,
                         smoothFactor = 2,
                         opacity = 1.0,
                         # Adding a highlight on hover
                         highlightOptions = highlightOptions(color = "White", 
                                                             weight = 6,
                                                             bringToFront = TRUE),
                         # Adding a popup
                         popup = paste0("<b>Location: </b>",
                                        temp$Subdistrict,
                                        "<br>",
                                        "<b>Diameter: </b>",
                                        temp$Diameter, " mm",
                                        "<br>",
                                        "<b>Pressure: </b>",
                                        temp$Pressure, " Pa",
                                        "<br>",
                                        "<b>Soil type: </b>",
                                        temp$Soil_Type,
                                        "<br>",
                                        "<b>Era built: </b>",
                                        temp$Era,
                                        "<br>",
                                        "<b>Average monthly rainfall (based on chosen scenario): </b>",
                                        round(temp[[isolate(input$rain)]], 2),
                                        "<br>",
                                        "<b>Risk category: </b>",
                                        get_risk_category(temp$risk),
                                        "<br>",
                                        "<b>Risk score: </b>",
                                        round(temp$risk, 2)
                         )
            )
    })
    
    # A similar observe event for the bwa districts map
    observe({
        # Storing the filtered pipes from the filtering reactive object in a temporary variable 
        temp <- map_filter()
        
        # Assigning a column for risk, by taking the values from the risk reactive objective. First filtering out only those risk scores that are in
        # the new filtered set and attaching them to the dataset
        temp$risk <- risk() %>% 
            filter(OBJECTID %in% temp$OBJECTID) %>% 
            pull(risk)
        # When triggered by a change in the risk calculation or a change in the filter, it removes the old pipes layer and redraws a new one on top
        leafletProxy("bwa_map", data = temp) %>%
            clearGroup("pipe_bwa") %>%
            addPolylines(group = "pipe_bwa",
                         color = ~pal_bin(risk),
                         weight = 4,
                         smoothFactor = 2,
                         opacity = 1.0,
                         # Adding a highlight on hover
                         highlightOptions = highlightOptions(color = "white",
                                                             weight = 6,
                                                             bringToFront = TRUE),
                         # Adding a popup
                         popup = paste0("<b>Location: </b>",
                                        temp$Subdistrict,
                                        "<br>",
                                        "<b>Diameter: </b>",
                                        temp$Diameter, " mm",
                                        "<br>",
                                        "<b>Pressure: </b>",
                                        temp$Pressure, " Pa",
                                        "<br>",
                                        "<b>Soil type: </b>",
                                        temp$Soil_Type,
                                        "<br>",
                                        "<b>Era built: </b>",
                                        temp$Era,
                                        "<br>",
                                        "<b>Average monthly rainfall (based on chosen scenario): </b>",
                                        round(temp[[isolate(input$rain)]], 2),
                                        "<br>",
                                        "<b>Risk category: </b>",
                                        get_risk_category(temp$risk),
                                        "<br>",
                                        "<b>Risk score: </b>",
                                        round(temp$risk, 2)
                         )
            )
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
