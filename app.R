#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Loading required libraries and scripts
library(shiny)
library(tidyverse)
library(sf)
library(readr)
library(leaflet)
# source("fuzzyModel_fuzzyR.R")

# Getting all the relevant basemaps and objects from the map script
# Boundary files --------
# bwa_districts <- read_sf("data/Bwa_districts_latlong.shp")
# bb_parish <- read_sf("data/BB_parishes_latlong.shp")

# Pipe data --------
# Spatial
pipes <- read_sf("data/pipes_final.shp") %>% 
    # Selecting only relevant variables
    dplyr::select(c(OBJECTID = "OBJECTI", Subdistrict = "Sbdstrc", Diameter = "Diametr", Length = "Lngth_M", 
                    Pressure = "Prssr_A", CNX_Density = "CNXDnst", Soil_Type = "Sol_Typ", "Era", "Landuse", 
                    rain_15yravg = "rn_15yr", rain_30yravg = "rn_30yr", rcp26_2035 = "r26_203", rcp26_2050 = "r26_205",
                    rcp45_2050 = "r45_203", rcp45_2035 = "r45_205", rcp85_2035 = "r85_203", rcp85_2050 = "r85_205"))
# Non-spatial
pipes_csv <- read_csv("data/pipes_final.csv") %>% 
    # selecting only the relevant variables (those related to the rainfall scenario)
    dplyr::select(OBJECTID, Diameter, Landuse = "Landuse_Du", Pressure = "Pressure_A", rain_15yravg, rain_30yravg, 
                  rcp26_2035, rcp26_2050, rcp45_2035, rcp45_2050, rcp85_2035, rcp85_2050) %>% 
    # Adding a month variable and setting it to a default value of one for now. This will be manipulated later.
    dplyr::mutate(month = 1)

# ==== Creating color palettes for maps ====

# A discrete color palette for the basemaps
pal_disc <- colorFactor(palette = "viridis", domain = bwa_districts$OBJECTID)

# Another bin-based colour palette to categorize pipe risk
pal_bin <- colorBin(palette = c("#E80000", "#ED9000", "#FFFFB8", "#F1F1F1"), domain = c(0,4), c(0,1,2,3,4), pretty = F, reverse = T)

# Defining a vector of rainfall values with names that can allow the user to select a rainfall scenario
vars <- c(
    "15-year Average Monthly Rainfall" = "rain_15yravg",
    "30-year Average Monthly Rainfall" = "rain_30yravg",
    "Projected Average Monthly Rainfall in 2035, RCP 2.6" = "rcp26_2035",
    "Projected Average Monthly Rainfall in 2035, RCP 4.5" = "rcp45_2035",
    "Projected Average Monthly Rainfall in 2035, RCP 8.5" = "rcp85_2035",
    "Projected Average Monthly Rainfall in 2050, RCP 2.6" = "rcp26_2050",
    "Projected Average Monthly Rainfall in 2050, RCP 4.5" = "rcp45_2050",
    "Projected Average Monthly Rainfall in 2050, RCP 8.5" = "rcp85_2050"
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

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "Vizualizing Risk in The Barbados Pipe Network",
    id = "nav",
    tabPanel("Parishes", 
             div(class = "outer",
                 tags$head(includeCSS("styles.css")),
                 tags$link(rel = "stylesheet", href="http://fonts.googleapis.com/css?family=Catamaran"),
                 
                 leafletOutput("parish_map", height = "100%"),
                 
                 absolutePanel(id = "filters", class = "panel panel-default filters", draggable = F, top = 60, left = 10, 
                            right = "auto", bottom = "auto", width = "20%", height = "auto", fixed = T,
                           
                            h2("Feature Selectors"),
                           
                            # Selection filter for soil type
                            selectInput("soil", "Soil Type", unique(pipes$Soil_Type), multiple = T),
                            
                            # Subdistrict
                            # selectInput("subdistrict", "Sub-District", unique(pipes$Subdistrict), multiple = F),
                            
                            # Era
                            selectInput("era", "Era", unique(pipes$Era), multiple = T),
                            
                            # Landuse
                            checkboxGroupInput("landuse", "Landuse", unique(pipes$Landuse), inline = T, width = "auto"),
                            
                            # Length
                            sliderInput("length", "Pipe Length (meters)", 
                                       min = floor(range(pipes$Length)[1]), 
                                       max = ceiling(range(pipes$Length)[2]),
                                       # Setting the default value range to be bewteen 1/4 and 3/4 of the total range
                                       value = (c((ceiling(range(pipes$Length)[2]) - floor(range(pipes$Length)[1]))/4, 
                                               ((ceiling(range(pipes$Length)[2]) - floor(range(pipes$Length)[1]))/4) * 3)),
                                       post = "m",
                                       dragRange = T),
                            
                            # Diameter
                            sliderInput("diameter", "Diameter (mm)",
                                       min = floor(range(pipes$Diameter)[1]), 
                                       max = ceiling(range(pipes$Diameter)[2]),
                                       # Setting the default value range to be bewteen 1/4 and 3/4 of the total range
                                       value = (c((ceiling(range(pipes$Diameter)[2]) - floor(range(pipes$Diameter)[1]))/4, 
                                               ((ceiling(range(pipes$Diameter)[2]) - floor(range(pipes$Diameter)[1]))/4) * 3)),
                                       post = "m",
                                       dragRange = T)
                            )
             )
    ),
    tabPanel("BWA Districts", 
             div(class = "outer",
                 tags$head(includeCSS("styles.css")),
                 tags$link(rel = "stylesheet", href="http://fonts.googleapis.com/css?family=Catamaran"),
                 
                 leafletOutput("bwa_map", height = "100%"),
                 
                 absolutePanel(id = "filters", class = "panel panel-default filters", fixed = TRUE,
                               draggable = F, top = 60, left = 10, right = "auto", bottom = "auto",
                               width = "20%", height = "auto",
                               
                               h2("Feature Selectors"),
                               
                               # Selection filter for soil type
                               selectInput("soil", "Soil Type", unique(pipes$Soil_Type), multiple = T),
                               
                               # Subdistrict
                               # selectInput("subdistrict", "Sub-District", unique(pipes$Subdistrict), multiple = F),
                               
                               # Era
                               selectInput("era", "Era", unique(pipes$Era), multiple = T),
                               
                               # Landuse
                               checkboxGroupInput("landuse", "Landuse", unique(pipes$Landuse), inline = T, width = "auto"),
                               
                               # Length
                               sliderInput("length", "Pipe Length (meters)", 
                                           min = floor(range(pipes$Length)[1]), 
                                           max = ceiling(range(pipes$Length)[2]),
                                           # Setting the default value range to be bewteen 1/4 and 3/4 of the total range
                                           value = (c((ceiling(range(pipes$Length)[2]) - floor(range(pipes$Length)[1]))/4, 
                                                   ((ceiling(range(pipes$Length)[2]) - floor(range(pipes$Length)[1]))/4) * 3)),
                                           post = "m",
                                           dragRange = T),
                               
                               # Diameter
                               sliderInput("diameter", "Diameter (mm)",
                                           min = floor(range(pipes$Diameter)[1]), 
                                           max = ceiling(range(pipes$Diameter)[2]),
                                           # Setting the default value range to be bewteen 1/4 and 3/4 of the total range
                                           value = (c((ceiling(range(pipes$Diameter)[2]) - floor(range(pipes$Diameter)[1]))/4, 
                                                   ((ceiling(range(pipes$Diameter)[2]) - floor(range(pipes$Diameter)[1]))/4) * 3)),
                                           post = "m",
                                           dragRange = T)
                               
                 )
             )
    ),
    absolutePanel(id = "scenarios", class = "panel panel-default scenarios", fixed = TRUE,
                  draggable = FALSE, top = 60, left = "auto", right = 10, bottom = "auto",
                  width = "20%", height = "auto",
                  
                  selectInput("rain", "Rainfall Scenario", vars, selected = "rain_15yravg"),
                  sliderInput(inputId = "month", label = "Month of the Year", min = 1, max = 12, value = 6)
                  
                  # textOutput("test1"),
                  # textOutput("test2")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Storing the inputs in reactive objects
    risk <- reactive({
        pipes_csv %>% 
            rowwise() %>%
            summarise(risk = evalfis(matrix(c(Diameter, Landuse, input$month, Pressure, get(input$rain)), 1, 5), fis)[1]) %>% 
            pull()
    })
    
    # The map using the parishes as base
    output$parish_map <- renderLeaflet({
        leaflet(bb_parish, options = leafletOptions(zoomControl = FALSE)) %>% 
            # htmlwidgets::onRender("function(el, x) {
            #     L.control.zoom({ position: 'bottomright' }).addTo(this)
            # }") %>%
            addProviderTiles(providers$Esri.WorldShadedRelief) %>% 
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.4)) %>% 
            addPolygons(color = "#1A1A1A", 
                        fillColor = ~pal_disc(bb_parish$OBJECTID),
                        weight = 0.8, 
                        smoothFactor = 0.5,
                        opacity = 0.8, 
                        fillOpacity = 0.5,
                        highlightOptions = highlightOptions(color = "White", 
                                                            weight = 2,
                                                            bringToFront = F),
                        popup = paste0("<b>Name: </b>",
                                       bb_parish$NAME,
                                       "<br>",
                                       "<b>Population (In 1990): </b>",
                                       bb_parish$POP_1990,
                                       "<br>",
                                       "<b>Area: </b>",
                                       round(bb_parish$Shape_Area, 2), " sqr m")) %>%
            addLegend(position = "bottomright", 
                      pal = pal_bin, 
                      values = 1:4,
                      title = "Burst Risk",
                      opacity = 1,
                      labFormat = function(type, cuts, p) {  # Here's the trick
                          paste0(c("Low", "Medium", "High", "Peak"))
                      })
    })
    
    # Adding an observe event that waits for changes to the risk variables
    observe({
        pipes$risk <- risk()
        # When triggered by a change in the risk calculation, it removes the old pipes layer and redraws a new one on top
        leafletProxy("parish_map", data = pipes) %>%
            clearGroup("pipe") %>%
            addPolylines(group = "pipe",
                         color = ~pal_bin(risk),
                         weight = 2,
                         smoothFactor = 2,
                         highlightOptions = highlightOptions(color = "White", weight = 2, bringToFront = TRUE),
                         popup = paste0("<b>Location: </b>",
                                        pipes$Subdistrict,
                                        "<br>",
                                        "<b>Diameter: </b>",
                                        pipes$Diameter, " mm",
                                        "<br>",
                                        "<b>Pressure: </b>",
                                        pipes$Pressure, " Pa",
                                        "<br>",
                                        "<b>Soil Type: </b>",
                                        pipes$Soil_Type,
                                        "<br>",
                                        "<b>Era: </b>",
                                        pipes$Era,
                                        "<br>",
                                        "<b>Average Monthly Rainfall (based on chosen scenario): </b>",
                                        pipes[[isolate(input$rain)]],
                                        "<br>",
                                        "<b>Risk Category: </b>",
                                        get_risk_category(pipes$risk),
                                        "<br>",
                                        "<b>Risk Score: </b>",
                                        pipes$risk
                        )
            )
    })
    
    # The map using the bwa districts as base
    output$bwa_map <- renderLeaflet({
        leaflet(bwa_districts, options = leafletOptions(zoomControl = FALSE)) %>% 
            addProviderTiles(providers$Esri.WorldShadedRelief) %>% 
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.4)) %>% 
            addPolygons(color = "#1A1A1A", 
                        fillColor = ~pal_disc(bwa_districts$OBJECTID),
                        weight = 0.8, 
                        smoothFactor = 0.5,
                        opacity = 0.8, 
                        fillOpacity = 0.5,
                        highlightOptions = highlightOptions(color = "White", 
                                                            weight = 2,
                                                            bringToFront = F),
                        popup = paste0("<b>Name: </b>",
                                       bwa_districts$DISTR_NAME,
                                       "<br>",
                                       "<b>Area: </b>",
                                       round(bwa_districts$Shape_Area, 2), " sqr m")) %>%
            addLegend(position = "bottomright", 
                      pal = pal_bin, 
                      values = 1:4,
                      title = "Burst Risk",
                      opacity = 1,
                      labFormat = function(type, cuts, p) {  # Here's the trick
                          paste0(c("Low", "Medium", "High", "Peak"))
                      })
    })
    
    # A similar observe event for the second map
    observe({
        pipes$risk <- risk()

        # When triggered by a change in the risk calculation, it removes the old pipes layer and redraws a new one on top
        leafletProxy("bwa_map", data = pipes) %>%
            clearGroup("pipe") %>%
            addPolylines(group = "pipe",
                         color = ~pal_bin(risk),
                         weight = 2,
                         smoothFactor = 2,
                         opacity = 1.0,
                         highlightOptions = highlightOptions(color = "white",
                                                             weight = 2,
                                                             bringToFront = TRUE),
                         popup = paste0("<b>Location: </b>",
                                        pipes$Subdistrict,
                                        "<br>",
                                        "<b>Diameter: </b>",
                                        pipes$Diameter, " mm",
                                        "<br>",
                                        "<b>Pressure: </b>",
                                        pipes$Pressure, " Pa",
                                        "<br>",
                                        "<b>Soil Type: </b>",
                                        pipes$Soil_Type,
                                        "<br>",
                                        "<b>Era: </b>",
                                        pipes$Era,
                                        "<br>",
                                        "<b>Average Monthly Rainfall (based on chosen scenario): </b>",
                                        pipes[[isolate(input$rain)]],
                                        "<br>",
                                        "<b>Risk Category: </b>",
                                        get_risk_category(pipes$risk),
                                        "<br>",
                                        "<b>Risk Score: </b>",
                                        pipes$risk
                         )
            )
    })
    
    # Create a reactive object to store the outputs of the the query selection
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
    
    # An observe event for the parish map to query the pipes based on their attributes
    observe({
        leafletProxy("parish_map", data = map_filter()) %>%
            clearGroup("query") %>%
            addPolylines(group = "query",
                         color = "White",
                         weight = 4,
                         smoothFactor = 2,
                         opacity = 0.7)
    })
    
    # A similar observe event for the bwa map
    observe({
        leafletProxy("parish_map", data = map_filter()) %>%
            clearGroup("query") %>%
            addPolylines(group = "query",
                         color = "White",
                         weight = 4,
                         smoothFactor = 2,
                         opacity = 0.7)
    })
    
    # A div for testing outputs - presently commented out
    # output$test1 <- renderPrint({
    #     map_filter() %>% class()
    # })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
