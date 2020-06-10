# Name: Saeesh Mangwani
# Date: 2020-05-07
# Description: A shiny web application visualizing risk in the municipal system on the island of Barbados

# ==== Loading libraries ====
library(shiny)
library(tidyverse)
library(sf)
library(readr)
library(leaflet)
# Loading the script that implements the fuzzy model
source("fuzzy.R")
# Loading the scripts that completes all of the setup for this app, including reading data, defining global variables and implementing helper
# functions
source("setup.R")

# Define UI for application - using a navbar page
ui <- fluidPage(
    # Defining the colour scheme for sliders
    chooseSliderSkin("Shiny", color = "#262626"),
    # Setting a title
    title = "Barbados Pipe Risk Platform",
    # Calling the styles.css stylesheet
    theme = "styles.css",
    # Adding the font catamaran from google fonts
    tags$link(rel = "stylesheet", href="http://fonts.googleapis.com/css?family=Catamaran"),
    
    div(class = "header", style = "padding: 10px; background-color: rgba(255,255,255,0.6)",
        h3("The Barbados Pipe Risk Platform"),
        h4("Vizualizing risk in the Barbados water distribution system")
    ),
    
    # The first tab which draws the map using Parishes
    div(class = "outer",
        # Setting the output of the map to cover the whole screen using the leaflet output command
        leafletOutput("map", height = "100%")
    ),
        
    # Creating a floating panel that contains the filtering options we want
    absolutePanel(id = "filters", class = "panel panel-default filters", draggable = F, top = 10, left = 10, 
                  right = "auto", bottom = "auto", width = "27%", fixed = T,
        # The title
        h3("Filtering Parameters"),
        
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
                  dragRange = T,
                  round = 1,
                  ticks = F),
        
        # Diameter (as a range slider)
        sliderInput("diameter", "Diameter (inches)",
                  min = floor(range(pipes$Diameter)[1]), 
                  max = ceiling(range(pipes$Diameter)[2]),
                  # Setting the default value range to be bewteen 1/4 and 3/4 of the total range
                  value = (c((ceiling(range(pipes$Diameter)[2]) - floor(range(pipes$Diameter)[1]))/4, 
                             ((ceiling(range(pipes$Diameter)[2]) - floor(range(pipes$Diameter)[1]))/4) * 3)),
                  dragRange = T,
                  round = 1,
                  ticks = F)
        ),
    # Creating a floating panel that contains the climate scenario options we want
    absolutePanel(id = "rainfall", class = "panel panel-default rainfall", fixed = TRUE,
                  draggable = FALSE, top = 10, left = "auto", right = 10, bottom = "auto",
                  width = "27%", height = "auto",
                  
                  h3("Risk Calculation Parameters"),
                  selectInput("rain", "Rainfall scenario", vars, selected = "rain_15yravg"),
                  sliderInput(inputId = "month", label = "Month of the year", min = 1, max = 12, value = 6, ticks = F),
                  
                  h3("Area Description:"),
                  htmlOutput("properties"),
    ),
)

# Define server logic required to draw the map and implement filter conditions
server <- function(input, output) {
    
    # Drawing the base-map using the parishes as base
    output$map <- renderLeaflet({
        leaflet(bb_parish %>% arrange(OBJECTID), options = leafletOptions(zoomControl = FALSE)) %>% 
            # htmlwidgets::onRender("function(el, x) {
            #     L.control.zoom({ position: 'bottomright' }).addTo(this)
            # }") %>%
            # Adding the background tileset
            addProviderTiles(providers$Esri.WorldShadedRelief) %>% 
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.4)) %>% 
            # Adding panes that allow for z-indexing different layers
            addMapPane("back_layers", zIndex = 400) %>% 
            addMapPane("pipe_layer", zIndex = 450) %>% 
            # Adding the boundary polygons and applying the appropriate colour palette so that each one is coloured uniquely.
            addPolygons(layerId = bb_parish %>% arrange(OBJECTID) %>% pull(UID),
                        group = "Parishes",
                        color = "#1A1A1A", 
                        fillColor = "FFFFFF",
                        weight = 1, 
                        smoothFactor = 1,
                        opacity = 1, 
                        fillOpacity = 0.2,
                        options = pathOptions(pane = "back_layers")) %>%
            addPolygons(data = bwa_districts %>% arrange(OBJECTID),
                        group = "BWA Districts",
                        layerId = bwa_districts %>% arrange(UID) %>% pull(UID),
                        color = "#1A1A1A", 
                        fillColor = "FFFFFF",
                        weight = 1, 
                        smoothFactor = 1,
                        opacity = 1, 
                        fillOpacity = 0.2,
                        options = pathOptions(pane = "back_layers")) %>%
            addLayersControl(
                baseGroups = c("Parishes", "BWA Districts"),
                options = layersControlOptions(collapsed = FALSE, sortLayers = FALSE, autoZIndex = FALSE)
            ) %>% 
            # Adding the legend for the risk scores
            addLegend(position = "bottomright", 
                      pal = pal_bin_invert, 
                      values = 1:4,
                      title = "Burst risk",
                      opacity = 0.9,
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
        pipes %>% 
            rowwise() %>%
            mutate(risk = evalfis(matrix(c(Diameter, Landuse_Bin, input$month, Pressure, get(input$rain)), 1, 5), fis)[1]) %>% 
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
        leafletProxy("map", data = temp %>% arrange(OBJECTID)) %>%
            clearGroup("pipe") %>%
            addPolylines(group = "pipe",
                         layerId = temp %>% arrange(OBJECTID) %>% pull(UID),
                         color = ~pal_bin(risk),
                         weight = 4,
                         smoothFactor = 2,
                         opacity = 1.0,
                         # Setting the path to ensure it z-index is higher than the back_layers
                         options = pathOptions(pane = "pipe_layer"),
                         # Adding a highlight on hover
                         highlightOptions = highlightOptions(color = "White", 
                                                             weight = 6,
                                                             bringToFront = TRUE),
                         # Adding a popup
                         popup = paste0("<h4 style = 'margin:0; margin-bottom:7px; font-size:200%;'>Pipe Description</h4>",
                                        "<b>Location: </b>",
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
                                        "<b>Mean monthly rainfall: </b>",
                                        round(temp[[isolate(input$rain)]], 2), " mm",
                                        "<br>",
                                        "<b>Risk category: </b>",
                                        get_risk_category(temp$risk),
                                        "<br>",
                                        "<b>Risk score: </b>",
                                        round(temp$risk, 2)
                         )
            )
    })
    
    # Rendering the properties of the baselayer in the output text box - due the limitations of the leaflet implementation in R this is some COMPLEX
    # functionality
    output$properties <- renderText(
        {
            mouseover <- input$map_shape_mouseover
            if(mouseout_valid == 1){
                mouseout <- input$map_shape_mouseout
            }
            
            if(is.null(mouseover)){
                textOut <<- "Hover over a region to see its information"
                textOut
            }else if(ifelse(length(mouseover$id == mouseout$id) == 0, FALSE, mouseover$id == mouseout$id) &
                     str_detect(mouseover$id, "pipe")){
                mouseout <- NULL
                mouseout_valid <<- 0
                textOut
            }else if(ifelse(length(mouseover$id == mouseout$id) == 0, FALSE, mouseover$id == mouseout$id)){
                mouseout <- NULL
                mouseout_valid <<- 0
                textOut <- "Hover over a region to see its information"
                textOut
            }else if(str_detect(mouseover$id, "bwa|parish")){
                if(is.null(mouseout)){
                    textOut <<- getFeatureInfo(mouseover$id)
                    mouseout <- input$map_shape_mouseout
                    mouseout_valid <<- 1
                    textOut
                }else if(mouseover$id != mouseout$id){
                    textOut <<- getFeatureInfo(mouseover$id)
                    textOut
                }
            }else{
                textOut
            }
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
