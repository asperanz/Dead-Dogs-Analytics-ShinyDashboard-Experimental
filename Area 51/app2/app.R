# app 2

library(tidyverse)
library(janitor)
library(jsonlite)
library(plotly) # BE CAREFUL!! load plotly pkg before httr pkg
library(httr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(googleAuthR)
source("global.R", local = FALSE)


# -----------------------------------------------------------------------------
# Dashboard UI
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = "Dead Dogs Analytics"
  ),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(leafletOutput("concerts_map", height = 780)),
      box(plotOutput("rank", height = 370)),
      box(plotlyOutput("timeseries", height = 370)),
    )
  )
  
)


# -----------------------------------------------------------------------------
# Dashboard server code
# -----------------------------------------------------------------------------
server <- function(input, output) { 
  
  output$concerts_map <- renderLeaflet({
    
    leaflet() %>%
      # Base groups
      addProviderTiles("CartoDB.Positron", group = "Clear") %>%
      addProviderTiles(providers$OpenStreetMap.DE, group = "Colour") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Black and white") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
      # Overlay groups
      addMarkers(dd_concerts$longitude, 
                 dd_concerts$latitude,
                 popup = dd_concerts$popupcontent,
                 clusterOptions = markerClusterOptions()) %>% 
      addHeatmap(
        lng = dd_concerts$longitude, 
        lat = dd_concerts$latitude,
        radius = 17,
        blur = 25,
        cellSize = 25) %>%
      addMiniMap(tiles = providers$Esri.WorldStreetMap,
                 minimized = FALSE,
                 position = "bottomleft") %>%
      addLayersControl(baseGroups = c("Clear", "Colour", "Black and white", "Dark"),
                       position = "topright")
    
  })
  
  output$rank <- renderPlot({
    
    ggplot(videos_final, aes(x = date, y = views)) +
      geom_line(color = "dark green")
    
  })
  
  output$timeseries <- renderPlotly({
    
    ggplotly(ggplot(videos_final, aes(x = date, y = views)) +
               geom_line(color = "dark green"))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
