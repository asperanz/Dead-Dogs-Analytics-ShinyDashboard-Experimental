library(tidyverse)
library(janitor)
library(jsonlite)
# library(plotly) # BE CAREFUL!! load plotly pkg before httr pkg
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(httr)

server <- function(input, output, session) {
  
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  
  # Manually create a token
  access_token <- oauth2.0_token(
    app = app,
    endpoint = api,
    credentials = oauth2.0_access_token(api, app, params$code),
    cache = FALSE
  )
  
  # resp <- GET("https://youtubeanalytics.googleapis.com/v2/reports?dimensions=day,video&metrics=views&filters=video==rZfCO3Jxb5E&maxResults=500&sort=-day&startDate=2015-01-01&endDate=2099-12-31&ids=channel==MINE", config(token = access_token))
 
  request <- "https://youtubeanalytics.googleapis.com/v2/reports?dimensions=day,video&metrics=views&filters=video==rZfCO3Jxb5E&maxResults=500&sort=-day&startDate=2015-01-01&endDate=2099-12-31&ids=channel==MINE"

  req_youtube <- GET(request, config(token = access_token))

  youtube_text <- content(req_youtube, "text", encoding="UTF-8")

  youtube_json <- fromJSON(youtube_text, flatten = TRUE)

  df <- as.data.frame(youtube_json[["rows"]]) %>%
    arrange(V1) %>%
    rename(date = V1,
           items_snippet_resource_id_video_id = V2,
           views = V3) %>%
    mutate(views = as.numeric(views))

  videos_final <- inner_join(df, playlistItems, by = "items_snippet_resource_id_video_id") %>%
    select(date, items_snippet_title, views) %>%
    mutate(date = as.Date(date),
           views = as.integer(views))
  
  
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
  
  output$tbl <- renderDT(
    videos, options = list(lengthChange = FALSE)
  )
  
  output$timeseries <- renderPlot({

    ggplot(videos_final, aes(x = date, y = views)) +
      geom_line(color = "dark green")

  })
  
  
}