library(tidyverse)
library(janitor)
library(jsonlite)
library(shiny)
library(plotly) # BE CAREFUL!! load plotly pkg before httr pkg
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(DT)
library(httr)


# OAuth setup --------------------------------------------------------

# Most OAuth applications require that you redirect to a fixed and known
# set of URLs. Many only allow you to redirect to a single URL: if this
# is the case for, you'll need to create an app for testing with a localhost
# url, and an app for your deployed app.

if (interactive()) {
  # testing url
  options(shiny.port = 1410)
  APP_URL <- "http://localhost:1410/"
} else {
  # deployed URL
  APP_URL <- "https://asperanz.shinyapps.io/ddad/" # This must be the same name of the app in the 'Authorized redirect URIs' section
}

# Note that secret is not really secret, and it's fine to include inline
app <- oauth_app("dead-dogs-analytics-311702",
                 key = "930372143435-m3db5ec7dl300guvbu5i3o5khk6g6fpe.apps.googleusercontent.com",
                 secret = "BijkxeCom14MlLYtW-vfofEh",
                 redirect_uri = APP_URL
)

# Here I'm using a canned endpoint, but you can create with oauth_endpoint()
api <- oauth_endpoints("google")

# Always request the minimal scope needed
scope <- "https://www.googleapis.com/auth/yt-analytics.readonly"




# Get data for leaflet map from github ------------------------------------

dd_concerts <- as_tibble(read.csv(file="https://raw.githubusercontent.com/asperanz/Dead-Dogs-Analytics/master/data/Dead%20Dogs%20Live%20Concerts.csv", header=TRUE, sep=",", stringsAsFactors=FALSE))

# new column for the popup label
dd_concerts <- dd_concerts %>%
  mutate(popupcontent = paste("<b>Concert:</b>", concert, "<br>",
                              "<b>Location:</b>", location, "<br>",
                              "<b>Date:</b>", dd_concerts$date,
                              playlist))



# Get data from YouTube Data API ------------------------------------------

# get_channel info using YouTube Data API
api_call_channel <- "https://www.googleapis.com/youtube/v3/channels?key=AIzaSyAaIN0FEpXtMhBi5T5wvWXZU2G7eGjx6Ec&id=UC6CV_32l8omBfcliOOQnIew&part=snippet,contentDetails,statistics&maxResults=15"

api_result_channel <- httr::GET(api_call_channel)

json_result_channel <- httr::content(api_result_channel, "text", encoding="UTF-8")

# Process the raw data into a data frame
json_channel <- fromJSON(json_result_channel, flatten = T)

channels <- as.data.frame(json_channel) %>%
  janitor::clean_names()


# get_playlists info using YouTube Data API
api_call_playlists <- "https://www.googleapis.com/youtube/v3/playlists?key=AIzaSyAaIN0FEpXtMhBi5T5wvWXZU2G7eGjx6Ec&channelId=UC6CV_32l8omBfcliOOQnIew&part=snippet,contentDetails,player,status&maxResults=15"

api_result_playlists <- httr::GET(api_call_playlists)

json_result_playlists <- httr::content(api_result_playlists, "text", encoding="UTF-8")

# Process the raw data into a data frame
json_playlists <- fromJSON(json_result_playlists, flatten = T)

playlists <- as.data.frame(json_playlists) %>%
  janitor::clean_names()


# get_playlistItems info using YouTube Data API
api_call_playlistItems <- "https://www.googleapis.com/youtube/v3/playlistItems?key=AIzaSyAaIN0FEpXtMhBi5T5wvWXZU2G7eGjx6Ec&playlistId=PLhIw1_0YGPETya3CXmcudNLE0edFQ3307&part=snippet,status&maxResults=50"

api_result_playlistItems <- httr::GET(api_call_playlistItems)

json_result_playlistItems <- httr::content(api_result_playlistItems, "text", encoding="UTF-8")

# Process the raw data into a data frame
json_playlistItems <- fromJSON(json_result_playlistItems, flatten = T)

playlistItems <- as.data.frame(json_playlistItems) %>%
  janitor::clean_names()


# get_videos info using YouTube Data API
api_call_videos <- "https://www.googleapis.com/youtube/v3/videos?key=AIzaSyAaIN0FEpXtMhBi5T5wvWXZU2G7eGjx6Ec&id=rZfCO3Jxb5E&part=statistics&maxResults=50"

api_result_videos <- httr::GET(api_call_videos)

json_result_videos <- httr::content(api_result_videos, "text", encoding="UTF-8")

# Process the raw data into a data frame
json_videos <- fromJSON(json_result_videos, flatten = T)

videos <- as.data.frame(json_videos) %>%
  janitor::clean_names() %>%
  select(items_id, items_statistics_view_count)



# Shiny -------------------------------------------------------------------

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

ui <- dashboardPage(
  dashboardHeader(
    title = "Dead Dogs Analytics"
  ),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(leafletOutput("concerts_map", height = 780)),
      # box(DTOutput("tbl")),
      box(DTOutput("tbl2")),
      # box(plotOutput("rank", height = 370))
      # box(plotOutput("timeseries", height = 370))
      box(plotlyOutput("timeseries", height = 370))
    )
  )
  
)

# A little-known feature of Shiny is that the UI can be a function, not just
# objects. You can use this to dynamically render the UI based on the request.
# We're going to pass this uiFunc, not ui, to shinyApp(). If you're using
# ui.R/server.R style files, that's fine too--just make this function the last
# expression in your ui.R file.
uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    url <- oauth2.0_authorize_url(api, app, scope = scope)
    redirect <- sprintf("location.replace(\"%s\");", url)
    tags$script(HTML(redirect))
  } else {
    ui
  }
}


server <- function(input, output, session) {
  
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
  
  # output$tbl <- renderDT(
  #   videos, options = list(lengthChange = FALSE)
  # )
  
  output$tbl2 <- renderDT(
    df, options = list(lengthChange = FALSE)
  )
  
  # output$timeseries <- renderPlot({
  # 
  #   ggplot(videos_final, aes(x = date, y = views)) +
  #     geom_line(color = "dark green")
  # 
  # })
  
  output$timeseries <- renderPlotly({

    ggplotly(ggplot(videos_final, aes(x = date, y = views)) +
               geom_line(color = "dark green"))

  })
  
  
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  
  # Manually create a token
  token <- oauth2.0_token(
    app = app,
    endpoint = api,
    credentials = oauth2.0_access_token(api, app, params$code),
    cache = TRUE
  )
  
  # YouTube Analytics API Call - Whole Lotta Shakin' Goin' On 
  resp <- GET("https://youtubeanalytics.googleapis.com/v2/reports?dimensions=day,video&metrics=views&filters=video==rZfCO3Jxb5E&maxResults=500&sort=-day&startDate=2015-01-01&endDate=2099-12-31&ids=channel==UC6CV_32l8omBfcliOOQnIew", httr::config(token = token))
  
  youtube_text <- content(resp, "text", encoding="UTF-8")
  
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
  
  # output$tbl2 <- renderDT(
  #   df, options = list(lengthChange = FALSE)
  # )
  
  
}

# Note that we're using uiFunc, not ui!
shinyApp(uiFunc, server)