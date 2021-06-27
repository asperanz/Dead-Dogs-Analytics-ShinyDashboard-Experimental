library(shiny)
library(tidyverse)
library(janitor)
library(jsonlite)
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
  APP_URL <- "https://asperanz.shinyapps.io/ddasd/" # This must be the same name of the app in the 'Authorized redirect URIs' section
}

# Note that secret is not really secret, and it's fine to include inline
app <- oauth_app("dead-dogs-analytics-yt",
                 key = "311472921616-i4e4lhb60sdn18i0tvbjcsveppevcju4.apps.googleusercontent.com",
                 secret = "ynWLz2HYInU3u92YYFKOovIT",
                 redirect_uri = APP_URL
)

# Here I'm using a canned endpoint, but you can create with oauth_endpoint()
api <- oauth_endpoints("google")

# Always request the minimal scope needed
scope <- "https://www.googleapis.com/auth/yt-analytics.readonly"



# Shiny -------------------------------------------------------------------

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}



# Get Data -------------------------------------------------------------------

# Get data locally from file system 
# dd_concerts <- as_tibble(read.csv("./data/Dead Dogs Live Concerts.csv", header=TRUE, sep=",", stringsAsFactors=FALSE))

# Get data for the leaflet map from github
dd_concerts <- as_tibble(read.csv(file="https://raw.githubusercontent.com/asperanz/Dead-Dogs-Analytics/master/data/Dead%20Dogs%20Live%20Concerts.csv", header=TRUE, sep=",", stringsAsFactors=FALSE))

# new column for the popup label    
dd_concerts <- dd_concerts %>% 
  mutate(popupcontent = paste("<b>Concert:</b>", concert, "<br>",
                              "<b>Location:</b>", location, "<br>",
                              "<b>Date:</b>", dd_concerts$date,
                              playlist))


# Get data from YouTube Data API

# get_channel info using YouTube Data API
api_call_channel <- "https://www.googleapis.com/youtube/v3/channels?key=AIzaSyCviXCt3rQPfDaNvuFIaWCE24gNx7q0Dfs&id=UC6CV_32l8omBfcliOOQnIew&part=snippet,contentDetails,statistics&maxResults=15"

api_result_channel <- httr::GET(api_call_channel)

json_result_channel <- httr::content(api_result_channel, "text", encoding="UTF-8")

# Process the raw data into a data frame
json_channel <- fromJSON(json_result_channel, flatten = T)

channels <- as.data.frame(json_channel) %>% 
  janitor::clean_names()


# get_playlists info using YouTube Data API
api_call_playlists <- "https://www.googleapis.com/youtube/v3/playlists?key=AIzaSyCviXCt3rQPfDaNvuFIaWCE24gNx7q0Dfs&channelId=UC6CV_32l8omBfcliOOQnIew&part=snippet,contentDetails,player,status&maxResults=15"

api_result_playlists <- httr::GET(api_call_playlists)

json_result_playlists <- httr::content(api_result_playlists, "text", encoding="UTF-8")

# Process the raw data into a data frame
json_playlists <- fromJSON(json_result_playlists, flatten = T)

playlists <- as.data.frame(json_playlists) %>% 
  janitor::clean_names()


# get_playlistItems info using YouTube Data API
api_call_playlistItems <- "https://www.googleapis.com/youtube/v3/playlistItems?key=AIzaSyCviXCt3rQPfDaNvuFIaWCE24gNx7q0Dfs&playlistId=PLhIw1_0YGPETya3CXmcudNLE0edFQ3307&part=snippet,status&maxResults=50"

api_result_playlistItems <- httr::GET(api_call_playlistItems)

json_result_playlistItems <- httr::content(api_result_playlistItems, "text", encoding="UTF-8")

# Process the raw data into a data frame
json_playlistItems <- fromJSON(json_result_playlistItems, flatten = T)

playlistItems <- as.data.frame(json_playlistItems) %>% 
  janitor::clean_names()


# get_videos info using YouTube Data API
api_call_videos <- "https://www.googleapis.com/youtube/v3/videos?key=AIzaSyCviXCt3rQPfDaNvuFIaWCE24gNx7q0Dfs&id=rZfCO3Jxb5E&part=statistics&maxResults=50"

api_result_videos <- httr::GET(api_call_videos)

json_result_videos <- httr::content(api_result_videos, "text", encoding="UTF-8")

# Process the raw data into a data frame
json_videos <- fromJSON(json_result_videos, flatten = T)

videos <- as.data.frame(json_videos) %>% 
  janitor::clean_names() %>% 
  select(items_id, items_statistics_view_count)


