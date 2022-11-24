# global 2

library(tidyverse)
library(janitor)
library(jsonlite)
library(httr)


# Google Authentication
# gar_set_client(scopes = c("https://www.googleapis.com/auth/yt-analytics.readonly"), activate = "web")
# google_token <- token_fetch()

# options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/yt-analytics.readonly"))
# options("googleAuthR.webapp.client_id" = "311472921616-i4e4lhb60sdn18i0tvbjcsveppevcju4.apps.googleusercontent.com")
# options("googleAuthR.webapp.client_secret" = "ynWLz2HYInU3u92YYFKOovIT")

endpoints <- oauth_endpoints("google")

myapp <- oauth_app("dead-dogs-analytics-yt",
                   key = "311472921616-i4e4lhb60sdn18i0tvbjcsveppevcju4.apps.googleusercontent.com",
                   secret = "ynWLz2HYInU3u92YYFKOovIT")

youtube_token <- oauth2.0_token(endpoints, myapp, scope = "https://www.googleapis.com/auth/yt-analytics.readonly")



# Get data for the leaflet map from github
dd_concerts <- as_tibble(read.csv(file="https://raw.githubusercontent.com/asperanz/Dead-Dogs-Analytics/master/data/Dead%20Dogs%20Live%20Concerts.csv", header=TRUE, sep=",", stringsAsFactors=FALSE))

# Get data locally from file system 
# dd_concerts <- as_tibble(read.csv("./data/Dead Dogs Live Concerts.csv", header=TRUE, sep=",", stringsAsFactors=FALSE))

# new column for the popup label    
dd_concerts <- dd_concerts %>% 
  mutate(popupcontent = paste("<b>Concert:</b>", concert, "<br>",
                              "<b>Location:</b>", location, "<br>",
                              "<b>Date:</b>", dd_concerts$date,
                              playlist))



# Get data from YouTube Data API

# get_channel info using YouTube Data API
# api_call_channel <- "https://www.googleapis.com/youtube/v3/channels?key=AIzaSyCviXCt3rQPfDaNvuFIaWCE24gNx7q0Dfs&id=UC6CV_32l8omBfcliOOQnIew&part=snippet,contentDetails,statistics&maxResults=15"
api_call_channel <- "https://www.googleapis.com/youtube/v3/channels?&id=UC6CV_32l8omBfcliOOQnIew&part=snippet,contentDetails,statistics&maxResults=15"

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
  janitor::clean_names()


# get_videos info using  YouTube Analytics and Reporting APIs - Video: Whole Lotta Shakin' Goin' On

request <- "https://youtubeanalytics.googleapis.com/v2/reports?dimensions=day,video&metrics=views&filters=video==rZfCO3Jxb5E&maxResults=500&sort=-day&startDate=2015-01-01&endDate=2099-12-31&ids=channel==MINE"

req_youtube <- GET(request, config(token = youtube_token))

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