setwd("C:/Users/asperanz/Downloads")

library(tidyverse)
library(plotly)
library(ggthemr)

dd <- read.csv('2019-03-29_UC6CV_32l8omBfcliOOQnIew_2015-02-11_2019-03-29_table_video.csv')

glimpse(dd)
colnames(dd)
# [1] "video"                           "video_title"                    
# [3] "watch_time_minutes"              "views"                          
# [5] "average_view_duration"           "video_thumbnail_impressions"    
# [7] "video_thumbnail_impressions_ctr"

# Creare una nuova variabile (con tidy) come 'concert' dove c'e' il nome del concerto

dd %>% 
  ggplot(aes(reorder(video_title, views), views)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggplotly(ggplot(dd, aes(reorder(video_title, views), views)) +
  geom_bar(stat = "identity") +
  coord_flip())

# Creare la top10 di tutti i video
# NB. Qui non posso usare la count(video_title) poiche' non voglio vedere la 
# distribuzione della sola variabile video_title (che poi sarebbe sempre 1), ma 
# voglio analizzare due variabili: video_title (categorical) e views (numerical)

dd %>%
  top_n(10, views) %>%
  filter(video != 'Total') %>% 
  ggplot(aes(reorder(video_title, views), views)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw()

dd %>%
  top_n(10, views) %>%
  filter(video != 'Total') %>% 
  ggplot(aes(reorder(video_title, views), views)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()



---------
  
  dd %>% 
  count(video_title)
