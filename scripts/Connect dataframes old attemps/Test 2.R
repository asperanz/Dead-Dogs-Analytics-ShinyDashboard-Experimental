library(tuber)
library(tidyverse)

library(lubridate)
library(stringi)
library(wordcloud)
library(gridExtra)
library(googleAuthR)

# Fare una join tra il dataframe proveniente dal file Dead Dogs Live Concerts.xlsx
# e il dataframe videos. In questo modo creo un dataset di dati di base su cui 
# costruire la shiny app


#-----------------------------------------------------------------------------------
# Using tuber

library(tuber)

yt_oauth("874248587347-8gdgaapljjqk45ie899pqernknlhtjiq.apps.googleusercontent.com", "2g7jtm2lwzCKFpGjNwmUXmGf")

a <- list_channel_resources(filter = c(channel_id = "UC6CV_32l8omBfcliOOQnIew"), part="contentDetails")

# Uploaded playlists:
playlist_id <- a$items[[1]]$contentDetails$relatedPlaylists$uploads

# Get videos on the playlist
vids <- get_playlist_items(filter= c(playlist_id=playlist_id))

str(vids$contentDetails.videoId) # contentDetails.videoId e' un factor e va convertito in vector

# Video ids
vid_ids <- as.vector(vids$contentDetails.videoId)

# Function to scrape stats for all vids
get_all_stats <- function(id) {
  get_stats(id)
} 

# Get stats and convert results to data frame 
res <- lapply(vid_ids, get_all_stats) 
# passo tutti i vid_ids alla funzione get_all_stats creando una lista di liste  

res2 <- lapply(res, data.frame)
# Prima di trasformare la lista di liste in un dataframe, devo trasformarla in una
# lista di dataframes
# N.B. Tutti i dataframes creati hanno la stessa struttura (1 records su 6 variabili)
# per cui si possono accorpare tutte insieme 

res_df <- do.call(rbind, res2)
# Qui faccio una union (rbind) tra tutti i records dei dataframe che compongono la
# lista. 
# N.B. Non uso piu' la funzione 'apply' ma la 'do.call' 

#oppure posso farlo un un'unica chiamata
res_df <- do.call(rbind, lapply(res, data.frame))

head(res_df)


#-----------------------------------------------------------------------------------
# Using the tuber package to analyse a YouTube channel | insightR

library(tuber)
library(tidyverse)
library(lubridate)
library(stringi)
library(RColorBrewer)
library(wordcloud)
library(gridExtra)
library(funModeling)

# = Autentication = #
yt_oauth("874248587347-8gdgaapljjqk45ie899pqernknlhtjiq.apps.googleusercontent.com", "2g7jtm2lwzCKFpGjNwmUXmGf")

# IMPORTANT!! 
# If I get the error 'Error: HTTP failure: 401', running a tuber function just refresh 
# the oauth connection


# = Download and prepare data = #

# = Channel stats = #
chstat = get_channel_stats("UC6CV_32l8omBfcliOOQnIew")

video_count <- chstat$statistics$videoCount #69


# = Videos = #
videos = yt_search(term="", type="video", channel_id = "UC6CV_32l8omBfcliOOQnIew")
#vedere perche' mi piglia solo 57 video su 69

videos = videos %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2016-01-01") %>%
  arrange(date)

str(videos$video_id) #videos$video_id e' un factor per cui va prima trasformato in character


# = Comments = #
comments = lapply(as.character(videos$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})

# Alternative way - Building before the function
comment_threads <- function(id){
  get_comment_threads(c(video_id = id), max_results = 1000)
}
# N.B. 'x' e' solo il nome del parametro da passare alla funzione; 
# modoficando il nome del parametro in 'id' non cambia nulla 

comments2 = lapply(as.character(videos$video_id), comment_threads)

identical(comments, comments2) # TRUE -> the two objects are the same


# = Prep the data = #
# = Video Stat Table = #

# 1st Option - using 'rbind.data.frame'

videostats = lapply(as.character(videos$video_id), function(x){
  get_stats(video_id = x)
}) # Lista di liste con la stessa struttura

# rbind.data.frame' combina le operazioni di creazione delle liste di dataframes
# e union dei record in un solo comando 
videostats_1st_option = do.call(rbind.data.frame, videostats)


# 2nd Option - using before 'data.frame' and then 'rbind'

# Oppure usando le due operazioni separate

videostats_2nd_option <- lapply(videostats, data.frame) # Lista di dataframes con la stessa struttura

videostats_2nd_option <- do.call(rbind, videostats_2nd_option) # dataframe finale

identical(videostats_1st_option, videostats_2nd_option) #FALSE

# NEW - 'all.equal' detects all the differences between two dataframes even if 
# apparently they seem the same
all.equal(videostats_1st_option, videostats_2nd_option) 


str(videostats_1st_option)
str(videostats_2nd_option)
df_status(videostats_1st_option)
df_status(videostats_2nd_option)

#I use the 1st option as the official one
videostats <- videostats_1st_option
rm(videostats_1st_option)

# NEW - Creating the variable 'title' in the dataframe videostats starting from an
# existing field on another dataframe without using a join on the video_id fields!!
videostats$title = videos$title

# NEW - Creating the variable 'date' in the dataframe videostats using the same
# technique
videostats$date = videos$date

videostats = select(videostats, date, title, viewCount, likeCount, dislikeCount, commentCount) %>%
  as.tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         dislikeCount = as.numeric(as.character(dislikeCount)),
         commentCount = as.numeric(as.character(commentCount)))


# NEW How to transform factors in integers

# 1st Option - using 'as.integer(levels(viewCount))[viewCount]' -> More efficient
videostats2 <- as.tibble(videostats %>%
                           select (date, title, viewCount, likeCount, dislikeCount, commentCount) %>% 
                           mutate(title = as.character(title),
                                  viewCount = as.integer(levels(viewCount))[viewCount],
                                  likeCount = as.integer(levels(likeCount))[likeCount],
                                  dislikeCount = as.integer(levels(dislikeCount))[dislikeCount],
                                  commentCount = as.integer(levels(commentCount))[commentCount]
                                 )
                        )

# Check - OK
df_status(videostats)
df_status(videostats2)


# 2nd Option - using 'as.integer(as.character(viewCount))' -> Slightly less efficient
videostats3 <- as.tibble(videostats %>%
                           select (date, title, viewCount, likeCount, dislikeCount, commentCount) %>% 
                           mutate(title = as.character(title),
                                  viewCount = as.integer(as.character(viewCount)),
                                  likeCount = as.integer(as.character(likeCount)),
                                  dislikeCount = as.integer(as.character(dislikeCount)),
                                  commentCount = as.integer(as.character(commentCount))
                                 )
                        )


# Check - OK
df_status(videostats3)
identical(videostats2, videostats3) # TRUE

#I use the 1st option as the official one
videostats <- videostats2
rm(videostats2, videostats3)


# = General Stat Table = #
genstat = data.frame(Channel="Dead Dogs", 
                     Subcriptions=chstat$statistics$subscriberCount,
                     Views = chstat$statistics$viewCount,
                     Videos = chstat$statistics$videoCount, 
                     Likes = sum(videostats$likeCount),
                     Dislikes = sum(videostats$dislikeCount), 
                     Comments = sum(videostats$commentCount))


# = videostats Plot = #

# Gli scatterplots che seguono non hanno nessun senso, poiche' non mettono in relazione
# il numero di viste totali con le altre variabili (non c'e' una correlazione), ma
# Per sapere invece quali pezzi hanno avuto piu' o meno 
# io userei un barchart orizzontale:
# asse x - numero di likes, dislikes o commenti
# asse y - titolo del pezzo 

p1 <- ggplot(data = videostats) + 
  geom_point(aes(x = viewCount, y = likeCount))

p2 <- ggplot(data = videostats) + 
  geom_point(aes(x = viewCount, y = dislikeCount))

p3 <- ggplot(data = videostats) + 
  geom_point(aes(x = viewCount, y = commentCount))

grid.arrange(p1, p2, p3, ncol = 2)

# Using plotly
# N.B. grid.arrange works with ggplot2 ONLY not using plotly + ggplot2

library(plotly)

ggplotly(
         ggplot(data = videostats) + 
         geom_point(aes(x = viewCount, y = likeCount))
        )

ggplotly(
  ggplot(data = videostats) + 
    geom_point(aes(x = viewCount, y = dislikeCount))
        )

ggplotly(
  ggplot(data = videostats) + 
    geom_point(aes(x = viewCount, y = commentCount))
        )









