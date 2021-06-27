library(tidyverse)
library(stringr)

df = data.frame(x = c("Highway To Hell (AC/DC Cover) - Dead Dogs 2019 Christmas Party", 
                      "You Shook Me All Night Long (AC/DC Cover) - Dead Dogs 2019 Christmas Party",
                      "Are You Gonna Be My Girl (Jet Cover) - Dead Dogs 2017 Rehearsals"), 
                y = c(4000, 600, 200))
df

df$newx = str_wrap(df$x, width = 10)
df

ggplot(df, aes(x, y)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_discrete (labels = function(x) str_wrap(x, width = 20))

df2 <- df %>% 
mutate(newx = str_split(df$x, "-"))

df2 <- df %>% 
  mutate(newx = str_extract(df$x, "[^-]*"))
  
df2 <- df %>% 
  separate(x, sep = '-', into = c("song","concert")) %>% 
  mutate(song = str_trim(song), concert = str_trim(concert))

gino <- paste(song, concert), collapse="\n"

creare una nuova colonna con la posizione del '-' e poi usare la colonna com width
con il str_wrap

pino <- str_locate("Highway To Hell (AC/DC Cover) - Dead Dogs 2019", "-")

df3 <- df %>%
  mutate(hyphen_position = str_locate(x, "-")-1)

class(df3$hyphen_position[,1])


class(df3$hyphen_position[1,])

glimpse(df3)
  
ggplot(df3, aes(x, y)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_discrete (labels = function(x) str_wrap(x, width = df3$hyphen_position[1])) +
  theme(axis.text.y = element_text(size = 12, face = "bold"))

ggplot(df3, aes(x, y)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 12, face = "bold"))


df4 <- df2 %>%
  mutate(comp_songs = str_c(df2$song, df2$concert, sep = "\n"))
  
ggplot(df4, aes(comp_songs, y)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 12, face = "bold"))


------------------------------------------
  
library(ggtext) # remotes::install_github("wilkelab/ggtext")
library(ggplot2)

ggplot(iris, aes(Species, Sepal.Length)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = function(x) {
      paste0("<i style='color:#FF0000'>Iris </i>",
             "<i style='color:#0000FF'>", x ,"</i>")
    }
  ) +
  theme(axis.text.x = element_markdown())


-----------------------------------------------
library(tidyverse)
library(ggtext)
library(glue) 
  
   
  data <- tibble(
    bactname = c("Staphylococcaceae", "Moraxella", "Streptococcus", "Acinetobacter"),
    OTUname = c("OTU 1", "OTU 2", "OTU 3", "OTU 4"),
    value = c(-0.5, 0.5, 2, 3)
  )

data2 <- data %>% mutate(
  color = c("#009E73", "#D55E00", "#0072B2", "#000000"),
  name = glue("<i style='color:{color}'>{bactname}</i> ({OTUname})"),
  name = fct_reorder(name, value)
)

data2 %>% 
  ggplot(aes(value, name, fill = color)) + 
  geom_col(alpha = 0.5) + 
  scale_fill_identity() +
  labs(caption = "Example posted on **stackoverflow.com**<br>(using made-up data)") +
  theme(
    axis.text.y = element_markdown(),
    plot.caption = element_markdown(lineheight = 1.2)
  )  