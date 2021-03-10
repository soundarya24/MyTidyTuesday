
# TidyTuesday 2021 week 11 ------------------------------------------------

# import data -------------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load('2021-03-09')
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)
bechdel <- tuesdata$raw_bechdel


# my faithful soldiers ----------------------------------------------------


library(tidyverse)
library(wordcloud)  
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(grid)


# towards wordcloud ---------------------------------------------------------------


text<- bechdel$title
docs<- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

head(df)

set.seed(1234) 

# wordcloud ---------------------------------------------------------------


wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

# saved this in folder as "CopyOfwordcloud_bechdel.png"
# read that PNG image to create background for plot
image<- png::readPNG("CopyOfwordcloud_bechdel.png")


# line plot --------------------------------------------------------------------


bechdel %>% 
  group_by(year) %>% 
  summarise(mean=mean(rating)) %>% 
  ggplot(aes(x=year, y=mean))+
  annotation_custom(rasterGrob(image))+
  geom_smooth(se=FALSE,color="black")+
  scale_x_continuous(breaks = c(1880,1900,1920,1940,1960,1980,2000,2020))+
  theme_classic()+
  geom_label(aes(x=2010,y=1),
             label="Most Common Word Used in Titles \n is still...")+
  geom_curve(aes(x = 2000, y = 0.75, xend = 1960, yend = 0.7), 
             colour = "brown", 
             size=0.5, 
             curvature = -1,
             arrow = arrow(length = unit(0.03, "npc")))+
  labs(title = "Is Hollywood recovering from the gender bias?",
       subtitle = "Insights from the Bechdel Test Scores \n0 = unscored, 1 = Has at least two [named] women in it, 2 = Who talk to each other, 3 = About something besides a man",
       x="",
       y="Mean Bechdel Scores of movies",
       caption = "Visualization by Soundarya \n Data:#TidyTuesday")

