#########
###Tidy tuesday week week 50, 08-12-2020----
########

# Load my faithful soldiers!!!
library(tidytuesdayR)
library(tidyverse)
library(magick)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-12-08')
tuesdata <- tidytuesdayR::tt_load(2020, week = 50)
women <- tuesdata$women
dim(women)
glimpse(women)

# lets see if any Indian women are there, very curious.

table(women$country)

# Wow.. 3 Indian women----
# who are they?

india_w<-women %>%
  filter(country=="India")
india_w

# a protester, musician and a para-athlete

# image processing of Indian women

# 1
india_1<- india_w$img[1]
india_1
india_1<- image_read(india_1)
print(india_1) 

# 2
india_2<- india_w$img[2]
india_2
india_2<- image_read(india_2)
print(india_2)

# 3
india_3<- india_w$img[3]
india_3
india_3<- image_read(india_3)
print(india_3)

# annotating names 
india_1_a<-image_annotate(
  india_1, text = "Bilkis", size = 50, color = "white",
  gravity = "southwest"
)

india_2_a<-image_annotate(
  india_2, text = "Isaivani", size = 50, color = "white",
  gravity = "southwest"
)

india_3_a<-image_annotate(
  india_3, text = "Manasi Joshi", size = 50, color = "white",
  gravity = "southwest"
)

# one image morphs into another and animate
women_animation<-image_resize(c(india_1_a, india_2_a,india_3_a), '200x200!') %>%
  image_background('white') %>%
  image_morph() %>%
  image_animate(delay = 22,loop = 5)
image_write(women_animation,"animation_women.gif")
