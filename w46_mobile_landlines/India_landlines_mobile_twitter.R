## TidyTuesday Week46
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

#Load libraries
library(tidyverse)
library(patchwork)
library(ggthemes)

#examine data
names(mobile)
dim(mobile)
head(mobile)

dim(landline)
names(landline)
head(landline)

# plots
p1<-mobile %>%
  filter(entity=="India")%>%
  ggplot(aes(x=year, y=mobile_subs))+
  geom_line(color="black")+
  theme_solarized_2()+
  labs(y="Fixed Mobile Subscriptions \n (per 100 people)",
       title = "Mobile Usage Growth in India")+
  theme(plot.title = element_text(size=24,face="bold",hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic",size = 10))

p1

p2<-landline %>%
  filter(entity=="India")%>%
  ggplot(aes(x=year, y=landline_subs))+
  geom_line(color="black")+
  theme_solarized_2()+
  labs(x="Year",
       y="Fixed Landline Subscriptions \n (per 100 people)",
       title = "Landline Usage Decline in India",
       caption = "DataSource: #TidyTuesday Historical Phone Usage \n Plot by Soundarya")+
  theme(plot.title = element_text(size=24,face="bold",hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic",size = 10))

p2

#patching plots
p1/p2
