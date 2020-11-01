# Load libraries
library(ggplot2)


# Load data
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')
names(friends_info)
head(friends_info)


# let me try with ggplot
theme_set(theme_classic())


# working plot
plot3<-ggplot(friends_info,
             aes(x=episode, y=season,fill=us_views_millions))+
  geom_tile(color="white")+
  geom_text(aes(label = round(us_views_millions, 1)))+
  scale_fill_gradient(
    name = "US Views in millions", 
    low = "white", high = "darkgreen",
    limit = c(min(friends_info$us_views_millions), max(friends_info$us_views_millions)),
    space = "Lab",
    guide = "colourbar")
plot3

plot4<- plot3 + scale_x_continuous(breaks = c(1:25),position = "top",expand = c(0, 0))+
  scale_y_reverse(breaks = c(1:10),expand = c(0, 0))
friends_views_heatmap<-plot4+theme(legend.position = "top")+
  labs(title = "F.R.I.E.N.D.S",
       subtitle = "Viewers in USA",
       caption = "Datasource: Friends TidyTuesday | Heatmap by Soundarya")+
  xlab("Episode")+ylab("Season")+
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=10, hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic"))
friends_views_heatmap



