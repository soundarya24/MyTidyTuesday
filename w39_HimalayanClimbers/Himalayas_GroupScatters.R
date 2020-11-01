# Load libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggpubr)
# Load data
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')

# Calculate days to terminate
expeditions_new<- expeditions %>%
  mutate(daystoterminate=termination_date-basecamp_date)
string<- c("Success (claimed)", "Success (main peak)", "Success (subpeak)")
expeditions_new_onlyunsucc<- expeditions_new %>%
  filter(!termination_reason %in% string)

#Let's plot
theme_set(theme_classic())

plot_24_9_1<-expeditions_new_onlyunsucc %>%
  filter(!season=="Unknown")%>%
  ggplot(aes(x=members, y=daystoterminate, color=season))+
  geom_point()+scale_colour_wsj("colors6")+
  geom_smooth(method = lm,se=FALSE)+
  stat_cor(method = "spearman")+
  theme(legend.position = "bottom")+
  facet_wrap(.~season)+
  xlab("Number of members in the group")+
  ylab("Days to terminate (Number of days from basecamp to termination date)")+
  labs(title = "Don't be a loner!",
       subtitle = "Larger groups sustain longer in the Himalayan expeditions!! \n Except for Summer :-) ",
       color="Season",
       caption = "Data shown only for unsuccessful attempts \n Data Source: Himalayan Expeditions #TidyTuesday | Scatterplot by Soundarya")+
  theme(plot.title = element_text(size=24,face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=18, hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic",size=10),
        legend.position = "top")
plot_24_9_1

plot_24_9_2<-expeditions_new_onlyunsucc %>%
  filter(!season=="Unknown")%>%
  ggplot(aes(x=members, y=daystoterminate,color=oxygen_used))+
  geom_point()+scale_colour_wsj("colors6")+
  geom_smooth(method = lm,se=FALSE)+
  stat_cor(method = "spearman")+
  theme(legend.position = "bottom")+
  facet_wrap(.~season)+
  xlab("Number of members in the group")+
  ylab("Days to terminate (Number of days from basecamp to termination date)")+
  labs(title = "Don't be a loner",
       subtitle = "Number of members in the Group expeditions vs Days to terminate \n with and without oxygen",
       caption = "Data shown only for unsuccessful attempts \n Data Source: Himalayan Expeditions #TidyTuesday | Scatterplot by Soundarya",
       color="Oxygen Usage")+
  theme(plot.title = element_text(size=24,face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=18, hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic",size=10),
        legend.position = "bottom")

plot_24_9_2
#Significant positive correlation between number of group members and days to terminate the expedition, i.e larger group sustained longer,
#This is true if oxygen was used/not used in autumn, only if oxygen was used in Spring, only if oxygen not used in winter, no such correlation in the Summer!