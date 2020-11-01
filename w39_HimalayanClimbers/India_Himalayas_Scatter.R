library(dplyr)
library(ggplot2)
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
names(members)

# Gathering info on citizenship
table(members$citizenship)
India_string<-c("UK/India", "Nepal/India?", "Nepal/India", "India?", "India")

# Filtering Indians
all_india_members<- members %>%
  filter(citizenship %in% India_string)
dim(all_india_members)

# Some cleaning for pretty output and renaming variables
all_india_members$sex[all_india_members$sex=="F"]<-"Females"
all_india_members$sex[all_india_members$sex=="M"]<-"Males"
all_india_members<- all_india_members %>%
  filter(!sex=="NA", !age=="NA")

# Gathering info on lowest and oldest age and conquered peaks
## females
india_females<- all_india_members %>%
  filter(sex=="Females")
which.max(india_females$age)
india_females[189,]
which.min(india_females$age)
india_females[126,]
## males
india_males<- all_india_members %>%
  filter(sex=="Males")
which.max(india_males$age)
india_males[1501,]
which.min(india_males$age)
india_males[991,]
which.max(all_india_members$age)
all_india_members[1732,]
all_india_members$highpoint_metres[1732]=8850
all_india_members[1732,]


# Let's plot :-)
theme_set(theme_classic()) #my favorite theme

India_Him_plot<-ggplot(all_india_members, 
       aes(x=age, y=highpoint_metres,color=success))+
  xlim(10,70)+
  geom_point()+
  geom_smooth(se=FALSE,method = lm)+
  facet_wrap(.~sex)+
  geom_label(data = filter(all_india_members, sex=="Females"),
             aes(x=13,y=9100),
             nudge_y = 0,nudge_x = 10,
             label="Youngest Age 13 Years \n Climbed Everest in 2014",
             color="black")+
  geom_curve(data = filter(all_india_members, sex=="Females"),
             aes(x = 12, y = 8850, xend = 11.5, yend = 8950), 
             colour = "brown", 
             size=0.5, 
             curvature = -2.0,
             arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(data = filter(all_india_members, sex=="Females"),
             aes(x=33,y=9100),
             nudge_y = 0,nudge_x = 14,
             label="Oldest Age 53 Years \n Climbed Everest in 2018",
             color="black")+
  geom_curve(data = filter(all_india_members, sex=="Females"),
             aes(x = 54, y = 8850, xend = 60, yend = 8950), 
             colour = "brown", 
             size=0.5, 
             curvature = 1,
             arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(data = filter(all_india_members, sex=="Males"),
             aes(x=67,y=9100),
             nudge_y = 0,nudge_x = -10,
             label="Oldest Age 67 Years \n Attempted Climbing Everest in 2019",
             color="black")+
  geom_curve(data = filter(all_india_members, sex=="Males"),
             aes(x = 68, y = 8850, xend = 70, yend = 8950), 
             colour = "brown", 
             size=0.5, 
             curvature = 2,
             arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(data = filter(all_india_members, sex=="Males"),
             aes(x=15,y=9100),
             nudge_y = 0,nudge_x = 8,
             label="Youngest Age 15 Years \n Climbed Everest in 2013",
             color="black")+
  geom_curve(data = filter(all_india_members, sex=="Males"),
             aes(x = 14, y = 8850, xend = 12, yend = 8950), 
             colour = "brown", 
             size=0.5, 
             curvature = -2.0,
             arrow = arrow(length = unit(0.03, "npc")))+
  xlab("Age")+ylab("High Point of Peak (in metres)")+
  labs(title = "Indians & Himalayan Expeditions",
       subtitle = "Relation Among Age, Sex and Peak Height",
       caption = "DataSource: Himalayan Expeditions #TidyTuesday | Scatterplot by Soundarya")+
  theme(plot.title = element_text(size=24,face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=18, hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic"),
        legend.position = "top")

India_Him_plot

