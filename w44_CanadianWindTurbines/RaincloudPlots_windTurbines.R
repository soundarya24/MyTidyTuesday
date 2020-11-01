#Loading libraries
library(readr)
library(tidyverse)
library(plyr)
library(ggpubr)
library(ggthemes)

#Read TidyTuesday Data
wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

#Arranging provinces by number of turbines
wind_turbine %>%
  group_by(province_territory)%>%
  summarise(ss=n())%>%
  arrange(desc(ss))

#selecting only top-4 
## making a list
province_list=c("Ontario", "Quebec", "Alberta", "Nova Scotia")

#using the list to subset data
wind_top<- wind_turbine %>%
  filter(province_territory %in%  province_list )
dim(wind_top)


my_theme = theme(plot.title = element_text(size=24,face="bold",hjust = 0.5),
                        plot.subtitle = element_text(size=18, hjust = 0.5),
                        plot.caption = element_text(color="darkgreen", face="italic",size=10),
                        legend.position = "bottom",
                   panel.border = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                 axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



# lets try RainCloud plots :-)
#link: https://micahallen.org/2018/03/15/introducing-raincloud-plots/

summary_stats<- ddply(wind_top, ~province_territory, 
              summarise, mean = mean(total_project_capacity_mw), sd = sd(total_project_capacity_mw))

head(summary_stats)
# Quebec has the highest mean total capacity of power, based on which i am deciding the comparisons
my_comparisons <- list(c("Quebec", "Ontario"),c("Quebec", "Alberta"),c("Quebec", "Nova Scotia"))

#running stats
compare_means(total_project_capacity_mw~province_territory,
              wind_top, method = "t.test")

#creating factor levels
wind_top$province_territory<- factor(wind_top$province_territory,
                                     levels = c("Quebec","Ontario", "Alberta", "Nova Scotia"))
#sample size to include in the plot
wind_top_n<- wind_top %>%
  group_by(province_territory)%>%
  summarise(ss=n())

#Lets make rainclouds :-)
rcplot <- ggplot(data = wind_top, 
                  aes(y = total_project_capacity_mw, x = province_territory, fill = province_territory)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = total_project_capacity_mw, color = province_territory), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  stat_compare_means(comparisons = my_comparisons,
                     method = "t.test",
                     aes(label=..p.signif..))+
  guides(fill = FALSE) +
  guides(color = FALSE) +
  geom_text(data = wind_top_n,aes(label=ss),y=-10)+
  theme_wsj()+
  my_theme+
  labs(x="Canadian Provinces",
       y="Capacity (in Mega Watts)",
       title = "Capacities of Wind Turbines in Canada",
       caption = "DataSource: Canadian Wind Turbines #TidyTuesday \n Plot by Soundarya")

#Here it is..!!
rcplot


# Hurrrraaaayyyy:-))))
#Saving my first rain clouds:-))
ggsave("RainCloudPlots_WindTurbinesCanada.png")
