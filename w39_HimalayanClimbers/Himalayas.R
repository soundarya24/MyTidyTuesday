#Tidy Tuesday 
# September 22, 2020

#Load data
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')
dim(peaks)
names(peaks)
head(peaks)
table(peaks$peak_name)

#Load libraries
library(dplyr)
library(ggplot2)
library(png)
library(grid)

#Finding max and min peak heights
which.max(peaks$height_metres)
x<-peaks[43,]
x_ti<-x$peak_name
which.min(peaks$height_metres)
y<-peaks[331,]
y_ti<-y$peak_name

him_bg<- png::readPNG("himalayas.png")

#lets plot :-)
him_plot<-ggplot(peaks, aes(x=reorder(peak_name,height_metres), y=height_metres, color=climbing_status))+
  annotation_custom(rasterGrob(him_bg, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf)+
  geom_point(stat="identity")+
  ylab("Peak Height (metres)")+
  xlab("Peaks")+
  theme(axis.text.x = element_blank())+
  theme(legend.position = "top")+
  geom_label(data = filter(peaks, peak_name=="Everest"),
            aes(x=peak_name,y=9000),
            hjust = 1, nudge_y = 0.05,
            label=x_ti,
            color="black")+
  geom_label(data = filter(peaks, peak_name=="Roma"),
            aes(x=peak_name,y=5500),
            hjust = 0, nudge_x = 0.05,
            label=y_ti,
            color="black")+
  labs(title = "Himalayan Peaks",
       subtitle = "Climbed and Unclimbed",
       caption = "Datasource: Himalayan Climbing Expeditions | Chart by Soundarya \n image and data accesible from #TidyTuesday",
       color="Climbing Status")+
  theme(plot.title = element_text(size=28,face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=16, hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic"))
  
him_plot

