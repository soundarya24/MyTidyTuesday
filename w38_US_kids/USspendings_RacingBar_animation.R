# bar charts animated (https://evamaerey.github.io/little_flipbooks_library/racing_bars/racing_barcharts.html#1)
library(tidyverse)
library(gganimate)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')
head(kids)
names(kids)
dim(kids)

kids_PK12ed<- kids %>%
  filter(variable=="PK12ed")
dim(kids_PK12ed)

max(ds_rankedbyyear$inf_adj_perchild)
min(ds_rankedbyyear$inf_adj_perchild)

#Ranking by year
ds_rankedbyyear<-kids_PK12ed %>%
  select(state,year,inf_adj_perchild) %>%
  group_by(year)%>%
  arrange(year,-inf_adj_perchild)%>%
  mutate(rank=1:n()) %>%
  filter(rank<=10)
dim(ds_rankedbyyear)

#Plot
theme_set(theme_classic())
plot_fr_animation<-ds_rankedbyyear %>%
  ggplot()+
  aes(xmin=0,
      xmax=inf_adj_perchild)+
  aes(ymin=rank-.45,
      ymax=rank+.45,
      y=reorder(rank,rank))+
  facet_wrap(.~year)+
  geom_rect(alpha=0.7)+
  aes(fill=state)+
  scale_fill_viridis_d(option = "magma",
                       direction = -1)+
  scale_x_continuous(limits = c(-10,20),breaks = c(4,8,12,16,20))+
  geom_text(col = "gray13",  
            hjust = "right",size=3,  
            aes(label = state),  
            x = -2)+
  labs(x="US Spendings ($1000 Per Child)")+
  labs(y="")+
  labs(title="US Public Spendings for \n Elementary and Secondary Education", caption = "Data: tidykids, RacingBarCharts by Soundarya",
       subtitle = "Top 10 Spending States")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=10, hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic"))
  
  
plot_fr_animation



#one-way
plot_fr_animation +
  facet_null()+
  geom_text(x=20, y=1,
            aes(label=as.character(year)),
            size=5,col="black")+
  aes(group=state)+
  gganimate::transition_time(year)

#working better way
a<-plot_fr_animation +
  facet_null()+
  geom_text(x=18, y=6,
            aes(label=as.character(year)),
            size=6,col="black")+
  aes(group=state)+
 transition_time(year)+ 
  ease_aes('cubic-in-out') 
animate(a,nframes=100,fps = 3,device = "png")
anim_save(filename = "myanim_usspendings_7.gif")
