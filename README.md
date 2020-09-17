# MyTidyTuesday
#TidyTuesday September 15
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)


kids_1997_PK12ed<- kids %>%
  filter(year==1997, variable=="PK12ed")
dim(kids_1997_PK12ed)
kids_2016_PK12ed<- kids %>%
  filter(year==2016, variable=="PK12ed")
dim(kids_2016_PK12ed)


theme_set(theme_classic())

#z-score calculation
kids_1997_PK12ed$inf_adj_perchild_zscore<-round(
  kids_1997_PK12ed$inf_adj_perchild-mean(kids_1997_PK12ed$inf_adj_perchild)/sd(kids_1997_PK12ed$inf_adj_perchild),
  digits=2)
#cut-off values
kids_1997_PK12ed$inf_adj_perchild_type<-ifelse(kids_1997_PK12ed$inf_adj_perchild_zscore<0,"below", "above")
#sort by ascending order of z-score
kids_1997_PK12ed<-kids_1997_PK12ed[order(kids_1997_PK12ed$inf_adj_perchild_zscore),]
class(kids_1997_PK12ed$state)
kids_1997_PK12ed$state<-factor(kids_1997_PK12ed$state,levels = kids_1997_PK12ed$state)
class(kids_1997_PK12ed$state)

#plot
plot1<-ggplot(kids_1997_PK12ed,
              aes(x=state,y=inf_adj_perchild_zscore,label=inf_adj_perchild_zscore))+
  geom_bar(stat = "identity",aes(fill=inf_adj_perchild_type,width=0.5))+
  scale_fill_manual(name="Spendings Per Child (Inflation adjusted)",
                    labels=c("Above average", "Below average"),
                    values = c("above"="#00ba38", "below"="#0b8fd3"))+
  labs(title = "US Public Spendings \n on Elementary and Secondary Education \n in 1997", caption = "Data: tidykids, Chart by Soundarya",
       subtitle = "Across States Comparison")+
  ylab("Spendings Per Child (Z-score)")+
  xlab("State")+
  ylim(-2,6)+
  theme(legend.position = "top")+
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=10, hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic"))+
  coord_flip()
plot1

#z-score calculation
kids_2016_PK12ed$s1<-kids_2016_PK12ed$inf_adj_perchild-mean(kids_2016_PK12ed$inf_adj_perchild)
kids_2016_PK12ed$s2<-kids_2016_PK12ed$s1/sd(kids_2016_PK12ed$inf_adj_perchild)
kids_2016_PK12ed$inf_adj_perchild_zscore<-round(kids_2016_PK12ed$s2,digits=2)
#cut-off values
kids_2016_PK12ed$inf_adj_perchild_type<-ifelse(kids_2016_PK12ed$inf_adj_perchild_zscore<0,"below", "above")
#sort by ascending order of z-score
kids_2016_PK12ed<-kids_2016_PK12ed[order(kids_2016_PK12ed$inf_adj_perchild_zscore),]
class(kids_2016_PK12ed$state)
kids_2016_PK12ed$state<-factor(kids_2016_PK12ed$state,levels = kids_2016_PK12ed$state)
class(kids_2016_PK12ed$state)

#plot
plot2<-ggplot(kids_2016_PK12ed,
              aes(x=state,y=inf_adj_perchild_zscore,label=inf_adj_perchild_zscore))+
  geom_bar(stat = "identity",aes(fill=inf_adj_perchild_type,width=0.5))+
  scale_fill_manual(name="Spendings Per Child (Inflation adjusted)",
                    labels=c("Above average", "Below average"),
                    values = c("above"="#00ba38", "below"="#0b8fd3"))+
  labs(title = "US Public Spendings \n on Elementary and Secondary Education \n in 2016", caption = "Data: tidykids, Chart by Soundarya",
       subtitle = "Across States Comparison")+
  ylab("Spendings Per Child (Z-score)")+
  xlab("State")+
  ylim(-2,6)+
  theme(legend.position = "top")+
  theme(plot.title = element_text(size=12,face="bold",hjust = 0.5),
        plot.subtitle = element_text(size=10, hjust = 0.5),
        plot.caption = element_text(color="blue", face="italic"))+
  coord_flip()
plot2

plot1+plot2+plot_layout(guides = "collect")& theme(legend.position = 'top')
