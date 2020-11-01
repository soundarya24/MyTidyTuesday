library(tidyverse)
library(ggpomological)
library(patchwork)
library(gtsummary)

theme_set(theme_pomological_nobg())
no_dino_plot<-datasaurus %>%
  filter(!dataset=="dino")%>%
  ggplot(aes(x=x, y=y,fill=dataset)) + 
  geom_violin(colour="darkgreen") +
  geom_point(color="black") + 
  facet_wrap(.~dataset)+
  theme(legend.position = "none")
no_dino_plot

dino_plot<-datasaurus %>%
  filter(dataset=="dino")%>%
  ggplot(aes(x=x, y=y)) + 
  geom_violin(colour="darkgreen") +
  geom_point(color="black")
dino_plot

dino_plot+no_dino_plot

datasaurus %>%
  tbl_summary(by=dataset,
              statistic = all_continuous()~"{mean}")
