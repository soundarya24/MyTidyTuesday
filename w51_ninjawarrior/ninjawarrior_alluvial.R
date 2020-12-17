########
### Tidy tuesday week week 51, 15-12-2020----
########

# Loading data----
tuesdata <- tidytuesdayR::tt_load("2020-12-15")
tuesdata <- tidytuesdayR::tt_load(2020, week = 51)

ninja_warrior <- tuesdata$ninja_warrior
dim(ninja_warrior)
str(ninja_warrior)

# Loading my faithful soldiers!!----
library(tidyverse)
library(ggalluvial)
library(ggthemes)

# What are the rounds?
table(ninja_warrior$round_stage)
# Choosing only semi-finals and finals
round_semiandfinal <- c("Finals (Regional/City)", "Semi-Finals")

df1 <- ninja_warrior %>%
  filter(round_stage %in% round_semiandfinal)
dim(df1)

# choosing top 3 obstacles
top3obs <- df1 %>%
  group_by(obstacle_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(rank = dense_rank(desc(n))) %>%
  filter(rank < 4)
filter_obst <- top3obs$obstacle_name
df2 <- df1 %>%
  filter(obstacle_name %in% filter_obst)
dim(df2)

# choosing top 3 locations
top3loc <- df2 %>%
  group_by(location) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(rank = dense_rank(desc(n))) %>%
  filter(rank < 4)
filter_locn <- top3loc$location
df3 <- df2 %>%
  filter(location %in% filter_locn)
dim(df3)

# Alluvial plots----
df3 %>%
  ggplot(aes(
    weight = obstacle_order, axis1 = location,
    axis2 = season,
    axis3 = obstacle_name
  )) +
  geom_alluvium(aes(fill = round_stage)) +
  geom_stratum() +
  scale_x_discrete(limits = c("Location", "Season", "Obstacle"), expand = c(.25, .05)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_brewer(palette = "Set1", type = "seq") +
  theme_solarized() +
  labs(
    caption = "Data: Ninja Warrior #TidyTuesday | Alluvial Plot by Soundarya",
    fill = "Round",
    title = "American Ninja Warrior",
    subtitle = "Top 3 obstacles in top 3 locations"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
    plot.caption = element_text(color = "blue", face = "italic", size = 10),
    legend.position = "top",
    legend.text = element_text(color = "black"),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(), axis.ticks.y = element_blank()
  )
