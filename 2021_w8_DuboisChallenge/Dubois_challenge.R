
# Load data ---------------------------------------------------------------


tuesdata <- tidytuesdayR::tt_load("2021-02-16")
tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

freed_slaves <- tuesdata$freed_slaves

# Data check --------------------------------------------------------------


dim(freed_slaves) # seems a small data, lets open it here
freed_slaves

# my faithful soldiers ----------------------------------------------------


library(tidyverse)


# pivot longer ------------------------------------------------------------


df <- freed_slaves %>%
  pivot_longer(cols = -Year, names_to = "group", values_to = "percent")
df

# some changes ------------------------------------------------------------


df_free <- df %>%
  filter(group == "Free")


# plot --------------------------------------------------------------------


p <- df %>%
  ggplot(aes(x = Year, y = percent, fill = group)) +
  geom_area() +
  scale_x_continuous(
    position = "top",
    limits = c(1790, 1870),
    breaks = c(1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870)
  ) +
  annotate("text", label = "SLAVES \n ESCLAVES", x = 1830, y = 50, color = "#f7ede1", size = 6) +
  geom_text(data = df_free, aes(x = Year, y = 100 - percent, label = paste(percent, "%", sep = "")), nudge_y = 1.5, color = "black", fontface = "bold") +
  annotate("text", label = "100%", x = 1870, y = 90, color = "black", fontface = "bold") +
  annotate("text", label = "FREE - LIBRE", x = 1830, y = 96, color = "black", fontface = "bold") +
  scale_fill_manual(values = c("darkgreen", "Black")) +
  labs(
    x = "", y = "",
    title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES . \n PROPORTION DES NEGRES LIBRES ET DES ESCLAVES EN AMERIQUE .",
    subtitle = "DONE BY ATLANTA UNIVERSITY",
    caption = "Visualization by Soundarya | #DuBoisChallenge"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 6, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "#f7ede1"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(family = "Arial")
  )
p

ggsave("DuboisChallenge_areachart.png",
  plot = last_plot()
)
# done!! ------------------------------------------------------------------
