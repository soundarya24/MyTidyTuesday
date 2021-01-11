# TidyTuesday Train Transit Project Data dated 5th January 2020##

# Data import----
tuesdata <- tidytuesdayR::tt_load("2021-01-05")
tuesdata <- tidytuesdayR::tt_load(2021, week = 2)
transit_cost <- tuesdata$transit_cost

# Initial checks----
dim(transit_cost)
names(transit_cost)
head(transit_cost)
table(transit_cost_india$city)
table(transit_cost$country)

# Load faithful soldiers----
library(patchwork)
library(tidyverse)
library(ggalt)

# Subset data----
transit_cost_india <- transit_cost %>%
  filter(country == "IN")
class(transit_cost_india$city)
transit_cost_india$city <- factor(transit_cost_india$city)

# Calculate time difference----
transit_cost_india$start_year <- as.numeric(transit_cost_india$start_year)
transit_cost_india$end_year <- as.numeric(transit_cost_india$end_year)
transit_cost_india <- transit_cost_india %>%
  mutate(timediff = end_year - start_year)

# checks
mean(transit_cost_india$timediff, na.rm = TRUE)
class(transit_cost_india$timediff)

# Plots----
mumbai <- transit_cost_india %>%
  filter(city == "Mumbai") %>%
  arrange(desc(timediff)) %>%
  ggplot(aes(
    x = start_year, xend = end_year,
    y = reorder(line, -timediff)
  )) +
  xlim(2008, 2030) +
  geom_segment(aes(
    x = start_year,
    xend = end_year,
    y = line,
    yend = line
  ),
  color = "black", size = 2, linetype = 2, alpha = 0.7
  ) +
  geom_dumbbell(
    size_x = 8,
    size_xend = 8,
    colour_x = "orange2",
    colour_xend = "olivedrab",
    dot_guide = TRUE, dot_guide_size = 0.25,
    show.legend = TRUE
  ) +
  geom_rect(
    xmin = 2027, xmax = 2029,
    ymin = 0, ymax = 13,
    fill = "khaki"
  ) +
  geom_label(aes(x = 2028, label = paste(timediff, "years")),
    color = "black",
    fontface = "bold"
  ) +
  geom_text(aes(x = 2028, y = 13.50, label = "Time"), color = "black", fontface = "bold") +
  labs(
    x = "",
    y = "",
    title = "Time to construct Train Transit Lines",
    subtitle = "Mumbai"
  ) +
  ggthemes::theme_pander() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold")
  )

mumbai

delhi <- transit_cost_india %>%
  filter(city == "Delhi") %>%
  arrange(desc(timediff)) %>%
  ggplot(aes(
    x = start_year, xend = end_year,
    y = reorder(line, -timediff)
  )) +
  xlim(2006, 2030) +
  geom_segment(aes(
    x = start_year,
    xend = end_year,
    y = line,
    yend = line
  ),
  color = "black", size = 2, linetype = 2, alpha = 0.7
  ) +
  geom_dumbbell(
    size_x = 8,
    size_xend = 8,
    colour_x = "orange2",
    colour_xend = "olivedrab",
    dot_guide = TRUE, dot_guide_size = 0.25,
    show.legend = TRUE
  ) +
  geom_rect(
    xmin = 2028, xmax = 2030,
    ymin = 0, ymax = 3,
    fill = "khaki"
  ) +
  geom_label(aes(x = 2029, label = paste(timediff, "years")),
    color = "black",
    fontface = "bold"
  ) +
  geom_text(aes(x = 2029, y = 3.5, label = "Time"), color = "black", fontface = "bold") +
  labs(
    x = "",
    y = "",
    title = "Delhi"
  ) +
  ggthemes::theme_pander() +
  theme(plot.title = element_text(size = 16, face = "bold"))

delhi

bang <- transit_cost_india %>%
  filter(city == "Bangalore") %>%
  arrange(desc(timediff)) %>%
  ggplot(aes(
    x = start_year, xend = end_year,
    y = reorder(line, -timediff)
  )) +
  xlim(2006, 2030) +
  geom_segment(aes(
    x = start_year,
    xend = end_year,
    y = line,
    yend = line
  ),
  color = "black", size = 2, linetype = 2, alpha = 0.7
  ) +
  geom_dumbbell(
    size_x = 8,
    size_xend = 8,
    colour_x = "orange2",
    colour_xend = "olivedrab",
    dot_guide = TRUE, dot_guide_size = 0.25,
    show.legend = TRUE
  ) +
  geom_rect(
    xmin = 2028, xmax = 2030,
    ymin = 0, ymax = 2,
    fill = "khaki"
  ) +
  geom_label(aes(x = 2029, label = paste(timediff, "years")),
    color = "black",
    fontface = "bold"
  ) +
  geom_text(aes(x = 2029, y = 2.5, label = "Time"), color = "black", fontface = "bold") +
  labs(
    x = "",
    y = "",
    title = "Bangalore"
  ) +
  ggthemes::theme_pander() +
  theme(plot.title = element_text(size = 16, face = "bold"))
bang

chen <- transit_cost_india %>%
  filter(city == "Chennai") %>%
  arrange(desc(timediff)) %>%
  ggplot(aes(
    x = start_year, xend = end_year,
    y = reorder(line, -timediff)
  )) +
  xlim(2006, 2030) +
  geom_segment(aes(
    x = start_year,
    xend = end_year,
    y = line,
    yend = line
  ),
  color = "black", size = 2, linetype = 2, alpha = 0.7
  ) +
  geom_dumbbell(
    size_x = 8,
    size_xend = 8,
    colour_x = "orange2",
    colour_xend = "olivedrab",
    dot_guide = TRUE, dot_guide_size = 0.25,
    show.legend = TRUE
  ) +
  geom_rect(
    xmin = 2028, xmax = 2030,
    ymin = 0, ymax = 3,
    fill = "khaki"
  ) +
  geom_label(aes(x = 2029, label = paste(timediff, "years")),
    color = "black",
    fontface = "bold"
  ) +
  geom_text(aes(x = 2029, y = 3.5, label = "Time"), color = "black", fontface = "bold") +
  labs(
    caption = "Plots by Soundarya | DataSource: #TidyTuesday Transit Costs Project",

    x = "",
    y = "",
    title = "Chennai"
  ) +
  ggthemes::theme_pander() +
  theme(plot.title = element_text(size = 16, face = "bold"))
chen


# lets patch the plots
mumbai | (delhi / bang / chen)
