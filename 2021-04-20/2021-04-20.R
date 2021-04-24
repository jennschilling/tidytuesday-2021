# Author : Jenn Schilling
# Title: #TidyTuesday Netflix
# Date: Apr 20 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(ggalluvial)
library(patchwork)

#### Data #### 

netflix_titles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv') %>%
  mutate(added_year = str_sub(date_added, start = -4)) 

# Aggregate data
netflix_agg_type <- netflix_titles %>%
  filter(!is.na(added_year) & !is.na(type)) %>%
  group_by(added_year, type) %>%
  summarise(num = n(),
            .groups = 'drop') %>%
  group_by(added_year) %>%
  mutate(pct = num / sum(num),
         added_year = parse_number(added_year)) %>%
  filter(added_year <= 2020)

# Labels
type_labels <- tibble(
  x_prop = c(2007.7, 2007.7),
  y_prop = c(0.25, 0.75),
  type = c("TV Show", "Movie"),
  label_prop = c("TV\nShow", "Movie")
)


#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

netflix_red <- "#e50914"
netflix_grey <- "#dedede"
black <- "#221f1f"
white <- "#ffffff"

#### Plot ####

# Sankey Flow Diagram of Proportion
sankey <- ggplot(data = netflix_agg_type,
       mapping = aes(x = added_year,
                     y = pct,
                     stratum = type,
                     alluvium = type,
                     fill = type)) +
  geom_alluvium() +
  geom_stratum() +
  geom_text(data = type_labels,
            mapping = aes(x = x_prop,
                          y = y_prop,
                          label = label_prop,
                          color = type),
            family = font, 
            size = 4,
            hjust = 1) +
  guides(fill = FALSE,
         color = FALSE) +
  scale_x_continuous(breaks = seq(from = 2008, to = 2020)) +
  scale_fill_manual(values = c(netflix_red, white)) +
  scale_color_manual(values = c(netflix_red, white)) +
  coord_cartesian(clip = "off") +
  labs(title = "Neflix began adding many more movies and tv shows in 2016.<br>
  Most new additions have been movies, especially in last four years.") +
  theme_void() +
  theme(axis.text.x = element_text(size = 9, color = white, family = font),
        
        panel.background = element_rect(fill = black, color = NA),
        plot.background = element_rect(fill = black, color = NA),
        
        plot.margin = margin(t = 15, r = 65, b = 15, l = 45),
        
        plot.title.position = "plot",
        plot.title = element_markdown(size = 12, color = white, family = font),
        
        plot.caption.position = "plot",
        plot.caption = element_markdown(size = 8, color = white, family = font))

# Line graph of total number added
line <- ggplot(data = netflix_agg_type %>%
                 group_by(added_year) %>%
                 summarise(total = sum(num),
                           .groups = "drop"),
       mapping = aes(x = added_year,
                     y = total)) +
  geom_line(size = 1.5,
            color = netflix_grey) +
  geom_text(data = netflix_agg_type %>%
              group_by(added_year) %>%
              summarise(total = sum(num),
                        .groups = "drop") %>%
              filter(added_year == 2020),
            mapping = aes(x = added_year + 0.1,
                          y = total,
                          label = paste0("In ", added_year, ", ",
                                         scales::comma(total), " movies\n",
                                         "and shows were added\nto Neflix.")),
            color = netflix_grey,
            hjust = 0,
            vjust = 0.5,
            family = font,
            size = 4) +
  guides(color = FALSE) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(from = 2008, to = 2020)) +
  labs(caption = "Data: <b>Kaggle</b> | Viz: <b>Jenn Schilling</b>",
       x = "Line shows the total number of movies and tv shows added each year",
       y = "") +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        
        axis.title.x = element_text(size = 10, color = netflix_grey, 
                                    family = font, hjust = 0),
        
        panel.border = element_blank(),
        panel.grid = element_blank(),
        
        panel.background = element_rect(fill = black, color = NA),
        plot.background = element_rect(fill = black, color = NA),
        
        plot.margin = margin(t = 15, r = 65, b = 15, l = 45),
        
        plot.caption.position = "plot",
        plot.caption = element_markdown(size = 8, color = white, family = font))


# Put plots together
sankey / line +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
    theme = theme(panel.background = element_rect(fill = black),
                  plot.background = element_rect(fill = black)))
