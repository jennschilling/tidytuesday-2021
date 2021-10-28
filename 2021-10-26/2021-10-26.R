# Author : Jenn Schilling
# Title: #TidyTuesday Ultra Trail Running
# Date: October 26 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)
library(lubridate)
library(patchwork) # using dev version

#### Data #### 

ultra_rankings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# Frequent racers

freq_racers <- ultra_rankings %>%
  count(runner, gender, sort = TRUE)

# Women-Identifying Racers with More than 20 races
w_freq_racers <- freq_racers %>%
  filter(gender == "W" & n > 20) %>%
  left_join(ultra_rankings, by = c("runner", "gender")) %>%
  left_join(race, by = "race_year_id") %>%
  # Assume races with 0 for the distance are actually 100 miles or 160.93 km
  mutate(distance = ifelse(distance == 0, 160.93, distance)) %>%
  # Remove states from some country entries
  mutate(country = country %>% str_replace("[^,]*, ", "")) %>%
  # Get maximum age since the age doesn't seem to change by year for many runners
  group_by(runner) %>%
  mutate(max_age = max(age)) %>%
  ungroup() %>%
  # Get columns of interest
  select(runner, n, race_year_id, date, rank, max_age, 
         nationality, time_in_seconds, country, distance) %>%
  mutate(race_year = year(date),
         race_month = month(date)) %>%
  separate(runner, into = c("lname", "fname"), sep = " ") %>%
  mutate(lname = str_to_title(lname)) %>%
  unite("runner", fname:lname, sep = " ") %>%
  # Add column for top 10 rank
  mutate(top_10_rank = ifelse(is.na(rank), FALSE, rank <= 10)) %>%
  # Arrange by age
  arrange(max_age) %>%
  mutate(runner = factor(runner, unique(runner))) %>%
  # Create segment parameters 
  mutate(
    x = case_when(
      country == "United States" | country == "Croatia" ~ date - 15,
      country == "Sweden" ~ date - 15,
      country == "France" | country == "Colombia" ~ date,
      country == "Switzerland" ~ date - 15
      ),
    xend = case_when(
      country == "United States" | country == "Croatia" ~ date + 15,
      country == "Sweden" ~ date + 15,
      country == "France" | country == "Colombia" ~ date,
      country == "Switzerland" ~ date + 15
    ),
    y = case_when(
      country == "United States" | country == "Croatia" ~ distance - distance / 50,
      country == "Sweden" ~ distance + distance / 50,
      country == "France" | country == "Colombia" ~ distance - distance / 50,
      country == "Switzerland" ~ distance
    ),
    yend = case_when(
      country == "United States" | country == "Croatia" ~ distance + distance / 50,
      country == "Sweden" ~ distance - distance / 50,
      country == "France" | country == "Colombia"  ~ distance + distance / 50,
      country == "Switzerland" ~ distance
    ),
    
    x2 = case_when(
      country == "Croatia"  ~ date - 15,
      country == "Colombia" ~ date - 15
    ),
    xend2 = case_when(
      country == "Croatia" ~ date + 15,
      country == "Colombia" ~ date + 15
    ),
    y2 = case_when(
      country == "Croatia" ~ distance + distance / 50,
      country == "Colombia" ~ distance
    ),
    yend2 = case_when(
      country == "Croatia" ~  distance - distance / 50,
      country == "Colombia" ~ distance
    )
)

#### Formatting ####

font <- "Trebuchet MS"
title_font <- "Candara"
fontcolor <- "gray30"
bcolor <- "#F8F8F8"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 12, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 20, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 30, color = fontcolor, family = title_font),
  
  plot.subtitle = element_markdown(size = 20, color = fontcolor, family = title_font),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 10, color = fontcolor),
  
  plot.margin = margin(t = 15, r = 20, b = 15, l = 20)
)


#### Plot ####

# Color is time in seconds - faster is darker
# Y position is the distance of the race - 155km to 174.2 km
# Runners are arranged from youngest to oldest
# Lines within the rectangle represent the location of the race


# Create plot

run_plot <- ggplot() +
  geom_rect(data = w_freq_racers,
            mapping = aes(xmin = date - 15,
                          xmax = date + 15,
                          ymin = distance - distance / 50,
                          ymax = distance + distance / 50,
                          color = time_in_seconds),
            fill = NA,
            size = 1) +
  geom_segment(data = w_freq_racers,
               mapping = aes(x = x,
                             xend = xend,
                             y = y,
                             yend = yend,
                             color = time_in_seconds),
                size = 1) +
  geom_segment(data = w_freq_racers %>% filter(!is.na(x2)),
               mapping = aes(x = x2,
                             xend = xend2,
                             y = y2,
                             yend = yend2,
                             color = time_in_seconds),
               size = 1) +
  # scale_color_viridis_c(option = "D",
  #                       direction = -1,
  #                       na.value = "#D7D7D7")+
  # scale_color_gradient2(high = "#787FF6",
  #                       mid = "#4ADEDE",
  #                       na.value = "#D7D7D7") +
  # scale_color_gradient2(high = "#205072",
  #                       mid = "#56C596",
  #                       na.value = "#D7D7D7") +
  scale_color_distiller(palette = "RdPu",
                        direction = -1,
                        na.value = "#D7D7D7") +
  geom_point(data = w_freq_racers %>%
               filter(top_10_rank),
             mapping = aes(x = date + 15,
                           y = distance + distance / 50),
             color = "#01AE7E",
             shape = 8,
             size = 2) +
  facet_wrap(~ runner,
             ncol = 1,
             scales = "free_x",
             strip.position = "left") +
  scale_x_date(limits = c(as.Date("2011-12-01"), as.Date("2022-01-01")),
               date_breaks = "years",
               date_labels = "%Y") +
#  scale_y_continuous(limits = c(145, 185)) +
  guides(color = "none") +
  coord_cartesian(clip = "off",
                  expand = FALSE) +
  labs(y = "",
       x = "",
       caption = "<b>Data:</b> International Trail Running Association (ITRA) | <b>Design:</b> Jenn Schilling") +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "cm"),
        strip.placement.y = "inside",
        strip.text.y.left = element_text(size = 18, color = fontcolor, 
                                         angle = 0, vjust = 0, hjust = 1,
                                         family = title_font))


# Create title, explanation, and legend

legend <- w_freq_racers %>%
  select(country) %>%
  unique() %>%
  mutate(
    date = 100,
    distance = 100,
    
    x = case_when(
      country == "United States" | country == "Croatia" ~ date - 15,
      country == "Sweden" ~ date - 15,
      country == "France" | country == "Colombia" ~ date,
      country == "Switzerland" ~ date - 15
    ),
    xend = case_when(
      country == "United States" | country == "Croatia" ~ date + 15,
      country == "Sweden" ~ date + 15,
      country == "France" | country == "Colombia" ~ date,
      country == "Switzerland" ~ date + 15
    ),
    y = case_when(
      country == "United States" | country == "Croatia" ~ distance - distance / 50,
      country == "Sweden" ~ distance + distance / 50,
      country == "France" | country == "Colombia" ~ distance - distance / 50,
      country == "Switzerland" ~ distance
    ),
    yend = case_when(
      country == "United States" | country == "Croatia" ~ distance + distance / 50,
      country == "Sweden" ~ distance - distance / 50,
      country == "France" | country == "Colombia"  ~ distance + distance / 50,
      country == "Switzerland" ~ distance
    ),
    
    x2 = case_when(
      country == "Croatia"  ~ date - 15,
      country == "Colombia" ~ date - 15
    ),
    xend2 = case_when(
      country == "Croatia" ~ date + 15,
      country == "Colombia" ~ date + 15
    ),
    y2 = case_when(
      country == "Croatia" ~ distance + distance / 50,
      country == "Colombia" ~ distance
    ),
    yend2 = case_when(
      country == "Croatia" ~  distance - distance / 50,
      country == "Colombia" ~ distance
    )
  )


legend_plot <- ggplot() +
  geom_rect(data = legend,
            mapping = aes(xmin = date - 15,
                          xmax = date + 15,
                          ymin = distance - distance / 50,
                          ymax = distance + distance / 50),
            fill = NA,
            color = fontcolor,
            size = 1) +
  geom_segment(data = legend,
               mapping = aes(x = x,
                             xend = xend,
                             y = y,
                             yend = yend),
               color = fontcolor,
               size = 1) +
  geom_segment(data = legend %>% filter(!is.na(x2)),
               mapping = aes(x = x2,
                             xend = xend2,
                             y = y2,
                             yend = yend2),
               color = fontcolor,
               size = 1) +
  geom_text(data = legend,
            mapping = aes(x = date + 18,
                          y = distance,
                          label = country),
            family = title_font,
            color = fontcolor,
            size = 6,
            hjust = "left") +
  facet_wrap(~ country,
             ncol = 1) +
  scale_x_continuous(limits = c(80, 200)) +
  theme_void() +
  theme(strip.text.x = element_blank())

titles <- tibble(
  title = "Each run by the women who have run<br>more than 20 Ultra Trail Runs",
  
  subtitle = "
  Runners are arranged from youngest to oldest; the youngest is 46 and the oldest is 62, as of their last race.<br>
  The x position along the timeline represents the date of the race (2012-2021).<br>
  The y position represents the distance of the race: the shortest race was 155km; the longest race was 174.2km.<br>
  The color represents the runner's time: 
        <span style = 'color:#ae017e;'>faster times</span>; 
       <span style = 'color:#fa9fb5;'>slower times</span>; 
       grey means no time.<br>
  A <span style = 'color:#01AE7E';>star</span> in the top corner represents a Top 10 finish.<br>
  The style of the box represents the country in which the race was held."
)

title_plot <- ggplot() +
  geom_textbox(data = titles,
               mapping = aes(x = 0,
                             y = 40,
                             label = title),
               fill = NA, 
               box.color = NA,
               family = title_font,
               color = fontcolor,
               size = 15,
               width = unit(10, "in")) +
  geom_textbox(data = titles,
            mapping = aes(x = 0,
                          y = 1,
                          label = subtitle),
            fill = NA, 
            box.color = NA,
            family = title_font,
            color = fontcolor,
            size = 8,
            width = unit(10, "in")) +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(-30, 45)) +
  theme_void()

# Put the pieces together

left_side <- title_plot / (plot_spacer() | legend_plot | plot_spacer()) / plot_spacer() +
  plot_layout(heights = c(2, 0.75, 2),
              ncol = 1)

left_side | run_plot 

# Save
ggsave("2021-10-26\\greatracers.png",
       plot = last_plot(),
       device = "png",
       width = 25,
       height = 20,
       type = "cairo")

