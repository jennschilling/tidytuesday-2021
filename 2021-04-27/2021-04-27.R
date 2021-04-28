# Author : Jenn Schilling
# Title: #TidyTuesday CEO Departures
# Date: Apr 27 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)

library(tidytext)

library(ggridges)

#### Data #### 

departures <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

# Get number of departures by code and year
departures_agg <- departures %>%
  filter(!is.na(departure_code) & fyear_gone <= 2021) %>%
  mutate(departure_reason = case_when(
    departure_code == 1 ~ "Death",
    departure_code == 2 ~ "Illness",
    departure_code == 3 ~ "Dismissal - Job Performance",
    departure_code == 4 ~ "Dismissal - Behavior",
    departure_code == 5 ~ "Retired",
    departure_code == 6 ~ "New Job",
    departure_code == 7 ~ "Other",
    departure_code == 8 | departure_code == 9 ~ "Unknown",
    TRUE ~ "Error"
  )) %>%
  mutate(departure_reason = factor(departure_reason,
                                   levels = c("Error",
                                              "Unknown",
                                              "Other",
                                              "Death",
                                              "Illness",
                                              "New Job",
                                              "Dismissal - Behavior",
                                              "Dismissal - Job Performance",
                                              "Retired"))) %>%
  group_by(fyear_gone, departure_code, departure_reason) %>%
  summarise(num = n(),
            .groups = "drop")

#### Formatting ####

font <- "Courier New"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.text = element_text(size = 7, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 7, color = fontcolor),
  
  plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
)

#### Plot ####

ggplot(data = departures_agg,
       mapping = aes(x = fyear_gone,
                     y = departure_reason,
                     height = num,
                     group = departure_reason,
                     fill = departure_reason,
                     color = departure_reason)) +
  geom_density_ridges(stat = "identity",
                      alpha = 0.75) +
  geom_text(data = 
              select(departures_agg, departure_reason) %>% 
              unique(.) %>%
              mutate(num = 1),
            mapping = aes(x = 1973,
                          y = departure_reason,
                          label = toupper(departure_reason),
                          color = departure_reason),
            family = font,
            vjust = -0.5,
            hjust = 0,
            size = 3) +
  scale_x_continuous(breaks = seq(from = 1990,
                                  to = 2020,
                                  by = 10)) +
  scale_fill_viridis_d(begin = 0,
                       end = 0.8,
                       direction = -1,
                       option = "C") +
  scale_color_viridis_d(begin = 0, 
                        end = 0.8,
                        direction = -1,
                        option = "C") +
  guides(fill = FALSE,
         color = FALSE) +
  labs(title = "Reasons for CEO Departures from S&P 1500 Firms, 1980-2021",
       subtitle = "",
       x = "",
       y = "",
       caption = "Data: Gentry et al. by way of DataIsPlural | Viz: Jenn Schilling") +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major = element_blank(),
        
        axis.text.y = element_blank(),
        
        axis.ticks.x = element_line(color = fontcolor,
                                    size = 0.2))

# Save
ggsave("2021-04-27\\ceo.png",
       plot = last_plot(),
       device = "png",
       width = 7,
       height = 4,
       dpi = 300,
       type = "cairo")
