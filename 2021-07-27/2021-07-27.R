# Author : Jenn Schilling
# Title: #TidyTuesday Olympics
# Date: July 27 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(magick)
library(grid)

#### Data #### 

olympics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

sport_events_year <- olympics %>%
  select(year, season, sport, event) %>%
  unique() %>%
  count(year, season, sport)
  
# Add WWII Years
sports <- sport_events_year %>% 
  filter(season == "Winter") %>%
  select(sport) %>%
  unique(.) %>%
  unlist()

wwii <- tibble(
    year = c(rep(1940, 17), rep(1944, 17)),
    season = rep("Winter", 34),
    sport = c(sports, sports),
    n = rep(NA, 34)
  )

winter_sport_events_year <- sport_events_year %>%
  filter(season == "Winter") %>%
  bind_rows(wwii)

# Olympic rings
rings <- image_read("https://stillmed.olympics.com/media/Images/OlympicOrg/IOC/The_Organisation/The-Olympic-Rings/Olympic_rings_TM_c_IOC_All_rights_reserved_1.jpg")

rings <- rasterGrob(rings, interpolate = TRUE)

#### Formatting ####

font <- "Rockwell"
titlefont <- "Rockwell"
fontcolor <- "#000000"
bcolor <- "#FFFFFF"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 10, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 12, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 20, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 11, color = fontcolor, family = font, lineheight = 1.5),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 20, r = 60, b = 20, l = 0)
)


#### Plot ####

ggplot(data = winter_sport_events_year,
       mapping = aes(x = as.factor(year),
                     y = sport,
                     fill = n)) +
  annotation_custom(rings,
                    xmin = -5,
                    xmax = 3,
                    ymin = -23) +
  geom_tile(color = bcolor) +
  scale_fill_gradient(low = "#67B6DB",
                      high = "#014261",
                      na.value = "#F0F0F0",
                      guide = guide_colorbar(title.position = "top",
                                             barwidth = 10,
                                             barheight = 1))  +
  annotate("text",
           x = 5.5,
           y = 17,
           vjust = -0.75,
           hjust = 0.5,
           label = "Olympics not held\ndue to WWII.",
           size = 3.5,
           family = font,
           color = "gray30") +
  annotate("text",
           x = 18.5,
           y = 17,
           vjust = -0.3,
           hjust = 0.5,
           label = "1992 and 1994\nOlympics only 2 years\napart to stagger winter\nand summer.",
           size = 3.5,
           family = font,
           color = "gray30") +
  scale_x_discrete(breaks = c(1924, 1936, 1948, 1960, 1972, 1984, 1994, 2010)) +
  scale_y_discrete(limits=rev) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(title = "The number of sports and events at the Winter Olympics have increased over time.<br><br><br><br>",
       x = "",
       y = "",
       fill = "Number of Events",
       caption = "<b>Data:</b> Kaggle | <b>Logo:</b> International Olympic Committee | <b>Design:</b> Jenn Schilling") +
  theme(legend.position = "bottom",
        legend.justification = "right",
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, hjust = 1, vjust = 0),
        axis.text.x = element_text(size = 12))
# Save
ggsave("2021-07-27\\winterolympics.png",
       plot = last_plot(),
       device = "png",
       width = 12,
       height = 8,
       type = "cairo")
