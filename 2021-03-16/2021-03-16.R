# Author : Jenn Schilling
# Title: #TidyTuesday Video Games
# Date: Mar 16 2021

#### Libraries ####

library(tidyverse)
library(extrafont)

#### Data #### 

games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv') %>%
  mutate(avg_peak_perc = parse_number(avg_peak_perc),
         month = factor(month,
                        levels = c("January",
                                   "February",
                                   "March",
                                   "April",
                                   "May",
                                   "June",
                                   "July",
                                   "August",
                                   "September",
                                   "October",
                                   "November",
                                   "December")),
         month_num = ifelse(match(month, month.name) < 10,
                        paste0(0, match(month, month.name)), 
                        match(month, month.name)),
         yr_month = paste0(year, month_num)) 

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 9, color = fontcolor),
  
  legend.title = element_text(size = 12, color = fontcolor),
  legend.text = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 16, color = fontcolor),
  
  plot.subtitle = element_text(size = 12, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 7, color = fontcolor),
  
  plot.margin = margin(t = 25, r = 25, b = 10, l = 25)
)

#### Make Plot ####

top_games <- games %>% 
  filter(avg > 150000) %>%
  arrange(avg) %>%
  select(gamename) %>%
  unique(.)

ggplot() +
  geom_line(data = anti_join(games, top_games, by = "gamename"),
            mapping = aes(y = avg,
                          x = yr_month,
                          group = gamename),
            alpha = 0.3,
            size = 0.3) +
  geom_line(data = right_join(games, top_games, by = "gamename"),
            mapping = aes(y = avg,
                          x = yr_month,
                          group = gamename,
                          color = reorder(gamename, -avg)),
            size = 0.7) +
  scale_y_continuous(breaks = seq(0, 1600000, 400000),
                     limits = c(0, 1600000),
                     labels = scales::label_number_si(accuracy = 0.1)) +
  scale_x_discrete(breaks = c("201207", "201301", "201401", "201501", "201601", 
                              "201701", "201801", "201901", "202001", "202101"),
                   labels = c("2012", "2013", "2014", "2015", "2016", 
                              "2017", "2018", "2019", "2020", "2021")) +
  ggthemes::scale_color_stata() +
  guides(color = guide_legend(title.position = "top")) +
  labs(title = "Average number of players at the same time of games on Steam by month",
       subtitle = "July 2012 through Februray 2021",
       x = "",
       y = "",
       color = "Games with more than 150K players at one time:",
       caption = "Data: **SteamCharts** | Viz: **Jenn Schilling**") +
  coord_cartesian(expand = FALSE) +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 9, color = fontcolor, hjust = 0),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown())


ggsave("2021-03-16\\steam_games.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 6,
       type = "cairo")




