# Author : Jenn Schilling
# Title: #TidyTuesday Employment and Earnings
# Date: Feb 23 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(skimr)

#### Fonts ####

#font_add_google("Lato") <- use with sysfonts library

title_font <- "Consolas"
axis_font <- "Microsoft Sans Serif"

#### Colors ####

background <- "#f0e4d8"
font_color <- "gray50"
pal <- c("#db2a41", "#5b688a", "#f8c211", "#ac9077", "#e9ddce",
         "#f0e4d8") # extra blocks to separate bars
pal_outline <- c("gray50", "gray50", "gray50", "gray50", "gray50",
                 "#f0e4d8")

#### Data ####

# Employed persons by industry, sex, race, and occupation (2015-2020)
employed <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

# Weekly median earnings and number of persons employed by race/gender/age group over time (2015-2020)
earn <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

#### Data Exploration ####

skim(employed)

table(employed$race_gender)

table(employed$industry)

skim(earn)

table(earn$race, earn$ethnic_origin)

#### Data Processing ####

# Goal is to recreate: https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/challenge03/original-plate-27.jpg

employed_industry <- employed %>%
  mutate(industry = str_replace_all(industry, "[\r]" , ""),
         industry = str_replace_all(industry, "[\n]" , " ")) %>%
  group_by(year, industry, race_gender) %>%
  summarise(employ_n = sum(employ_n),
            .groups = "keep") %>%
  filter(!is.na(employ_n)) %>% # there are some race_gender values in the industry column that have NA
  ungroup()
  
# Map Du Bois Categories to Industries
classification <- tibble(
  dubois = c("Agriculture, Fisheries and Mining",
             "Manufacturing and Mechanical Industries",
             "Professions",
             "Professions",
             "Professions",
             "Domestic and Personal Service",
             "Manufacturing and Mechanical Industries",
             "Agriculture, Fisheries and Mining",
             "Domestic and Personal Service",
             "Professions",
             "Professions",
             "Trade and Transportation",
             "Trade and Transportation"
             ),
  industry = c("Agriculture and related",
               "Construction",
               "Education and health services",
               "Financial activities",
               "Information",
               "Leisure and hospitality",
               "Manufacturing",
               "Mining, quarrying, and oil and gas extraction",
               "Other services",
               "Professional and business services",
               "Public administration",
               "Transportation and utilities",
               "Wholesale and retail trade"
               ))

employed_industry_sub <- employed_industry %>%
  filter(race_gender %in% c('White', 'Black or African American')) %>%
  # Remove sub-industries that are included in other industries
  # Based on data source: https://www.bls.gov/cps/cpsaat17.htm
  filter(industry != "Durable goods" & # Manufacturing
           industry != "Nondurable goods" & # Manufacturing
           industry != "Wholesale trade" & # Wholesale and retail trade
           industry != "Retail trade" & # Wholesale and retail trade 
           industry != "Other services, except private households" & # Other services
           industry != "Private households" # Other services
  ) %>%
  left_join(classification, by = "industry") %>%
  group_by(year, race_gender, dubois) %>%
  summarise(employ_n = sum(employ_n), .groups = "keep") %>%
  ungroup() %>%
  group_by(year, race_gender) %>%
  mutate(pct = employ_n / sum(employ_n)) %>%
  mutate(dubois = toupper(dubois),
         race_gender = toupper(race_gender))

employed_industry_sub_20 <- employed_industry_sub %>%
  ungroup() %>%
  filter(year == 2020) %>%
  # From: https://github.com/winterstat/tidytuesday/blob/master/R/tt2021_wk8.R
  mutate(pct = (pct * 61)/ 100) %>% 
  add_row(year = 2020,
          race_gender = "BLACK OR AFRICAN AMERICAN",
          dubois = "",
          pct = 0.195)  %>%
  add_row(year = 2020,
          race_gender = "WHITE",
          dubois = "",
          pct = 0.195) %>%
  mutate(dubois = factor(dubois,
                         levels = 
                           c("AGRICULTURE, FISHERIES AND MINING",
                             "MANUFACTURING AND MECHANICAL INDUSTRIES",
                             "DOMESTIC AND PERSONAL SERVICE",
                             "PROFESSIONS",
                             "TRADE AND TRANSPORTATION",
                             "")))

employed_industry_sub_20_labs <- employed_industry_sub_20 %>%
  filter(!is.na(employ_n)) %>%
  mutate(pct = scales::percent(pct, accuracy = 0.1)) %>%
  mutate(
    x = case_when(
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "AGRICULTURE, FISHERIES AND MINING" ~ 0.8,
      TRUE ~ 0 ),
    y = case_when(
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "AGRICULTURE, FISHERIES AND MINING" ~ 1.4,
      TRUE ~ 0 ))


#### Plot ####

ggplot(data = employed_industry_sub_20,
       mapping = aes(y = "",
                     x = pct,
                     fill = dubois,
                     color = dubois,
                     group = race_gender)) +
  
  geom_point() +
  
  geom_col(show.legend = FALSE) +
  
  geom_text(data = employed_industry_sub_20_labs,
            mapping = aes(x = x,
                          y = y,
                          label = pct),
            color = font_color) +
  
  coord_polar(start = pi - (65*pi)/180) +
  
  labs(title = "OCCUPATIONS OF BLACK AND WHITE PEOPLE IN THE UNITED STATES IN 2020.\n",
       x = "",
       y = "",
       color = "",
       caption = "Source: U.S. Bureau of Labor Statistics | Viz: Jenn Schilling") +
  
  scale_fill_manual(values = pal) +
  
  scale_color_manual(values = pal) +
  
  guides(fill = FALSE,
         color = guide_legend(ncol = 2, override.aes = list(size = 6,
                                                            fill = pal,
                                                            color = pal_outline,
                                                            shape = 21))) +
  
  theme(legend.background = element_rect(fill = background, color = NA),
        legend.key = element_blank(),
        legend.text = element_text(family = axis_font, 
                                   size = 7,
                                   color = font_color),

        legend.position = "bottom",
        
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.background = element_rect(fill =  background, color = NA),
        panel.background = element_rect(fill = background, color = NA),
        
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
        
        plot.title = element_text(family = title_font, size = 11,
                                  face = "bold", hjust = 0.5),
        
        plot.caption = element_text(family = axis_font, color = font_color, size = 7),
        
        plot.title.position = "plot",
        plot.caption.position = "plot"
        
  ) +
  annotate("text", x = 1.1, y = 1.5, label = "BLACK.", 
           family = axis_font, color = font_color, size = 3.5) +
  annotate("text", x = 0.28, y = 1.5, label = "WHITE.", 
           family = axis_font, color = font_color, size = 3.5)

ggsave("2021-02-23\\occupations_2020.png",
       plot = last_plot(),
       device = "png",
       width = 7,
       height = 7,
       type = "cairo")

