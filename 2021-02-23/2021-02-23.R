# Author : Jenn Schilling
# Title: #TidyTuesday Employment and Earnings
# Date: Feb 23 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(skimr)
library(ggthemes)
library(ggtext)
library(sysfonts)
library(showtext)

#### Fonts ####

font_add_google("Lato") # use with sysfonts library

title_font <- "Consolas"
axis_font <- "Microsoft Sans Serif"

#### Colors ####

background <- "#f0e4d8"
font_color <- "gray50"
label_color <- "gray10"
pal <- c("#db2a41", "#f8c211", "#5b688a", "#e9ddce", "#ac9077", 
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

#### Employment Data Processing ####

# Goal is to recreate: https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/challenge03/original-plate-27.jpg

employed_industry <- employed %>%
  mutate(industry = str_replace_all(industry, "[\r]" , ""),
         industry = str_replace_all(industry, "[\n]" , " ")) %>% # make Mining industry label consistent
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

# Subset of employment data; add Du Bois classification
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

# Plot Data
employed_industry_sub_20 <- employed_industry_sub %>%
  ungroup() %>%
  filter(year == 2020) %>%
  # From: https://github.com/winterstat/tidytuesday/blob/master/R/tt2021_wk8.R
  mutate(pct_label = pct,
         pct = (pct * 61)/ 100) %>% # Make circular plot points
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
                             "DOMESTIC AND PERSONAL SERVICE",
                             "MANUFACTURING AND MECHANICAL INDUSTRIES",
                             "TRADE AND TRANSPORTATION",
                             "PROFESSIONS",
                             ""),
                         ordered = T)) %>%
  arrange(race_gender, dubois)

# Labeling the plot
employed_industry_sub_20_labs <- employed_industry_sub_20 %>%
  filter(!is.na(employ_n)) %>%
  mutate(pct_label = scales::percent(pct_label, accuracy = 0.1)) %>%
  mutate(
    x = case_when(
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "AGRICULTURE, FISHERIES AND MINING" ~ 0.8,
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "DOMESTIC AND PERSONAL SERVICE" ~ 0.85,
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "MANUFACTURING AND MECHANICAL INDUSTRIES" ~ 0.92,
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "TRADE AND TRANSPORTATION" ~ 1.02,
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "PROFESSIONS" ~ 1.25,
      race_gender == "WHITE" &
        dubois == "AGRICULTURE, FISHERIES AND MINING" ~ 1.6,
      race_gender == "WHITE" &
        dubois == "DOMESTIC AND PERSONAL SERVICE" ~ 0.05,
      race_gender == "WHITE" &
        dubois == "MANUFACTURING AND MECHANICAL INDUSTRIES" ~ 0.15,
      race_gender == "WHITE" &
        dubois == "TRADE AND TRANSPORTATION" ~ 0.25,
      race_gender == "WHITE" &
        dubois == "PROFESSIONS" ~ 0.45,
      TRUE ~ 0 ),
    y = case_when(
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "AGRICULTURE, FISHERIES AND MINING" ~ 1.45,
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "DOMESTIC AND PERSONAL SERVICE" ~ 1.35,
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "MANUFACTURING AND MECHANICAL INDUSTRIES" ~ 1.35,
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "TRADE AND TRANSPORTATION" ~ 1.35,
      race_gender == "BLACK OR AFRICAN AMERICAN" &
        dubois == "PROFESSIONS" ~ 1.35,
      race_gender == "WHITE" &
        dubois == "AGRICULTURE, FISHERIES AND MINING" ~ 1.50,
      race_gender == "WHITE" &
        dubois == "DOMESTIC AND PERSONAL SERVICE" ~ 1.35,
      race_gender == "WHITE" &
        dubois == "MANUFACTURING AND MECHANICAL INDUSTRIES" ~ 1.35,
      race_gender == "WHITE" &
        dubois == "TRADE AND TRANSPORTATION" ~ 1.35,
      race_gender == "WHITE" &
        dubois == "PROFESSIONS" ~ 1.35,
      TRUE ~ 0 ))


#### Employment Plot ####

ggplot(data = employed_industry_sub_20,
       mapping = aes(y = "",
                     x = pct,
                     fill = dubois,
                     color = dubois,
                     group = race_gender)) +
  
  geom_point() + # to get circles in the legend
  
  geom_col(show.legend = FALSE) +
  
  geom_text(data = employed_industry_sub_20_labs,
            mapping = aes(x = x,
                          y = y,
                          label = pct_label),
            color = label_color,
            size = 2.5,
            family = axis_font) +
  
  coord_polar(start = pi - (65*pi)/180) +
  
  labs(title = "OCCUPATIONS OF BLACK AND WHITE PEOPLE\nIN THE UNITED STATES IN 2020.\n",
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
        
        plot.margin = margin(t = 10, r = 5, b = 10, l = 5),
        
        plot.title = element_text(family = title_font, size = 11,
                                  face = "bold", hjust = 0.5),
        
        plot.caption = element_text(family = axis_font, color = font_color, size = 7),
        
        plot.title.position = "plot",
        plot.caption.position = "plot"
        
  ) +
  
  annotate("text", x = 1.1, y = 1.6, label = "BLACK.", 
           family = axis_font, color = font_color, size = 3.5) +
  
  annotate("text", x = 0.28, y = 1.6, label = "WHITE.", 
           family = axis_font, color = font_color, size = 3.5)

ggsave("2021-02-23\\occupations_2020.png",
       plot = last_plot(),
       device = "png",
       width = 7,
       height = 7,
       type = "cairo")



#### Earnings Data Plot ####

earn_sub <- earn %>%
  filter(sex %in% c('Men', 'Women') &
           race %in% c('Black or African American', 'White') &
           age %in% c('16 to 24 years', '25 to 54 years', '55 years and over')) %>%
  mutate(year_quarter = paste(year, quarter, sep = "_"))

showtext_auto()

ggplot(data = earn_sub,
       mapping = aes(x = year_quarter,
                     y = median_weekly_earn,
                     color = race,
                     group = race)) +
  geom_line() +
  scale_x_discrete(breaks = c("2010_2", "2012_2", "2014_2", 
                              "2016_2", "2018_2", "2020_2"),
                   labels = c("2010", "2012", "2014",
                              "2016", "2018", "2020")) +
  scale_y_continuous(labels = scales::dollar,
                     position = "right") +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  facet_grid(age~sex,
             switch = "y") +
  labs(title = "Weekly Median Earnings (2010-2020)",
       subtitle = 'Men consistently outearn women. 
       <b style="color:#1b9e77">Black</b> people earn less than <b style="color:#7570b3">white</b> people',
       x = "",
       y = "",
       color = "",
       caption = "Source: U.S. Bureau of Labor Statistics | Viz: Jenn Schilling") +
  guides(color = FALSE) +
#  theme_few() +
  theme_fivethirtyeight() +  
  theme(text = element_text(family = "Lato", size = 20),
        
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
      
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        strip.text.y.left = element_text(angle = 0,
                                         vjust = 1,
                                         hjust = 1))

ggsave("2021-02-23\\earnings_age_2020.png",
       plot = last_plot(),
       device = "png",
       width = 5,
       height = 5,
       type = "cairo")
