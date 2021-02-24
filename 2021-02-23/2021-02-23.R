# Author : Jenn Schilling
# Title: #TidyTuesday Employment and Earnings
# Date: Feb 23 2021

#### Libraries ####

library(tidyverse)
library(sysfonts)
library(patchwork)
library(skimr)

#### Fonts ####
font_add_google("Lato")

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

# Goal is to recreate: https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge03/original-plate-27.jpg

employed_industry <- employed %>%
  group_by(year, industry, race_gender) %>%
  summarise(employ_n = sum(employ_n),
            .groups = "keep") %>%
  filter(!is.na(employ_n)) %>% # there are some race_gender values in the industry column that have NA
  ungroup()
  
employed_industry_sub <- employed_industry %>%
  filter(race_gender %in% c('White', 'Black or African American')) %>%
  mutate(pct = employ_n / sum(employ_n)) # not working

#### Plot ####

