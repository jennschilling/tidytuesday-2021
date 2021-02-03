# Author : Jenn Schilling
# Title: #TidyTuesday HBCU
# Date: Feb 2 2021

#### Libraries ####

library(tidyverse)
library(ggthemes)
library(skimr)

#### Get the Data ####

# 1976 - 2015 HBCU enrollment by sex and school type (second dataset is Black student enrollment)

hbcu_all <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

hbcu_black <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_black.csv')


# 1940 - 2016 Bachelor's and High School completions by sex and ethnicity
# Percentage of students aged 25 and older who have graduated HS or Bachelor's

female_bach_students <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/female_bach_students.csv')

female_hs_students <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/female_hs_students.csv')

male_bach_students <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/male_bach_students.csv')

male_hs_students <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/male_hs_students.csv')

#### Data Processing ####

# Change Total label to year
# Make symbols NA / turn into numbers
# Add sex column

female_bach_students <- female_bach_students %>%
  rename(year = Total) %>%
  mutate(across(where(is.character), parse_number),
         sex = 'female')

male_bach_students <- male_bach_students %>%
  rename(year = Total) %>%
  mutate(across(where(is.character), parse_number),
         sex = 'male')

female_hs_students <- female_hs_students %>%
  rename(year = Total) %>%
  mutate(across(where(is.character), parse_number),
         sex = 'female')

male_hs_students <- male_hs_students %>%
  rename(year = Total) %>%
  mutate(across(where(is.character), parse_number),
         sex = 'male')

# Bind male and female together

bach_students <- rbind(female_bach_students,
                       male_bach_students)

hs_students <- rbind(female_hs_students,
                     male_hs_students)

# Pivot long and then wide to get a better data format

bach_students_long <- bach_students %>%
  rename(perc_all = "Total, percent of all persons age 25 and over",
         perc_se = "Standard Errors - Total, percent of all persons age 25 and over") %>%
  pivot_longer(cols = -c(year, sex, perc_all, perc_se), values_drop_na = TRUE) %>%
  filter(!grepl("Total - Asian/Pacific Islander", name)) %>%
  mutate(type = case_when(
    grepl("Standard Error", name) ~ "se_race_eth",
    TRUE ~ "perc_race_eth")) %>%
  mutate(name = str_replace(name, "Standard Errors - ", "")) 

bach_students_wide <- bach_students_long %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  rename(race_ethnicity = name) %>%
  mutate(race_ethnicity = str_replace(race_ethnicity, "1", ""),
         deg = "Bachelor's Degree")

hs_students_long <- hs_students %>%
  rename(perc_all = "Total, percent of all persons age 25 and over",
         perc_se = "Standard Errors - Total, percent of all persons age 25 and over") %>%
  pivot_longer(cols = -c(year, sex, perc_all, perc_se), values_drop_na = TRUE) %>%
  filter(!grepl("Total - Asian/Pacific Islander", name)) %>%
  mutate(type = case_when(
    grepl("Standard Error", name) ~ "se_race_eth",
    TRUE ~ "perc_race_eth")) %>%
  mutate(name = str_replace(name, "Standard Errors - ", "")) 

hs_students_wide <- hs_students_long %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  rename(race_ethnicity = name) %>%
  mutate(race_ethnicity = str_replace(race_ethnicity, "1", ""),
         deg = "High School Diploma")

completions <- rbind(hs_students_wide,
                     bach_students_wide) %>%
  mutate(diff_total = perc_race_eth - perc_all,
         deg = factor(deg, 
                      levels = c("High School Diploma", "Bachelor's Degree"))) 

# Follow a similar process for HBCU data

hbcu_all <- hbcu_all %>% 
  mutate(race_ethnicity = "all") %>%
  janitor::clean_names()

hbcu_black <- hbcu_black %>%
  mutate(race_ethnicity = "black") %>%
  janitor::clean_names()

hbcu_enrl <- rbind(hbcu_all,
                   hbcu_black)

hbcu_enrl_long <- hbcu_enrl %>%
  pivot_longer(cols = -c(year, race_ethnicity))

#### Plot the Data ####

# Completions

ggplot(subset(completions, year >= 2006),
       aes(x = year, 
           y = perc_race_eth,
           color = sex)) +
  geom_line() +
  facet_grid(deg ~ race_ethnicity)

ggplot(subset(completions, year >= 2006),
       aes(x = year, 
           y = diff_total,
           fill = sex)) +
  geom_bar(stat = "Identity",
           position = "dodge") +
  facet_grid(deg ~ race_ethnicity) +
  labs(subtitle = "Difference between the percent of all persons and the percent of persons of each race/ethnicity group 25+ to attain a High School Diploma or Bachelor's Degree",
       xlab = "",
       ylab = "") +
  theme_linedraw()



ggplot(subset(completions, year >= 2006),
       aes(x = year, 
           y = perc_all,
           color = sex)) +
  geom_line() +
  facet_wrap(~ deg, 
             ncol = 1) +
  labs(subtitle = "Percent of all persons 25+ to attain a High School Diploma or Bachelor's Degree")
