# Author : Jenn Schilling
# Title: #TidyTuesday Ask a Manager Survey
# Date: May 18 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)

#### Data #### 

survey <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 12, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Subset Data ####

us_survey <- survey %>%
  mutate(country = tolower(country)) %>%
  filter(country %in% c('usa', 'us', 'united states', 'united states of america') &
           currency == 'USD' &
           industry %in% c('Computing or Tech',
                           'Law',
                           'Business or Consulting',
                           'Engineering or Manufacturing',
                           'Accounting, Banking & Finance',
                           'Health care',
                           'Marketing, Advertising & PR',
                           'Media & Digital',
                           'Government and Public Administration',
                           'Nonprofits',
                           'Education (Higher Education)',
                           'Education (Primary/Secondary)') &
           !is.na(gender)) %>%
  mutate(gender = factor(gender,
                         levels = c('Other or prefer not to answer',
                                    'Non-binary', 
                                    'Woman',
                                    'Man'))) %>%
  mutate(race = str_replace(race, 'Hispanic, Latino, or Spanish origin', 'Hispanic or Latino'),
         race_agg = ifelse(str_detect(race, ','), 'Two or more races', race),
         race_agg = ifelse(is.na(race_agg), 'Not reported', race_agg))
  
us_survey_agg <- us_survey %>%
  mutate(race_sep = race) %>%
  separate(race_sep,
           c('race_1', 'race_2', 'race_3', 'race_4'),
           sep = ', ') %>%
  pivot_longer(race_1:race_4, 
               values_to = 'race_all',
               values_drop_na = TRUE) %>%
  select(-name) %>%
  group_by(industry, gender, race_all) %>%
  summarise(median_salary = median(annual_salary),
            n = n(),
            .groups = 'drop')

us_survey_agg_gender <- us_survey %>%
  group_by(industry, gender) %>%
  summarise(median_salary = median(annual_salary),
            n = n(),
            .groups = 'drop')


#### Graph Data ####

ggplot(data = us_survey) +
  geom_boxplot(mapping = aes(x = annual_salary,
                             color = gender)) +
  facet_wrap(~reorder(industry, -annual_salary),
             scales = 'free') +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-3,
                                                    suffix = "K"))
  coord_cartesian(clip = "off",
                  expand = FALSE) +
  theme(axis.text.y = element_blank())
  
  
point_data <- us_survey_agg %>%
  filter(race_all != 'Another option not listed here or prefer not to answer') %>%
  filter(race_all != 'Middle Eastern or Northern African') %>%
  filter(gender != 'Other or prefer not to answer') %>%
  filter(gender != 'Non-binary') %>%
  filter(n > 1) %>%
  mutate(race_all = factor(race_all,
                           levels = c('White',
                                      'Asian or Asian American',
                                      'Black or African American',
                                      'Hispanic or Latino',
                                      'Native American or Alaska Native')))

line_data <- point_data %>%
  group_by(industry, race_all) %>%
  summarise(min_med_salary = min(median_salary),
            max_med_salary = max(median_salary),
            .groups = 'drop')

ggplot() +
  geom_point(data = point_data,
             mapping = aes(x = median_salary,
                           y = race_all,
                           color = gender)) +
  geom_segment(data = line_data,
               mapping = aes(x = min_med_salary, 
                             xend = max_med_salary,
                             y = race_all,
                             yend = race_all)) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_discrete() +
  facet_wrap(~industry,
             ncol = 2,
             scales = 'free_y') +
  theme(panel.grid.major = element_blank())


