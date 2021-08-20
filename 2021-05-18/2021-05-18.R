# Author : Jenn Schilling
# Title: #TidyTuesday Ask a Manager Survey
# Date: May 18 2021

#### Libraries ####

library(tidyverse)
library(gghalves)
library(forcats)
library(scales)
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
  panel.grid.major = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text.x = element_text(size = 9, color = fontcolor),
  axis.text.y = element_blank(),
  
  axis.line.x = element_blank(),
  axis.ticks.x = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
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
  
#### Graph Data ####

ggplot(data = us_survey,
       mapping = aes(y = annual_salary,
                     x = fct_rev(industry),
                     color = industry,
                     fill = industry)) +
  geom_half_violin(side = "r") +
  geom_half_point(shape = 39,
                  range_scale = 0,
                  size = 4,
                  side = "l") +
  geom_text(data = us_survey %>% 
              group_by(industry) %>%
              summarise(max_salary = max(annual_salary),
                        .groups = "drop") %>%
              mutate(max_salary = case_when(
                industry == "Education (Higher Education)" ~ max_salary * 1.2,
                industry == "Education (Primary/Secondary)" ~ max_salary * 1.43,
                TRUE ~ max_salary)),
            mapping = aes(y = max_salary,
                          x = fct_rev(industry),
                          color = industry,
                          label = industry),
            family = font,
            hjust =  1,
            vjust = -0.5) +
  scale_y_continuous(labels = dollar_format(),
                     limits = c(0, 2e6),
                     breaks = seq(from = 0, to = 2e6, by = 250000)) +
  scale_color_manual(values = c("#208eb7", "#1c4c5e", "#7ec993", "#1c5f1e", 
                                "#9bc732", "#5826a6", "#bcaff9", "#5064be", 
                                "#cd49dc", "#af2168", "#f372a8", "#7b3f5b")) +
  scale_fill_manual(values = c("#208eb7", "#1c4c5e", "#7ec993", "#1c5f1e", 
                               "#9bc732", "#5826a6", "#bcaff9", "#5064be", 
                               "#cd49dc", "#af2168", "#f372a8", "#7b3f5b")) +
  coord_flip() +
  guides(color = "none",
         fill = "none") +
  labs(title = "Annual salary in the United States by Industry",
       x = "",
       y = "",
       caption = "**Data:** Ask a Manager Salary Survey | **Design**: Jenn Schilling") 

ggsave("2021-05-18\\us_salaries.png",
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 7,
       height = 9,
       dpi = 300)

