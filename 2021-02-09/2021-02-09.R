# Author : Jenn Schilling
# Title: #TidyTuesday Wealth and Income Over Time
# Date: Nov 09 2021

#### Libraries ####

library(tidyverse)
library(skimr)
library(ggthemes)
library(ggtext)

#### Get the Data ####

student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')

#### Explore the Data ####

skim(student_debt) # Starts in 1989, Black/Hispanic/White

skim(retirement) # Starts in 1989, Black/Hispanic/White

skim(home_owner) # Starts in 1976, Black/Hispanic/White

skim(race_wealth) # Starts in 1963, but more consistent starting in 1983, Black/Hispanic/Non-White/White, Average/Median

skim(income_distribution) # Starts in 1967, 8 racial groups


### Make Graphs ####

ggplot(subset(income_distribution,
              race %in% c('Black Alone',
                          'White Alone, Not Hispanic',
                          'Hispanic (Any Race)') &
                year %in% c(1989, 2016)) %>%
         mutate(race = case_when(
           race == 'Black Alone' ~ 'Black',
           race == 'Hispanic (Any Race)' ~ 'Hispanic',
           TRUE ~ 'White'
         )),
       aes(x = factor(income_bracket,
                      levels = c('Under $15,000',
                                 '$15,000 to $24,999',
                                 '$25,000 to $34,999',
                                 '$35,000 to $49,999',
                                 '$50,000 to $74,999',
                                 '$75,000 to $99,999',
                                 '$100,000 to $149,999',
                                 '$150,000 to $199,999',
                                 '$200,000 and over')),
           y = income_distribution,
           fill = race)) +
  geom_col() +
  facet_grid(race ~ year,
             switch = "y") +
  labs(title = '**Income Distribution by Race in 1989 and 2016**',
       subtitle = '*Proportion of people by income bracket*<br>') +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = FALSE,
         color = FALSE) +
  scale_x_discrete(labels = c('under<br>$15,000',
                              '',
                              '',
                              '',
                              '',
                              '',
                              '',
                              '',
                              'over<br>$200,000')) +
  theme_void() +
  theme(plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 12),
        plot.title.position = "plot",
        strip.text.y.left = element_markdown(angle = 0, 
                                             vjust = 1, 
                                             hjust = 1,
                                             size = 12,
                                             face = 'bold'),
        strip.text.x = element_markdown(vjust = 1, 
                                        size = 14,
                                        face = 'bold'),
        axis.text.x = element_markdown(size = 8),
        plot.background = element_rect(fill = "#F0F0F0", color = NA),
        plot.margin = margin(t = 25, r = 25, b = 10, l = 25))


# NEed to fix jagged line

ggplot(subset(race_wealth, 
              year %in% c(1989, 2016) &
              type == 'Median' &
              race != 'Non-White'),
       aes(x = year,
           y = wealth_family, # (median) family wealth, normalized to 2016 dollars
           color = factor(race, levels = c('Black', 'Hispanic', 'White', 'Non-White')))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = c(1989, 2016)) +
  scale_y_continuous(breaks = c(0, 50000, 100000, 150000), 
                     labels = scales::dollar) +
  guides(color = FALSE) +
  labs(title = '**Median Family Wealth by Race in 1989 and 2016**',
       subtitle = '*Normalized to 2016 dollars*') +
  theme_void() +
  theme(plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 12),
        plot.title.position = "plot",
        axis.text = element_markdown(size = 12),
        axis.ticks = element_line(),
        plot.background = element_rect(fill = "#F0F0F0", color = NA),
        plot.margin = margin(t = 25, r = 25, b = 10, l = 25))

ggplot(subset(race_wealth, 
              year >= 1983 &
                type == 'Average' &
                !is.na(wealth_family)),
       aes(x = year,
           y = wealth_family, # (average) family wealth, normalized to 2016 dollars
           color = race)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  geom_point()


ggplot(subset(home_owner),
       aes(x = year,
           y = home_owner_pct, # homeownership percentage
           color = race)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  geom_point()


ggplot(subset(retirement),
       aes(x = year,
           y = retirement, # average family liquid retirement savings, normalized to 2016 dollars
           color = race)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  geom_point()

ggplot(subset(student_debt),
       aes(x = year,
           y = loan_debt_pct, # percent of families with student loan debt
           color = race)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  geom_point()

ggplot(subset(student_debt),
       aes(x = year,
           y = loan_debt, # average family student loan debt for aged 25-55, normalized to 2016 dollars
           color = race)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  geom_point()
