# Author : Jenn Schilling
# Title: #TidyTuesday Wealth and Income Over Time
# Date: Nov 09 2021

#### Libraries ####

library(tidyverse)
library(skimr)
library(ggthemes)

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
              race %in% c('Black Alone') &
                year %in% c(1983, 2019)),
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
           y = income_distribution)) +
  geom_col(fill = "#1b9e77") +
  facet_grid(~ year) +
  labs(x = "",
       y = "",
       subtitle = 'Black Alone') +
  guides(fill = FALSE,
         color = FALSE) +
  coord_flip() +
  theme_void()


ggplot(subset(income_distribution,
              race %in% c('White Alone, Not Hispanic') &
                year %in% c(1983, 2019)),
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
           y = income_distribution)) +
  geom_col(fill = "#7570b3") +
  facet_grid(~ year) +
  labs(x = "",
       y = "",
       subtitle = 'White Alone, Not Hispanic') +
  guides(fill = FALSE,
         color = FALSE) +
  coord_flip() +
  theme_void()

ggplot(subset(income_distribution,
              race %in% c('Hispanic (Any Race)') &
                year %in% c(1983, 2019)),
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
           y = income_distribution)) +
  geom_col(fill = "#d95f02") +
  facet_grid(~ year) +
  labs(x = "",
       y = "",
       subtitle = 'Hispanic (Any Race)') +
  guides(fill = FALSE,
         color = FALSE) +
  coord_flip() +
  theme_void()

ggplot(subset(race_wealth, 
              year >= 1983 &
              type == 'Median' &
              !is.na(wealth_family)),
       aes(x = year,
           y = wealth_family, # (median) family wealth, normalized to 2016 dollars
           color = race)) +
  geom_line() +
  geom_point()

ggplot(subset(race_wealth, 
              year >= 1983 &
                type == 'Average' &
                !is.na(wealth_family)),
       aes(x = year,
           y = wealth_family, # (average) family wealth, normalized to 2016 dollars
           color = race)) +
  geom_line() +
  geom_point()


ggplot(subset(home_owner),
       aes(x = year,
           y = home_owner_pct, # homeownership percentage
           color = race)) +
  geom_line() +
  geom_point()


ggplot(subset(retirement),
       aes(x = year,
           y = retirement, # average family liquid retirement savings, normalized to 2016 dollars
           color = race)) +
  geom_line() +
  geom_point()

ggplot(subset(student_debt),
       aes(x = year,
           y = loan_debt_pct, # percent of families with student loan debt
           color = race)) +
  geom_line() +
  geom_point()

ggplot(subset(student_debt),
       aes(x = year,
           y = loan_debt, # average family student loan debt for aged 25-55, normalized to 2016 dollars
           color = race)) +
  geom_line() +
  geom_point()
