# Author : Jenn Schilling
# Title: #TidyTuesday Wealth and Income Over Time
# Date: Nov 09 2021

#### Libraries ####

library(tidyverse) 
library(skimr) # view data summary
library(ggthemes) # extra themes
library(ggtext) # text formatting
library(wesanderson) # color palettes
library(patchwork) # put plots together 

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

income_hist <- ggplot(subset(income_distribution,
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
  labs(title = '**Income Distribution**',
       subtitle = '*proportion of people by race and income bracket*<br>') +
#  scale_fill_brewer(palette = "Dark2") +
  scale_fill_manual(values = wes_palette("Darjeeling1")) +
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
 # scale_y_continuous(position = "right",
 #                      breaks = c(0, 25),
 #                      limits = c(0, 31),
 #                      labels = scales::percent_format(scale = 1)) +
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
                                        size = 12,
                                        face = 'bold'),
        axis.text.x = element_markdown(size = 8),
        plot.background = element_rect(fill = "#F0F0F0", color = NA),
        plot.margin = margin(t = 25, r = 25, b = 10, l = 25))


avg_wealth <- ggplot(subset(race_wealth, 
              year %in% c(1989, 2016) &
                type == 'Average' &
                !is.na(wealth_family)),
       aes(x = year,
           y = wealth_family, # (average) family wealth, normalized to 2016 dollars
           color = race)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #  scale_color_brewer(palette = "Dark2") + 
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  scale_x_continuous(breaks = c(1989, 2016)) +
  scale_y_continuous(breaks = c(250000, 500000, 750000), 
                     labels = scales::dollar) +
  guides(color = FALSE) +
  labs(title = '**Family Wealth**',
       subtitle = '*average wealth; normalized to 2016 $*') +
  theme_void() +
  theme(plot.title = element_markdown(size = 10),
        plot.subtitle = element_markdown(size = 8),
        plot.title.position = "plot",
        axis.text = element_markdown(size = 7),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(.1, "cm"),
        plot.background = element_rect(fill = "#F0F0F0", color = NA),
        plot.margin = margin(t = 25, r = 25, b = 10, l = 25))


home <- ggplot(subset(home_owner,
              year %in% c(1989, 2016)),
       aes(x = year,
           y = home_owner_pct, # home ownership percentage
           color = race)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #  scale_color_brewer(palette = "Dark2") + 
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  scale_x_continuous(breaks = c(1989, 2016)) +
  scale_y_continuous(limits = c(0.38, 0.72),
                     breaks = c(0.40,  0.5, 0.60, 0.70), 
                     labels = scales::percent) +
  guides(color = FALSE) +
  labs(title = '**Home Ownership**',
       subtitle = '*percent of people by race who own a home*') +
  theme_void() +
  theme(plot.title = element_markdown(size = 10),
        plot.subtitle = element_markdown(size = 8),
        plot.title.position = "plot",
        axis.text = element_markdown(size = 7),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(.1, "cm"),
        plot.background = element_rect(fill = "#F0F0F0", color = NA),
        plot.margin = margin(t = 25, r = 25, b = 10, l = 25))


retire <- ggplot(subset(retirement,
              year %in% c(1989, 2016)),
       aes(x = year,
           y = retirement, # average family liquid retirement savings, normalized to 2016 dollars
           color = race)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #  scale_color_brewer(palette = "Dark2") + 
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  scale_x_continuous(breaks = c(1989, 2016)) +
  scale_y_continuous(limits = c(0, 165000),
                     breaks = c(0, 40000, 80000, 120000, 160000), 
                     labels = scales::dollar) +
  guides(color = FALSE) +
  labs(title = '**Family Liquid Retirement Savings**',
       subtitle = '*average savings; normalized to 2016 $*') +
  theme_void() +
  theme(plot.title = element_markdown(size = 10),
        plot.subtitle = element_markdown(size = 8),
        plot.title.position = "plot",
        axis.text = element_markdown(size = 7),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(.1, "cm"),
        plot.background = element_rect(fill = "#F0F0F0", color = NA),
        plot.margin = margin(t = 25, r = 25, b = 10, l = 25))

amount_student <- ggplot(subset(student_debt,
              year %in% c(1989, 2016)),
       aes(x = year,
           y = loan_debt, # average family student loan debt for aged 25-55, normalized to 2016 dollars
           color = race)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #  scale_color_brewer(palette = "Dark2") + 
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  scale_x_continuous(breaks = c(1989, 2016)) +
  scale_y_continuous(limits = c(0, 17000),
                     breaks = c(0, 5000, 10000, 15000), 
                     labels = scales::dollar) +
  guides(color = FALSE) +
  labs(title = '**Family Student Loan Debt**',
       subtitle = '*average debt; normalized to 2016 $*') +
  theme_void() +
  theme(plot.title = element_markdown(size = 10),
        plot.subtitle = element_markdown(size = 8),
        plot.title.position = "plot",
        axis.text = element_markdown(size = 7),
        axis.ticks.y = element_line(),
        axis.ticks.length.y = unit(.1, "cm"),
        plot.background = element_rect(fill = "#F0F0F0", color = NA),
        plot.margin = margin(t = 25, r = 25, b = 10, l = 25))

final_plot <- (home | avg_wealth | retire | amount_student) / income_hist 

ggsave("2021-02-09\\wealth_distribution.png",
       plot = final_plot,
       device = "png",
       width = 12,
       height = 7,
       dpi = 300)
