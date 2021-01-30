# Author : Jenn Schilling
# Title: #TidyTuesday Plastics
# Date: Jan 26 2021

#### Libraries ####

library(tidyverse)
library(skimr)
library(ggalluvial)
library(ggrepel)

#### Get the Data ####

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

skim(plastics)

#### Data Cleaning ####

# Replace null with unbranded
plastics_clean <- plastics %>%
  # Replace null with Unbranded in Parent Company
  mutate(parent_company = ifelse(tolower(parent_company) == 'null', 
                                 'Unbranded', 
                                 parent_company)) %>%
  # Remove Grand Total in Parent Company
  filter(!(tolower(parent_company) == "grand total")) %>%
  # Standardize Nestlé vs Nestle
  mutate(parent_company = ifelse(parent_company == 'Nestle',
                                 'Nestlé',
                                 parent_company)) %>%
  # Make long by plastic type
  pivot_longer(empty:grand_total, 
               names_to = 'plastic_type', 
               values_to = 'count',
               values_drop_na = TRUE) %>%
  # Remove 0 counts
  filter(count != 0)

#### Explore the Data ####

# Companies Worldwide - must be in more than 1 country
plastics_company <- plastics_clean %>%
  group_by(year, parent_company, plastic_type) %>%
  summarise(total = sum(count),
            n = n()) %>%
  filter(n > 1) %>%
  ungroup()

# Which company has the highest plastic count?
plastics_company %>%
  filter(parent_company != 'Unbranded') %>%
  filter(plastic_type == 'grand_total') %>%
  arrange(-total) 

# Which company was found in the most countries?
plastics_company %>%
  filter(parent_company != 'Unbranded') %>%
  filter(plastic_type == 'grand_total') %>%
  arrange(-n)

# Which plastic type has the highest count?
plastics_company %>%
  filter(plastic_type != 'grand_total') %>%
  group_by(year, plastic_type) %>%
  summarise(total = sum(total)) %>%
  arrange(-total)

# PET - Polyester plastic count (Polyester fibers, soft drink bottles, food containers, plastic bottles)

# Which company has the highest PET count?
plastics_company %>%
  filter(parent_company != 'Unbranded') %>%
  filter(plastic_type == 'pet') %>%
  arrange(-total) 

#### Plot the Data ####

# Code Source: https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

plastics_company_sub <- plastics_company %>%
  # Fix PepsiCo names so the years match
  mutate(parent_company = ifelse(parent_company == 'Pepsico',
                                 'PepsiCo',
                                 parent_company)) %>%
  # Remove grand total and empty plastic types
  filter(plastic_type != 'grand_total' &
           plastic_type != 'empty') %>%
  # Get companies with highest totals
  filter(parent_company %in% c(
                              'The Coca-Cola Company',
                              'PepsiCo',
                              'Nestlé',
                              'Pure Water, Inc.',
                              'Universal Robina Corporation',
                              'Colgate-Palmolive',
                              'Unilever'
                              )) %>%
  # Just 2020
  filter(year == 2020) %>%
  # Capitalize Plastic Code
  mutate(plastic_type = toupper(plastic_type))
 

ggplot(plastics_company_sub,
       aes(y = total,
           axis1 = fct_reorder(plastic_type, total),
           axis2 = fct_reorder(parent_company, total),
           fill = fct_reorder(plastic_type, total))) +
  geom_alluvium(width = 1/16) +
  geom_stratum(width = 1/16, alpha = 0.5) +
  geom_text(stat = "stratum", 
            aes(label = fct_reorder(parent_company, total)),
            nudge_x = 0.05) + #after_stat(stratum))) +
  coord_flip() +
  labs(title = "Plastic Types from the Companies with the Most Plastic Found Worldwide in 2020",
       subtitle = "Plastic waste is collected by volunteers around the world at Break Free from Plastic cleanup events.Volunteers track the number and types of plastic found by company.
This graph represents the volume of plastic by type and company collected at events in 2020, for the  six companies with the most plastic found over all the events.",
       fill = "Plastic Type") +
 # guides(fill = FALSE) +
  theme_void() +
  scale_fill_brewer(palette = "Dark2",
    limits =  c("PP", "O", "PET", "HDPE", "LDPE", "PVC", "PS"),
    labels =  c("Polypropylene", 
                "Other", 
                "Polyethylene terephthalate", 
                "High-density polyethylene", 
                "Low-density polyethylene", 
                "Polyvinyl chloride", 
                "Polystyrene")) +
  theme(legend.position = "bottom")
