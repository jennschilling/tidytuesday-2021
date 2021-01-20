# Author : Jenn Schilling
# Title: #TidyTuesday Kenya Census
# Date: Jan 19 2021

#### Libraries ####

library(tidyverse)
library(skimr)
library(ggrepel)

#### Get the Data ####

gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')

#### Explore the Data ####

skim(gender)

#### Data Processing ####

# Proportion of Women
gender_prop <- gender %>%
  filter(County != 'Total') %>%
  mutate(prop_female = Female / Total) %>%
  mutate(County = toupper(County)) %>%
  select(County, prop_female, Total)

# Get Shape Files
kenya_county_shape <- rKenyaCensus::KenyaCounties_SHP

# Make a copy of the shape data
kenya_county_shape_combined <- kenya_county_shape

# Give data an id to match shapes
kenya_county_shape_combined@data$id <- rownames(kenya_county_shape_combined@data)

# Add gender data 
kenya_county_shape_combined@data <- full_join(kenya_county_shape_combined@data,
                                              gender_prop,
                                              by = "County")

# Process Shape Files into a Data Frame
# Source: https://medium.com/@wanjirumaggie45/beautiful-mapping-with-r-part-1-5fa56312f063
kenya_county_shape_df <- fortify(kenya_county_shape_combined)

# Put it all together
kenya_county_shape_df <- full_join(kenya_county_shape_df,
                                   kenya_county_shape_combined@data,
                                   by = "id")

# Make a Map
map <- ggplot(data = kenya_county_shape_df, 
              aes(x = long, 
                  y = lat, 
                  group = group,
                  fill = prop_female))

map + 
  geom_polygon() + 
  scale_fill_distiller(palette = "Purples",
                       direction = 1,
                       labels = scales::percent_format(1L)) +
  labs(title = "Proportion of Female Identifying People by County in Kenya in 2019",
       caption = "TidyTuesday 19 Jan 2021 | Data: {rKenyaCensus} | Designer: Jenn Schilling | jennschilling.me",
       fill = "% Female") +
  theme_void() +
  theme(aspect.ratio = 1) # Resize map nicely

# Population by County
pop <- ggplot(data = gender_prop,
              aes(y = reorder(County, Total),
                  x = Total,
                  label = scales::percent(prop_female, accuracy = 1L)))

pop + 
  geom_bar(stat = "identity") +
  geom_text(hjust = "left",
            color = "Purple") +
  scale_x_continuous(expand = expansion(mult = c(0, .1)),  # move bars to labels 
                     labels = scales::comma) +
  geom_bar(data = gender_prop, 
           aes(y = reorder(County, Total), 
               x = Total * prop_female),
           stat = "identity",
           fill = "Purple") +
  labs(title = "Population by County",
       subtitle = "Purple bar shows number of female identifying people. 
Percentage shows proportion of Female identifying people.",
       x = "Population",
       y = "") +
  theme_classic()

