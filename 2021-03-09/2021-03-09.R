# Author : Jenn Schilling
# Title: #TidyTuesday Bechdel Test
# Date: Mar 9 2021

#### Libraries ####

library(tidyverse)
library(extrafont)

#### Data #### 

movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv') %>%
  mutate(domgross_2013 = parse_number(domgross_2013),
         intgross_2013 = parse_number(intgross_2013))

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 10, color = fontcolor),
  
  legend.title = element_text(size = 12, color = fontcolor),
  legend.text = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 16, face = "bold"),
  
  plot.subtitle = element_text(size = 12, color = fontcolor, margin = margin(b = 15)),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 25, r = 25, b = 10, l = 25)
)

#### Exploration ####

# FiveThirtyEight Article:
# https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/

# Recreate FiveThirtyEight plot
movies_pass_fail <- movies %>%
  mutate(year_bin = cut_interval(year, n = 9)) %>%
  group_by(year_bin, clean_test) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  group_by(year_bin) %>%
  mutate(pct = n / sum(n),
         clean_test = factor(clean_test,
                             levels = c("nowomen", "notalk", "men", "dubious", "ok")))

ggplot(data = movies_pass_fail,
       mapping = aes(x = year_bin,
                     y = pct,
                     fill = clean_test)) +
  geom_col()


ggplot(data = movies,
       mapping = aes(x = year,
                     y = budget_2013,
                     color = binary)) +
  geom_point() 

# Explore some aggregations by year since 2000
movies_pass_fail_yr <- movies %>%
  filter(year >= 2000) %>%
  group_by(year, binary) %>%
  summarise(med_budget = median(budget_2013, na.rm = TRUE),
            med_dom_gross = median(domgross_2013, na.rm = TRUE),
            med_int_gross = median(intgross_2013, na.rm = TRUE),
            med_rating = median(imdb_rating, na.rm = TRUE),
            n = n(),
            .groups = "drop")

ggplot(data = movies_pass_fail_yr,
       mapping = aes(x = year,
                     y = med_dom_gross,
                     fill = binary)) +
  geom_col(position = "dodge")


# Summarize 
movies_pass_fail_summary <- movies %>%
  filter(year >= 2000) %>%
  group_by(binary) %>%
  summarise(med_budget = median(budget_2013, na.rm = TRUE),
            med_dom_gross = median(domgross_2013, na.rm = TRUE),
            med_int_gross = median(intgross_2013, na.rm = TRUE),
            med_rating = median(imdb_rating, na.rm = TRUE),
            med_dom_ratio = median(domgross_2013 / budget_2013, na.rm = TRUE),
            med_int_ratio = median(intgross_2013 / budget_2013, na.rm = TRUE),
            n = n(),
            .groups = "drop") %>%
  pivot_longer(med_budget:n,
               names_to = "measure",
               values_to = "value")

ggplot(data = movies_pass_fail_summary %>%
         filter(measure %in% c('med_int_gross', 'med_dom_gross', 'med_budget')),
       mapping = aes(x = value,
                     y = measure,
                     color = binary,
                     group = measure)) +
  geom_line(color = "grey60") +
  geom_point(size = 3) +
  scale_x_continuous(labels = scales::label_number_si(accuracy = 1, prefix = "$")) +
  scale_color_brewer(palette = "Dark2", direction = -1) +
  scale_y_discrete(labels = c("Median Budget", "Median Domestic Gross", "Median International Gross")) +
  labs(x = "",
       y = "",
       title = "Financial Investment and Returns on Movies Bechdel Test",
       subtitle = "Movies released 2000 - 2013; $ normalized to 2013",
       color = "Bechdel Test") +
  theme(legend.position = "bottom")

ggplot(data = movies_pass_fail_summary %>%
         filter(measure %in% c('med_dom_ratio', 'med_int_ratio')),
       mapping = aes(x = value,
                     y = measure,
                     color = binary,
                     group = measure)) +
  geom_line(color = "grey60") +  
  geom_point(size = 3) +
  scale_x_continuous(limits = c(1, 3),
                     labels = scales::label_number_si(accuracy = 0.01, prefix = "$")) +
  scale_color_brewer(palette = "Dark2", direction = -1) +
  scale_y_discrete(labels = c("Median Domestic Return", "Median International Return")) +
  labs(x = "",
       y = "",
       title = "Financial Return on Investment by Bechdel Test",
       subtitle = "Movies released 2000 - 2013; $ normalized to 2013",
       color = "Bechdel Test") +
  theme(legend.position = "bottom")

#### Plot ####



# ggsave("2021-03-09\\.png",
#        plot = last_plot(),
#        device = "png",
#        width = 8,
#        height = 7,
#        type = "cairo")


