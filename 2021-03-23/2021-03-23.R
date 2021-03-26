# Author : Jenn Schilling
# Title: #TidyTuesday UN Votes
# Date: Mar 23 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(patchwork)

#### Data #### 

library(unvotes)

un_combined <- left_join(un_votes, un_roll_call_issues, by = "rcid") %>%
  left_join(un_roll_calls, by = "rcid") %>%
  mutate(year = lubridate::year(date))

un_combined_summary <- un_combined %>%
  group_by(year, country, issue, vote) %>%
  summarise(n = n(),
            .groups = 'drop') %>%
  ungroup() %>%
  group_by(year, country, issue) %>%
  mutate(pct = n / sum(n),
         n_votes = sum(n)) %>%
  ungroup() %>%
  mutate(issue = factor(ifelse(is.na(issue), "Unknown", as.character(issue))))

original_countries <- un_combined %>%
  filter(year == 1946) %>%
  select(country) %>%
  unique(.) %>%
  mutate(original = 1)

un_combined_summary <- un_combined_summary %>%
  left_join(original_countries) %>%
  mutate(original = ifelse(is.na(original), 0, original))

n_votes <-  un_combined %>%
  select(year, issue, rcid) %>%
  unique(.) %>%
  group_by(year, issue) %>%
  summarise(n = n(),
            .groups = 'drop') %>%
  ungroup() %>%
  mutate(issue = factor(ifelse(is.na(issue), "Unknown", as.character(issue))))

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 12, color = fontcolor),
  
  plot.subtitle = element_text(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 7, color = fontcolor),
  
  plot.margin = margin(t = 5, r = 10, b = 5, l = 10)
)

#### Make Plot ####

# List of issues
issues <- n_votes %>% 
  select(issue) %>% 
  filter(issue != 'Unknown') %>%
  unique(.) %>%
  mutate(issue = as.character(issue)) %>%
  pull(.)

# Function Create individual Plots 
plot_issues <- function(issue_selected){
  
  p1 <- ggplot(data = subset(un_combined_summary,
                             original == 1 & vote == 'yes' &
                               country != 'United States' &
                               issue == issue_selected),
               mapping = aes(x = year,
                             y = pct)) +
    geom_point(color = 'gray50', 
               alpha = 0.25,
               size = 1) +
    geom_point(data = subset(un_combined_summary,
                             original == 1 & vote == 'yes' &
                               country == 'United States' &
                               issue != 'Unknown'),
               mapping = aes(x = year,
                             y = pct),
               color = '#1b9e77',
               size = 2) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "",
         y = "",
         title = issue_selected) 
  
  p2 <- ggplot(data = subset(n_votes, issue == issue_selected),
               mapping = aes(x = year,
                             y = n)) +
    geom_line(color = fontcolor) +
    scale_y_continuous(limits = c(0, 45)) +
    labs(x = "",
         y = "") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank()) 
  
  p1 / p2 + plot_layout(nrow = 2, heights = c(4, 1))
  
  
}

plots <- lapply(issues, plot_issues)


# Create combined plot

(plots[[1]] | plots [[2]] | plots[[3]]) /
  (plots[[4]] | plots [[5]] | plots[[6]]) +
  plot_annotation(title = "TEST",
                  subtitle = "TEST",
                  caption = "TEST")

