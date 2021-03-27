# Author : Jenn Schilling
# Title: #TidyTuesday UN Votes
# Date: Mar 23 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(patchwork)
library(ggtext)

#### Data #### 

library(unvotes)

un_combined <- left_join(un_votes, un_roll_call_issues, by = "rcid") %>%
  left_join(un_roll_calls, by = "rcid") %>%
  mutate(year = lubridate::year(date))

un_combined_summary <- un_combined %>%
  group_by(year, country, issue, vote) %>%
  summarise(n = n(),
            .groups = "drop") %>%
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

un_combined_summary_yes <- un_combined_summary %>%
  mutate(not_yes = ifelse(vote != "yes", "not_yes", "yes")) %>%
  group_by(year, country, issue, not_yes) %>%
  summarise(sum_pct = sum(pct),
            .groups = "drop") %>%
  pivot_wider(names_from = not_yes, 
              values_from = sum_pct,
              values_fill = 0) %>%
  pivot_longer(not_yes:yes,
               names_to = "vote",
               values_to = "pct")

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
  plot.title = element_markdown(size = 12, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Make Plot ####

# List of issues
issues <- n_votes %>% 
  select(issue) %>% 
  filter(issue != 'Unknown') %>%
  unique(.) %>%
  mutate(issue = as.character(issue)) %>%
  pull(.)

# Median of Yes Votes by Issue and Year
un_combined_summary_yes_median <- un_combined_summary_yes %>%
  filter(vote == "yes") %>%
  group_by(year, issue, vote) %>%
  summarise(med_pct_yes = median(pct),
            .groups = "drop")

# Function Create individual Plots 
plot_issues <- function(issue_selected){
  
  p1 <- ggplot(data = subset(un_combined_summary_yes,
                             vote == 'yes' &
                               issue == issue_selected),
               mapping = aes(x = year,
                             y = pct,
                             group = year)) +
    
    # IQR of box plot
    geom_boxplot(fill = 'gray50', 
                 color = NA,
                 outlier.shape = NA) +
    
    # Bar for median
    geom_point(data = subset(un_combined_summary_yes_median,
                             issue == issue_selected),
               mapping = aes(x = year,
                             y = med_pct_yes),
               color = 'white',
               shape = 15,
               size = 0.7) +
    
    # Point for US
    geom_point(data = subset(un_combined_summary_yes,
                               vote == 'yes' &
                               country == 'United States' &
                               issue == issue_selected),
               mapping = aes(x = year,
                             y = pct),
               color = '#1b9e77') +
    
    
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_x_continuous(breaks = seq(1945, 2020, 15)) +
    labs(x = "",
         y = "",
         title = issue_selected) +
    theme(plot.margin = margin(t = 5, r = 10, b = 0, l = 10))
  
  p2 <- ggplot(data = subset(n_votes, issue == issue_selected),
               mapping = aes(x = year,
                             y = n)) +
    geom_line(color = fontcolor) +
    scale_y_continuous(limits = c(0, 45)) +
    scale_x_continuous(breaks = seq(1945, 2020, 15)) +
    labs(x = "",
         y = "") +
    theme(panel.grid = element_blank(),
          axis.text = element_blank()) +
    theme(plot.margin = margin(t = 0, r = 10, b = 5, l = 10))
  
  p1 / p2 + plot_layout(nrow = 2, heights = c(5, 1))
  
  
}

plots <- lapply(issues, plot_issues)


# Create combined plot

(plots[[1]] | plots [[2]] | plots[[3]]) /
  (plots[[4]] | plots [[5]] | plots[[6]]) +
  plot_annotation(title = 'Since 1970, the <b style="color:#1b9e77">United States</b> has voted <b>yes</b> less 
                  frequently then most other countries in the U.N. General Assembly.',
                  
                  subtitle = "The main graph shows the percent of votes that were <b>yes</b> by issue each year. 
                  The line graph shows the number of votes by issue each year.<br> 
                  The box shows the interquartile range for all countries with the median represented by the gap; 
                  the point represents the United States. <br>",
                  
                  caption = "Data: <b>{unvotes}</b> | Viz: <b>Jenn Schilling</b>")

ggsave("2021-03-23\\un_votes.png",
       plot = last_plot(),
       device = "png",
       width = 12,
       height = 8,
       type = "cairo")

