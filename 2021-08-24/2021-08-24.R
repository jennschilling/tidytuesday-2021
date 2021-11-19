# Author : Jenn Schilling
# Title: #TidyTuesday Lemurs
# Date: August 24 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(gghalves)

#### Data #### 

lemurs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')
taxonomy <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/taxonomy.csv') %>%
  mutate(taxon = ifelse(taxon == "CMEAD", "CMED", taxon))

lemur_birth_death <- lemurs %>%
  select(taxon, dlc_id, name, dob, dod, age_at_death_y, age_of_living_y) %>%
  left_join(taxonomy, by = "taxon") %>%
  unique() %>%
  mutate(label = paste(latin_name, " (", common_name, ")", sep = ""))

# Median age at death for factor levels
lemur_levels <- lemur_birth_death %>%
  group_by(label) %>%
  summarise(med_life_exp = median(age_at_death_y, na.rm = TRUE),
            max_life_exp = max(age_at_death_y, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(max_life_exp)

lemur_birth_death <- lemur_birth_death %>%
  mutate(label = factor(label,
                        levels = lemur_levels$label))

lemur_white <- "#EBEDEE"
lemur_black <- "#151513"
lemur_grey <- "#808680"
lemur_d_brown <- "#3E2C20"
lemur_m_brown <- "#3D231A"
lemur_l_brown <- "#653322"

#### Formatting ####

font <- "Consolas"
titlefont <- "Consolas"
fontcolor <- lemur_black
bcolor <- lemur_white

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 14, color = fontcolor),
  axis.text = element_text(size = 8, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 12, color = fontcolor),
  legend.title = element_text(size = 14, color = fontcolor, lineheight = 0.5),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 14, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor, family = font, lineheight = 1.25),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####


ggplot(data = lemur_birth_death %>% filter(!is.na(age_at_death_y)),
       mapping = aes(y = label,
                     x = age_at_death_y)) +
  geom_point(shape = "|",
             size = 2,
             color = lemur_m_brown) +
  geom_point(data = lemur_levels,
             mapping = aes(x = med_life_exp),
             size = 1,
             color = lemur_l_brown) +
  geom_text(data = lemur_levels,
            mapping = aes(y = label,
                          x = 0,
                          label = label),
            family = font,
            color = fontcolor,
            size = 2.5,
            vjust = -1,
            hjust = 0) +
  scale_x_continuous(limits = c(0, 45)) +
  coord_cartesian(clip = "off",
                  expand = FALSE) +
  labs(x = "",
       y = "",
       title = "Lemur life expectancies",
       subtitle = "Median life expectancy for each taxonomy is shown by a dot.<br>",
       caption = "**Data:** Duke Lemur Center | **Design**: Jenn Schilling") +
  theme(axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


# Save plot

ggsave("2021-08-24\\lemurs.png",
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 7,
       height = 7)
