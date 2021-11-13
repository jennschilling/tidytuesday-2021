# Author : Jenn Schilling
# Title: #TidyTuesday BEA Infrastructure Investment
# Date: August 10 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(patchwork)

#### Data #### 

chain_investment <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')

social_ed_investment <- chain_investment %>%
  filter(meta_cat == "Social" & 
           category %in% c("Education", "Health", "Public safety")) %>%
  group_by(year) %>%
  mutate(total = sum(gross_inv_chain)) %>%
  ungroup() %>%
  mutate(prop = gross_inv_chain / total,
         category = factor(category,
                           levels = c("Public safety", "Health", "Education")))

ed_investment <- chain_investment %>%
  filter(meta_cat == "Education") %>%
  mutate(category = ifelse(grepl("S&L", category), "State and Local Government", category)) %>%
  group_by(year, category) %>%
  summarise(gross_inv_chain = sum(gross_inv_chain),
            .groups = "drop") %>%
  group_by(year) %>%
  mutate(total = sum(gross_inv_chain)) %>%
  ungroup() %>%
  mutate(prop = gross_inv_chain / total)

total_investment <- chain_investment %>%
  filter(grepl("Total", category) &
           meta_cat == "Total infrastructure") %>%
  group_by(year) %>%
  mutate(total = sum(gross_inv_chain)) %>%
  ungroup() %>%
  mutate(prop = gross_inv_chain / total)

#### Formatting ####

font <- "Trebuchet MS"
titlefont <- "Georgia"
fontcolor <- "gray30"
bcolor <- "#F8F8F8"

bea_orange <- "#EC6A20"
bea_blue <- "#005F9F"
bea_grey <- "#939799"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_blank(),
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  
  strip.text = element_markdown(size = 8, color = fontcolor, 
                                family = titlefont, hjust = 0.5),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 8, color = fontcolor, 
                                family = titlefont, lineheight = 1.2),
  
  plot.subtitle = element_markdown(size = 6, color = fontcolor, 
                                   family = titlefont),

  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 6, color = fontcolor, 
                                  family = titlefont, hjust = 0.95),
  
  plot.margin = margin(t = 5, r = 0, b = 5, l = 5)
)

#### Plot ####


# Social Spending
p1 <- ggplot(data = total_investment %>% 
         filter(year == 1957 | year == 1987 | year == 2017),
       mapping = aes(x = "",
                     y = prop,
                     fill = category)) +
  geom_col() +
  coord_polar("y") +
  scale_fill_manual(values = c(bea_grey, bea_grey, bea_orange)) +
  guides(fill = "none") +
  facet_wrap(~ year) +
  labs(title = "The share of total infrastructure spending on <span style = 'color:#EC6A20;'>social infrastructure</span> has increased slightly over the past<br>
       60 years from 26% in 1957 to 28% in 1987 to 31% in 2017.",
       caption = "Social infrastructure spending includes education, health, and public safety.") +
  theme(
    plot.caption = element_markdown(size = 6, color = fontcolor, 
                                    family = titlefont, hjust = 0))

# Education Spending
p2 <- ggplot(data = social_ed_investment %>% 
         filter(year == 1957 | year == 1987 | year == 2017),
       mapping = aes(x = "",
                     y = prop,
                     fill = category)) +
  geom_col() +
  coord_polar("y") +
  scale_fill_manual(values = c(bea_grey, bea_grey, bea_blue)) +
  guides(fill = "none") +
  facet_wrap(~ year) +
  labs(title = "The share of <span style = 'color:#EC6A20;'>social infrastructure</span> spending on <span style = 'color:#005F9F;'>education-related</span> infrastructure has decreased over the past<br>
       60 years from 76% in 1957 to 41% in 1987 to 37% in 2017.",
       caption = "Education-related infrastructure spending includes school structures, libraries, and higher education buildings.") +
  theme(
    plot.caption = element_markdown(size = 6, color = fontcolor, 
                                    family = titlefont, hjust = 0))

# Education Spending Source
p3 <- ggplot(data = ed_investment %>% 
         filter(year == 1957 | year == 1987 | year == 2017),
       mapping = aes(x = "",
                     y = prop,
                     fill = category)) +
  geom_col() +
  coord_polar("y") +
  scale_fill_manual(values = c("#6c7072", "#939799", "#007a7a")) +
  guides(fill = "none") +
  facet_wrap(~ year) +
  labs(title = "
  <span style = 'color:#007a7a;'>State and local governments</span> have the largest share of investment in <span style = 'color:#005F9F;'>education-related</span> infrastructure.<br>
  Private sources make up the second largest share of investment, and the share of investment by the<br>
  federal government is the smallest. In 2017, <span style = 'color:#007a7a;'>state and local governments</span> provided 81% of the investment,<br> 
  private sources provided 19%, and the federal government provided 0.2%.",
       caption = "<b>Data:</b> Bureau of Economic Analysis | <b>Design:</b> Jenn Schilling") 

# Put plots together
p1 / p2 / p3

# Save
ggsave("2021-08-10\\edspending.png",
       plot = last_plot(),
       device = "png",
       width = 5.5,
       height = 7,
       type = "cairo")
