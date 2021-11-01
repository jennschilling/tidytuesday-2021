# Author : Jenn Schilling
# Title: Combine #TidyTuesday Plots
# Date: Apr 8 2021

#### Libraries ####

library(magick)
library(here)

#### Get Plots for 2021 [Long] ####

row_1_21 <- image_append(
  c(image_border(image_resize(image_read("2021-01-12\\turner_oil_colors_circle.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-01-19\\county_female_map.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-01-19\\county_female_bar.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-01-26\\sankey_plastic.png"), "500x400!"), color = "white", geometry = "10x10")))


row_2_21 <- image_append(
  c(image_border(image_resize(image_read("2021-02-02\\bach_hs_attain_women.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-02\\bach_hs_attain.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-09\\wealth_distribution.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-16\\c1_final.png"), "500x400!"), color = "white", geometry = "10x10")))

row_3_21 <- image_append(
  c(image_border(image_resize(image_read("2021-02-16\\c2_final.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-23\\earnings_age_2020.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-23\\occupations_2020.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-02\\superbowl_2.png"), "500x400!"), color = "white", geometry = "10x10")))

row_4_21 <- image_append(
  c(image_border(image_resize(image_read("2021-03-09\\genre_summary.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-16\\steam_games.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-23\\un_votes.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-30\\foundation_shades.png"), "500x400!"), color = "white", geometry = "10x10")))

row_5_21 <- image_append(
  c(image_border(image_resize(image_read("2021-04-06\\percent_forest.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-04-20\\netflix.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-04-27\\ceo.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-04\\water_art.png"), "500x400!"), color = "white", geometry = "10x10")))

row_6_21 <- image_append(
  c(image_border(image_resize(image_read("2021-05-11\\az_broadband.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-18\\us_salaries.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-25\\mario_kart.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-01\\survivor.png"), "500x400!"), color = "white", geometry = "10x10")))

row_7_21 <- image_append(
  c(image_border(image_resize(image_read("2021-06-08\\greatlakes.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-15\\duboischallenge.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-22\\tucsonparks.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-29\\cat_rescues.png"), "500x400!"), color = "white", geometry = "10x10")))

row_8_21 <- image_append(
  c(image_border(image_resize(image_read("2021-07-06\\independence_days.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-07-13\\scooby.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-07-20\\usdrought.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-10-19\\giantpumpkins.png"), "500x400!"), color = "white", geometry = "10x10")))

row_9_21 <- image_append(
  c(image_border(image_resize(image_read("2021-10-26\\greatracers.png"), "500x400!"), color = "white", geometry = "10x10")))

final_21_long <- image_append(c(row_1_21,
                           row_2_21,
                           row_3_21,
                           row_4_21,
                           row_5_21,
                           row_6_21,
                           row_7_21,
                           row_8_21,
                           row_9_21),
                         stack = TRUE)

# image_write(final_21,
#             "2021-all-plots\\2021-summary.png")


#### Get Plots for 2021 [Wide] ####

row_1_21 <- image_append(
  c(image_border(image_resize(image_read("2021-01-12\\turner_oil_colors_circle.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-01-19\\county_female_map.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-01-19\\county_female_bar.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-01-26\\sankey_plastic.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-02\\bach_hs_attain_women.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-02\\bach_hs_attain.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-09\\wealth_distribution.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-16\\c1_final.png"), "500x400!"), color = "white", geometry = "10x10")))

row_2_21 <- image_append(
  c(image_border(image_resize(image_read("2021-02-16\\c2_final.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-23\\earnings_age_2020.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-23\\occupations_2020.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-02\\superbowl_2.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-09\\genre_summary.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-16\\steam_games.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-23\\un_votes.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-30\\foundation_shades.png"), "500x400!"), color = "white", geometry = "10x10")))

row_3_21 <- image_append(
  c(image_border(image_resize(image_read("2021-04-06\\percent_forest.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-04-20\\netflix.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-04-27\\ceo.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-04\\water_art.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-11\\az_broadband.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-18\\us_salaries.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-25\\mario_kart.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-01\\survivor.png"), "500x400!"), color = "white", geometry = "10x10")))

row_4_21 <- image_append(
  c(image_border(image_resize(image_read("2021-06-08\\greatlakes.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-15\\duboischallenge.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-22\\tucsonparks.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-29\\cat_rescues.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-07-06\\independence_days.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-07-13\\scooby.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-07-20\\usdrought.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-10-19\\giantpumpkins.png"), "500x400!"), color = "white", geometry = "10x10")))

row_5_21 <- image_append(
  c(image_border(image_resize(image_read("2021-10-26\\greatracers.png"), "500x400!"), color = "white", geometry = "10x10")))

final_21_w <- image_append(c(row_1_21,
                             row_2_21,
                             row_3_21,
                             row_4_21,
                             row_5_21),
                           stack = TRUE)

image_write(final_21_w,
            "2021-all-plots\\2021-summary.png")


#### Get Plots for 2020 ####

row_1_20 <- image_append(
  c(image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-07-28/tidytuesday-palmerpenguins.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-08-04/top10_european_energy.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-08-11/avatar.ratings.characters.png"), "500x400!"), color = "white", geometry = "10x10"), 
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-08-18/extinct.plants.png"), "500x400!"), color = "white", geometry = "10x10")))

row_2_20 <- image_append(
  c(image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-08-25/chopped.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-01/banana.yield.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-08/friends_seasons.gif"), "500x400!")[1], color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-15/k12_spend_per_child.png"), "500x400!"), color = "white", geometry = "10x10")))

row_3_20 <- image_append(
  c(image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-22/first_ascents.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-29/worldwide_sales.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-11-03/ikea_category_prices.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-11-24/washington_hikes.png"), "500x400!"), color = "white", geometry = "10x10")))

final_20<- image_append(c(row_1_20,
                          row_2_20,
                          row_3_20),
                        stack = TRUE)

image_write(final_20,
            "2021-all-plots\\2020-summary.png")


#### Put 2020 and 2021 Together ####

final <- image_append(c(final_20,
                        final_21_long),
                      stack = TRUE)

image_write(final,
            "2021-all-plots\\summary_long.png")


#### Alternate Layout 4 rows by 7 columns ####

row_1 <- image_append(
  c(image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-07-28/tidytuesday-palmerpenguins.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-08-04/top10_european_energy.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-08-11/avatar.ratings.characters.png"), "500x400!"), color = "white", geometry = "10x10"), 
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-08-18/extinct.plants.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-08-25/chopped.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-01/banana.yield.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-08/friends_seasons.gif"), "500x400!")[1], color = "white", geometry = "10x10")))

row_2 <- image_append(    
  c(image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-15/k12_spend_per_child.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-22/first_ascents.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-09-29/worldwide_sales.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-11-03/ikea_category_prices.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("https://github.com/jennschilling/tidytusesday/raw/master/2020-11-24/washington_hikes.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-01-12\\turner_oil_colors_circle.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-01-19\\county_female_map.png"), "500x400!"), color = "white", geometry = "10x10")))

row_3 <- image_append(
  c(image_border(image_resize(image_read("2021-01-19\\county_female_bar.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-01-26\\sankey_plastic.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-02\\bach_hs_attain_women.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-02\\bach_hs_attain.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-09\\wealth_distribution.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-16\\c1_final.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-16\\c2_final.png"), "500x400!"), color = "white", geometry = "10x10")))

row_4 <- image_append(
  c(image_border(image_resize(image_read("2021-02-23\\earnings_age_2020.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-02-23\\occupations_2020.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-02\\superbowl_2.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-09\\genre_summary.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-16\\steam_games.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-23\\un_votes.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-03-30\\foundation_shades.png"), "500x400!"), color = "white", geometry = "10x10")))

row_5 <- image_append(
  c(image_border(image_resize(image_read("2021-04-06\\percent_forest.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-04-20\\netflix.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-04-27\\ceo.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-04\\water_art.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-11\\az_broadband.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-18\\us_salaries.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-05-25\\mario_kart.png"), "500x400!"), color = "white", geometry = "10x10")))

row_6 <- image_append(
  c(image_border(image_resize(image_read("2021-06-01\\survivor.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-08\\greatlakes.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-15\\duboischallenge.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-22\\tucsonparks.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-06-29\\cat_rescues.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-07-13\\scooby.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-07-20\\usdrought.png"), "500x400!"), color = "white", geometry = "10x10")))

row_7 <- image_append(
  c(image_border(image_resize(image_read("2021-10-19\\giantpumpkins.png"), "500x400!"), color = "white", geometry = "10x10"),
    image_border(image_resize(image_read("2021-10-26\\greatracers.png"), "500x400!"), color = "white", geometry = "10x10")))

final_together <- image_append(c(row_1,
                                 row_2,
                                 row_3,
                                 row_4,
                                 row_5,
                                 row_6,
                                 row_7),
                               stack = TRUE)

image_write(final_together,
            "2021-all-plots\\summary_wide.png")
