library(tidytuesdayR)
library(tidyverse)

data_file <- "2021-02-23/raw_data.RDS"
dat <- readRDS(data_file)
    
earn <- dat$earn

# Earnings by sex in 2020
#   x-axis = age
#   y-axis = earnings
#   color = race
#   facet = sex

to_plot <- earn %>%
    filter(year == 2020, quarter == 1, 
           sex %in% c("Men", "Women"), 
           race %in% c("White", "Black or African American", "Asian"), 
           age %in% c("16 to 24 years", 
                      "25 to 54 years", 
                      "55 years and over")) %>%
    mutate(age = factor(age))

p <- to_plot %>%
    ggplot(aes(x = age, y = median_weekly_earn, 
               color = race, group = race)) + 
    facet_wrap(~sex) + 
    geom_line() + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90))

ggsave("2021-02-23/earnings_age-race-sex_2020Q1.png", p, 
       width = 6, height = 4)
