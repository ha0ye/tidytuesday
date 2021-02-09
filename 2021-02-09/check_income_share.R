library(tidytuesdayR)
library(tidyverse)

tt_data <- tt_load("2021-02-09")

income_mean <- tt_data$income_mean %>%
    mutate(income_quintile = recode(income_quintile, Middle = "Third"))
income_aggregate <- tt_data$income_aggregate

# get total dollar amounts for year x race x income_quintile

dat <- income_aggregate %>%
    left_join(income_mean, by = c("year", "race", "income_quintile")) %>%
    mutate(proportion_of_racial_group = ifelse(income_quintile == "Top 5%", 0.05, 0.20), 
           num_households = proportion_of_racial_group * number, 
           group_income = num_households * income_dollars) %>%
    filter(dollar_type == "2019 Dollars") %>%
    select(-c("number", "dollar_type"))

income_total <- dat %>%
    filter(race == "All Races", income_quintile != "Top 5%") %>%
    group_by(year) %>%
    summarize(total_income_for_year = sum(group_income))

to_plot <- dat %>%
    left_join(income_total) %>%
    mutate(group_income_frac = income_share, 
           total_income_frac = group_income / total_income_for_year) %>%
    mutate(income_quintile = factor(income_quintile, 
                                    levels = rev(c("Lowest", "Second", "Third", "Fourth", "Highest", "Top 5%")))) %>%
    filter(income_quintile != "Top 5%")

p <- ggplot(to_plot, 
       aes(x = year, y = total_income_frac, 
           group = income_quintile)) +
    facet_wrap(~race) + 
    geom_line(aes(color = race, linetype = income_quintile)) + 
    theme_bw()

ggsave("2021-02-09/total_income_share.png", p, width = 8, height = 8)

## What happened to the "Asian alone or in combination" level of `race`?
#    somewhere in our processing, it was dropped
#    perhaps due to filtering for `dollar_type` == "2019 dollars"
#    that may filter out years, for which normalized dollar amounts don't exist

# we are missing categories of `race` because normalized dollar amounts don't exist
table(income_mean[, c("race", "dollar_type")])

# we are not missing categories of `year` because normalized dollar amounts don't exist
table(income_mean[, c("year", "dollar_type")])


