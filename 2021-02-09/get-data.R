library(tidytuesdayR)
library(tidyverse)

dat <- tt_load("2021-02-09")
names(dat)

## initial exploration of data
# `lifetime_earn` table
#   average values for gender x race
#   gender: "Men" or "Women"
#   race: "White", "Black", or "Hispanic any race"
#   6 total rows

# `student_debt` table
#   data collected every 3 years
#   unclear whether `loan_debt` column is the average over 
#     families with debt, or over all families (`loan_debt_pct`
#     column is relevant for transformation between those interpretations)

# `race_wealth` table
#   averaged over racial group and year of data collection, also separated by 
#   measure of central tendency (mean or median) - 
#   why not just give us the raw data?

# `income_limits` table
#   dollar amounts are measured in units of dollars for year of data collection 
#   AND in 2019 dollars (but different from 2016 dollars in other tables)

# `income_aggregate` table
#   `income_quantile` identifies which "fifth" of the population is represented, 
#   and not the boundary in the sense of a quantile (and as used in `income_limits`)
# Q: is `income_share` relative to the total population (in that year), 
#   or is it subdivided by the race category already?
# quick check by calculation - drop "Top 5%" so the quintiles are mutually exclusive
#   and sum up `income_share` by {race x year}; 
# 100 means `income_share` is relative to that race category
dat$income_aggregate %>% 
    filter(income_quintile != "Top 5%") %>% 
    group_by(year, race) %>% 
    summarize(total_income_share = sum(income_share))



