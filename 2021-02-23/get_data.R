library(tidytuesdayR)
library(tidyverse)

data_file <- "2021-02-23/raw_data.RDS"
if (!file.exists("data_file"))
{
    dat <- tt_load("2021-02-23")
    saveRDS(dat, data_file)
} else {
    dat <- readRDS(data_file)
}

# Employment numbers
employed <- dat$employed

## It looks like the rows are `year` x 
##   `industry/major_occupation/minor_occupation` x `race_gender`
## So summing up `employ_n` in those groupings should get the same 
##   total counts as `industry_total`

test_summary <- employed %>%
    group_by(year, industry, race_gender) %>%
    summarize(industry_total = first(industry_total), 
              employ_n_sum = sum(employ_n))

## numbers mostly line up, but off by a few thousand (least significant digit), 
##   suggesting rounding that then introduces some error when summed up again

## Check unique values of `race_gender` to see if we have intersections
##   of race x gender

unique(employed$race_gender)

## values of `race_gender` only include TOTAL, Men, Women, White, 
##   Black or African American, and Asian. Do people who don't identify
##   as one of those categories get included in TOTAL, since there is 
##   not a specific corresponding identifier?

race_by_industry <- employed %>%
    filter(race_gender %in% c("White", "Black or African American", "Asian")) %>%
    group_by(year, industry, race_gender) %>%
    summarize(industry_total = first(industry_total))

total_race_by_industry <- race_by_industry %>%
    group_by(year, industry) %>%
    summarize(industry_total = sum(industry_total))

test_2 <- employed %>%
    filter(race_gender == "TOTAL") %>%
    group_by(year, industry) %>%
    summarize(industry_total = first(industry_total))

## one industry_total is sum(white, asian, black), the other is 
##   the `TOTAL` label of race_gender
total_race_by_industry %>%
    left_join(test_2, by = c("year", "industry"))
## differences in the column values indicate there are people who 
##   are represented in `race_gender` == "TOTAL", but not in a specific
##   race category (because the race categories are not exhaustive)

# earnings
earn <- dat$earn

## What are the distribution of `major_occupation` and 
##   `minor_occupation` values in different industries?
set.seed(42)
sample_industries <- sample(unique(employed$industry), 2)

# check number of levels in major_occupation and minor_occupation within industry
table(employed %>% 
          filter(industry %in% sample_industries) %>% 
          select(industry, major_occupation))
table(employed %>% 
          filter(industry %in% sample_industries) %>% 
          select(industry, minor_occupation))
# identical numerical values indicate same major_occupation and minor_occupation 
#   labels across industry

