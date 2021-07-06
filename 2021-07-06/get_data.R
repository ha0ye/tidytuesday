library(tidytuesdayR)
library(tidyverse)

# retrieve the raw data and save it
dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')
saveRDS(dat, "2021-07-06/holidays.RDS")

# load the binary data file
holidays <- readRDS("2021-07-06/holidays.RDS")

# Q: is each country represented once, 
#    OR do some countries have multiple independence days?

length(unique(holidays$country))
table(holidays$country)

holidays %>%
    filter(country == "India")

# check if date_parsed == day, month, year
dd_parsed <- as.Date(paste(holidays$year, holidays$month, holidays$day), 
                     "%Y %b %d")
identical(is.na(dd_parsed), is.na(holidays$year)) # check that NAs are because data are NA
identical(dd_parsed, holidays$date_parsed)



