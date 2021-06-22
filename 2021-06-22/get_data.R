library(tidytuesdayR)
library(tidyverse)

# download the data and save it to disk
dat <- tt_load("2021-06-22")
    
str(dat, max.level = 2)
parks <- dat$parks

saveRDS(parks, file = "2021-06-22/parks.RDS")

parks <- readRDS("2021-06-22/parks.RDS")
# clean up percentage and dollar columns
parks$park_pct_city_data <- sub("(\\d+)%", "\\1", parks$park_pct_city_data) %>%
    as.numeric()

parks$pct_near_park_data <- sub("(\\d+)%", "\\1", parks$pct_near_park_data) %>%
    as.numeric()

parks$spend_per_resident_data <- sub("\\$(\\d+)", "\\1", parks$spend_per_resident_data) %>%
    as.numeric()

parks$city <- recode(parks$city, "Washington, DC" = "Washington, D.C.")

saveRDS(parks, file = "2021-06-22/parks_tidy.RDS")

# what do the rows represent?
parks[1:10, 1:5]

## rows seem to be per city, per year
## data on the parks within a city, and other related information


