library(tidytuesdayR)
library(tidyverse)

dat <- tt_load("2021-03-23")

# check if unvotes$country_code has any missing values, because
#   Namibia's 2-letter country code is "NA" and that sometimes 
#   gets converted to, e.g., NA_character_

any(is.na(dat$unvotes$country_code))
## yes there are missing values

## look at the rows of `unvotes` where `is.na(country_code)`
unvotes <- dat$unvotes

unvotes %>%
    filter(is.na(country_code)) %>%
    pull(country) %>%
    unique()

unvotes %>%
    filter(grepl("yemen", country, ignore.case = TRUE)) %>%
    pull(country) %>%
    unique()

# check `rcid` values
unique(unvotes$rcid) %>% length() # 6202 values

# check `rcid` values in `unvotes` match exactly those in `roll_calls`
roll_calls <- dat$roll_calls
setequal(unvotes$rcid, roll_calls$rcid)
issues <- dat$issues
setequal(unvotes$rcid, issues$rcid)

# save separate data.frames so we don't have to re-download
saveRDS(unvotes, "2021-03-23/unvotes.RDS")
saveRDS(issues, "2021-03-23/issues.RDS")
saveRDS(roll_calls, "2021-03-23/roll_calls.RDS")


