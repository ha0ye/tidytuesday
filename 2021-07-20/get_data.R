library(tidyverse)

# Download the data

tuesdata <- tidytuesdayR::tt_load('2021-07-20')
saveRDS(tuesdata$drought, file = "2021-07-20/drought.RDS")

drought <- readRDS("2021-07-20/drought.RDS")

summary(drought)

# do rows of the data correspond to week x state x drought level?
drought %>%
    filter(valid_start == drought$valid_start[1]) %>%
    pull(state_abb, drought_lvl)

# percent change in different drought level over time for an individual state
to_plot <- drought %>%
    filter(state_abb == "FL")

to_plot %>%
    filter(valid_start > as.Date("2001-01-01"), 
           valid_start < as.Date("2003-01-01")) %>%
ggplot(aes(x = valid_start, y = area_pct, fill = drought_lvl)) + 
    geom_col() + 
    theme_bw()

# why do some weeks have total area pct > 100?
to_plot %>%
    group_by(valid_start) %>%
    summarize(total_area_pct = sum(area_pct))

drought %>%
    filter(valid_start < "2001-12-17", 
           state_abb == "FL") %>%
    select(valid_start, drought_lvl, area_pct, area_total) %>%
    mutate(pct_ratio = area_total/area_pct) %>%
    pull(pct_ratio)

# try and find total area percent for all the states
state_area_pct_summary <- drought %>%
    group_by(state_abb, valid_start) %>%
    summarize(total_area_pct = sum(area_pct))

ggplot(state_area_pct_summary, 
       aes(x = valid_start, y = total_area_pct, color = state_abb)) + 
    geom_line() + 
    theme_bw() + 
    guides(color = FALSE)

# check against raw data downloaded from the drought monitor website
dat <- read.csv("2021-07-20/dm_export_20010720_20011231.csv")

dat <- dat %>% 
    mutate(valid_start = as.Date(ValidStart), 
           state_abb = StateAbbreviation, 
           total_area_pct = None + D0 + D1 + D2 + D3)

ggplot(dat, 
       aes(x = valid_start, y = total_area_pct, color = state_abb)) + 
    geom_line() + 
    theme_bw()

## check individual area percentages against tidytuesday download
downloaded_snippet <- dat %>%
    filter(state_abb == "FL", 
           valid_start == "2001-07-17") %>%
    pivot_longer(cols = None:D4, 
                 names_to = "drought_lvl", 
                 values_to = "area_pct")

print(downloaded_snippet, n = Inf, width = Inf)

tt_snippet <- drought %>%
    filter(state_abb == "FL", 
           valid_start == "2001-07-17")

print(tt_snippet, n = Inf, width = Inf)

## transform data so that the drought categories are NOT cumulative

# test case for one week and one state
tt_snippet %>%
    filter(drought_lvl != "None") %>%
    arrange(desc(drought_lvl)) %>%
    mutate(area_pct_new = c(first(area_pct), diff(area_pct))) %>%
    print(n = Inf, width = Inf) %>%
    bind_rows(filter(tt_snippet, drought_lvl == "None"))

drought_corrected <-
    bind_rows(
        drought %>%
            filter(drought_lvl != "None") %>%
            group_by(state_abb, valid_start) %>%
            arrange(desc(drought_lvl)) %>%
            mutate(area_pct = c(first(area_pct), diff(area_pct))) %>%
            ungroup(), 
        drought %>%
            filter(drought_lvl == "None")
    ) %>%
    mutate(drought_lvl = factor(drought_lvl, 
                                levels = rev(c("None", "D0", "D1", "D2", "D3", "D4"))))
saveRDS(drought_corrected, file = "2021-07-20/drought_corrected.RDS")




