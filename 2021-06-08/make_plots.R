# setup ----
library(tidyverse)

# load data ----
fishing <- readRDS("2021-06-08/fishing.RDS")

# data wrangling ----

# to get grand totals by lake, we can use the `grand_total` column, 
#   except for Lake Michigan, where we should use region == "U.S. Total"

mich_data <- fishing %>%
    filter(lake == "Michigan", 
           region == "U.S. Total") 

# no Rainbow Smelt data (missing rows) in Lake Michigan in 1979, 
# first instance of 1980 data should probably be 1979 data

## correct lake michigan data
row_id <- which(mich_data$year == 1980 & 
                    mich_data$species == "Rainbow Smelt")
mich_data$year[1541] <- 1979

row_id <- which(mich_data$year == 1970 & 
                    mich_data$species == "Lake Sturgeon")
mich_data <- mich_data[-1066,]

mich_data$grand_total <- mich_data$values

other_data <- fishing %>%
    filter(lake != "Michigan") %>%
    select(year, lake, species, grand_total) %>%
    distinct()

counts <- table(paste(other_data$year, other_data$lake, other_data$species))

# no Walleye data (missing rows) in Lake Huron in 1978, 
# first instance of 1979 data should probably be 1978 data

## correct lake huron data
row_id <- which(other_data$year == 1979 & 
                    other_data$lake == "Huron" & 
                    other_data$species == "Walleye")
other_data$year[6935] <- 1978

dat <- bind_rows(mich_data %>% select(year, lake, species, grand_total), 
                 other_data)

## how many NAs in grand_total?
dat <- dat %>%
    drop_na() %>%
    mutate(species = recode(species, 
                            "Amercian Eel" = "American Eel", 
                            "Bullheads" = "Bullhead", 
                            "Channel catfish" = "Channel Catfish", 
                            "Cisco and chubs" = "Cisco and Chub", 
                            "Cisco and Chubs" = "Cisco and Chub", 
                            "Crappies" = "Crappie", 
                            "Pacific salmon" = "Pacific Salmon", 
                            "White bass" = "White Bass"))

top_species <- dat %>% 
    count(species, wt = grand_total) %>%
    slice_max(n, n = 8) %>%
    pull(species)

to_plot <- dat %>%
    filter(species %in% top_species)

# plot counts for each species over time, facetted by lake

p <- to_plot %>%
    ggplot(aes(x = year, y = grand_total, color = species)) +
    geom_line() + 
    facet_wrap(~lake, scales = "free_y") + 
    theme_bw() + 
    labs(x = "Year", 
         y = "Total Production (thousands of pounds)")

ggsave("2021-06-08/great_lakes_fish_over_time.pdf", p, width = 10, height = 6)


