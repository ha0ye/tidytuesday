library(tidyverse)

drought <- readRDS("2021-07-20/drought_corrected.RDS")

## test plot using Florida and 2001-2005
to_plot <- drought %>%
    filter(state_abb == "FL", 
           valid_start > as.Date("2001-01-01"), 
           valid_start < as.Date("2005-01-01"))

to_plot %>%
    ggplot(aes(x = valid_start, y = area_pct, fill = drought_lvl)) + 
    geom_area(position = "stack") + 
    scale_fill_viridis_d(option = "A") + 
    theme_bw()

## all states and all time
p <- drought %>%
    ggplot(aes(x = valid_start, y = area_pct, fill = drought_lvl)) + 
    facet_wrap(~state_abb) + 
    geom_area(position = "stack") + 
    scale_fill_viridis_d(option = "A") + 
    theme_bw()  

ggsave("2021-07-20/state_drought_lvl_over_time.png", p, width = 12, height = 10)

library(geofacet)

p <- drought %>%
    ggplot(aes(x = valid_start, y = area_pct, fill = drought_lvl)) + 
    facet_geo(~state_abb) + 
    geom_area(position = "stack") + 
    scale_fill_viridis_d(option = "A") + 
    theme_bw()  

ggsave("2021-07-20/state_drought_lvl_map.png", p, width = 12, height = 8)

