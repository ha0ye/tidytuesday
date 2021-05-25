library(tidyverse)
library(ggeasy)

drivers <- readRDS("2021-05-25/drivers.RDS")
records <- readRDS("2021-05-25/records.RDS")

## progression of world record


to_plot <- records %>%
    filter(type == "Three Lap", 
           system_played == "NTSC")

## world record progression on individual tracks
## (no shortcut)
## (three lap race)
## (NTSC system)
p <- ggplot(to_plot, 
       aes(x = date, 
           y = time, 
           color = shortcut)) + 
    facet_wrap(facets = ~track, scales = "free_y") + 
    geom_point() + 
    geom_path() + 
    theme_bw() + 
    easy_rotate_x_labels() + 
    labs(title = "World Record Progression in Mario Kart 64 (3 laps, NTSC)", 
         x = "Calendar Date", 
         y = "Time (in seconds)")
ggsave("2021-05-25/record_progression.pdf", plot = p, 
       width = 8, height = 8)

## summary frequency table
## number of world records per track
records %>%
    filter(type == "Three Lap", 
           system_played == "NTSC") %>%
    group_by(track, shortcut) %>%
    summarize(num_records = n())




