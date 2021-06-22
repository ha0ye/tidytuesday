library(tidyverse)

parks <- readRDS("2021-06-22/parks_tidy.RDS")

# look at how cities change in rank over time

ggplot(parks, 
       aes(x = year, y = rank, group = city, color = city)) + 
    geom_line() + 
    theme_bw()

# data transform:
#   normalize ranks
#   invert so that higher rank (lower #) is at the top
#   order city by rank instead of alphabetical

parks <- parks %>%
    group_by(year) %>%
    mutate(rank_n = 1 - (rank - 1) / (n() - 1))

ggplot(parks, 
       aes(x = year, y = rank_n, group = city, color = city)) + 
    geom_line() + 
    theme_bw()

# number of cities in 2020?
parks %>%
    filter(year == 2020) %>%
    NROW()

# number of unique values in parks$city
length(unique(parks$city))

# define city order as descending median rank_n
city_order <- parks %>%
    group_by(city) %>%
    summarize(med_rank_n = median(rank_n)) %>%
    arrange(desc(med_rank_n)) %>%
    pull(city)

# convert city into a factor, levels = city_order
parks$city <- factor(parks$city, levels = city_order)

top_n <- seq(1, 20)
to_plot <- parks %>%
    filter(city %in% city_order[top_n])

p <- ggplot(to_plot, 
            aes(x = year, y = rank_n, group = city, color = city)) + 
    geom_line(size = 1, alpha = 0.5) + 
    geom_point(alpha = 0.5) + 
    scale_color_viridis_d() + 
    theme_bw(base_size = 20) + 
    labs(x = "Year", y = "Normalized Score (higher is better)")

ggsave("2021-06-22/city_ranks.pdf", p, width = 10, height = 6)
