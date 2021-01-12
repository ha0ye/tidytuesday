library(tidytuesdayR)
tt_data <- tt_load("2021-01-12")

## dataset notes
# artists:
# `name` seems to be structured as "last, first" - it may exclude or abbreviate 
#   certain hyphenated names or multiple first/middle/last names

# check if artwork$artist_id matches up with artists$id
library(tidyverse)
artwork <- tt_data$artwork
artists <- tt_data$artists

artwork_subset <- artwork %>%
    select(artist, artistId, title)
artist_subset <- artists %>% 
    select(id, name)
joined_data <- left_join(artwork_subset, artist_subset, by = c("artistId" = "id"))

# is the artist name in artwork = the name looked up in the artists table by `id`?
all.equal(joined_data$artist, joined_data$name) # no, some missing artist ids
idx <- which(is.na(joined_data$name))
joined_data[idx,]
12951 %in% artists$id

## computing aspect ratio (width/height)
#    - aspect ratio > 1 (landscape orientation)
#    - aspect ratio < 1 (portrait orientation)
# we could have both kinds of artwork / styles mixed in the data, or 
#   different relative proportions of those categories - something to consider 
#   for more in-depth analysis

artwork %>%
    mutate(aspect_ratio = width / height) %>%
    select(id, artist, title, medium, year, width, height, aspect_ratio) %>%
    {.} -> art

# plot aspect ratio over time
ggplot(art, 
       aes(x = year, y = aspect_ratio)) + 
    geom_point() + 
    theme_bw()
## warning for 7544 rows with missing values

## how many data points are missing year?
sum(is.na(art$year))

## how many data points are missing aspect_ratio?
sum(is.na(art$aspect_ratio))
sum(is.na(art$width))
sum(is.na(art$height))

## some wide murals may be ~10 wider than tall; an aspect ratio of ~600 seems extreme
art %>% filter(aspect_ratio > 500) %>% pull(id)
filter(artwork, id == 93796) %>% pull(url)
# "http://www.tate.org.uk/art/artworks/gillick-big-conference-platform-platform-t12429"
# So this is a piece that is mostly horizontal, so height is small, and depth is 
#   the more appropriate dimension by which to calculate aspect ratio

## Computing aspect ratio (revised)
# To deal with pieces that lie flat, check if `depth` of the artwork exists, and 
#   if depth > height, THEN
#   use width / depth to compute the aspect ratio

artwork %>%
    mutate(depth = replace_na(depth, 0), 
           effective_height = pmax(depth, height, na.rm = FALSE), 
           aspect_ratio = width / effective_height) %>%
    select(id, artist, title, medium, year, width, height, depth, 
           effective_height, aspect_ratio) %>%
    {.} -> art

# plot aspect ratio over time
ggplot(art, 
       aes(x = year, y = aspect_ratio)) + 
    scale_y_continuous(limits = c(0, 10)) + 
    geom_point() + 
    geom_hline(yintercept = 1, color = "red") + 
    theme_bw()

## It looks like art pieces that lie flat do not have depth properly 
#    extracted from the text dimensions, e.g.
artwork %>% 
    filter(width/height > 100) %>%
    select(width, height, depth, dimensions)

# so a corrected aspect ratio will need to do text processing on the 
#   `dimensions` variable to extract out the correct numbers

## In the meantime, we can do a simple filter for 3D artpieces?
idx_3d_art <- grepl("\\d+\\s*x\\s*\\d+\\s*x\\s*\\d+", artwork$dimensions)
artwork[idx_3d_art, c("medium", "dimensions", "width", "height")] %>%
    filter(width/height > 10)

# That looks like it includes pieces that have 3 dimensions, but are e.g. 
#   paintings with data for both the piece and a frame
#   However, there are only about 5000 of these, and it is detecting the 
#   3D pieces that are missing `depth` when they should have depth, so 
#   this might be an acceptable initial filter.

## Using this filter for 3D pieces:
artwork %>%
    filter(!grepl("\\d+\\s*x\\s*\\d+\\s*x\\s*\\d+", dimensions)) %>% 
    mutate(aspect_ratio = width / height) %>%
    select(id, artist, title, medium, year, width, height, aspect_ratio) %>%
    {.} -> art

# plot aspect ratio over time
ggplot(art, 
       aes(x = year, y = aspect_ratio)) + 
#    scale_y_continuous(limits = c(0, 10)) + 
    geom_point() + 
    geom_hline(yintercept = 1, color = "red") + 
    theme_bw()

# add categorical variable for portrait vs. landscape
art %>%
    filter(is.finite(aspect_ratio)) %>%
    mutate(portrait = aspect_ratio < 1) %>%
    {.} -> to_plot

p <- ggplot(to_plot, 
       aes(x = year, y = aspect_ratio, group = portrait)) + 
    geom_point() + 
    stat_smooth() + 
    geom_hline(yintercept = (1 + sqrt(5))/2, color = "red") + 
    geom_hline(yintercept = (-1 + sqrt(5))/2, color = "red") + 
    theme_bw()

ggsave("tt_plot.png", plot = p, width = 6, height = 4)
