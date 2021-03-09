library(tidytuesdayR)
library(tidyverse)

dat <- tt_load("2021-03-09")

dat$raw_bechdel$imdb_id
dat$movies$imdb
unique(dat$movies$test)
unique(dat$movies$clean_test)
dat$movies$code

saveRDS(dat$movies, "2021-03-09/movies.RDS")

