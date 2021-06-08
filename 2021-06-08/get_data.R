# setup ----
library(tidyverse)

# download the data files ----

fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
stocked <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv')

# save the data files for later reuse ----
saveRDS(fishing, file = "2021-06-08/fishing.RDS")
saveRDS(stocked, file = "2021-06-08/stocked.RDS")

# examine the data ----
summary(fishing)

unique(fishing$lake)
unique(fishing$species)
unique(fishing$region)

## check if some fish might be represented more than once in 
## multiple rows, because region includes various totals

dat <- fishing %>%
    filter(species == "Lake Trout", 
           year == 1978)

# notes:
# - Grand Total is duplicated across rows in the same lake, 
#   so these grand totals represent lake-level summary
# - Grand Total is equal to U.S. Total + Canada, but sometimes 
#   Canada is an NA (missing data), which makes the Grand Total also NA
# - the name of the region for Canada total in each lake is different:
#   + Erie: Canada (ONT)
#   + Ontario: Canada (ONT)
#   + Huron: Total Canada (ONT)
#   + Superior: Canada (ONT)




