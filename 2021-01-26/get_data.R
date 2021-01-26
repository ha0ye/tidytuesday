library(tidytuesdayR)
library(tidyverse)

dat <- tt_load("2021-01-26")

plastics_data <- dat[[1]]
print(plastics_data[1,], n = Inf, width = Inf)

unique(plastics_data$year)
unique(plastics_data$country)
unique(plastics_data$parent_company)

#### Dataset details
# data are from various cleanup events? across the globe
# data are summarized by:
#   "country of cleanup" x year x "parent company" (manufacturer)
# data are otherwise aggregated together, which is why the columns are 
#   counts of plastic of different types, and number of cleanup events 
#   for which the row includes aggregated data from

#### Correct for duplicate entries in parent company
# "Mcdonald'S Corporation" -> "McDonald's Corporation"
# "Nestlé" -> "Nestle"
# We also have "Unbranded" and "null" as two separate values for parent_company;
#   these might both represent NA (missing values)

plastics_data %>%
    mutate(parent_company = recode(parent_company, 
                                   `Mcdonald'S Corporation` = "McDonald's Corporation", 
                                   `Nestlé` = "Nestle")) %>%
    {.} -> plastics_data

saveRDS(plastics_data, "2021-01-26/plastics_data.RDS")
