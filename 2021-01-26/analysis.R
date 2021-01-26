library(tidyverse)

plastics_data <- readRDS("2021-01-26/plastics_data.RDS")

#### How widespread are plastics from any individual parent company
# for each parent company, in how many countries was a plastic found 
#   during a cleanup?

summary_1 <- plastics_data %>%
    group_by(parent_company) %>%
    summarize(num_countries = n_distinct(country)) %>%
    arrange(desc(num_countries))

# view top 40 parent_company in terms of # of countries in which plastic was found during cleanup
print(summary_1, n = 40, width = Inf)

# results suggest some duplication of parent company, perhaps due to data entry 
#   or different labeling schemes (e.g. Nestle/Nestlé)

#### Across top 40 parent companies, what is the distribution of plastic types?
plastics_data %>%
    filter(parent_company %in% summary_1$parent_company[1:40]) %>%
    pivot_longer(empty:pvc, names_to = "plastic_type", values_to = "count") %>%
    select(country, year, parent_company, plastic_type, count) %>%
    mutate(plastic_type = recode(plastic_type, 
                                 empty = "NA", 
                                 o = "other")) %>%
    count(parent_company, plastic_type, wt = count, name = "count") %>%
    {.} -> plastic_type_data

# aggregated across parent_company
summary_2 <- plastic_type_data %>%
    group_by(plastic_type) %>%
    summarize(count = sum(count))

ggplot(summary_2, 
       aes(x = plastic_type, y = count)) +
    geom_col() + 
    theme_bw()

# high count for plastic_type = "other" might mask useful signal 
#   for known plastic_type
# let's try excluding plastic_type == "other" and re-plotting

plastics_data %>%
    filter(parent_company %in% summary_1$parent_company[1:40]) %>%
    pivot_longer(empty:pvc, names_to = "plastic_type", values_to = "count") %>%
    select(country, year, parent_company, plastic_type, count) %>%
    mutate(plastic_type = recode(plastic_type, 
                                 empty = "NA", 
                                 o = "other")) %>%
    count(parent_company, plastic_type, wt = count, name = "count") %>%
    filter(plastic_type != "other")  %>%
    {.} -> plastic_type_data

summary_3 <- plastic_type_data %>%
    group_by(plastic_type) %>%
    summarize(count = sum(count))

p1 <- ggplot(summary_3, 
             aes(x = plastic_type, y = count)) +
    geom_col() + 
    theme_bw(base_size = 18) + 
    ggtitle("Plastics from top 40 companies")

ggsave("2021-01-26/plastic_types_aggregated.png", plot = p1, 
       width = 6, height = 6)

# now try plotting each company in its own facet

ggplot(plastic_type_data, 
       aes(x = plastic_type, y = count)) + 
    facet_wrap(~parent_company, nrow = 8, 
               scales = "free_y") + # need different y-axis for each facet
    geom_col() + 
    theme_bw()

# we want the facets to be ordered by `summary_1` (# of countries in which that 
#   company's plastics are found)

str(plastic_type_data)
plastic_type_data %>%
    mutate(parent_company = factor(parent_company, 
                                   levels = summary_1$parent_company)) %>% 
    {.} -> ordered_plastic_type_data

p2 <- ggplot(ordered_plastic_type_data, 
       aes(x = plastic_type, y = count)) + 
    facet_wrap(~parent_company, nrow = 8, 
               scales = "free_y") + 
    geom_col() + 
    theme_bw(base_size = 14) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    ggtitle("Plastics from top 40 companies")

ggsave("2021-01-26/plastic_types_individual.png", plot = p2, 
       width = 10, height = 15)

