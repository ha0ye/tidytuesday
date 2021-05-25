library(tidyverse)

unvotes <- readRDS("2021-03-23/unvotes.RDS")

# how are votes recorded?
unique(unvotes$vote) # "yes", "no", "abstain"

# We have 200 unique countries
length(unique(unvotes$country))

# Their votes on issues probably do not span a 200-dimensional space
# What are some primary axes along which votes vary?

## preprocessing, convert `vote` to numeric ("no" -> 0, "yes" -> 1, drop "abstain")

unvotes %>%
    filter(vote != "abstain") %>%
    mutate(vote = recode(vote, "yes" = 1, "no" = 0)) %>%
    {.} -> votes

## preprocess, transform data
## start: data in long-form, each row is `rcid` x `country`
## end: data in short-form, each row is `rcid`, 
##                          each col is `country`, 
##                          entries are `vote`

votes %>%
    select(rcid, country, vote) %>%
    pivot_wider(names_from = "country", values_from = "vote") %>%
    {.} -> votes_tbl

## preprocess, transform data structure
## start: tibble, `rcid` is its own column
## end:   matrix, rownames are `rcid`, 
##                colnames are same as `country`

votes_matrix <- as.matrix(votes_tbl[,-1])
rownames(votes_matrix) <- votes_tbl$rcid

# somewhere in the transformation, we lost a country
#   originally 200, now 199
setdiff(unique(unvotes$country), colnames(votes_matrix))
## "Zanzibar"

## If all the rows where Zanzibar has votes are vote == "abstain", 
##   then we would have dropped this data, and it would explain why 
##   Zanzibar does not appear in votes_matrix
## Check:
unvotes %>%
    filter(country == "Zanzibar") %>%
    pull(vote) %>%
    unique()

# Try a PCA on the matrix
pca <- prcomp(votes_matrix)

## NAs produced errors -> that `country` did not 
##   have a recorded vote for that `rcid`

is.na(votes_matrix[,"Sweden"])

roll_calls <- readRDS("2021-03-23/roll_calls.RDS")
plot(roll_calls$rcid, roll_calls$date)

## let's try re-ordering the rows of votes_matrix by `rcid` numerically

row_idx <- rownames(votes_matrix) %>%
    as.numeric() %>%
    sort() %>%
    as.character()
votes_matrix <- votes_matrix[row_idx,]

is.na(votes_matrix[,"Sweden"])

## We need to filter the votes_matrix to a block with no missing values
## Let's try using thresholds, 
##   retaining only `rcid` with at least a certain % of non-missing values, and 
##   retaining only `country` with at least a certain % non-missing values

votes_data <- votes_matrix

filter_NAs <- function(votes_data, 
                       retain_vote_threshold_frac = 0.5, 
                       retain_country_threshold_frac = 0.5)
{
    num_countries_voted <- rowSums(!is.na(votes_data))
    # plot(rownames(votes_data), num_countries_voted)
    retain_vote_threshold <- retain_vote_threshold_frac * NCOL(votes_data)
    votes_data <- votes_data[num_countries_voted >= retain_vote_threshold, ]
    
    num_votes_per_country <- colSums(!is.na(votes_data))
    # plot(num_votes_per_country)
    retain_country_threshold <- retain_country_threshold_frac * NROW(votes_data)
    votes_data <- votes_data[,num_votes_per_country >= retain_country_threshold]
}

retain_vote_threshold_frac <- 0.5
retain_country_threshold_frac <- 0.5
filter_pass_count <- 1
while(any(apply(is.na(votes_data), 1, any)))
{
    initial_size <- dim(votes_data)
    votes_data <- filter_NAs(votes_data, 
                             retain_vote_threshold_frac, 
                             retain_country_threshold_frac)
    final_size <- dim(votes_data)
    if (final_size[1] == initial_size[1])
    {
        retain_vote_threshold_frac <- retain_vote_threshold_frac + 0.05
    }
    if (final_size[2] == initial_size[2])
    {
        retain_country_threshold_frac <- retain_country_threshold_frac + 0.05
    }
    message("Filter Pass #", filter_pass_count, ": ",
            "[", initial_size[1], ", ", initial_size[2], "]", 
            " --> ", 
            "[", final_size[1], ", ", final_size[2], "]")
    if (retain_vote_threshold_frac >= 0.90 ||
        retain_country_threshold_frac >= 0.90)
    {
        break()
    }
    filter_pass_count <- filter_pass_count + 1
}

## Let's just impute NAs ->
for (j in seq_len(NCOL(votes_data)))
{
    x <- votes_data[,j]
    na_idx <- is.na(x)
    perc_yes <- sum(x, na.rm = TRUE) / sum(!na_idx)
    votes_data[na_idx, j] <- perc_yes
}

## check for any remaining NAs
any(apply(is.na(votes_data), 1, any)) # should be FALSE

# Try a PCA on the matrix
pca <- prcomp(votes_data)

biplot(pca)




