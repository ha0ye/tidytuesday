library(tidyverse)
library(topicmodels)

movies <- readRDS("2021-03-09/movies.RDS")


# construct document term matrix from movie plots
library(tm)

movies_plot_dat <- select(movies, doc_id = imdb, text = plot)
movies_corpus <- VCorpus(DataframeSource(movies_plot_dat))
movies_dtm <- DocumentTermMatrix(movies_corpus, 
                                 control = list(removePunctuation = TRUE, 
                                                stopwords = TRUE))
## filter movies with zero terms (empty plot text?)
dtm_zero_idx <- apply(movies_dtm, 1, sum) == 0
movies_dtm <- movies_dtm[!dtm_zero_idx,]

# compute LDA
num_topics <- 20

# extract out topic info
movies_TM <- LDA(movies_dtm, num_topics)
movies_id <- movies_TM@documents
movies_gamma <- movies_TM@gamma
movies_topics <- terms(movies_TM, 10)

# assemble data
primary_topic <- topics(movies_TM)
movies_topics <- data.frame(id = movies_id, 
                            primary_topic = primary_topic)
colnames(movies_gamma) <- paste("topic", seq(num_topics))
movies_dat <- cbind(movies_topics, movies_gamma) %>%
    left_join(movies, by = c("id" = "imdb")) %>%
    mutate(clean_test = fct_relevel(clean_test, "dubious", "nowomen", "notalk", "men", "ok"))

ggplot(movies_dat, 
       aes(x = primary_topic, 
           fill = clean_test)) + 
    geom_bar() + 
    theme_bw()



