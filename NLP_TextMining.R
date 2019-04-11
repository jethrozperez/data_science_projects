## Load Libraries
library(dplyr)
library(tidyr)
library(tidytext)

## Load Data
reviews <- read.csv("rcc_reviews.csv", stringsAsFactors = FALSE)

## Transform into a tidy text document 1. one word doc, 2. two word ngram
tidy_review<-reviews %>% unnest_tokens(word,comment) 
tidy_review_n<-reviews %>% unnest_tokens(word,comment, token = "ngrams", n = 3) 

## Clean/remove stopwords only for single token
other <- c("color", "hair", "madison", "reed")
tidy_clean<- tidy_review_n %>% anti_join(stop_words) %>% filter(!word %in% other)

## Count of words
tidy_clean %>% count(word, sort=TRUE)

## Clean ngrams 
ngram_sep <- tidy_review_n %>% 
  separate(word, c("word1","word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word1 %in% other) %>%
  filter(!word2 %in% other) %>%
  filter(!word3 %in% other) 
 
ngram_unite <- ngram_sep %>% unite(ngram, word1, word2, word3, sep = " ")

##Count words
ngram_sep %>% count(word1,word2,word3, sort = TRUE)
ngram_sep %>% count(ngram, sep = TRUE)
