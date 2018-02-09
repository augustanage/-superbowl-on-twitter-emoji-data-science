#superbowl wordcloud
library(dplyr)
library(stringr)
library(lubridate)
library(qdap)
library(tm)
library(wordcloud)

library(readr)
df <- read_csv("~/Desktop/superbowl twitter emoji data scinece project/superbowl tweets.csv")
df_tears_of_joy <- filter(df,grepl('<ed><a0><bd><ed><b8><82>',text)) 
df_american_flag <- filter(df,grepl('<ed><a0><bc><ed><b7><ba><ed><a0><bc><ed><b7><b8>',text)) 
text_to_clean1 <- df_tears_of_joy$text
text_to_clean2 <- df_american_flag$text
source_df1 <- VectorSource(df_tears_of_joy$text)
source_df2 <- VectorSource(df_american_flag$text)
tweet_corpus1 <- VCorpus(source_df1)
tweet_corpus2 <- VCorpus(source_df2)

#writing a function to preprocess text data :
# 1. remove white spaces
# 2. replace abbreviation
# 3. tolower
# 4. removing punctuation
# 5. removing numbers
# 6. remove stop words - will return later if the texts have too many random
# words
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords,  c(stopwords("en"),
                                           "another","let","one","amp","now",
                                           "edabcedbeb","day","today","get","just","download",
                                           "sure", "via", "time","send",
                                           "will","best","santos", "please",
                                           "needs","thanks"))
  return(corpus)
}


##########
clean_tweets1 <- clean_corpus(tweet_corpus1)
clean_tweets2 <- clean_corpus(tweet_corpus2)

tweets_tdm1 <- TermDocumentMatrix(clean_tweets1)
tweets_tdm2 <- TermDocumentMatrix(clean_tweets2)

tweets_tdm1
tweets_tdm2


# converting tdm to matrix
tweets_m1 <- as.matrix(tweets_tdm1)
tweets_m2 <- as.matrix(tweets_tdm2)

term_frequency1 <- rowSums(tweets_m1)
term_frequency2 <- rowSums(tweets_m2)

word_frequency1 <- data.frame(term = names(term_frequency1), num = term_frequency1)
word_frequency2 <- data.frame(term = names(term_frequency2), num = term_frequency2)

word_frequency_rank1 <- word_frequency1 %>% dplyr::arrange(desc(num))
word_frequency_rank2 <- word_frequency2 %>% dplyr::arrange(desc(num))

word_frequency_rank1[1:10,]
word_frequency_rank2[1:10,]
word_frequency_rank_clean1 <- word_frequency_rank1[-c(1,2,3,4,20,29),]
word_frequency_rank_clean2 <- word_frequency_rank2[-c(1,3,12,20,24),]

wordcloud(word_frequency_rank_clean1$term, word_frequency_rank_clean1$num,
          max.words = 100, min.freq = 20, colors = "gold")

wordcloud(word_frequency_rank_clean2$term, word_frequency_rank_clean2$num,
          max.words = 200, min.freq = 10, colors = "blue")

