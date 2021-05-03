library(sbo)
library(dplyr)
library(tidytext)

train_blog <- USblog[c(1:(0.7*length(USblog)))] %>%
  as.list()
train_news <- USnews[c(1:(0.7*length(USnews)))] %>%
  as.list()
train_twitter <- UStwitter[c(1:(0.7*length(UStwitter)))] %>%
  as.list()
train <- c(train_blog, train_news, train_twitter) %>%
  unlist()
rm(train_blog) ; rm(train_news) ; rm(train_twitter)

test_blog <- USblog[c((0.7*length(USblog) + 1) : length(USblog))] %>%
  as.list()
test_news <- USnews[c((0.7*length(USnews) + 1) : length(USnews))] %>%
  as.list()
test_twitter <- UStwitter[c((0.7*length(UStwitter) + 1) : length(UStwitter))] %>%
  as.list()
test <- c(test_blog, test_news, test_twitter) %>%
  unlist()
rm(test_blog) ; rm(test_news) ; rm(test_twitter)

### build predictive model using Stupid-Backoff Ngram models
train_freq <- kgram_freqs_fast(train, N=4, dict = max_size ~ 10000, EOS = ".?!:;")
small_train_freq <- kgram_freqs_fast(train, N=4, dict = max_size ~ 708, EOS = ".?!:;")
med_train_freq <- kgram_freqs_fast(train, N=4, dict = max_size ~ 1688, EOS = ".?!:;")

predict_table <- sbo_predtable(train_freq)
small_predict_table <- sbo_predtable(small_train_freq)
med_predict_table <- sbo_predtable(med_train_freq)

predictor <- sbo_predictor(predict_table)
small_predictor <- sbo_predictor(small_predict_table)
med_predictor <- sbo_predictor(med_predict_table)

