library(dplyr)
library(tidytext)
library(stringr)

file_USblog <- "~/R/Coursera Data Science Projects/CapstoneProject/final/en_US/en_US.blogs.txt"
file_USnews <- "~/R/Coursera Data Science Projects/CapstoneProject/final/en_US/en_US.news.txt"
file_UStwitter <- "~/R/Coursera Data Science Projects/CapstoneProject/final/en_US/en_US.twitter.txt"

blogtemp <- file(file_USblog,open="r")
USblog <-readLines(blogtemp)
close(blogtemp)
rm(blogtemp)

newstemp <- file(file_USnews,open="r")
USnews <-readLines(newstemp)
close(newstemp)
rm(newstemp)

twittertemp <- file(file_UStwitter,open="r")
UStwitter <-readLines(twittertemp)
close(twittertemp)
rm(twittertemp)

USblog <- str_remove_all(USblog, "[^a-zA-Z'\\s]+") %>%
  tolower() 
USnews <- str_remove_all(USnews, "[^a-zA-Z'\\s]+") %>%
  tolower()
UStwitter <- str_remove_all(UStwitter, "[^a-zA-Z'\\s]+") %>%
  tolower()