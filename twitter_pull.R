install.packages("rtweet")
install.packages("dplyr")
library(dplyr)
library("rtweet")
library("ggplot2")

setwd("C:/Users/aaron/OneDrive/Documents/Columbia Journalism/Computational Journalism/Impeachalytics/")


candidates_info <- read.csv("twitter_handles.csv") #load candidates and their twitter handles

#vignette("intro", package = "rtweet") general info on rtweet package

candidate_accounts <- lookup_users(as.vector(candidates_info[,1])) #load twitteruserobjects


handle_vector <- as.vector(candidates_info[,1]) #vector of all the candidates handles

Tweets <- get_timelines(handle_vector, n=3200, retryonratelimit = TRUE) #returns 87983 tweets as a list

df_tweets <- NULL  
typeof(Tweets[[1]])
count <- 1
for (i in length(Tweets[[1]])){
  colname <- names(Tweets[count])
  #df_tweets$colname <- Tweets[[i]]
  print(i)
  count <- count + 1
}
df_tweets <- as.data.frame(df_tweets)
typeof(df_tweets)
colnames(Tweets)

tweets_by_candidate <- Tweets %>% group_by(screen_name) %>% summarise(count = n()) #Views the number of tweets by candidate

impeachment_mention <- function(text){ #function to check whether text of a tweet includes the word "Impeach" or "impeachment" case insensitive
  if (grepl("impeach",text, ignore.case = TRUE)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

Tweets <- data.frame(matrix(unlist(Tweets), nrow=length(Tweets), byrow=T))
typeof(Tweets)
write.csv( as.data.frame(Tweets), "candidate_tweets.csv")

impeachment_mention(Tweets$text[5])

impeachment_tweets <- Tweets %>% mutate(impeachment = impeachment_mention(text))


