# pomp_tweets

### load environemnt

if(!require(rtweet)){
  install.packages("rtweet")
  library(rtweet)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(rvest)){
  install.packages("rvest")
  library(rvest)
}

### get Pomp's most recent tweets (Twitter seems to have set a limit of ~3200 results, so there won't actually be 5k rows in response)
### note that you will have to authetucate your Tiwtter account at this point. R should automatically open up your browser
tweets <- get_timelines("APompliano", n = 5000)

### get the earliest and latest timestamps in the dataset to see the scope of time that the dataset covers
### these dates, and the results at large will be different as Pomp continues to tweet
min(tweets$created_at)
# 2019-02-28 16:15:12 UTC

max(tweets$created_at)
# 2019-05-05 18:06:29 UTC

### filter out retweets and replies ("quoted" retweets remain) to isolate original tweets
tweets_filtered <- tweets %>%
  filter(is_retweet == FALSE, is.na(reply_to_status_id) == TRUE)

### get number of rows of the filtered dataset 
nrow(tweets_filtered)
# 272

### define new features of the dataset "btc_mention" and "sto_mention" based on the text contents of Pomp's tweets
### each tweet will get a "yes" for these new features if it includes at least one of the selected words/phrases listed below that correlate to bitcoin/BTC and STOs
### if not, the tweet will receive a "no" for the new features
tweets_filtered <- mutate(tweets_filtered, 
                          btc_mention = ifelse("bitcoin" %>% grepl(tweets_filtered$text,ignore.case = T) | 
                                                 "btc" %>% grepl(tweets_filtered$text,ignore.case = T),"yes","no"),
                          sto_mention = ifelse("sto[[:punct:]]" %>% grepl(tweets_filtered$text,ignore.case = T) |  
                                                 "[[:punct:]]sto" %>% grepl(tweets_filtered$text,ignore.case = T) |
                                                 "security token" %>% grepl(tweets_filtered$text,ignore.case = T) |
                                                 "token offerring" %>% grepl(tweets_filtered$text,ignore.case = T) | 
                                                 "securities" %>% grepl(tweets_filtered$text,ignore.case = T) | 
                                                 "tokenize" %>% grepl(tweets_filtered$text,ignore.case = T) | 
                                                 "tokenizing" %>% grepl(tweets_filtered$text,ignore.case = T) | 
                                                 "tokenization" %>% grepl(tweets_filtered$text,ignore.case = T), "yes","no"))

### run this for a summary statement of the analysis
paste("Out of Pomp's",nrow(tweets_filtered),"tweets from", min(tweets_filtered$created_at), "to", max(tweets_filtered$created_at),
      "the tweets contained", table(tweets_filtered$btc_mention)['yes'], "mentions of bitcoin/BTC and", 
      table(tweets_filtered$sto_mention)['yes'], "mentions of STOs.")

### h/t to Michael W. Kearney and the rtweet package, which was used in this analysis (https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html)
