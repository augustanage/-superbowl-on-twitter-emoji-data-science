#superbowl emoji data science project

#######part 1
# set up Twitter Authentication
library(twitteR)
library(reshape)
###### GRAB TWEETS, PROCESS, AND WRITE TO DISK ######
# authenticate with twitter: get your credentials by creating an app at apps.twitter.com
api_key <- '---'
api_secret <- '---'
access_token <- '---'
access_token_secret <- '---'
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


# 2018/02/05, #superbowl
set.seed(20180205); ht <- '#superbowl'; 
tweets.raw <- searchTwitter(ht, n = 100000, lang = 'en', since = '2018-02-03', until = '2018-02-05');
df <- twListToDF(strip_retweets(tweets.raw, strip_manual = TRUE, strip_mt = TRUE)); df$hashtag <- ht; df$created <- as.POSIXlt(df$created); df$text <- iconv(df$text, 'latin1', 'ASCII', 'byte'); df$url <- paste0('https://twitter.com/', df$screenName, '/status/', df$id); df <- rename(df, c(retweetCount = 'retweets'));
df.a <- subset(df, select = c(text, created, url, latitude, longitude, retweets, hashtag));
nrow(df.a); head(df.a);
setwd("~/Desktop");
write.csv(df.a, paste0('tweets.cleaned_', format(min(df.a$created), '%m%d'), '-', format(max(df.a$created), '%m%d'), '_', ht, '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(df.a), '.csv'), row.names = FALSE);
write.csv(df, paste0('tweets_', format(min(df.a$created), '%m%d'), '-', format(max(df.a$created), '%m%d'), '_', ht, '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(df.a), '.csv'), row.names = FALSE);

tweets <- df; tweets$z <- 1; tweets$created <- as.POSIXlt(tweets$created); 
nrow(tweets); min(tweets$created); max(tweets$created); median(tweets$created)
#49012
#"2018-02-04 23:23:58 UTC"
#"2018-02-04 23:59:59 UTC"
#"2018-02-04 23:41:01 UTC"