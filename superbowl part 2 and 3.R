#superbowl part 2&3
library(readr)
df <- read_csv("~/Desktop/tweets_0204-0204_#superbowl_2018-02-06_03-58-27_n49012.csv")
df.a <- read_csv("~/Desktop/tweets.cleaned_0204-0204_#superbowl_2018-02-06_03-58-24_n49012.csv")
tweets <- df; tweets$z <- 1; tweets$created <- as.POSIXlt(tweets$created); 
nrow(tweets); min(tweets$created); max(tweets$created); median(tweets$created)

########part 2
library(plyr)
library(ggplot2)
library(splitstackshape)
library(stringr)

####### READ IN SAVED TWITTER DATA
setwd("~/Desktop");
fnames <- c('tweets_raw');
fnames <- paste0(fnames, '.csv'); 
#df <- do.call(rbind.fill, lapply(fnames, read.csv));
df$username <- substr(substr(df$url, 21, nchar(as.character(df$url))), 1, nchar(substr(df$url, 21, nchar(as.character(df$url))))-26);
tweets.full <- df; tweets.full$X <- NULL; tweets.full$z <- 1; 

#### sanity checking
tweets.full$created <- as.POSIXlt(tweets.full$created); min(tweets.full$created); max(tweets.full$created); median(tweets.full$created); nrow(tweets.full); length(unique(tweets.full$username))
#unique 35575

## dedupe dataset by url
tweets.dupes <- tweets.full[duplicated(tweets.full$url), ]; nrow(tweets.full); nrow(tweets.dupes); # test <- subset(tweets.full, url %in% tweets.dupes$url); test <- test[with(test, order(url)), ];
tweets <- tweets.full[!duplicated(tweets.full$url), ]; tweets <- arrange(tweets, url); row.names(tweets) <- NULL; tweets$tweetid <- as.numeric(row.names(tweets)); nrow(tweets);
tweets.final <- tweets;



#### READ IN EMOJI DICTIONARIES
setwd("~/Desktop");
emdict.la <- read.csv('emoticon_conversion_noGraphic.csv', header = F); #Lauren Ancona; https://github.com/laurenancona/twimoji/tree/master/twitterEmojiProject
emdict.la <- emdict.la[-1, ]; 
row.names(emdict.la) <- NULL; 
names(emdict.la) <- c('unicode', 'bytes', 'name'); 
emdict.la$emojiid <- row.names(emdict.la);

emdict.jpb <- read.csv('emDict.csv', header = F); #Jessica Peterka-Bonetta; http://opiateforthemass.es/articles/emoticons-in-R/
emdict.jpb <- cSplit(emdict.jpb, "V1", ";");
emdict.jpb <- emdict.jpb[-1,];
row.names(emdict.jpb) <- NULL; 
names(emdict.jpb) <- c('name', 'emoji', 'bytes', 'rencoding'); 
emdict.jpb$name <- tolower(emdict.jpb$name);
emdict.jpb$bytes <- NULL;

## merge dictionaries
emojis <- merge(emdict.la, emdict.jpb, by = 'name');  
emojis$emojiid <- as.numeric(emojis$emojiid); 
emojis <- arrange(emojis, emojiid)
#write.csv(emojis, file = "emojis dictionary.csv", row.names = FALSE)



###### FIND TOP EMOJIS FOR A GIVEN SUBSET OF THE DATA
tweets <- tweets.final;
#write.csv(tweets, file = "superbowl tweets.csv", row.names = FALSE)

## create full tweets by emojis matrix
df.s <- matrix(NA, nrow = nrow(tweets), ncol = ncol(emojis)); 
system.time(df.s <- sapply(emojis$rencoding, regexpr, tweets$text, ignore.case = T, useBytes = T));
rownames(df.s) <- 1:nrow(df.s); colnames(df.s) <- 1:ncol(df.s); df.t <- data.frame(df.s); df.t$tweetid <- tweets$tweetid;

# merge in hashtag data from original tweets dataset
df.a <- subset(tweets, select = c(tweetid, hashtag)); 
df.u <- merge(df.t, df.a, by = 'tweetid'); df.u$z <- 1; df.u <- arrange(df.u, tweetid); 
tweets.emojis.matrix <- df.u;

## create emoji count dataset
df <- subset(tweets.emojis.matrix)[, c(2:843)]; count <- colSums(df > -1);
emojis.m <- cbind(count, emojis); emojis.m <- arrange(emojis.m, desc(count));
emojis.count <- subset(emojis.m, count > 1); emojis.count$dens <- round(1000 * (emojis.count$count / nrow(tweets)), 1); emojis.count$dens.sm <- (emojis.count$count + 1) / (nrow(tweets) + 1);
emojis.count$rank <- as.numeric(row.names(emojis.count));
emojis.count.p <- subset(emojis.count, select = c(name, dens, count, rank));
#write.csv(emojis.count, file = "superbowl emojis count.csv", row.names = FALSE)

#################################
# print summary stats
subset(emojis.count.p, rank <= 10);
#                                                                     name  dens  count rank
#1                                                        american football 28.0  1370    1
#2                                                   face with tears of joy 12.0   587    2
#3  regional indicator symbol letter u + regional indicator symbol letter s  6.0   295    3
#4                                                        heavy black heart  5.7   279    4
#5                                                      clapping hands sign  5.2   257    5
#6                                 person raising both hands in celebration  5.0   244    6
#7                                      smiling face with heart-shaped eyes  3.9   191    7
#8                                                                     fire  3.4   165    8
#9                                                           thumbs up sign  2.7   133    9
#10                                                                  trophy  2.6   125   10

num.tweets <- nrow(tweets); df.t <- rowSums(tweets.emojis.matrix[, c(2:843)] > -1); num.tweets.with.emojis <- length(df.t[df.t > 0]); num.emojis <- sum(emojis.count$count);
min(tweets$created); max(tweets$created); median(tweets$created);

num.tweets; #49012
num.tweets.with.emojis; #5058
round(100 * (num.tweets.with.emojis / num.tweets), 1); #10.3
num.emojis; #6998
nrow(emojis.count) #241



##### part 3
##### MAKE BAR CHART OF TOP EMOJIS IN NEW DATASET
df.plot <- subset(emojis.count.p, rank <= 10); xlab <- 'Rank'; ylab <- 'Overall Frequency (per 1,000 Tweets)';
setwd("~/Desktop/emoji_bitcoin/ios_9_3_emoji_files");
df.plot <- arrange(df.plot, name);
imgs <- lapply(paste0(df.plot$name, '.png'), png::readPNG); g1 <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens); df.plot$xsize <- k; df.plot$ysize <- k; #df.plot$xsize <- k * (df.plot$dens / max(df.plot$dens)); df.plot$ysize <- k * (df.plot$dens / max(df.plot$dens));
df.plot <- arrange(df.plot, name);

g1 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g1[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g1;

setwd("~/Desktop");
png(paste0('emoji_barchart_', as.Date(min(tweets$created)), '_', as.Date(max(tweets$created)), '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(tweets), '.png'), 
    width = 6600, height = 4000, units = 'px', res = 1000);
g1; dev.off();

##################top 10 face emoji
library(dplyr)
emojis_face.count.p <- filter(emojis.count.p,grepl('face',name))
emojis_face.count.p$ID <- seq.int(nrow(emojis_face.count.p))
write.csv(emojis_face.count.p, file = "superbowl emojis_face count.csv", row.names = FALSE)
setwd("~/Desktop")
emojis_face.count.p <- read_csv("~/Desktop/superbowl emojis_face count.csv")
df.plot2 <- subset(emojis_face.count.p, ID <= 10); xlab <- 'Rank'; ylab <- 'Overall Frequency (per 1,000 Tweets)';
setwd("~/Desktop/emoji_bitcoin/ios_9_3_emoji_files");
df.plot2 <- arrange(df.plot2, name);
imgs2 <- lapply(paste0(df.plot2$name, '.png'), png::readPNG); g2 <- lapply(imgs2, grid::rasterGrob);
k2 <- 0.20 * (10/nrow(df.plot2)) * max(df.plot2$dens); df.plot2$xsize <- k2; df.plot2$ysize <- k2; #df.plot$xsize <- k * (df.plot$dens / max(df.plot$dens)); df.plot$ysize <- k * (df.plot$dens / max(df.plot$dens));
df.plot2 <- arrange(df.plot2, name);

g12 <- ggplot(data = df.plot2, aes(x = ID, y = dens)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g2[[i]], xmin = x-0.5*df.plot2$xsize[i], xmax = x+0.5*df.plot2$xsize[i], 
                      ymin = y-0.5*df.plot2$ysize[i], ymax = y+0.5*df.plot2$ysize[i])},
    df.plot2$ID, df.plot2$dens, seq_len(nrow(df.plot2))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot2), 1), labels = seq(1, nrow(df.plot2), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot2$dens))) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
g12;

setwd("~/Desktop");
png(paste0('emoji_barchart_', as.Date(min(tweets$created)), '_', as.Date(max(tweets$created)), '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(tweets), '.png'), 
    width = 6600, height = 4000, units = 'px', res = 1000);
g12; dev.off()
