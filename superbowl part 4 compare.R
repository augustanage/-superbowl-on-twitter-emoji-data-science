#######part 4
#compare patriots and eagles

##### CREATE MASTER DATASET OF ORIGINAL TWEETS appended with array of emojis
## EMOJIS: create reduced tweets+emojis matrix
df.s <- data.frame(matrix(NA, nrow = nrow(tweets), ncol = 2)); names(df.s) <- c('tweetid', 'emoji.ids'); df.s$tweetid <- 1:nrow(tweets);
system.time(df.s$emoji.ids <- apply(tweets.emojis.matrix[, c(2:843)], 1, function(x) paste(which(x > -1), sep = '', collapse = ', '))); 
system.time(df.s$num.emojis <- sapply(df.s$emoji.ids, function(x) length(unlist(strsplit(x, ', '))))); 
df.s.emojis <- subset(df.s, num.emojis > 0);
df.s.nonemojis <- subset(df.s, num.emojis == 0); 
df.s.nonemojis$emoji.names <- '';

# convert to long, only for nonzero entries
df.l <- cSplit(df.s.emojis, splitCols = 'emoji.ids', sep = ', ', direction = 'long')
map <- subset(emojis, select = c(emojiid, name)); map$emojiid <- as.numeric(map$emojiid);
df.m <- merge(df.l, map, by.x = 'emoji.ids', by.y = 'emojiid'); df.m <- arrange(df.m, tweetid); 
df.m <- rename(df.m, c(name = 'emoji.name')); #can't use dplyr for rename only in plyr
tweets.emojis.long <- subset(df.m, select = c(tweetid, emoji.name));
df.n <- aggregate(emoji.name ~ tweetid, paste, collapse = ', ', data = df.m);

## merge back with original tweets dataset
df.f <- merge(df.s.emojis, df.n, by = 'tweetid'); df.f <- rename(df.f, c(emoji.name = 'emoji.names'));
df.g <- rbind(df.f, df.s.nonemojis); df.g <- arrange(df.g, tweetid);
df.h <- merge(tweets, df.g, by = 'tweetid', all.x = TRUE); df.h$emoji.ids <- NULL; df.h$tweetid <- as.numeric(df.h$tweetid); df.h <- arrange(df.h, tweetid);
tweets.emojis <- df.h;

#### MAKE TWO WAY PLOT FOR A SET OF MUTUALLY EXCLUSIVE SUBSETS OF THE DATA
df.1 <- subset(tweets.emojis, grepl(paste(c('eagles'), collapse = '|'), tolower(tweets.emojis$text)));
df.2 <- subset(tweets.emojis, grepl(paste(c('patriots'), collapse = '|'), tolower(tweets.emojis$text)));
nrow(df.1); nrow(df.2);

# dataset 1 #eagles
df.a <- subset(subset(df.1, emoji.names != ''), select = c(tweetid, emoji.names)); df.a$emoji.names <- as.character(df.a$emoji.names);
df.b <- data.frame(table(unlist(strsplit(df.a$emoji.names, ',')))); names(df.b) <- c('var', 'freq'); df.b$var <- trimws(df.b$var, 'both'); df.b <- subset(df.b, var != '');
df.c <- aggregate(freq ~ var, data = df.b, function(x) sum(x)); df.c <- df.c[with(df.c, order(-freq)), ]; row.names(df.c) <- NULL;
df.d <- subset(df.c, freq > 1); df.d$dens <- round(1000 * (df.d$freq / nrow(df)), 1); df.d$dens.sm <- (df.d$freq + 1) / (nrow(df) + 1); df.d$rank <- as.numeric(row.names(df.d)); df.d <- rename(df.d, c(var = 'name'));
df.e <- subset(df.d, select = c(name, dens, dens.sm, freq, rank)); 
df.e$ht <- as.character(arrange(data.frame(table(tolower(unlist(str_extract_all(df.1$text, '#\\w+'))))), -Freq)$Var1[1]);
df.e[1:10, ]; emojis.count.1 <- df.e;
#                                                                      name  dens   dens.sm   freq
#1                                                        american football  9.1 0.0090792239  444
#2                                                              green heart  1.1 0.0011017485   53
#3                                                   face with tears of joy  1.0 0.0010201375   49
#4                                 person raising both hands in celebration  0.9 0.0009589293   46
#5                                                        heavy black heart  0.8 0.0008569155   41
#6                                                                   trophy  0.8 0.0008569155   41
#7                                                                     fire  0.8 0.0008365128   40
#8  regional indicator symbol letter u + regional indicator symbol letter s  0.8 0.0008161100   39
#9                                                      clapping hands sign  0.7 0.0007140963   34
#10                                                           flexed biceps  0.7 0.0007140963   34

# dataset 2 #patriots
df.a <- subset(subset(df.2, emoji.names != ''), select = c(tweetid, emoji.names)); df.a$emoji.names <- as.character(df.a$emoji.names);
df.b <- data.frame(table(unlist(strsplit(df.a$emoji.names, ',')))); names(df.b) <- c('var', 'freq'); df.b$var <- trimws(df.b$var, 'both'); df.b <- subset(df.b, var != '');
df.c <- aggregate(freq ~ var, data = df.b, function(x) sum(x)); df.c <- df.c[with(df.c, order(-freq)), ]; row.names(df.c) <- NULL;
df.d <- subset(df.c, freq > 1); df.d$dens <- round(1000 * (df.d$freq / nrow(df)), 1); df.d$dens.sm <- (df.d$freq + 1) / (nrow(df) + 1); df.d$rank <- as.numeric(row.names(df.d)); df.d <- rename(df.d, c(var = 'name'));
df.e <- subset(df.d, select = c(name, dens, dens.sm, freq, rank));
df.e$ht <- as.character(arrange(data.frame(table(tolower(unlist(str_extract_all(df.2$text, '#\\w+'))))), -Freq)$Var1[1]);
df.e[1:10, ]; emojis.count.2 <- df.e;
#                                                                     name  dens    dens.sm   freq
#1                                                        american football  6.3 0.0063044498  308
#2                                                   face with tears of joy  1.2 0.0012037623   58
#3                                                        heavy black heart  1.0 0.0009997348   48
#4  regional indicator symbol letter u + regional indicator symbol letter s  0.9 0.0008773183   42
#5                                                                   trophy  0.9 0.0008773183   42
#6                                                               blue heart  0.6 0.0005916798   28
#7                                                            flexed biceps  0.5 0.0005100688   24
#8                                                                     fire  0.4 0.0004488605   21
#9                                 person raising both hands in celebration  0.4 0.0004284578   20
#10                                                     clapping hands sign  0.3 0.0003264440   15

#########################################
# combine datasets and create final dataset
names(emojis.count.1)[-1] <- paste0(names(emojis.count.1)[-1], '.1'); names(emojis.count.2)[-1] <- paste0(names(emojis.count.2)[-1], '.2'); 
df.a <- merge(emojis.count.1, emojis.count.2, by = 'name', all.x = TRUE, all.y = TRUE);
df.a[, c(2:4, 6:8)][is.na(df.a[, c(2:4, 6:8)])] <- 0; df.a <- df.a[with (df.a, order(-dens.1)), ];
df.a$index <- ifelse(df.a$dens.1 > 0 & df.a$dens.2 > 0 & (df.a$dens.1 > df.a$dens.2), round(100 * ((df.a$dens.1 / df.a$dens.2) - 1), 0),
                     ifelse(df.a$dens.1 > 0 & df.a$dens.2 > 0 & (df.a$dens.2 > df.a$dens.1), -1 * round(100 * ((df.a$dens.2 / df.a$dens.1) - 1), 0), NA));
df.a$logor <- log(df.a$dens.sm.1 / df.a$dens.sm.2);
df.a$dens.mean <- 0.5 * (df.a$dens.1 + df.a$dens.2);
k <- 50; df.b <- subset(df.a, (rank.1 <= k | rank.2 <= k) & 
                          (freq.1 >= 5 | freq.2 >= 5) & 
                          (freq.1 > 0 & freq.2 > 0) & dens.mean > 0); nrow(df.b);
df.c <- subset(df.b, select = c(name, dens.1, dens.2, freq.1, freq.2, dens.mean, round(logor, 2)));
df.c <- df.c[with(df.c, order(-logor)), ]; row.names(df.c) <- NULL; nrow(df.c); df.c;
emojis.comp.p <- df.c;
rbind(head(emojis.comp.p), tail(emojis.comp.p))
#                                       name  dens.1  dens.2 freq.1 freq.2 dens.mean  logor
#1                               green heart    1.1    0.1     53      3      0.60  2.6026897
#2                              party popper    0.5    0.1     25      7      0.30  1.1786550
#3                        clinking beer mugs    0.1    0.0      7      2      0.05  0.9808293
#4                            thumbs up sign    0.6    0.2     29     11      0.40  0.9162907
#5                              winking face    0.3    0.1     17      7      0.20  0.8109302
#6  person raising both hands in celebration    0.9    0.4     46     20      0.65  0.8056252
#43                        heavy black heart    0.8    1.0     41     48      0.90 -0.1541507
#44                   face with tears of joy    1.0    1.2     49     58      1.10 -0.1655144
#45                  double exclamation mark    0.1    0.2      6      9      0.15 -0.3566749
#46                   white heavy check mark    0.1    0.1      3      5      0.10 -0.4054651
#47                           slice of pizza    0.1    0.1      3      6      0.10 -0.5596158
#48                               blue heart    0.1    0.6      5     28      0.35 -1.5755364

#########################################
##### PLOT TOP EMOJIS SCATTERPLOT: FREQ VS VALENCE  
## read in custom emojis
setwd("~/Desktop/emoji_bitcoin/ios_9_3_emoji_files");
df.t <- arrange(emojis.comp.p, name);
imgs <- lapply(paste0(df.t$name, '.png'), png::readPNG)
g <- lapply(imgs, grid::rasterGrob);
## make plot  
df.t <- arrange(emojis.comp.p, logor)
xlab <- paste0('Emoji Valence: Log Odds Ratio (', paste0(unique(emojis.count.2$ht), ' <--> ', unique(emojis.count.1$ht), ')'));
ylab <- 'Overall Frequency (Per 1,000 Tweets)'
k <- 8 # size parameter for median element
xsize <- (k/100) * (max(df.t$logor) - min(df.t$logor)); ysize <- (k/100) * (max(df.t$dens.mean) - min(df.t$dens.mean));
df.t$xsize <- xsize; df.t$ysize <- ysize;
df.t$dens.m <- ifelse(df.t$dens.mean > median(df.t$dens.mean), round(sqrt((df.t$dens.mean / min(df.t$dens.mean))), 2), 1);
df.t$xsize <- df.t$dens.m * df.t$xsize; df.t$ysize <- df.t$dens.m * df.t$ysize;
df.t <- arrange(df.t, name);
g1 <- ggplot(df.t, aes(jitter(logor), dens.mean)) +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.t$xsize[i], xmax = x+0.5*df.t$xsize[i], 
                      ymin = y-0.5*df.t$ysize[i], ymax = y+0.5*df.t$ysize[i])},
    jitter(df.t$logor), df.t$dens.mean, seq_len(nrow(df.t))) +
  scale_x_continuous(limits = c(1.15 * min(df.t$logor), 1.15 * max(df.t$logor))) +
  scale_y_continuous(limits = c(0, 1.20 * max(df.t$dens.mean))) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
#setwd('.../PRISMOJI/tutorial/');
setwd("~/Desktop");
png(paste0('emojis.comp.p_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '.png'), 
    width = 6600, height = 4000, units = 'px', res = 1000);
g1; dev.off()

##############exclude american football emoji
emojis.comp.p_new <- emojis.comp.p[-c(24), ]

setwd("~/Desktop/emoji_bitcoin/ios_9_3_emoji_files");
df.t2 <- arrange(emojis.comp.p_new, name);
imgs2 <- lapply(paste0(df.t2$name, '.png'), png::readPNG)
g2 <- lapply(imgs2, grid::rasterGrob);
## make plot  
df.t2 <- arrange(emojis.comp.p_new, logor)
xlab <- paste0('Emoji Valence: Log Odds Ratio (', paste0(unique(emojis.count.2$ht), ' <--> ', unique(emojis.count.1$ht), ')'));
ylab <- 'Overall Frequency (Per 1,000 Tweets)'
k <- 5 # size parameter for median element
xsize <- (k/100) * (max(df.t2$logor) - min(df.t2$logor)); ysize <- (k/100) * (max(df.t2$dens.mean) - min(df.t2$dens.mean));
df.t2$xsize <- xsize; df.t2$ysize <- ysize;
df.t2$dens.m <- ifelse(df.t2$dens.mean > median(df.t2$dens.mean), round(sqrt((df.t2$dens.mean / min(df.t2$dens.mean))), 2), 1);
df.t2$xsize <- df.t2$dens.m * df.t2$xsize; df.t2$ysize <- df.t2$dens.m * df.t2$ysize;
df.t2 <- arrange(df.t2, name);

g12 <- ggplot(df.t2, aes(jitter(logor), dens.mean)) +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g2[[i]], xmin = x-0.5*df.t2$xsize[i], xmax = x+0.5*df.t2$xsize[i], 
                      ymin = y-0.5*df.t2$ysize[i], ymax = y+0.5*df.t2$ysize[i])},
    jitter(df.t2$logor), df.t2$dens.mean, seq_len(nrow(df.t2))) +
  scale_x_continuous(limits = c(1.15 * min(df.t2$logor), 1.15 * max(df.t2$logor))) +
  scale_y_continuous(limits = c(0, 1.20 * max(df.t2$dens.mean))) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
#setwd('.../PRISMOJI/tutorial/');
setwd("~/Desktop");
png(paste0('emojis.comp.p_new_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '.png'), 
    width = 6600, height = 4000, units = 'px', res = 1000);
g12; dev.off()
