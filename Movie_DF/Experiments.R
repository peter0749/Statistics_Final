rm(list=ls(all=TRUE))
require(rpart)
require(rpart.plot)
require(ggplot2)
require(plyr)

rankData <- function(X, c, n=10) {
  freq <- count(X[, c])
  freq <- freq[order(freq[,2], decreasing = TRUE), ][1:n,]
  activate <- factor(freq$x)
  return(X[X[,c] %in% activate, ])
}

find_mode <- function(x) {
  return(names(table(x)[table(x)%in%max(table(x))]))
}

data <- read.csv('./Youtubelist.csv', header=TRUE)
data <- subset(data, select= -c(X, Path) )

observations <- summary(data)

decision_tree = rpart(Box ~ Release.Date+Runtime+MPAA+Budget+FB_likes+YoutubeViews, data=data)
rpart.plot(decision_tree)
summary(decision_tree)

dtypes = c(2,6,8,10,11)
dnames = names(data)[dtypes]

for(i in 1:length(dtypes)) {
  temp = data.frame(data[!is.na(data[dtypes[i]]),dtypes[i]])
  names(temp) = c('x')
  tmean = mean(temp$x)
  tmedian = median(temp$x)
  
  p <- ggplot(data = temp, aes(x=x)) + geom_density() + xlab(dnames[i])
  #p_log <- p + geom_vline(xintercept = sumx, colour=c('red', 'green', 'blue', 'magenta', 'yellow', 'orange')) + scale_x_log10()
  p_log <- p + geom_vline(xintercept = tmean, color='green') + geom_vline(xintercept = tmedian, color='blue') + scale_x_log10()
  plot(p)
  plot(p_log)
}

ftypes = c(3,4,5,7,9)
fnames = names(data)[ftypes]

thres=20 # 只列出前二十名，太多也看不完
for(i in 1:length(ftypes)) {
  counts <- count(data[!is.na(data[ftypes[i]]),ftypes[i]])
  counts <- counts[order(counts$freq, decreasing = TRUE),]
  if(nrow(counts)>thres) counts <- counts[1:thres,]
  names(counts) <- c('x', 'freq')
  p <- ggplot(data = counts, aes(x=reorder(x, -freq), y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab(fnames[i]) + ylab('Frequency')
  #p <- ggplot(data = counts, aes(x=x, y=freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab(fnames[i]) + ylab('Frequency')
  plot(p)
}

movie_aov <- aov(data = data, Box ~ Runtime * Budget * FB_likes * YoutubeViews )
summary_movie_aov <- summary(movie_aov)
summary_movie_aov
## 測試的結果： Box ~ Budge (預算會影響票房)
## 因為 p < 0.05
## 而不能確定 Box ~ YoutubeViews  (Youtube 播放數對票房)
## 還有       Box ~ FB_likes      (FB 按讚數對票房)
## 還有       Box ~ Runtime       (電影長度對票房)
## 個別的影響，因為 
## FB_likes:YoutubeViews, FB_likes:Runtime, FB_likes:YoutubeViews:Runtime
## 是顯著的，所以我們只能假設，FB 按讚數、Youtube 瀏覽數、電影時長的組合，會影響電影票房
