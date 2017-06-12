rm(list=ls(all=TRUE))
require(rpart)
require(rpart.plot)

rankData <- function(X, c, n=10) {
  freq <- count(X[, c])
  freq <- freq[order(freq[,2], decreasing = TRUE), ][1:n,]
  activate <- factor(freq$x)
  return(X[X[,c] %in% activate, ])
}

data <- read.csv('./Youtubelist.csv', header=TRUE)
data <- subset(data, select= -c(X, Path) )

observations <- summary(data)

subData <- data

#subData <- rankData(subData, 'Genre', 30)

test = rpart(Box ~ Release.Date+Runtime+MPAA+Budget+FB_likes+YoutubeViews, data=data)
rpart.plot(test)
summary(test)

require(party)

test = ctree(Box ~ Release.Date+Runtime+MPAA+Budget+FB_likes+YoutubeViews, data=data, controls = ctree_control(maxdepth = 4))
plot(test)
summary(test)

test = ctree(Box ~ Release.Date+MPAA, data=data, controls = ctree_control(maxdepth = 2))
plot(test)
summary(test)

test = ctree(Box ~ YoutubeViews+FB_likes, data=data, controls = ctree_control(maxdepth = 2))
plot(test)
summary(test)