rm(list=ls(all=TRUE))
require(XML)
require(bitops)
require(RCurl)
require(NLP)
require(httr)
require(chron)
require(Rfacebook)
source('./Functions.R')

tok = 'your token' 
alldata = read.csv("Fulllist.csv")

youtubeSRC = 'https://www.youtube.com/results?q='
yAppendURL = '%20trailer&sp=CAA%253D'
youtubeURL = ''

fulldata = data.frame()

myHttpheader<- c(
  "User-Agent"="Chrome/51.0.2704.103",
  "Upgrade-Insecure-Requests"="1"
)

for( i in 1:length(alldata$X))
{
  testframe = data.frame( 'FB_likes' = Rfacebook::searchPages(alldata$Title[i], tok, n=1)$likes , 'YoutubeViews'=NA)
  youtubeURL <- paste(youtubeSRC, alldata$Title[i], yAppendURL, sep='')
  youtubeURL <- gsub(" ","%20",youtubeURL)
  #youtubeURL <- iconv(youtubeURL, "big5", "utf8")
  #Encoding(youtubeURL) = "UTF-8"
  print(youtubeURL)
  URLExist = url.exists(youtubeURL)
  print(URLExist)
  if(URLExist)
  {
    html = getURL(youtubeURL, ssl.verifypeer = FALSE, encoding='UTF-8', httpheader = myHttpheader)
    xml = htmlParse(html, encoding='UTF-8')
    text = xpathSApply(xml,'//li/div/div/div[2]/div[2]/ul/li[2]/text()', sessionEncoding='utf8', xmlValue)
    if(length(text)<1) next
    #text = substring(text,6)
    #text <- gsub(",","",text)
    text <- pureDigit(text)
    text <- as.numeric(text)
    testframe$'YoutubeViews' = t(text)[1]
    testframe = cbind(alldata[i,-1],testframe)
    fulldata = rbind(fulldata, testframe)
  }
}

write.csv(fulldata,"Youtubelist.csv")
