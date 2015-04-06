# Forecasting EV Sales
devtools::install_github('rstudio/dygraphs')
devtools::install_github('ramnathv/htmlwidgets')
library(dygraphs)
library(fpp)
library(forecast)
library(magrittr)

# Data from http://insideevs.com/monthly-plug-in-sales-scorecard/
ev<-read.csv(file.path("~/Documents","EVCSV.csv"))
evsales<-ts(data=ev$X.1, start= c(2010,12), frequency=12)
dygraph(evsales, main = "EV Sales in the US") %>% 
  dyRangeSelector()
# Observations
# Seasonal pattern
# Variance is increasing


#plot(evsales)
seasonplot(evsales, year.labels=TRUE)
#Observations
# overlap between years
# peak is shifting 

# Let's investigate further
plot(stl(evsales,s.window="periodic",robust=TRUE))
# Observations
# More clarity on seasonality
# trend becoming stationary
# increasing variance

fit <-ets(evsales, lambda=BoxCox.lambda(evsales), additive=TRUE)
plot(fit)
accuracy(fit)
plot(forecast(fit,22))
#Observations
# Huge variation indicates high uncertainty

fit1 <- auto.arima(evsales, lambda=BoxCox.lambda(evsales))
summary(fit1)
accuracy(fit1)
plot(forecast(fit1,22))

# Insights:
# Barriers to ev adoption are strong. Hence decreasing trend.
# Cannot rule out crossing the chasm
# http://www.rand.org/content/dam/rand/pubs/working_papers/2012/RAND_WR775.pdf

gp<-read.csv(file.path("~/Documents","gasPrices.csv"))
gasPrices<-ts(data=gp$Price, start= 2010 + 340/7/365.25, frequency=365.25/7)
print(gasPrices)
dygraph(gasPrices, main = "Gas Prices in the US") %>% 
  dyRangeSelector()

## Twitter Sentiment Analysis
#install the necessary packages
install.packages(c("devtools", "rjson", "bit64"))
library(devtools)
devtools::install_github("hadley/httr")
install_github("jamespaul007/twitteR")
#install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")
packageVersion("httr")
library("twitteR")
library("wordcloud")
library("tm")

searchTerm <-"Chargepoint"
EnsurePackage<-function(x)
{x <- as.character(x)
 if (!require(x,character.only=TRUE))
 {
   install.packages(pkgs=x,repos="http://cran.r-project.org")
   require(x,character.only=TRUE)
 }
}

#Identifying packages required  (Stanton 2013)
PrepareTwitter<-function()
{
  #EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
  EnsurePackage("tm")
  EnsurePackage("RJSONIO")
  EnsurePackage("wordcloud")
  EnsurePackage("gridExtra")
  EnsurePackage("httr") 
  EnsurePackage("plyr")
}

PrepareTwitter()

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "9355PzAb9wd8S5AtmMnFdS2iQ"
consumerSecret <- "Y9k5CpEJKNgv55rKfmQejaMjKHN1QObD59uqErvBl4u3UzVg68"
access_token<-"91194876-mDUu0qZb2aTW8oU71tDvKGjPGg79xojjIMrAQfftq"
access_token_secret <-"Lgj9CyiC1veXjjdLFctkNZ8Ee2kaYIs2oLHtYa2MXp0qe"
#twitCred <- OAuthFactory$new(consumerKey=consumerKey,
#                             consumerSecret=consumerSecret,
#                             requestURL=reqURL,
#                             accessURL=accessURL,
#                             authURL=authURL)
#twitCred$handshake()
#registerTwitterOAuth(twitCred)
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_token_secret)

CleanTweets<-function(tweets)
{
  # Remove redundant spaces
  tweets <- str_replace_all(tweets," "," ")
  # Get rid of URLs
  tweets <- str_replace_all(tweets, "http://t.co/[a-z,A-Z,0-9]*{8}","")
  # Take out retweet header, there is only one
  tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
  # Get rid of hashtags
  tweets <- str_replace_all(tweets,"#[a-z,A-Z]*","")
  # Get rid of references to other screennames
  tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
  return(tweets)
  
}

TweetFrame<-function(searchTerm, maxTweets, oldest=NULL)
{
  #twtList<-searchTwitter(searchTerm,n=maxTweets,cainfo="cacert.pem",lang="en")
  twtList<-searchTwitter(searchTerm,n=maxTweets,lang="en", since="2010-01-01", until=oldest, resultType="recent")
  
  twtList1<- do.call("rbind",lapply(twtList,as.data.frame))
  twtList1$text<-iconv(twtList1$text, 'UTF-8', 'ASCII') #WILL THIS SOLVE THE UTF ENCODING PROBLEM: http://lists.hexdump.org/pipermail/twitter-users-hexdump.org/2013-May/000335.html
  return(twtList1)
  
}

tf <- rbind(tf, TweetFrame(searchTerm, 100))

for i in 1:10{
oldest <-as.Date(min(tf$created))
  tf <- rbind(tf, TweetFrame(searchTerm, 100, oldest))
  
}

wordcloud<-function(cleantext)
{
  tweetCorpus<-Corpus(VectorSource(CleanTweets(entitycleantext)))
  tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                        stopwords=c(stopwords('english')),
                                                        removeNumbers=TRUE,tolower=TRUE))
  tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
  sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
  cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
  
  wcloud<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=100, colors=brewer.pal(8,"Dark2"),scale=c(8,1), random.order=TRUE)
  print(wcloud)
}






