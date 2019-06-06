# twitteR scrip #
rm(list = ls()) 
if (!requireNamespace("base64enc"))
  install.packages("base64enc")
if (!requireNamespace("RCurl"))
  install.packages("RCurl")
if (!requireNamespace("ROAuth"))
  install.packages("ROAuth")
if (!requireNamespace("twitteR"))
  install.packages("twitteR")
library("base64enc")
library("twitteR")
library("ROAuth")
library("RCurl")

reqURL <- "http://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURP <- "http://api.twitter.com/oauth/authorize"

consumerKey <- "0hj2HIxjNZfVh5KxU84Sm6cp8"
consumerSecert <-
  "AolyqCnyMm8pXCaGpBXVzxW6cY77OxQWi23ym4QxAGvT3ttPdF"
accesstoken <- "518299811-pfAvgAV2tWGmQ1eEZyAcZ1SScS0DlRDXVUTQimFl"
accesstokensecret <- "r7hri2qlI3evKL5LS59k7zVTDxvbtBAV9sNFwLxg2iItV"

options(RCurlOptions = list(cainfo = system.file("CurlssL", "cacert.pem", package = "RCurl")))
download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

setup_twitter_oauth(consumerKey, consumerSecert, accesstoken, accesstokensecret)

n <- 2000
keyword <- '은퇴'
keyword <- enc2utf8(keyword)

rdmTweets <- searchTwitter(keyword, n)

nDcos <- length(rdmTweets)


library(KoNLP)
library(tm)

df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

removeTwit <- function(x) {
  gsub("@[[:graph:]]*", "", x)
}

df$ptext <- sapply(df$text, removeTwit)

removeURL1 <- function(x) {
  gsub("http://[[:graph:]]*", "", x)
}
df$ptext <- sapply(df$ptext, removeURL1)


removeURL2 <- function(x) {
  gsub("https://[[:graph:]]*", "", x)
}


df$ptext <- sapply(df$ptext, removeURL2)

useNIADic()

df$ptext <- sapply(df$ptext, function(x) {
  paste(extractNoun(x), collapse = " ")
})


myCorpus <- Corpus(VectorSource(df$ptext))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, tolower)
myStopwords <- c(stopwords("english"), "rt")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

myTdm <-
  TermDocumentMatrix(myCorpus, control = list(wordLengths = c(4, 10)))
Encoding(rownames(myTdm)) <- "UTF-8"

findFreqTerms(myTdm, lowfreq = 50)

findAssocs(myTdm, "", 0.25)

myTdm2 <- removeSparseTerms(myTdm, sparse = 0.95)
m2 <- as.matrix(myTdm2)

distMatrix <- dist(scale(m2))

fit <- hclust(distMatrix, method = "single")

plot(fit)

rect.hclust(fit, k = 10)