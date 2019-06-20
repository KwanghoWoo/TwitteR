# Making wordcloud with R #

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
library("KoNLP")
library("tm")

reqURL <- "http://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURP <- "http://api.twitter.com/oauth/authorize"

consumerKey <- 
  "0hj2HIxjNZfVh5KxU84Sm6cp8"
consumerSecert <-
  "AolyqCnyMm8pXCaGpBXVzxW6cY77OxQWi23ym4QxAGvT3ttPdF"
accesstoken <- 
  "518299811-pfAvgAV2tWGmQ1eEZyAcZ1SScS0DlRDXVUTQimFl"
accesstokensecret <- 
  "r7hri2qlI3evKL5LS59k7zVTDxvbtBAV9sNFwLxg2iItV"

options(RCurlOptions = list(cainfo = system.file("CurlssL", "cacert.pem", package = "RCurl")))
download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
setup_twitter_oauth(consumerKey, consumerSecert, accesstoken, accesstokensecret)

n <- 2000
keyword <- '근로시간'
#keyword <- enc2utf8(keyword)
# option; language, date, altitude & latitude 
tweets.wh<-searchTwitter(keyword,n=2000, lang="ko", 
                         since ="2018-7-01", 
                         until="2019-06-20", 
                         geocode='37.566535,126.977969, 500km')

is.data.frame(tweets.wh) #데이터 프레임 여부 체크 
tweets.wh.df<-twListToDF(tweets.wh) #트윗 리스트 - 데이터 프레임 변환
is.data.frame(tweets.wh.df) 
names(tweets.wh.df) #트윗 변수 정보 출력 
head(tweets.wh.df,3) #트윗 데이터 첫 3개 출력 

tweets.text<-tweets.wh.df$text #분석 대상 트윗 내용 

# 불필요한 문자를 필터링
tweets.text <- gsub("\n", "", tweets.text)
tweets.text <- gsub("\r", "", tweets.text)
tweets.text <- gsub("RT", "", tweets.text) 
tweets.text <- gsub("H3", "", tweets.text) 
tweets.text <- gsub("h3", "", tweets.text)
tweets.text <- gsub("http","", tweets.text)

# Apply extract Noun. : koNLP(Korean Natural Lanaguage Processing) 
library(rJava)
library(KoNLP)
library(plyr)
useSejongDic() #사용할 사전 설정

# 문장에서 단어(명사) 분리 - Map 이용 
tweets.nouns<- Map(extractNoun, tweets.text) 
head(tweets.nouns,1) 
tweets.word<- unlist(tweets.nouns, use.name=F) 
head(tweets.word,5)

#sapply 함수 이용 
txt.nouns<- sapply(tweets.text,extractNoun,USE.NAMES = F) 
head(txt.nouns,1)
txt.word<- unlist(txt.nouns)
head(txt.word,5)

# 워드 클라우드 사용하지 않은 단어 제거
tweets.word<- gsub("[[:punct:]]","", tweets.word)
tweets.word<- gsub("[0-9]+[A-Za-z]", "", tweets.word) #숫자+알파벳 제거 
tweets.word<- gsub("남북회담", "", tweets.word) #빈도 분석 후 불필요 단어 제거 
tweets.word<- gsub("문재", "", tweets.word)
tweets.word<- gsub("남북", "", tweets.word)
tweets.word<- gsub("회담", "", tweets.word)
tweets.word<- gsub("co", "", tweets.word)
#단어 길이 2개 이상 선택
tweets.word<- Filter(function(x){nchar(x)>=2}, tweets.word)


# 단어별 카운팅, 상위 10개 단어 선택 
tweets.count<- table(tweets.word) 
head(sort(tweets.count, decreasing=T), 10)

library(RColorBrewer)
library(wordcloud)
pal <- brewer.pal(7,"Paired") # 컬러 세팅 
wordcloud(names(tweets.count),freq=tweets.count,scale=c(4,0.5),min.freq=5,random.order=F,rot.per=.1,colors=pal,family='AppleGothic')

