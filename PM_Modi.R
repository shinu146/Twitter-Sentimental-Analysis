library(ROAuth)
library(twitteR)
install.packages("RCurl", repos = "http://cran.us.r-project.org")
install.packages("httr", repos = "http://cran.us.r-project.org")
install.packages("syuzhet", repos = "http://cran.us.r-project.org")
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
library(dplyr)
library(RSentiment)
nstall.packages('XML', dependencies=T);
install.packages('plyr', dependencies=T);
install.packages('doBy', dependencies=T);
install.packages('tm', dependencies=T);
install.packages('RJSONIO', dependencies=T)
install.packages('RWeka')
install.packages('base64enc')


library(doBy)
library(RJSONIO)
library(tm)
library(RWeka)

consumer_key <-"lAn5g5m6wOuhWBqyDP6woludH"
consumer_secret <- "GAg27sMjz7MKLxSfShsVLMe4SVA5FLswkc5GZUFra5vCvjZxLU"
access_token<-"1030016266852098048-MEZNa6pA6Fm8iQKmJMfAiicaCNqpts"
access_secret <- "esbqHJ8wBXd6ikTy90Dgw2xt5tm2DtD65Pf6hzGNAzd0O"

setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )
tweets = searchTwitter("#PMModi", n = 1000, lang = "en")
# store the tweets into dataframe
tweets.df = twListToDF(tweets)


tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)
tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")



# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(tweets.df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])


# Visualize the emotions from NRC sentiments
library(plotly)
plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #PMModi")
word.df <- as.vector(tweets.df$text)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets.df, emotion.df) 
head(emotion.df2)
sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.negative <- word.df[sent.value <= min(sent.value)] 
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
feelings <- cbind(tweets.df$text,category_senti,sent.value)
feelings<-as.data.frame(feelings)
plot(table(feelings$category_senti))


###word cloud

tweets_text = sapply(tweets, function(x) x$getText())
df <- do.call("rbind", lapply(tweets, as.data.frame))
tweets_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
str(tweets_text)

#corpus is a collection of text documents
library(tm)
quake_corpus <- Corpus(VectorSource(tweets_text))
quake_corpus
inspect(quake_corpus[1])
#clean text
quake_clean <- tm_map(quake_corpus, removePunctuation)

quake_clean <- tm_map(quake_clean, content_transformer(tolower))
quake_clean <- tm_map(quake_clean, removeWords, stopwords("english"))
quake_clean <- tm_map(quake_clean, removeNumbers)
quake_clean <- tm_map(quake_clean, stripWhitespace)
quake_clean <- tm_map(quake_clean, removeWords, c("Italy","earthquake"))#removing search words
#wordcloud
#wordcloud(quake_clean)
#wordcloud(quake_clean, random.order=F)
#wordcloud(quake_clean, random.order=F, scale=c(4,0.5))#max font size,min font size
#wordcloud(quake_clean, random.order=F,col=rainbow(50), scale=c(4,0.5))
wordcloud(quake_clean,random.color=T, random.order=F,max.words=45, col=rainbow(40), scale=c(4,0.5))
