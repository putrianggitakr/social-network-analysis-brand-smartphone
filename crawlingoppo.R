#import library
library(rtweet)
library(dplyr)
library(tidyr)
library(NLP)
library(tm)
library(stringr)
library(caret)
#library(katadasaR)
library(tau)
library(parallel)
library(twitteR)
library(RCurl)
library(ggplot2)
library(SnowballC)
#library(Rstem)
library(RColorBrewer)
library(wordcloud)
library(tokenizers)

keyword <- "Oppo"
jumlahtweet <- 1000
bahasa <- "id"

token <- create_token(
  app = "SNA Brand HP",
  consumer_key = "XXXX",
  consumer_secret = "XXXX",
  access_token = "XXXX",
  access_secret = "XXXX")

crawling1 <- search_tweets(keyword,
                           n = jumlahtweet,
                           since="2022-04-01",
                           until="2022-04-11",
                           lang = bahasa,
                           retryonratelimit = FALSE)

View(crawling1)

#save sebagai csv
write_as_csv(crawling1, "data_mentah_oppo.csv",
             prepend_ids = TRUE, na="", fileEncoding = "UTF-8")

#mengimpor dataset dengan memilih langsung file menurut saya
crawling2 <-read.csv(file.choose(), header = TRUE)


#hanya melihat data text saja
crawling2<-crawling2[5]
View(crawling2)

#mengubah file ke dalam corpus
corpusdata <- Corpus(VectorSource(crawling2$'text'))
View(corpusdata)

#mengubah semua huruf kapital menjadi huruf kecil
data_casefolding <- tm_map(corpusdata,content_transformer(tolower))

#menghapus url pada dokumen
removeURL <- function(x)gsub("http[^[:space:]]*","",x)
data_URL <- tm_map(data_casefolding, content_transformer(removeURL))

#menghapus mention pada dokumen
remove.mention <- function(x)gsub("@\\S+","",x)
data_mention <- tm_map(data_URL,remove.mention)

#menghapus hastag
remove.hashtag <- function(x)gsub("#\\S+","",x)
data_hashtag <- tm_map(data_mention, remove.hashtag)

#menghapus NL
remove.NL <- function(x)gsub("\n","",x)
data_NL <- tm_map(data_hashtag,remove.NL)

#menghapus koma
replace.comma <- function(x)gsub(",","",x)
data_comma <- tm_map(data_NL,replace.comma)
#untuk melihat data yang sudah diedit
inspect (data_comma[1:100])

#menghapus RT
remove.RT <-function(x)gsub("RT","",x)
data_RT <- tm_map(data_comma,remove.RT)

#Menghapus titik 2
remove.titik2 <- function(x)gsub(":","",x)
data_titik2 <- tm_map(data_RT,remove.titik2)

#menghapus titik koma
remove.titikkomma <- function(x)gsub(";","",x)
data_titikkomma <- tm_map(data_titik2,remove.titikkomma)

#menghapus tanda baca
data_punctuation <- tm_map (data_titikkomma,content_transformer(removePunctuation))

#menghapus spasi berlebih
data_whitespace <- tm_map(data_punctuation, stripWhitespace)

#menyimpan data bersih
databersih<-data.frame(text=unlist(sapply(data_whitespace,'[')),stringsAsFactors = F)
write.csv(databersih,"data_bersih_oppo.csv")


