#import library
library(rtweet)
library(dplyr)
library(tidyr)
library(NLP)
library(tm)
library(stringr)
library(caret)
library(tau)
library(parallel)
library(twitteR)
library(RCurl)
library(ggplot2)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(tokenizers)
library(katadasaR)

#install library katadasaR
library(devtools)
install_github("nurandi/katadasaR")
library(katadasaR)
keyword <- "Oppo"
jumlahtweet <- 1000
bahasa <- "id"

token <- create_token(
  app = "SNA Smartphone",
  consumer_key = "agIvZr3RtN7gWA1HeVr2jpvio",
  consumer_secret = "qAZXAWU8l2RIBfbS2P9WGOlLn3p4Dv41UBABRabvUC08WzvEn5",
  access_token = "298884110-g8EhF1cMozLCj2Y7bWyMfU2XWOxl8dgryCwZKsv1",
  access_secret = "TOPRMjcaFGNiWn06a7baL7vvDq06ZrX0KZdTnYzxrNlBH")

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
inspect (data_casefolding[1:100])

#menghapus url pada dokumen
removeURL <- function(x)gsub("http[^[:space:]]*","",x)
data_URL <- tm_map(data_casefolding, content_transformer(removeURL))
inspect (data_URL[1:100])

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

#cleaning number
data_number <- tm_map(data_punctuation,content_transformer(removeNumbers))
inspect (data_number[1:100])

#menghapus spasi berlebih
data_whitespace <- tm_map(data_number, stripWhitespace)
inspect (data_whitespace[1:100])
#stemming
stem_text<-function(text,mc.cores=1)
{
  stem_string<-function(str)
  {str<-tokenize(x=str)
  str<-sapply(str,katadasaR)
  str<-paste(str,collapse = '')
  return(str)}
  x<-mclapply(X=text,FUN=stem_string,mc.cores = mc.cores)
  return(unlist(x))
  
}
data_stemming<-tm_map(data_whitespace,stem_text)
inspect (data_stemming[1:100])

#menghapus kata-kata tidak penting
cStopwordID<-readLines("E:/stopwordbahasa.csv")
data_stopword<-tm_map(data_stemming,removeWords,cStopwordID)
inspect (data_stopword[1:100])

data_stopword<-tm_map(data_stopword,removeWords,
                      c('wkwk','btw','pls','rt','gue','dll','spt',
                        'trs','bpk','ufrt','plis','ke','yg','knp',
                        'lho','wqwq','ufuffb','dong','gw','di'))
#menyimpan data bersih
databersih<-data.frame(text=unlist(sapply(data_stopword,'[')),stringsAsFactors = F)
write.csv(databersih,"databersihoppo.csv")

#mengimpor dataset dengan memilih langsung file
databersih1<-read.csv(file.choose(), header = TRUE)
View(databersih1)

#Tokenizing
corpustext<-Corpus(VectorSource(databersih1$text))
inspect(corpustext[1:10])
View(corpustext)

text=corpustext

strsplit_space_tokenizer<-function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(text)

#visualisasi wordcloud
text<-as.character(text)
wordcloud(text,max.words = 100,random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8,"Dark2"))

#term-weighting
tdm<-TermDocumentMatrix(corpustext)
inspect(tdm)



