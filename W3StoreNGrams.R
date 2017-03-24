knitr::opts_chunk$set(echo = TRUE)
library(stringr)
library(tm)
library(SnowballC)
library(data.table)

datapath<-file.path(".","final","en_US")

datapath<-file.path(".","final","en_US","sampled")

removeURL <- function(x) gsub("http:[[:alnum:]]*", "", x)
removeHashTags <- function(x) gsub("#\\S+", "", x)
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)

#file="en_US.blogs.txt"
myText<-Corpus(DirSource(datapath,pattern="*"))


myText <- tm_map(myText, removeNumbers)
myText<-tm_map(myText,content_transformer(removeURL))
myText<-tm_map(myText,content_transformer(removeHashTags))
myText<-tm_map(myText,content_transformer(removeTwitterHandles))
myText <- tm_map(myText, removePunctuation)
myText <- tm_map(myText, content_transformer(tolower))


myText<-tm_map(myText, PlainTextDocument)
myText <- tm_map(myText, removeWords, stopwords("english")) 
myText<-tm_map(myText, stemDocument, language = "english")  
myText <- tm_map(myText ,stripWhitespace)


freq<-data.frame(word=character(0),frequency=character(0),words=numeric(0))
for(ngrams in c(1:5)){
  print(paste("Doing ",ngrams," ngrams"))
for(i in c(1:3)){
  print(paste("Doing file ",i))
  myTextDtm<-DocumentTermMatrix(myText[i],
                                control=list(tokenizer=function(x){
                                  unlist(lapply(ngrams(words(x), ngrams), paste, collapse = " "), use.names = FALSE)
                                }))
  freq<-rbind(freq,data.frame(word=myTextDtm$dimnames$Terms,frequency=myTextDtm$v,words=ngrams))
}
}

saveRDS(freq,"ngrams.rds") #Save intermediate result

#freq<-readRDS("ngrams.rds")

freq_agg<-aggregate(frequency~.,data=freq,FUN=sum)

saveRDS(freq_agg,"ngrams_agg.rds")

#freq_agg<-readRDS("ngrams_agg.rds")

freq_agg$word<-unlist(lapply(lapply(strsplit(as.character(freq_agg$word)," "),rev),paste,collapse=" "))#Reverse order of words in each phrase

saveRDS(freq_agg,"ngrams_agg_rev.rds")

rm(list=ls()) #Cleanup

freq_clean<-data.table(readRDS("ngrams_agg_rev.rds"))

write.csv(freq_clean,"ngram.csv",row.names=FALSE)

system("cat ngram.csv | sed -e 's/ /\",\"/g' | sed -e 's/\",1/\",\"\",\"\",\"\",\"\",1/' | sed -e 's/\",2/\",\"\",\"\",\"\",2/' | sed -e 's/\",3/\",\"\",\"\",3/' | sed -e 's/\",4/\",\"\",4/' > ngram_words.csv") #Split into a columnd per word

ngrams<-data.table(read.csv("ngram_words.csv",skip=1,col.names=c("last_word","word_1","word_2","word_3","word_4","words","freq")))

ngrams<-ngrams[order(-words,-freq)]

saveRDS(ngrams,"ngrams_final.rds")

#Create smaller version for the shiny app
ngrams_app<-ngrams[ngrams$freq>=2,]

saveRDS(ngrams_app,"ngrams_app.rds")
