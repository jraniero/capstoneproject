# This script will create the dictionnary used by the shiny app
# For that, the sampled text (see Report.Rmd) is used to create the occurrences of n-grams
# As the script takes considerable time to process, intermediate results are saved
# This allows for resuming the process should it be necessary

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

#Preprocess text
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

#Calculate from 1-gram to 5-grams
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

#Aggregate the results of the different files
freq_agg<-aggregate(frequency~.,data=freq,FUN=sum)

saveRDS(freq_agg,"ngrams_agg.rds")

#freq_agg<-readRDS("ngrams_agg.rds")

#Reverse the order of words in each phrase
freq_agg$word<-unlist(lapply(lapply(strsplit(as.character(freq_agg$word)," "),rev),paste,collapse=" "))

saveRDS(freq_agg,"ngrams_agg_rev.rds")

rm(list=ls()) #Cleanup

freq_clean<-data.table(readRDS("ngrams_agg_rev.rds"))

#Write to csv for cleanup with unix tools
write.csv(freq_clean,"ngram.csv",row.names=FALSE)

#Add necessary empty words for 3, 4 and 5-grams
system("cat ngram.csv | sed -e 's/ /\",\"/g' | sed -e 's/\",1/\",\"\",\"\",\"\",\"\",1/' | sed -e 's/\",2/\",\"\",\"\",\"\",2/' | sed -e 's/\",3/\",\"\",\"\",3/' | sed -e 's/\",4/\",\"\",4/' > ngram_words.csv") #Split into a columnd per word

ngrams<-data.table(read.csv("ngram_words.csv",skip=1,col.names=c("last_word","word_1","word_2","word_3","word_4","words","freq")))

#Order in decreasing frequency and number of words order
ngrams<-ngrams[order(-words,-freq)]

saveRDS(ngrams,"ngrams_final.rds")

#Create smaller version for the shiny app
#By just removing the items appearing only once
ngrams_app<-ngrams[ngrams$freq>=2,]

saveRDS(ngrams_app,"ngrams_app.rds")
