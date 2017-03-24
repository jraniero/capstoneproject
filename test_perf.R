#This script purpose is to measure the performance of the prediction algorithm
#It uses the test test from the source corpus. For each line in the text files, it will
#verify if the prediction algorithm is able to predict the last word in each line
#The same processing as for creating the n-grams is used:
#Remove punctuation, lowercase, remove stopwords, stem words
#If the stem of the prediction matches the stem of the actual word it will count as a good prediction

library(stringr)
library(tm)
library(SnowballC)
library(data.table)

source("TextPredictor/predictor.R")

numFails<-0
numSuccess<-0

initialization("TextPredictor")
datapath<-file.path(".","final","en_US","test")

files<-list.files(path=datapath,pattern="*.txt")

removeURL <- function(x) gsub("http:[[:alnum:]]*", "", x)
removeHashTags <- function(x) gsub("#\\S+", "", x)
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)

results<-data.frame(target=character(0),predicted=character(0))

for(file in files){
  myLines<-readLines(file.path(datapath,file))
  
  for(myLine in myLines){
    #Script stoped at 4164 lines in first file
    myLine<-iconv(myLine, "latin1", "ASCII", sub="")
    #file="en_US.blogs.txt"
    print(myLine)
    myText<-Corpus(VectorSource(myLine))
    
    
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
    
    #Remove lastword
    
    words<-unlist(strsplit(unlist(trimws(myText[[1]]$content))," "))
    
    if (length(words)>1){
    
      searchWords<-paste(words[1:(length(words)-1)],collapse=" ")
      
      targetWord<-words[length(words)]
        
      #with_suffix<-stemCompletion(targetWord,dictCorpus,type=c("first"))
      
      #if(!is.na(with_suffix)){
       # targetWord<-with_suffix
      #}
      
      prediction_result<-searchNext(searchWords,n=1,verbose=FALSE,completeStem=FALSE)
      
      predicted<-as.character(prediction_result[1]$last_word)
      
      if(is.na(predicted)) predicted<-""
      
      print(paste("Actual Word:",targetWord,"predicted:",predicted))
      
      results<-rbind(results,data.frame(target=targetWord,predicted=predicted))
      
      if (predicted!=targetWord)
      {
        numFails<-numFails+1
      }
      else
      {
        numSuccess<-numSuccess+1
      }
    }
  }
}

