#This script contains the lightweight prediction algorithm for the shiny app
#This algorithm uses a preprocessed list of ngrams and tries to match the longuest
#sequence of the ngram to predict the last_word
#Long matched ngrams are given more weight than shorter ones in order to bias the algorithm
#towars words predicted based on longer ngrams

library(stringr)
library(tm)
library(SnowballC)
library(data.table)

ngrams<-NULL

initialization<-function(path="."){
  print("Initialization Begin")
  print("Load ngram Begin")
  ngrams<<-readRDS(file.path(path,"ngrams_app.rds"))
  print("Load ngram End")
  results_per_n_gram<-5

  #dict<-read.csv2("top5000words.csv")
  #Downloaded from http://www.wordfrequency.info/top5000.asp and converted to csv with excel
  
  #THis dictionnary will be used to destem the words
  print("Load Dictionnary Begin")
  dict0<-read.csv(file.path(path,"corpus_words.txt"),header=FALSE,col.names=c("Word"))
  #This dictionary set was produced from the corpus of data in the set
  
    dict1<-read.csv(file.path(path,"words.txt"),header=FALSE,col.names=c("Word"))
    
    #Merge the two dictionnaries
    dict<-data.frame(Word=character(0))
    dict<-unique(rbind(dict0,dict1))


  #Downloaded from https://github.com/dwyl/english-words
  # Direct URL: https://raw.githubusercontent.com/dwyl/english-words/master/words.txt
  

  print("Load Dictionnary End")

  
  print("Constructing dictionary corpus begin")
  #Remove small words (improves stem completion performance and accuracy)
  dict<-subset(dict,str_length(Word)>3)
  #Remove huge words (improves stem completion performance and accuracy)
  dict<-subset(dict,str_length(Word)<15)
  #Remove common names
  dict<-subset(dict,!(Word %in% c("elsa")))
  
  dict$Word<-dict$Word[order(dict$Word)]
  dictCorpus<<-Corpus(VectorSource(paste(dict$Word,collapse=" ")))
  #dictCorpus<<-Corpus(VectorSource(dict$Word))
  dictCorpus<<-tm_map(dictCorpus,removePunctuation)
  dictCorpus <<- tm_map(dictCorpus, removeWords, stopwords("english"))  
  
  print("Constructing dictionary corpus end")
  print("Initialization End")
}



searchNext<-function(phrase,num_results=5,verbose=TRUE,completeStem=TRUE){
  if (verbose) print(paste("Predict next word for ",phrase))
  if (phrase=="")
  {
    return(data.frame(last_word=c("Please","enter", "a", "phrase"),
                      weight=c(1,2,3,4)))
  }
  my_stopwords<-rbind(stopwords("english"),"id")
  searchCorpus<-Corpus(VectorSource(phrase))
  
  searchCorpus<-tm_map(searchCorpus,removeNumbers)
  searchCorpus<-tm_map(searchCorpus,removePunctuation)
  searchCorpus<-tm_map(searchCorpus,content_transformer(tolower))
  searchCorpus<-tm_map(searchCorpus,stripWhitespace)
  
  searchCorpus<-tm_map(searchCorpus, PlainTextDocument)
  searchCorpus <- tm_map(searchCorpus, removeWords, stopwords("english")) 
  searchCorpus<-tm_map(searchCorpus, stemDocument, language = "english")  
  searchCorpus <- tm_map(searchCorpus ,stripWhitespace)
  
  search_terms<-unlist(strsplit(unlist(trimws(searchCorpus[[1]]$content))," "))
  
  search_terms<-rev(search_terms)
  
  if (verbose) print(paste("Words to look for: ",paste(search_terms,collapse=", ")))
  
  i<-length(search_terms)
  
  #Look for 4-grams
  results_4_gram<-ngrams[0,]
  if(i>=4){
    results_4_gram<-head(ngrams[
      ngrams$word_1==search_terms[1] &
        ngrams$word_2==search_terms[2] &
        ngrams$word_3==search_terms[3] &
        ngrams$word_4==search_terms[4]
    ],num_results)
  }
  if (verbose) print(paste(length(results_4_gram$freq)," 5 grams results"))
  
  results_3_gram<-ngrams[0,]
  if(i>=3){
    results_3_gram<-head(ngrams[
      ngrams$word_1==search_terms[1] &
        ngrams$word_2==search_terms[2] &
        ngrams$word_3==search_terms[3] &
        ngrams$words==4
      ],num_results)
  }
  if (verbose) print(paste(length(results_3_gram$freq)," 4 grams results"))  
  
  results_2_gram<-ngrams[0,]
  if(i>=2){
    results_2_gram<-head(ngrams[
      ngrams$word_1==search_terms[1] &
        ngrams$word_2==search_terms[2] &
        ngrams$words==3
      ],num_results)
  }
  
  if (verbose) print(paste(length(results_2_gram$freq)," 3 grams results"))  
  
  results_1_gram<-head(ngrams[
    ngrams$word_1==search_terms[1] &
      ngrams$words==2
    ],num_results)
  
  if (verbose) print(paste(length(results_1_gram$freq)," 2 grams results")) 
  
  #In case empty results, return most frequent words
  if(length(results_1_gram$last_word)==0){
    if (verbose) print("No results found")
    results_1_gram<-head(ngrams[ngrams$words==1,],num_results)
  }
  results<-rbind(results_1_gram,results_2_gram,results_3_gram,results_4_gram)
  
  results$weight<-(results$freq)*(results$words) #Weight frequency by length of ngram
  
  results<-results[order(results$weight,decreasing=TRUE),]
  if (completeStem){
    if (verbose) print("Adding suffix")
    with_suffix<-stemCompletion(results$last_word,dictCorpus,type=c("first"))
    if(!is.na(with_suffix)){
      results$last_word<-with_suffix
    }
    #FIrst is usually the shortest
    if (verbose) print("Suffixes added")
  }
  return(results)
}

testQuizz<-function(x){
  
  phrase<-c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
            "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
            "I'd give anything to see arctic monkeys this",
            "I'd give anything to see this",
            "Talking to your mom has the same effect as a hug and helps reduce your",
            "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
            "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
            "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
            "Every inch of you is perfect from the bottom to the",
            "I'm thankful my childhood was filled with imagination and bruises from playing",
            "I like how the same people are in almost all of Adam Sandler's"
            )
  
  for(i in 1:length(phrase))
  {
    print(paste("Phrase: ",phrase[i]))
    print(searchNext(phrase[i]))
  }
}