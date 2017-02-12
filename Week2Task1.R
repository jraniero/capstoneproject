library(stringr)
datapath<-file.path(".","final","en_US")

files<-list.files(path=datapath,pattern="*.txt")

sampleLines<-1000
skipLines<-100
word_freq<-data.frame(word=character(0),count=numeric(0))
word_vect<-rep("",10000)
word_count<-rep(0,10000)

for(file in files){
  print(file)
  con <- file(file.path(datapath,file), "r") 
  #docs <- Corpus(DirSource(datapath)) 
  #summary(docs)   
  for(i in c(1:sampleLines)){
    
    myLine<- readLines(con, 1) ## Read the first line of text
    words<-strsplit(myLine,split=" ") #Split into words
    words2<-str_split(myLine," ")
    #print(unlist(words))
    for(word_t in unlist(words))
    {#print(word_t)
      word_t<-gsub("[,|.|!|?]","",word_t) #remove punctuation characters
      word_t<-tolower(word_t)
      searchIdx<-match(word_t,word_vect)
      
      if (is.na(searchIdx)){
        #Assign to first empty
        searchIdx<-match("",word_vect)
        word_vect[searchIdx]=word_t
      }
        word_count[searchIdx]=word_count[searchIdx]+1
        #word_freq$count[searchIdx]<-word_freq$count[searchIdx]+1
       #print("**")
      
    }
    #print(words)
    #print(regmatches(myLine,regexpr("([^\s]+)",myLine)))

  }
  close(con)
}
#Order decreasing
word_freq<-data.frame(word=word_vect,count=word_count)
word_freq<-word_freq[order(word_freq$count,decreasing=TRUE),]