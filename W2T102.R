datapath<-file.path(".","final","en_US")

files<-list.files(path=datapath,pattern="*.txt")

sampleLines<-50000
skipLines<-100
word_freq<-data.frame(word=character(0),count=numeric(0))
word_vect<-new.env()

num_word<-0

for(file in files){
  con <- file(file.path(datapath,file), "r") 
  #docs <- Corpus(DirSource(datapath)) 
  #summary(docs)   
  print(paste(Sys.time(),file,sep=" "))
  myLine<- readLines(con, sampleLines) 
  print(paste(Sys.time(),"Read",length(myLine),"lines",sep=" "))
  while(length(myLine)>1){
    #for(myLine in myLines){
    myLine<-gsub("[-|(|)]"," ",myLine) #Replace some characters by space for further word identification
    words<-strsplit(myLine,split=" ") #Split into words
    #print(words)
    #print(unlist(words))
    for(word_t in unlist(words))
    {#print(word_t)
      #word_t<-gsub("[,|.|!|?]","",word_t) #remove punctuation characters
      #word_t<-str_replace(word_t,"[,|.|!|?]","")
      word_t<-tolower(word_t)
      #searchIdx<-match(word_t,names(word_vect))
      #print(length(word_t))
      if (word_t!=""){
        #print(word_t)
      if (is.null(get0(word_t,envir=word_vect))){
        word_vect[[word_t]]<-1
        #print(word_t)
      }
      else{
       
      word_vect[[word_t]]<-word_vect[[word_t]]+1
      #word_freq$count[searchIdx]<-word_freq$count[searchIdx]+1
      #print("**")
      
      }
    }
    #print(words)
    #print(regmatches(myLine,regexpr("([^\s]+)",myLine)))
    
  }
    #}
    myLine<- readLines(con, sampleLines) 
    print(paste(Sys.time(),"Read",length(myLine),"lines",sep=" "))
  }
  close(con)
}
#Order decreasing
word_freq<-unlist(mget(ls(word_vect),env=word_vect))
word_freq<-data.frame(word=names(word_freq),count=word_freq)
word_freq<-word_freq[order(word_freq$count,decreasing=TRUE),]
print(paste(Sys.time(),"Done",sep=" "))