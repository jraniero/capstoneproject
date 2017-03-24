datapath<-file.path("..","final","en_US")
skipLines<-5 #For the training set we took 1 line out of 5.We will take the next line for testing
files<-list.files(path=datapath,pattern="*.txt")

for(file in files){
  command<-paste("awk 'NR == 2 || NR % ",skipLines," == 1' ",datapath,"/",file," >",datapath,"/test/",file,sep="")
  print(command)
  system(command)
}
