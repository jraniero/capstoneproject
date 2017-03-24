#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)

source("predictor.R") #Load text predicting algorithm
initialization() #Function in predictor.R to load the dictionnaries and the ngrams

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  phrase<-reactive({return (input$phrase)})
  
  predictions<-reactive({

    setProgress(message = "Searching terms...")
    results<-searchNext(phrase(),20)
    results<-aggregate(weight~last_word,data=results,FUN=sum)
    results<-results[order(results$weight,decreasing=TRUE),]
    return (results)})
   
  #predictions<-terms()
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$distPlot <-renderPlot({
       wordcloud_rep(predictions()$last_word, 
                     predictions()$weight, scale=c(4,0.5),
                  min.freq = 1, max.words=100,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  output$prediction<-renderText(head(predictions()$last_word,10))
  
  output$histPlot<-renderPlot({
    barplot(names=predictions()$last_word,
            height=predictions()$weight,
            horiz=TRUE,
            las=1)
  })
    
  })
  
