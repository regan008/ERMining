library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(magrittr)
library(wordcloud)
library(ggplot2)
library(plotly)
library(scales)
library(leaflet)
library(tidyr)
library(stringr)


#the directory path will vary based on machine

topicwordsweightresult <- readRDS("data/topic_words_weight.rds")
tm_values <- readRDS("data/tm_values.rds")
topic_keys <- readRDS("data/topickeys.rds")

server <- function(input, output) {
  
  
  
  output$topicCorpusWeight <- renderText({ 
    topicweight <- topic_keys %>% filter(V1 == input$topic) 
    topicweight <- topicweight$V2
    paste(topicweight, "% of the corpus in total.")
  })
  output$TopWordsInTopicNumber <- renderText({ 
    paste("Top Words in Topic #",input$topic)
  })
  
  
  ##output topicwordcloud
  output$topic_plot <- renderPlot({  
    exploretopicwords <- function(topicnum, numwords) {
      require(magrittr)
      require(wordcloud)
      require(dplyr)
      topicwords <- topicwordsweightresult %>% filter(V1 == topicnum) %>% arrange(desc(V3))
      topicwords$V1 <- NULL
      topicwords <- topicwords[1:numwords,]
      wc <- wordcloud(topicwords$V2, topicwords$V3,scale=c(5,1.5), colors=brewer.pal(8, "Dark2"), random.order=FALSE)
      return(wc)  }
    exploretopicwords(input$topic, input$word_num)
  })
  ##output topics over time
  output$time_graph <- renderPlotly({
    exploretopics_byyear <- function(topic) {
      byyear <- ddply(tm_values, .(topic_number, year), summarise, mean_value = mean(document_weight) * 100)
      
      filterbytopicnum <- byyear %>% filter(topic_number == topic)
      tby_plot <- ggplot(filterbytopicnum, aes(year, mean_value)) + geom_bar(stat="identity") + scale_x_continuous(name="Year", breaks=seq(1935, 1962, 2)) + scale_y_continuous(limits=c(0,5))
      tby_plot    }
    exploretopics_byyear(input$topic)
  })
  
  output$doc_topics <- renderDataTable({
    doc_topic_assoc <- function(topic) {
      
      
      bytopic <- tm_values %>% filter(topic_number == topic) %>% arrange(desc(document_weight)) 
      bytopic <- bytopic %>% select(-topic_number, -longitude, -latitude, -year)
      #bytopic$value <- bytopic$value 
      head(bytopic, n=30)
    }
    doc_topic_assoc(input$topic)
  }, escape=FALSE)
  output$topic_table <- renderDataTable({
    topic_keys <- plyr::rename(topic_keys, c("V1"="Topic", "V2"="V2","V3"="Top Words in Topic")) %>% select(-V2)
    topic_keys
  }, escape=FALSE)
  
  output$doc_map <- renderLeaflet({
    tm_1 <- tm_values %>%
      filter(topic_number == input$topic) %>%
      arrange(desc(document_weight)) 
    tm_1 <- tm_1[1:input$number_of_topdocs,]
    
    pal <- colorFactor(c("navy", "red", "green"), domain = c("1", "2", "3"))
    
    leaflet(tm_1) %>%
      addTiles() %>%
      addCircleMarkers(radius = ~(document_weight) *50,
                       color = ~pal(topic_number),
                       stroke = FALSE,
                       label = ~paste("Topic Number = ", input$topic, " | ", "Percent = ", str_sub(document_weight, 1, 4), "%", " | ", column_date, " | ", "Place Name: ", Place_Name, sep = ""),
                       fillOpacity = 0.3) %>%
      addLegend("topright", pal = pal, values = paste("Topic #", input$topic))
  })
  
}

