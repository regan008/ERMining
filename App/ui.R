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



ui <- fluidPage(
  theme= shinytheme("cosmo"),
  
  headerPanel(HTML("Mining the Eleanor Roosevelt <i>My Day</i> Columns")),
  sidebarPanel(
    numericInput(inputId = "topic", "Select a Topic", 1, min = 0, max = 59, step = 1, width = NULL),
    numericInput(inputId = "word_num", "Select a Number of Words to Display", 50, min = 1, max = 50, step = 1, width = NULL),
    sliderInput(inputId = "number_of_topdocs", "Select the Number of Top Documents", 0, 100, 30, step = 1)
  ),
  mainPanel(
    h2(textOutput("TopWordsInTopicNumber")),
    plotOutput("topic_plot") ),
  fluidPage(
    fluidRow(
      column(6, 
             h2("Locations of Top Columns"),
             leafletOutput("doc_map")),
      
      column(6,
             h2("Yearly proportion of words in topic"),
             textOutput("topicCorpusWeight"),
             plotlyOutput("time_graph")))),
  fluidPage(fluidRow(column(width=12),
                     h2("Top Columns"),
                     tabsetPanel(
                    tabPanel("Top Columns in Topic", dataTableOutput("doc_topics")),
                    tabPanel("All Topics", dataTableOutput("topic_table")))))
)
