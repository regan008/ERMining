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
  theme= shinytheme("readable"),
  
  titlePanel(HTML("Mining the Eleanor Roosevelt <i>My Day</i> Columns")),
  fluidRow(
    column(4, wellPanel(
    includeHTML("about.html"),
    numericInput(inputId = "topic", "Select a Topic (0 through 59)", 1, min = 0, max = 59, step = 1, width = NULL),
    numericInput(inputId = "word_num", "Select a Number of Words to Display", 50, min = 1, max = 50, step = 1, width = NULL),
    sliderInput(inputId = "number_of_topdocs", "Select the Number of Top Documents", 0, 100, 30, step = 1),
    includeHTML("explanation.html")
  )),
  column(8,
    h2(textOutput("TopWordsInTopicNumber")),
    plotOutput("topic_plot"))),
  fluidRow(
    column(5,
    h2("Locations of Top Columns"),
    leafletOutput("doc_map")),
    column(7,
           h2("Yearly Proportion of Words in Topic"),
             textOutput("topicCorpusWeight"),
             plotlyOutput("time_graph", width="100%", height="100%", inline=TRUE))
    ),
  fluidRow(column(12,
                     h2("Top Columns"),
                     tabsetPanel(
                    tabPanel("Top Columns in Topic", dataTableOutput("doc_topics")),
                    tabPanel("All Topics", dataTableOutput("topic_table"))))
)
)