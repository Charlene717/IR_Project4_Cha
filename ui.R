##### Load library ########
library(readr)
library(DT)
library(magrittr) 
library(ggplot2)
library(pdp)
library(shiny)
library(XML)
library(jsonlite)
library(tibble)
library(tidytext)

library(data.table)
library(dplyr) # For `filter_all` and `mutate_all`.
library(Hmisc)
library(quanteda)
library(stringr)

library(hcandersenr)
library(tidyverse)
library(tidytext)

library(SnowballC) # For wordStem

library(dynprog) # Dynamic Programming
library(Biostrings) # Dynamic Programming (pairwiseAlignment)

library(rword2vec)
library(Rtsne)

# Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
##### UI ########
ui =   fluidPage( 
  # https://stackoverflow.com/questions/57037758/r-shiny-how-to-color-margin-of-title-panel
  titlePanel(h1("IR Project 4",
                           style='background-color:#ece4db;  
                     color:#474973;
                     font-weight: 500;
                     font-family: Arial Black;
                     line-height: 1.2;
                     padding-left: 15px')),
  

## Summary
  tabsetPanel(
  tabPanel("Summary",  fluidPage(
    sidebarLayout(
      position = "right",
      sidebarPanel(
        fileInput("file1", "Choose XML Files", accept = ".xml", multiple = T),
        fileInput("file2", "Choose JSON File", accept = ".json", multiple = T),
        actionButton("SearchKW", "Go"),
        textInput("word_select", label = "Word to search")
        
      ),
      
      mainPanel(
        plotOutput("HisFig"),
        tableOutput("SumTable"))
    )
    
  )),
  
## Text search
  tabPanel("Text search",fluidPage(
    sidebarLayout(position = "right",
                  sidebarPanel(
                    
                    textInput("word_select4", label = "word to search","Cachexia"),
                    actionButton("SearchKW4", "Go")
                  ),
                  
    mainPanel(fluidRow(
    dataTableOutput("table")
    ))
  )
    
  )),

## Analysis
  navbarMenu("Analysis", 
    tabPanel("Word2Vector",    
      fluidPage(
      sidebarLayout(position = "right",
                    sidebarPanel(
                      
                      textInput("word_select2", label = "word to search","Cachexia"),
                      actionButton("SearchKW2", "Go")
                    ),
                    
                    mainPanel(tableOutput("W2VTable_SRP"))      
      ))
      ),
    tabPanel("W2V Dimension Reduction",
             
       fluidPage(
         sidebarLayout(position = "right",
                       sidebarPanel(
                         
                         textInput("word_select3", label = "word to search","Cachexia"),
                         actionButton("SearchKW3", "Go")
                       ),
                     mainPanel(plotOutput("W2V_DR"))
      )))
  )
)
)

# ##### UI ########
# ui = tagList(
#   # https://stackoverflow.com/questions/57037758/r-shiny-how-to-color-margin-of-title-panel
#   titlePanel(h1("IR Project 3",
#                 style='background-color:#ece4db;  
#                      color:#474973;
#                      font-weight: 500;
#                      font-family: Arial Black;
#                      line-height: 1.2;
#                      padding-left: 15px')), 
#   fluidPage(
#   sidebarLayout(
#     position = "right",
#     sidebarPanel(
#       fileInput("file1", "Choose XML Files", accept = ".xml", multiple = T),
#       fileInput("file2", "Choose JSON File", accept = ".json", multiple = T),
#       actionButton("SearchKW", "Go"),
#       textInput("word_select", label = "Word to search")
#       
#     ),
#     
#     mainPanel(
#       tableOutput("SumTable"),
#       plotOutput("HisFig"))
#   ),
#   fluidRow(
#     dataTableOutput("table")
#   )
# )
# )