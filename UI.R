library(shiny)
library(rCharts)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Headroom Analysis"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    
    checkboxInput("intercept", "Non-Zero Intercept", F),
    checkboxInput("xlog", "log(x)", T),
    checkboxInput("ylog", "log(y)", T)
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Upload/Output", 
               fileInput("datfiles", "Pick your data file:")
      ),
      
      tabPanel("Data", tableOutput("dat")),
      tabPanel('Model', 
               plotOutput('thePlot'),
               verbatimTextOutput('model')),
      tabPanel("Goal Seek",
               numericInput("goal", "What is your CPA Goal?", 0, 0, 10000),
               numericInput("maxspend", "What is your maximum daily spend?", 0, 0, 10000000),
               verbatimTextOutput("SpendHeadroom")
      )
      
    )
  )
))
