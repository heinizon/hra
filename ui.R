install_load <- function (package1, ...) 
{
  # convert arguments to vector
  packages <- c(package1, ...)
  
  for(package in packages){
    
    # if packages exists, load into environment
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package)) 
    
    # if package does not exist, download, and then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
    
  }
  
}


install_load('shiny')

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
      tabPanel("Upload", 
               fileInput("datfiles", "Pick your data file:"),
               verbatimTextOutput("DataInputMessage"),
               verbatimTextOutput('DataSummary.UploadTab')
      ),
      
      tabPanel("Data", verbatimTextOutput('DataSummary.DataTab'), tableOutput("dat")),
      tabPanel('Model', 
               plotOutput('thePlot'),
               verbatimTextOutput('model')),
      tabPanel("Daily Goal Seek",
               numericInput("goal", "What is your CPA Goal?", 0, 0, 10000),
               numericInput("maxspend", "What is your maximum daily spend?", 0, 0, 10000000),
               verbatimTextOutput("SpendHeadroom")
      ), #End DailyGoalSeek tab
      
      tabPanel("Flight Goal Seek", 
               h4("Flight Estimates"),
               dateInput("goalstartdate", "Start Date", value= "2014-1-1", min = "2010-1-1", max = "2020-12-31",
                         format = "mm/dd/yyyy"),
               dateInput("goalenddate", "End Date", value= "2014-3-31", min = "2010-1-1", max = "2020-12-31",
                         format = "mm/dd/yyyy"),
               
               numericInput("flightgoal", "What is the desired CPA for the remaining flight?",0,0,1000000000),
               numericInput("maxspendflight", "What is your maximum daily spend?",0,0,1000000000),
               verbatimTextOutput("GoalSeekFlight")
      ), # End GoalSeek Flight Tab
      #Budget Seek  Tab
      tabPanel("Daily Budget Seek",
              h4("Daily Estimates"),
              numericInput("dailybudget", "What is your Daily Spend Limit?",0,0,1000000000),
              verbatimTextOutput("BudgetSeekOutput")
               
      ), #End Daily BudgetSeek Tab
      tabPanel("Flight Budget Seek", 
               h4("Flight Estimates"),
               dateInput("startdate", "Start Date", value= "2014-1-1", min = "2010-1-1", max = "2020-12-31",
                         format = "mm/dd/yyyy"),
               dateInput("enddate", "End Date", value= "2014-3-31", min = "2010-1-1", max = "2020-12-31",
                         format = "mm/dd/yyyy"),
               
               numericInput("flightbudget", "What is your Remaining Budget with Incremental?",0,0,1000000000),
               verbatimTextOutput("BudgetSeekFlight")
      ) # End BudgetSeek Flight Tab
      
    ) #End TabSetPanel
  )  #End MainPanel
))