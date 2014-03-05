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

install_load('shiny', 'xlsx', 'ggplot2', 'scales', 'psych')

shinyServer(function(input, output) {
  
  output$dat <- renderTable({
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)

    dat <- dataThreshold()
    
    dat
  }) #End data rendering for Data tab
  
  output$DataInputMessage <- renderText(paste("Your file should contain at least the following columns:",
        "Date, Gross Media Spend, Conversions", "\n",
        "Each day should appear only once in your data."))  #End of DataInputMessage
  

  
  output$DataSummary.UploadTab <- renderPrint({
    DataSummary()
  }) #end of DataSummary
  
  output$DataSummary.DataTab <- renderPrint({
    DataSummary()
  }) #end of DataSummary
  
  output$model <- renderPrint({
    infile <- input$datfiles
    if (is.null(infile))
      return(paste("No Data Has Been Uploaded"))
    
    dat <- dataThreshold()
    if (dat$Gross.Media.Spend[1] == -99){
      return(paste("Data does not meet minimum observation threshold.",
                      "At least 14 days of data should be included"))
    }
    
    model <- modelhr(dat, input$ylog, input$xlog, input$intercept)
    
    summary(model)
  }) #End of Model tab Model output
  
  output$thePlot <- renderPlot({
    infile <- input$datfiles
    if (is.null(infile))
      return(paste("No Data Has Been Uploaded"))
    
    dat <- read.xlsx(infile$datapath, 1)
    
    if (input$xlog)
      if (input$ylog)
        thePlot <- ggplot(dat, aes(x = log1p(Gross.Media.Spend), 
                                   y = log1p(Conversions))) +
      stat_smooth() +
      geom_point()
    else
      thePlot <- ggplot(dat, aes(x = log1p(Gross.Media.Spend), 
                                 y = Conversions)) +
      stat_smooth() +
      geom_point()
    else
      if (input$ylog)
        thePlot <- ggplot(dat, aes(x = Gross.Media.Spend,
                                   y = log1p(Conversions))) +
      stat_smooth() +
      geom_point()
    else
      thePlot <- ggplot(dat, aes(x = Gross.Media.Spend,
                                 y = Conversions)) +
      stat_smooth() +
      geom_point()
    
    print(thePlot)
  }) #End thePlot variable output for Model tab
  
  #Daily Goal Seek Output
  #outputs daily spend estimate to stay within specified CPA
  output$SpendHeadroom <- renderText({
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)
    
    effgoal <- input$goal
    if(is.null(effgoal))
      return(NULL)
    
    if(effgoal == 0)
      return(NULL)
    
    maxspendin <- input$maxspend
    if(maxspendin == 0)
      return(NULL)
    
    dat <- dataThreshold()
    if (dat$Gross.Media.Spend[1] == -99){
      return(paste("Data does not meet minimum observation threshold.",
                   "At least 14 days of data should be included"))
    }
    
    model <- modelhr(dat, input$ylog, input$xlog, input$intercept)
    
    
    if (input$intercept){
      slope <- data.frame(summary(model)$coef)$Estimate[2]
      intercept <- data.frame(summary(model)$coef)$Estimate[1]
    }
    else {
      slope <- data.frame(summary(model)$coef)$Estimate[1]
      intercept <- 0
    }
    
    xmin <- min(dat$Gross.Media.Spend)
    
    df <- data.frame(x=seq(xmin, input$maxspend, (input$maxspend - xmin)/10000))
    
    df <- evalmodelhr(df, dat, input$ylog, input$xlog, input$intercept)
    
    df$goal <- effgoal
    df$cpa <- df$x / df$y
    df$goaldif <- abs(df$goal - df$cpa)
    mindif <- min(df$goaldif)
    outputdf <- subset(df, goaldif == mindif, select = x)[1]
    outputy <- subset(df, goaldif == mindif, select = y)[1]
    outputy$y <- round(outputy$y)
    outputcpa <- subset(df, goaldif == mindif, select = cpa)[1]
    outputcpa$cpa <- round(outputcpa$cpa, digits=4)
    
    output <- paste("We can efficiently spend up to ", dollar(outputdf$x), "daily",
                    "\n", "Estimated Conv:", outputy,
                    "\n", "Estimated CPA:", outputcpa)
    
    f.pvalue <- anova(model)$`Pr(>F)`[1]
    rsq <- summary(model)$adj.r.squared
    
    if (f.pvalue > .05 | rsq < .7){
      output <- paste("WARNING:  Model is not statistically significant.", "\n", 
                      "F p-value:", round(f.pvalue,4), "\n", "Adj R Sqrd:", round(rsq,4), "\n", "\n",
                      output)
    }
    
    output
  }) #End Daily Goal Seek Output
  
  #GoalSeekFlight
  #Outputs text string containing conversions & spend
  #for a given flight period, assuming a CPA constraint
  output$GoalSeekFlight <- renderText({
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)
    
    effgoal <- input$flightgoal
    if(is.null(effgoal))
      return(NULL)
    
    if(effgoal == 0)
      return(NULL)
    
    dailybud <- input$maxspendflight
    if(is.null(dailybud))
      return(NULL)
    
    if(dailybud == 0)
      return(NULL)
    
    dat <- dataThreshold()
    if (dat$Gross.Media.Spend[1] == -99){
      return(paste("Data does not meet minimum observation threshold.",
                   "At least 14 days of data should be included"))
    }
    daysinflight <- input$goalenddate - input$goalstartdate + 1
  
    xmin <- min(dat$Gross.Media.Spend)
    
    df <- data.frame(x=seq(xmin, input$maxspendflight, (input$maxspendflight - xmin)/10000))
    
    df <- evalmodelhr(df, dat, input$ylog, input$xlog, input$intercept)
    df$goal <- effgoal
    df$cpa <- df$x / df$y
    df$goaldif <- abs(df$goal - df$cpa)
    mindif <- min(df$goaldif)
    outputx <- subset(df, goaldif == mindif, select = x)[1]
    outputy <- subset(df, goaldif == mindif, select = y)[1]
    #outputx <- outputdf$x[1]
    print(paste("Days:", daysinflight, "\n", outputx$x))
    outputy <- outputy * as.numeric(daysinflight)
    outputx <- outputx * as.numeric(daysinflight)
    outputy$y <- round(outputy$y)
    outputcpa <- subset(df, goaldif == mindif, select = cpa)[1]
    outputcpa$cpa <- round(outputcpa$cpa, digits=4)

    output <- paste("At can efficiently spend up to :", dollar(outputx$x),
                    "\n", "Estimated Conv:", outputy,
                    "\n", "Estimated CPA:", outputcpa)
    
    f.pvalue <- anova(model)$`Pr(>F)`[1]
    rsq <- summary(model)$adj.r.squared
    
    if (f.pvalue > .05 | rsq < .7){
      output <- paste("WARNING:  Model is not statistically significant.", "\n", 
                      "F p-value:", round(f.pvalue,4), "\n", "Adj R Sqrd:", round(rsq,4), "\n", "\n",
                      output)
    }
    
    output
  }) #End Flight Goal Seek output
  
  #BudgetSeekOutput
  #Outputs CPA & Conversions for a single day
  output$BudgetSeekOutput <- renderText({
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)
    
    dailybud <- input$dailybudget
    if(is.null(dailybud))
      return(NULL)
    
    if(dailybud == 0)
      return(NULL)
    
    dat <- dataThreshold()
    if (dat$Gross.Media.Spend[1] == -99){
      return(paste("Data does not meet minimum observation threshold.",
                   "At least 14 days of data should be included"))
    }
    
    model <- modelhr(dat, input$ylog, input$xlog, input$intercept)
    
    
    if (input$intercept){
      slope <- data.frame(summary(model)$coef)$Estimate[2]
      intercept <- data.frame(summary(model)$coef)$Estimate[1]
    }
    else {
      slope <- data.frame(summary(model)$coef)$Estimate[1]
      intercept <- 0
    }
    
    if (input$xlog){
      if (input$ylog) {  
        #branch for log1p(y) ~ log1p(x)
        dailybud <- dailybud + 1
        outputy <- ((dailybud)^slope) * exp(intercept) - 1
      }
      else {
        #branch for y~log1p(x)
        dailybud <- dailybud + 1
        outputy <- (log(dailybud) * slope) + intercept
      }
    }
    else if (input$ylog) {
      #branch for log1p(y) ~ x
      outputy <- exp((dailybud) * slope) * exp(intercept) - 1
    }
    else {
      #branch for y~x
      outputy <- ((dailybud) * slope) + intercept
    }
    
    outputcpa <- input$dailybudget/ outputy
    outputy <- round(outputy)
    outputcpa <- round(outputcpa, digits=4)
    
    output <- paste("We can efficiently spend up to ", dollar(input$dailybudget), "daily",
                    "\n", "Estimated Conv:", outputy,
                    "\n", "Estimated CPA:",outputcpa)
    f.pvalue <- anova(model)$`Pr(>F)`[1]
    rsq <- summary(model)$adj.r.squared
    
    if (f.pvalue > .05 | rsq < .7){
      output <- paste("WARNING:  Model is not statistically significant.", "\n", 
                      "F p-value:", round(f.pvalue,4), "\n", "Adj R Sqrd:", round(rsq,4), "\n", "\n",
                      output)
    }
    
    output
  })#End OutputBugetSeek output
  
  #BudgetSeekFlight
  #Outputs text string containing conversions & CPA
  #for remaining flight
  output$BudgetSeekFlight <- renderText({
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)
    
    dailybud <- input$flightbudget
    if(is.null(dailybud))
      return(NULL)
    
    if(dailybud == 0)
      return(NULL)

    dat <- dataThreshold()
    if (dat$Gross.Media.Spend[1] == -99){
      return(paste("Data does not meet minimum observation threshold.",
                   "At least 14 days of data should be included"))
    }
    daysinflight <- input$enddate - input$startdate + 1
    daysremaining <- input$enddate - Sys.Date() + 1
    
    dailyspend <- input$flightbudget / as.numeric(daysremaining)
    xdf <- data.frame(x=c(dailyspend))
    xdf <- evalmodelhr(xdf, dat, input$ylog, input$xlog, input$intercept)

    outputy <- xdf$y[1] * as.numeric(daysremaining)
    outputcpa <- input$flightbudget/ outputy
    outputy <- round(outputy)
    outputcpa <- round(outputcpa, digits=4)
    
    output <- paste("Budget Remaining: ", dollar(input$flightbudget), "for the flight",
                    "\n", "Estimated Conv for remaining flight:", outputy,
                    "\n", "Estimated CPA for remaining flight:",outputcpa)
    f.pvalue <- anova(model)$`Pr(>F)`[1]
    rsq <- summary(model)$adj.r.squared
    
    if (f.pvalue > .05 | rsq < .7){
      output <- paste("WARNING:  Model is not statistically significant.", "\n", 
                      "F p-value:", round(f.pvalue,4), "\n", "Adj R Sqrd:", round(rsq,4), "\n", "\n",
                      output)
    }
    
    output
  }) #End BudgetSeekFlight output


  
  #evalmodelhr
  #returns a dataframe object with a column ("y")
    #containing the evaluated model
  #Input is a data frame containing a column "x", used to evaluate the y's
  evalmodelhr <- function(xdf, dat, ylog, xlog, intercept) {
      model <- modelhr(dat, input$ylog, input$xlog, input$intercept)
      df <- data.frame(xdf)
      
      
      if (input$intercept){
        slope <- data.frame(summary(model)$coef)$Estimate[2]
        intercept <- data.frame(summary(model)$coef)$Estimate[1]
      }
      else {
        slope <- data.frame(summary(model)$coef)$Estimate[1]
        intercept <- 0
      }
      
      if (input$xlog){
        if (input$ylog) {  
          #branch for log1p(y) ~ log1p(x)
          df$y <- ((df$x + 1)^slope) * exp(intercept) - 1
        }
        else {
          #branch for y~log1p(x)
          df$y <- (log1p(df$x) * slope) + intercept
        }
      }
      else if (input$ylog) {
        #branch for log1p(y) ~ x
        df$y <- exp((df$x) * slope) * exp(intercept) - 1
      }
      else {
        #branch for y~x
        df$y <- ((df$x) * slope) + intercept
      }
      df
  } #end EvalModelHR Function
  
  
  #modelhr
  #returns a model object given a data input
  #and boolean objects to include y log, x log and intercept
  modelhr <- function(dat, ylog, xlog, intercept) {
    
    if (input$xlog)
      if (input$ylog)
        if (input$intercept)
          model <- lm(log1p(Conversions) ~ log1p(Gross.Media.Spend),
                      data = dat)
    else
      model <- lm(log1p(Conversions) ~ 0 + log1p(Gross.Media.Spend),
                  data = dat)
    else
      if (input$intercept)
        model <- lm(Conversions ~ log1p(Gross.Media.Spend), data = dat)
    else
      model <- lm(Conversions ~ 0 + log1p(Gross.Media.Spend), data = dat)
    else if (input$ylog)
      if(input$intercept)
        model <- lm(log1p(Conversions) ~ Gross.Media.Spend, data = dat)
    else
      model <- lm(log1p(Conversions) ~ 0 + Gross.Media.Spend, data = dat)
    else
      if (input$intercept)
        model <- lm(Conversions ~ Gross.Media.Spend, data = dat)
    else
      model <- lm(Conversions ~ 0 + Gross.Media.Spend, data = dat)
    model
  } #End ModelHR Function
  
  
  
  #dataThreshold
  #returns a dataframe containing imported data
  #will return -99 in 1st cell of column "Gross.Media.Spend" if there is not enough data
  #function discards days with Spend<10
  #and ensures more than 14 days of data are available
  dataThreshold <- function(){
    
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)
    
    dat <- read.xlsx(infile$datapath, 1)
    dat$Date <- as.Date(dat$Date, format= "%m/%d/%y",
                        origin = "1970-01-01")
    dat$Date <- paste0(dat$Date)
    dat$Spend <- dat$Gross.Media.Spend
    
    dat <- subset(dat, Gross.Media.Spend > 10)
    
    if (input$xlog)
      dat$Spend <-log1p(dat$Gross.Media.Spend)
    
    dat$Conv <- dat$Conversions
    if (input$ylog)
      dat$Conv <- log1p(dat$Conversions)
    
    if(nrow(dat) >= 14)
      dat
    else
      dat <- data.frame(Gross.Media.Spend=c(-99,paste('Does not meet minimum observations threshold.
                                      Need at least 14 days of data to perform analysis')))
    
    dat
  } # End DataThreshold function

#DataSummary function
#returns back dstats of the uploaded data
#or a text explanation of reason for failure
DataSummary <- function(){
  infile <- input$datfiles
  if (is.null(infile))
    return(paste("Data has not been Uploaded yet"))
  
  dat <- dataThreshold()
  if (dat$Gross.Media.Spend[1] == -99){
    return(paste("Data does not meet minimum observation threshold.",
                 "At least 14 days of data should be included"))}
  
  dat2 <- dat[,c("Date","Gross.Media.Spend","Conversions")]
  output <- describe(dat2, skew=FALSE)
  output
} #End DataSummary function
  
    
}) # end ShinyServer I/O
