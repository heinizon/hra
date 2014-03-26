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

install_load('shiny', 'xlsx', 'ggplot2', 'scales', 'psych', "plyr")

shinyServer(function(input, output) {
  output$model.output <- renderText({
      if (input$xlog==TRUE)
          x.label <- paste("log(x)")
      else
          x.label <- paste("x")
      
      if (input$ylog == TRUE)
          y.label <- paste("log(y)")
      else
          y.label <- paste("y")
      
      if (input$intercept == TRUE)
          intercept.label <- paste("b")
      else
          intercept.label <- paste("")
        
      modelstring <- paste(y.label,"=",x.label,intercept.label)
      return(paste("Current Model:", "\n", modelstring))
  })
  
  output$dat <- renderTable({
    infile <- input$datfiles
    if (is.null(infile)){
      #return(NULL)
      dat <- data.frame(Date=c("1/1/2014","1/2/2014","1/3/2014"), 
                        Gross.Media.Spend=c("$2589.14","$2177.04","$2165.78"),
                        Conversions=c(126,107,111))
      return(dat)
    }
      
    
    dat <- dataThreshold()
    dat
  }) #End data rendering for Data tab
  
  output$data.header <- renderText({
    infile <- input$datfiles
    if (is.null(infile)){
      return(paste("Example Data Below"))
    }
    return(paste("Uploaded Data Below"))
  })
  
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
      return(paste("No Data Has Been Uploaded"))
    
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
      return(paste("No Data Has Been Uploaded"))
    
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
    
    model <- modelhr(dat, input$ylog, input$xlog, input$intercept)
    
    df <- evalmodelhr(df, dat, input$ylog, input$xlog, input$intercept)
    df$goal <- effgoal
    df$cpa <- df$x / df$y
    df$goaldif <- abs(df$goal - df$cpa)
    mindif <- min(df$goaldif)
    outputx <- subset(df, goaldif == mindif, select = x)[1]
    outputy <- subset(df, goaldif == mindif, select = y)[1]

    
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
      return(paste("No Data Has Been Uploaded"))
    
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
      return(paste("No Data Has Been Uploaded"))
    
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

  #SummaryTabOutput
  output$summarytab <- renderTable({
    infile <- input$datfiles
    if (is.null(infile))
      return(data.frame(Warning=c("No Data Has Been Uploaded")))
    
    dat <- dataThreshold()
    if (dat$Gross.Media.Spend[1] == -99){
      return(dat)
    }
    
    output <- Calculate.Model.Outputs(dat)
    output

  })# end Summary Tab Output
  
  output$best.model.guess <- renderText({
    infile <- input$datfiles
    if (is.null(infile))
      return(data.frame(Warning=c("No Data Has Been Uploaded")))
    
    dat <- dataThreshold()
    if (dat$Gross.Media.Spend[1] == -99){
      return(paste("Data does not meet minimum observation threshold.",
                   "At least 14 days of data should be included"))
    }
    
    output <- Calculate.Model.Outputs(dat)
    
    output <- subset(output, output$Ftest.p.Value < .05)
    output$absmean <- abs(output$mean)
    minmean <- min(output$absmean)
    bestmodel <- row.names(subset(output, mean==minmean)[1])
    
    modelinputs <- data.frame(inputs=strsplit(bestmodel, "[.]"))
    colnames(modelinputs) <- c("inputs")

    if (nrow(modelinputs) == 3)
      output <- paste(modelinputs$inputs[1], "=",modelinputs$inputs[2])
    else
      output <-paste(modelinputs$inputs[1], "=", modelinputs$inputs[2],"+",modelinputs$inputs[3])
    
    output <- paste("The Best Fit Model was estimated to be \n", output)
    
#     mindif <- min(df$goaldif)
#     outputdf <- subset(df, goaldif == mindif, select = x)[1]
  })
  
Calculate.Model.Outputs <- function(dat){
  evalinput <- data.frame(x=dat$Gross.Media.Spend)
  evalresults <- subset(dat, select=c(Date, Gross.Media.Spend, Conversions))
  modelstats <- data.frame(stat=c("Adj.R.Sqrd","Ftest.p.Value"))
  
  #logy ~ logx + b   (y, x, b)
  evalresults$y <- evalmodelhr(evalinput, dat, TRUE, TRUE, TRUE)$y
  evalresults$logy.logx.b <- evalresults$y
  evalresults$logy.logx.b.resid <- evalresults$Conversions - evalresults$y
  model <- modelhr(dat, TRUE, TRUE, TRUE)
  modelstats$logy.logx.b.resid <- c(summary(model)$adj.r.squared, anova(model)$`Pr(>F)`[1])
  
  #logy ~ logx
  evalresults$y <- evalmodelhr(evalinput, dat, TRUE, TRUE, FALSE)$y
  evalresults$logy.logx <- evalresults$y
  evalresults$logy.logx.resid <- evalresults$Conversions - evalresults$y
  model <- modelhr(dat, TRUE, TRUE, FALSE)
  modelstats$logy.logx.resid <- c(summary(model)$adj.r.squared, anova(model)$`Pr(>F)`[1])
  
  #logy ~ x + b
  evalresults$y <- evalmodelhr(evalinput, dat, TRUE, FALSE, TRUE)$y
  evalresults$logy.x.b <- evalresults$y
  evalresults$logy.x.b.resid <- evalresults$Conversions - evalresults$y
  model <- modelhr(dat, TRUE, FALSE, TRUE)
  modelstats$logy.x.b.resid <- c(summary(model)$adj.r.squared, anova(model)$`Pr(>F)`[1])
  
  # logy ~ x
  evalresults$y <- evalmodelhr(evalinput, dat, TRUE, FALSE, FALSE)$y
  evalresults$logy.x <- evalresults$y
  evalresults$logy.x.resid <- evalresults$Conversions - evalresults$y
  model <- modelhr(dat, TRUE, FALSE, FALSE)
  modelstats$logy.x.resid <- c(summary(model)$adj.r.squared, anova(model)$`Pr(>F)`[1])
  
  #y ~ logx + b
  evalresults$y <- evalmodelhr(evalinput, dat, FALSE, TRUE, TRUE)$y
  evalresults$y.logx.b <- evalresults$y
  evalresults$y.logx.b.resid <- evalresults$Conversions - evalresults$y
  model <- modelhr(dat, FALSE, TRUE, TRUE)
  modelstats$y.logx.b.resid <- c(summary(model)$adj.r.squared, anova(model)$`Pr(>F)`[1])
  
  #y ~ logx
  evalresults$y <- evalmodelhr(evalinput, dat, FALSE, TRUE, FALSE)$y
  evalresults$y.logx <- evalresults$y
  evalresults$y.logx.resid <- evalresults$Conversions - evalresults$y
  model <- modelhr(dat, FALSE, TRUE, FALSE)
  modelstats$y.logx.resid <- c(summary(model)$adj.r.squared, anova(model)$`Pr(>F)`[1])
  
  #y ~ x + b
  evalresults$y <- evalmodelhr(evalinput, dat, FALSE, FALSE, TRUE)$y
  evalresults$y.x.b <- evalresults$y
  evalresults$y.x.b.resid <- evalresults$Conversions - evalresults$y
  model <- modelhr(dat, FALSE, FALSE, TRUE)
  modelstats$y.x.b.resid <- c(summary(model)$adj.r.squared, anova(model)$`Pr(>F)`[1])
  
  #y ~ x
  evalresults$y <- evalmodelhr(evalinput, dat, FALSE, FALSE, FALSE)$y
  evalresults$y.x <- evalresults$y
  evalresults$y.x.resid <- evalresults$Conversions - evalresults$y
  model <- modelhr(dat, FALSE, FALSE, FALSE)
  modelstats$y.x.resid <- c(summary(model)$adj.r.squared, anova(model)$`Pr(>F)`[1])
  
  evalresids <- subset(evalresults, select=c("logy.logx.b.resid", "logy.logx.resid",
                                             "logy.x.b.resid", "logy.x.resid", 
                                             "y.logx.b.resid", "y.logx.resid", "y.x.b.resid",
                                             "y.x.resid"))
  n <- modelstats$stat
  modelstats2 <- as.data.frame(t(modelstats[,-1]))
  colnames(modelstats2) <- n
  
  
  output <- data.frame(describe(evalresids, skew = FALSE, range=FALSE))
  output$Adj.R.Sqrd <- modelstats2$Adj.R.Sqrd
  output$Ftest.p.Value <- round(modelstats2$Ftest.p.Value,4)
  
  output
}  


  #evalmodelhr
  #returns a dataframe object with a column ("y")
    #containing the evaluated model
  #Input is a data frame containing a column "x", used to evaluate the y's
  evalmodelhr <- function(xdf, dat, ylog, xlog, intercept) {
      model <- modelhr(dat, ylog, xlog, intercept)
      df <- data.frame(xdf)
      
      
      if (intercept){
        slope <- data.frame(summary(model)$coef)$Estimate[2]
        intercept <- data.frame(summary(model)$coef)$Estimate[1]
      }
      else {
        slope <- data.frame(summary(model)$coef)$Estimate[1]
        intercept <- 0
      }
      
      if (xlog){
        if (ylog) {  
          #branch for log1p(y) ~ log1p(x)
          df$y <- ((df$x + 1)^slope) * exp(intercept) - 1
        }
        else {
          #branch for y~log1p(x)
          df$y <- (log1p(df$x) * slope) + intercept
        }
      }
      else if (ylog) {
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
    
    if (xlog)
      if (ylog)
        if (intercept)
          model <- lm(log1p(Conversions) ~ log1p(Gross.Media.Spend),
                      data = dat)
    else
      model <- lm(log1p(Conversions) ~ 0 + log1p(Gross.Media.Spend),
                  data = dat)
    else
      if (intercept)
        model <- lm(Conversions ~ log1p(Gross.Media.Spend), data = dat)
    else
      model <- lm(Conversions ~ 0 + log1p(Gross.Media.Spend), data = dat)
    else if (ylog)
      if(intercept)
        model <- lm(log1p(Conversions) ~ Gross.Media.Spend, data = dat)
    else
      model <- lm(log1p(Conversions) ~ 0 + Gross.Media.Spend, data = dat)
    else
      if (intercept)
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



