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

install_load('shiny', 'xlsx', 'ggplot2', 'scales')

shinyServer(function(input, output) {
  
  output$dat <- renderTable({
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)
    
    dat <- read.xlsx(infile$datapath, 1)
    dat$Date <- as.Date(dat$Date, format= "%m/%d/%y",
                        origin = "1970-01-01")
    dat$Date <- paste0(dat$Date)
    dat$Spend <- dat$Gross.Media.Spend
    if (input$xlog)
      dat$Spend <-log1p(dat$Gross.Media.Spend)
    
    dat$Conv <- dat$Conversions
    if (input$ylog)
      dat$Conv <- log1p(dat$Conversions)
    
    dat
  })
  
  output$model <- renderPrint({
    infile <- input$datfiles
    if (is.null(infile))
      return(paste("No Data Has Been Uploaded"))
    
    dat <- read.xlsx(infile$datapath, 1)
    
    model <- modelhr(dat, input$ylog, input$xlog, input$intercept)
    
    summary(model)
  })
  
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
  })
  
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
    
    dat <- read.xlsx(infile$datapath, 1)
    dat$Date <- as.Date(dat$Date, format= "%m/%d/%y",
                        origin = "1970-01-01")
    dat$Date <- paste0(dat$Date)
    
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
    
    if (input$xlog){
      if (input$ylog) {  
        #branch for log1p(y) ~ log1p(x)
        df$x1 <- df$x + 1
        df$y <- ((df$x1)^slope) * exp(intercept) - 1
      }
      else {
        #branch for y~log1p(x)
        df$x1 <- df$x + 1
        df$y <- (log(df$x1) * slope) + intercept
      }
    }
    else if (input$ylog) {
      #branch for log1p(y) ~ x
      df$x1 <- df$x
      df$y <- exp((df$x1) * slope) * exp(intercept) - 1
    }
    else {
      #branch for y~x
      df$x1 <- df$x
      df$y <- ((df$x1) * slope) + intercept
    }
    
    df$goal <- effgoal
    df$cpa <- df$x / df$y
    df$goaldif <- abs(df$goal - df$cpa)
    mindif <- min(df$goaldif)
    outputdf <- subset(df, goaldif == mindif, select = x)[1]
    outputy <- subset(df, goaldif == mindif, select = y)[1]
    outputy$y <- round(outputy$y)
    outputcpa <- subset(df, goaldif == mindif, select = cpa)[1]
    outputcpa$cpa <- round(outputcpa$cpa, digits=4)
<<<<<<< HEAD
    
    output <- paste("We can efficiently spend up to ", dollar(outputdf$x), "daily",
                    "\n", "Estimated Conv:", outputy,
                    "\n", "Estimated CPA:",outputcpa)
    output
  })
  
  output$BudgetSeekOutput <- renderText({
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)
    
    dailybud <- input$dailybudget
    if(is.null(dailybud))
      return(NULL)
    
    if(dailybud == 0)
      return(NULL)
    
    dat <- read.xlsx(infile$datapath, 1)
    dat$Date <- as.Date(dat$Date, format= "%m/%d/%y",
                        origin = "1970-01-01")
    dat$Date <- paste0(dat$Date)
    
    model <- modelhr(dat, input$ylog, input$xlog, input$intercept)
    
    
    if (input$intercept){
      slope <- data.frame(summary(model)$coef)$Estimate[2]
      intercept <- data.frame(summary(model)$coef)$Estimate[1]
    }
    else {
      slope <- data.frame(summary(model)$coef)$Estimate[1]
      intercept <- 0
    }
    
=======
    
    output <- paste("We can efficiently spend up to ", dollar(outputdf$x), "daily",
                      "\n", "Estimated Conv:", outputy,
                      "\n", "Estimated CPA:",outputcpa)
    output
  })
  
  output$BudgetSeekOutput <- renderText({
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)
    
    dailybud <- input$dailybudget
    if(is.null(dailybud))
      return(NULL)
    
    if(dailybud == 0)
      return(NULL)
    
    dat <- read.xlsx(infile$datapath, 1)
    dat$Date <- as.Date(dat$Date, format= "%m/%d/%y",
                        origin = "1970-01-01")
    dat$Date <- paste0(dat$Date)
    
    model <- modelhr(dat, input$ylog, input$xlog, input$intercept)
    
    
    if (input$intercept){
      slope <- data.frame(summary(model)$coef)$Estimate[2]
      intercept <- data.frame(summary(model)$coef)$Estimate[1]
    }
    else {
      slope <- data.frame(summary(model)$coef)$Estimate[1]
      intercept <- 0
    }
    
>>>>>>> 99868d74a354b7bcf29de7ae2ee80b5ce70f3c12
    
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
    output
  })
  
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
  }
  
<<<<<<< HEAD
  dataThreshold <- function(dat){
    
    infile <- input$datfiles
    if (is.null(infile))
      return(NULL)
    
    dat <- read.xlsx(infile$datapath, 1)
    dat$Date <- as.Date(dat$Date, format= "%m/%d/%y",
                        origin = "1970-01-01")
    dat$Date <- paste0(dat$Date)
    dat$Spend <- dat$Gross.Media.Spend
    
    dat <- subset(dat, Gross.Media.Spend > 10)
    
    if(nrow(dat) >= 14)
      dat
    else
      paste('Does not meet minimum observations threshold')
  }
=======
>>>>>>> 99868d74a354b7bcf29de7ae2ee80b5ce70f3c12
  
})