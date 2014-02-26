library(shiny)
library(datasets)
library(xlsx)
library(ggplot2)
library(scales)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))



# Define server logic required to plot various variables against mpg
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
    
#     dat$Spend <- dat$Gross.Media.Spend
#     if (input$xlog)
#       dat$Spend <-log1p(dat$Gross.Media.Spend)
#     
#     dat$Conv <- dat$Conversions
#     if (input$ylog)
#       dat$Conv <- log1p(dat$Conversions)
# model <- lm(Conv ~ Spend, data = dat)



#     if (input$xlog)
#         if (input$ylog)
#             if (input$intercept)
#               model <- lm(log1p(Conversions) ~ log1p(Gross.Media.Spend),
#                          data = dat)
#             else
#               model <- lm(log1p(Conversions) ~ 0 + log1p(Gross.Media.Spend),
#                           data = dat)
#         else
#             if (input$intercept)
#               model <- lm(Conversions ~ log1p(Gross.Media.Spend), data = dat)
#             else
#               model <- lm(Conversions ~ 0 + log1p(Gross.Media.Spend), data = dat)
#     else if (input$ylog)
#           if(input$intercept)
#             model <- lm(log1p(Conversions) ~ Gross.Media.Spend, data = dat)
#           else
#             model <- lm(log1p(Conversions) ~ 0 + Gross.Media.Spend, data = dat)
#     else
#           if (input$intercept)
#             model <- lm(Conversions ~ Gross.Media.Spend, data = dat)
#           else
#             model <- lm(Conversions ~ 0 + Gross.Media.Spend, data = dat)
    
    model<-modelhr(dat, input$ylog, input$xlog, input$intercept)

    summary(model)
  })
  
  output$thePlot <- renderPlot({
    infile <- input$datfiles
    if (is.null(infile))
      return(paste("No Data Has Been Uploaded"))
    
    dat <- read.xlsx(infile$datapath, 1)
    
    if (input$xlog)
        if (input$ylog)
              if (input$intercept)
                  func <- "log1p(Conversions) ~ log1p(Gross.Media.Spend)"
              else
                  func <- "log1p(Conversions) ~ 0 + log1p(Gross.Media.Spend)"
        else
              if (input$intercept)
                  func <- "Conversions ~ log1p(Gross.Media.Spend)"
              else
                  func <- "Conversions ~ 0 + log1p(Gross.Media.Spend)"
  
    else if (input$ylog)
              if (input$intercept)  
                  func <- "log1p(Conversions) ~ Gross.Media.Spend"
              else
                  func <- "log1p(Conversions) ~ 0 + Gross.Media.Spend"
    else
              if (input$intercept)
                  func <- "Conversions ~ Gross.Media.Spend"
              else
                  func <- "Conversions ~ 0 + Gross Media Spend"
    
    
    thePlot <- ggplot(dat, aes(x = Gross.Media.Spend, y = Conversions)) +
      stat_smooth(formula = func) +
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
    
    dat <- read.xlsx(infile$datapath, 1)
    dat$Date <- as.Date(dat$Date, format= "%m/%d/%y",
                        origin = "1970-01-01")
    dat$Date <- paste0(dat$Date)
    
    #hi <- "hi"
    #spendest <- spendSolve(dat, dat$Conversions, dat$Gross.Media.Spend, input$ylog, input$xlog, input$goal)
    
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
    df$x1 <- df$x + 1
    
    df$y <- ((df$x1)^slope) * exp(intercept) - 1
    df$goal <- effgoal
    df$cpa <- df$x / df$y
    df$goaldif <- abs(df$goal - df$cpa)
    mindif <- min(df$goaldif)
    outputdf <- subset(df, goaldif == mindif, select = x)[1]
    
    output <- paste("MAX SPEND: ", dollar(outputdf$x))
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

spendSolve <-
  function(df, yvar = NULL, xvar = NULL, ylog = TRUE, xlog = TRUE, goal = NULL){
    df <- df
    yvar <- eval(substitute(df$yvar))
    yvar <- ifelse(ylog, log(yvar), yvar)
    xvar <- eval(substitute(df$xvar))
    xvar <- ifelse(xlog, log(xvar), xvar)
    model <- lm(yvar ~ xvar, data = df)
    intercept <- data.frame(summary(model)$coef)$Estimate[1]
    slope <- data.frame(summary(model)$coef)$Estimate[2]
    goalStep1 <- log(goal) - intercept
    goalStep2 <- goalStep1/slope
    goalStep3 <- exp(goalStep2)
    return(goalStep3)
  }

})
