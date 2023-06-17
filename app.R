#########################################################
#  Economic forecasting and analysis
#  Arnold Coffie & Ibrahim Salman
#  Shiny forecasting App with FRED data
##########################################################

# load libraries
library(shiny)
library(fpp2)
library(quantmod)
library(shinythemes)
library(urca)

# load data from FRED
symbols <- c("APU0000702111","APU0000709112","APU0000701111")
m = length(symbols)

getSymbols(symbols,src="FRED")

BREAD <- APU0000702111
MILK <- APU0000709112
FLOUR <- APU0000701111


# Define UI 
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme("cyborg"),
  pageWithSidebar(
    
    # Application title
    headerPanel("R Shiny App Project"),
    
    # Sidebar with controls to select the dataset and forecast ahead duration
    sidebarPanel(
      # Select variable
      h6(selectInput("variable", "Variable:",
                     choices=c("BREAD", "MILK","FLOUR"))),
      h6(textOutput("text1")),
      br(),
      h6(sliderInput("ahead", "Periods to Forecast Ahead:",min=0, max=36, value= 12, step = 2)),
      h6(numericInput("start", "Starting year:", 2000)),
      h6(checkboxInput("cleand", "Clean data", TRUE)),
      
      submitButton("Update View"),
      br(),
      
      h6(p("ECON 4210 Forecasting App")),
      br(),
      
    ),
    
    
    
    # Show the caption and forecast plots
    mainPanel(
      h3(textOutput("caption")),
      tabsetPanel(
        tabPanel("Timeseries plot", plotOutput("tsPlot")),
        tabPanel("Unit Root Test", verbatimTextOutput ("unitroot")),
        tabPanel("ETS Forecast", plotOutput("etsForecastPlot"), verbatimTextOutput("etsForecastTable")), 
        tabPanel("Arima Forecast", plotOutput("arimaForecastPlot"), verbatimTextOutput("arimaForecastTable")),
        tabPanel("TBATS Forecast", plotOutput("tbatsForecastPlot"), verbatimTextOutput("tbatsForecastTable")),
        tabPanel("Holt-Winters Additive Forecast", plotOutput("hwForecastPlot"), verbatimTextOutput("hwForecastTable")),
        tabPanel("Neural Network Autoregression Forecast", plotOutput("nnForecastPlot"), verbatimTextOutput("nnForecastTable")),
        tabPanel("Average forecast", verbatimTextOutput("averageForecastTable"))
        
        
      )
    )
    
  ))

server <- (function(input, output) {
  
  getDataset <- reactive({
    data1 <- switch(input$variable,
                    BREAD = BREAD,
                    MILK = MILK,
                    FLOUR = FLOUR)
  })
  
  
  
  output$caption <- renderText({
    paste("Dataset: ", input$variable)
  })
  
  #TIME SERIES PLOT
  output$tsPlot <- renderPlot({
    
    y <- getDataset()
    date.start = input$start
    y   <-  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    p1 = autoplot(y) + ylab("U.S Dollars") + ggtitle("Average price over time")
    p2 = ggAcf(y) + ggtitle("ACF")
    gridExtra::grid.arrange(p1, p2, nrow=2)
  })
   
  #UNIT ROOT
  output$unitroot <- renderPrint({
    
    y <- getDataset()
    date.start = input$start
    y   <-  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    u <- ur.kpss(y) 
    print(summary(u))
    
  })
  
  
  #ARIMA
  output$arimaForecastPlot <- renderPlot({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- auto.arima(y)
    autoplot(forecast(fit, h=input$ahead)) + ylab("U.S Dollars")
  })
  
  output$arimaForecastTable <- renderPrint({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- auto.arima(y)
    forecast(fit, h=input$ahead)
  })
  
  
  #ETS
  output$etsForecastPlot <- renderPlot({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- ets(y)
    autoplot(forecast(fit, h=input$ahead)) + ylab("U.S Dollars")
  })
  
  output$etsForecastTable <- renderPrint({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- ets(y)
    forecast(fit, h=input$ahead)
  })
  
  #TBATS
  output$tbatsForecastPlot <- renderPlot({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- tbats(y)
    autoplot(forecast(fit, h=input$ahead)) + ylab("U.S Dollars")
  })
  
  output$tbatsForecastTable <- renderPrint({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- tbats(y)
    forecast(fit, h=input$ahead)
    
  })
  
  #Holt Winters
  output$hwForecastPlot <- renderPlot({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- hw(y,seasonal="additive", h=36)
    autoplot(forecast(fit, h=input$ahead))+ ylab("U.S Dollars")
  })
  
  output$hwForecastTable <- renderPrint({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- hw(y,seasonal="additive",h=36)
    forecast(fit, h=input$ahead)
  })
  
  #NN Forecast
  output$nnForecastPlot <- renderPlot({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- nnetar(y, lambda=0)
    autoplot(forecast(fit, h=input$ahead))+ ylab("U.S Dollars")
  })
  
  output$nnForecastTable <- renderPrint({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit <- nnetar(y,lamda=0)
    forecast(fit, h=input$ahead)
  })
  
  
  output$averageForecastTable <- renderPrint({
    y <- getDataset()
    date.start = input$start
    y   =  ts(y[paste(date.start,end(y),sep="/")], start=date.start, freq=12 )
    
    # clean data
    if(isTRUE(input$cleand)){
      y <- tsclean(y)
    }
    # If clean is FALSE
    if(!isTRUE(input$cleand)){
      y <- y
    }
    
    fit1 <- ets(y)
    fc1 = forecast(fit1, h=input$ahead)
    
    fit2 = auto.arima(y)
    fc2 = forecast(fit2, h=input$ahead)
    
    fit3 <- hw(y,seasonal="additive",h=36)
    fc3 <- forecast(fit3, h=input$ahead)
    
    fit4 <- nnetar(y,lamda=0)
    fc4 <- forecast(fit4, h=input$ahead)
    
    fit5 <- tbats(y)
    fc5 <- forecast(fit5, h=input$ahead)
    
    
    fc = (fc1$mean + fc2$mean + fc3$mean + fc4$mean + fc5$mean )/5
    fc
  })
  
  
  
  output$text1 <- renderText({
    
    switch(input$variable,
           BREAD = "Average Price: Bread, White, Pan in U.S City Avg.",
           MILK = "Average Price: Milk, Fresh, Whole, Fortified in U.S City Avg.",
           FLOUR = "Average Price: Flour, White, All Purpose in U.S City Avg.")
  })
  
})


# Run the application 
shinyApp(ui = ui, server = server)
