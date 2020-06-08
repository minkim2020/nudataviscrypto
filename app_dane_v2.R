library(shiny)
library(tidyverse)
library(shinydashboard)
library(tidyquant)

ui <- dashboardPage(
  
  dashboardHeader(
    title = "CRYPTO-DASH"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(h4(strong("Closing Price")), tabName = "closing_price"),
      menuItem(h4(strong("Market Capitalization")), tabName = "market_cap"),
      menuItem(h4(strong("About")), tabName = "about"))
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      
      tabItem(tabName = "closing_price",
              fluidRow(
                box(
                  width = 3,
                  selectInput("symb", 
                              label = "Cryptocurrency Symbol",
                              choices = c("BTC-USD", 
                                          "ETH-USD",
                                          "LTC-USD"),
                              selected = "BTC-USD"),
                  dateRangeInput('dateRange',
                                 label = paste("Date Range"),
                                 start = Sys.Date() -7, end = Sys.Date(),
                                 min = Sys.Date() - 365, max = Sys.Date(),
                                 separator = " to ", format = "mm/dd/yyyy"),
                  verbatimTextOutput("dateRangeText")
                ),
                box(width = 9 , plotOutput("graph", width = "100%"))
              )
      ),
      
      tabItem(tabName = "market_cap", 
              fluidRow(
                box(width = 12, plotOutput("graph", width = "100%"))
              )
      ),
      
      tabItem(tabName = "about", 
              h2("About", align="center"),
              p("Min, Nimon, and Dane"),
              p("Sample Text"),
              p("Bottom Text", style = "align: bottom"))
    )))

server <- function(input, output) {
  
  tiingo_api_key('a517f985b208a4b4a535f99f26e303625d699caa')
  
  output$graph <- renderPlot({
    
    select_coin <- switch(input$symb, 
                          "BTC-USD" = "btcusd",
                          "ETH-USD" = "ethusd",
                          "LTC-USD" = "ltcusd")
    
    Coin <- tq_get(select_coin,
                   get    = "tiingo.crypto",
                   from   = input$dateRange[1],
                   to     = input$dateRange[2],
                   resample_frequency = "1hour")
    
    Coin %>%
      ggplot(aes(x = date, y = close)) +
        geom_line() +
          labs(title = "Closing Price Line Chart", y = "Closing Price", x = NULL) +
          theme_minimal()
    
  })
  
}

shinyApp(ui, server)









