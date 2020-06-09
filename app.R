library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(tidyquant)
library(coinmarketcapr)
library(treemap)
library(janitor)
library(scales)
library(statebins)
library(viridis)

price_dat <- read_csv("data/price_dat.csv")
google_geo_dat <- read_csv("data/google_geo_dat.csv")
google_time_dat <- read_csv("data/google_time_dat.csv")

ui <- dashboardPage(
  
  dashboardHeader(
    title = "CRYPTO-DASH"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(h4(strong("Hourly Closing Price")), tabName = "closing_price"),
      menuItem(h4(strong("Market Capitalization")), tabName = "market_cap"),
      menuItem(h4(strong("Search Trends")), tabName = "search_trends"),
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
                                 separator = " to ", format = "mm/dd/yyyy")
                ),
                box(width = 9 , plotlyOutput("graph_closing_price", width = "100%"))
              ),
              fluidRow(
                box(width = 3,
                    checkboxGroupInput("CoinsMin", 
                                       label = "Select Currencies", 
                                       choices = list("BTC-USD" = "BTC", 
                                                      "ETH-USD" = "ETH", 
                                                      "LTC-USD" = "LTC",
                                                      "USDT-USD" = "USDT",
                                                      "XRP-USD" = "XRP"),
                                       selected = "BTC"),
                  dateRangeInput('dateRangeMin',
                                 label = paste("Date Range"),
                                 start = as_date(2019-01-01), end = as_date(2019-05-31),
                                 min = as_date(2013-04-28), max = as_date(2019-12-04),
                                 separator = " to ", format = "mm/dd/yyyy")
                ),
                box(width = 9, plotlyOutput("candle_stick", width = "100%"))
              )
      ),
      
      tabItem(tabName = "market_cap", 
              fluidRow(
                box(width = 12, plotOutput("graph_tree_map", width = "100%"))
              ),
              fluidRow(
                box(width = 3,
                         checkboxGroupInput("coinselect", 
                                            label = "Select Currencies", 
                                            choices = list("BTC-USD" = "BTC", 
                                                           "ETH-USD" = "ETH", 
                                                           "LTC-USD" = "LTC",
                                                           "USDT-USD" = "USDT",
                                                           "XRP-USD" = "XRP"),
                                            selected = "BTC")),
                box(width = 9, plotlyOutput("graph_market_cap", width = "100%"))
              )
      ),
      
      tabItem(tabName = "search_trends", 
              fluidRow(
                box(width = 3,
                    radioButtons("coin_search", "Select Coin",
                                 c("BTC-USD" = "BTC", 
                                   "ETH-USD" = "ETH", 
                                   "LTC-USD" = "LTC",
                                   "USDT-USD" = "USDT",
                                   "XRP-USD" = "XRP"),
                                 selected = "BTC")),
                box(width = 9, plotOutput("search_output", width = "100%"))
              ),
              fluidRow(
                box(width = 3,
                    radioButtons("coin_search_trends", "Select Coin",
                                 c("BTC-USD" = "BTC", 
                                   "ETH-USD" = "ETH", 
                                   "LTC-USD" = "LTC",
                                   "USDT-USD" = "USDT",
                                   "XRP-USD" = "XRP"),
                                 selected = "BTC")),
                box(width = 9, plotOutput("search_trends_output", width = "100%"))
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
  setup(api_key = "b66a77f4-a35c-4ef2-81a8-c9fabf528ccc")
  
  output$graph_closing_price <- renderPlotly({
    
    select_coin <- switch(input$symb, 
                          "BTC-USD" = "btcusd",
                         "ETH-USD" = "ethusd",
                          "LTC-USD" = "ltcusd")
    
    Coin <- tq_get(select_coin,
                   get    = "tiingo.crypto",
                   from   = input$dateRange[1],
                   to     = input$dateRange[2],
                  resample_frequency = "1day")
    
    ggplotly(Coin %>%
        ggplot(aes(x = date, y = close)) +
        geom_line() +
          labs(y = "Closing Price", x = NULL))
    
    
  })
  
  output$candle_stick <- renderPlotly({
    
    ggplotly(price_dat %>%
               filter(symbol == c(input$CoinsMin),
                      date < date("2019-05-31") & date > date("2019-01-01")) %>% 
               ggplot(aes(x = date, y = close)) +
               geom_smooth(size = 0.5, se = FALSE, span =  0.3, color = "gold") +
               geom_candlestick(aes(open = open, high = high, low = low, close = close), 
                                colour_up = "darkgreen", colour_down = "darkred", 
                                fill_up  = "darkgreen", fill_down  = "darkred") +
               ggtitle("Historical Trading Prices") +
               labs(x = "Date", y = "Price (USD)"))
    
    
  })
  
  output$graph_tree_map <- renderPlot({
    
    all_coins <- get_crypto_listings() %>% 
      as_tibble() %>% 
      clean_names()
    
    all_coins %>%
      select(name, symbol, usd_market_cap) %>%
      mutate(usd_market_cap_formated = paste0(name, '\n', "(",symbol, "-USD", ")", '\n', 
                                              dollar(usd_market_cap))) %>%
      treemap(index = "usd_market_cap_formated", 
              vSize = "usd_market_cap", 
              title = "Cryptocurrency Market Cap", 
              palette = "Purples")
  })
  
  output$graph_market_cap <- renderPlotly({
      
    price_dat %>%
      filter(symbol == c(input$coinselect)) %>%
      ggplot(aes(x = date, y = market_cap, color = symbol)) +
      geom_line() + 
      ggtitle("Historical Market Cap") +
      labs(x = "Date", y = "Market Cap")

  })
  
  output$search_output <- renderPlot({
    
    google_geo_dat %>% 
      pivot_longer(cols = 2:12, names_to = "currency", values_to = "search_popularity") %>%
      filter(currency == input$coin_search) %>%
      ggplot(aes(fill = search_popularity, geometry = geometry)) +
      geom_statebins(aes(state = Region)) + 
      scale_fill_viridis(discrete = FALSE) + 
      theme_void()
    
  })
  
  output$search_trends_output <- renderPlot({
    
    google_time_dat %>%
      rename("date" = "Region") %>%
      pivot_longer(cols = 2:12, names_to = "symbol", values_to = "search_popularity") %>% 
      filter(symbol == input_currency_2,
             date < input_date_max_2 & date > input_date_min_2) %>% 
      ggplot(aes(x = date, y = search_popularity, color = symbol)) +
      geom_line() + 
      ggtitle("Historical Google Search Popularity") +
      labs(x = "Date", y = "Search Popularity")
    
  })
  
}

shinyApp(ui, server)