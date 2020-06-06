library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(quantmod)

source("helpers.R")
source("theme.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "CRYPTO-DASH"
  ),
  
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Charts", tabName = "charts", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
    
  ),
  
  dashboardBody(
    
    dark_theme,
    
    tabItem(tabName = "charts",
            
            fluidRow(
              
              box(width = 2,
                  textInput("symb", "Symbol", "BTC-USD"),
                  
                  dateRangeInput("dates",
                                 "Date range",
                                 start = "2019-01-01",
                                 end = as.character(Sys.Date())),
                  
                  checkboxInput("log",
                                "Plot y axis on log scale",
                                value = FALSE),
                  
                  checkboxInput("adjust",
                                "Adjust prices for inflation", 
                                value = FALSE)),
              
              box(width = 10, plotOutput("plot", height = 1000))
              
                     )
            )
    )
  )


server <- function(input, output) {
  
  dataInput <- reactive({  
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  `Technical Charts` <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
  })
  
  output$plot <- renderPlot({
    chartSeries(`Technical Charts`(),
                log.scale = input$log,
                TA = "addVo(); addCCI()")
  })
}

shinyApp(ui, server)

         