library(dplyr)
library(plotly)
library(Quandl)
library(shiny)
# ---------------------------------------------------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------------------------------------------------

cache = list()

server <- function(input, output) {
  observe({
    input$smooth
    message("Keeping track of smoothing...")
  })
  stock.data <- reactive({
    dep.city <- input$dep.city
  stock.data <- eventReactive(input$search, {
    ticker <- input$ticker
    if (!(ticker %in% names(cache))) {
      message("Data not in cache. Retrieving now.")
      cache[[ticker]] <<- stocks <- Quandl(paste0("WIKI/", ticker), collapse = "weekly") %>%
        dplyr::rename(date = Date, adj_close = `Adj. Close`) %>%
        dplyr::select(date, adj_close)
    }
    cache[[ticker]]
  })
  price.data <- eventReactive(input$search, {
    destination <- input$arrive
    origin <- input$origin
    date <- input$date
  })
  output$flights <- renderPlotly({
    flightPrice <- price.data()
    p <- plot_ly(flightPrice, x=as.numeric(difftime(date, Sys.Date())), y=price, name = "raw") %>% 
      layout(
      showLegend = T,
      xaxis = list(title = "Days to Flight"),
      yaxis = list(title = "Price")
    )
  })
  output$stockPlot <- renderPlotly({
    stocks <- stock.data()
    p <- plot_ly(stocks, x = date, y = adj_close, name = "raw") %>%
      layout(
        showlegend = F,
        xaxis = list(title = NA),
        yaxis = list(title = "Adjusted Close")
      )
    if (input$smooth) {
      p <- add_trace(p, y = fitted(loess(adj_close ~ as.numeric(date), span = max(input$span, 0.01))), x = date, name = "smoothed")
    }
    p
  })
  output$recentQuote <- renderText({
    recent <- head(stock.data(), 1)
    #
    sprintf("Adjusted close price on %s was %.2f.", recent$date, recent$adj_close)
  })
}
# ---------------------------------------------------------------------------------------------------------------------
# INTERFACE
# ---------------------------------------------------------------------------------------------------------------------
?shinyUI

ui <- fluidPage(
  titlePanel("Flight Oracle"),
  
  sidebarPanel(
    
    selectInput("dep.city", "Departure Airport:", 
                choices = c("Cape Town (CPT)", "Johannesburg (JNB)")),
    
    selectInput("arr.city", "Arrival Airport:", 
                choices = c("Johannesburg (JNB)", "Cape Town (CPT)")),
ui <- shinyUI(navbarPage(theme = shinytheme("united"), "Travel Oracle",
                         tabPanel("Price Watch", fluidPage(
                           titlePanel("Flight Oracle"),
                          
                           sidebarPanel(
                             
                             selectInput("origin", "Departure Airport:", 
                                         choices = c("Cape Town (CPT)", "Johannesburg (JNB)")),
                             
                             selectInput("destination", "Arrival Airport:", 
                                         choices = c("Johannesburg (JNB)", "Cape Town (CPT)")),
                             
                             dateInput("date", "Travel Date:",
                                       value = Sys.Date()
                             ),
                             selectInput("ticker", "Stock", width = NULL,
                                         choices = c("AAPL", "AMD", "RDEN", "REV", "CYTK", "REXI", "CMA")
                             ),
                             checkboxInput("smooth", "Smooth", value = TRUE),
                             # Display this only if smoothing is activated.
                             conditionalPanel(
                               condition = "input.smooth == true",
                               sliderInput("span", "Smoother Span", min = 0, max = 1, value = 0.5)
                             ),
                             actionButton("search", "Search")
                           ),
                           mainPanel(
                             plotlyOutput("stockPlot"),
                             textOutput("recentQuote")
                           )
                         )),
                         tabPanel("Airline Comparison"),
                         tags$h3(class="header")
))




# ---------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)