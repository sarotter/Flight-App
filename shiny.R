library(dplyr)
library(plotly)
library(Quandl)
library(shiny)
# runExample("01_hello") # a histogram
# runExample("02_text") # tables and data frames
# runExample("03_reactivity") # a reactive expression
# runExample("04_mpg") # global variables
# runExample("05_sliders") # slider bars
# runExample("06_tabsets") # tabbed panels
# runExample("07_widgets") # help text and submit buttons
# runExample("08_html") # Shiny app built from HTML
# runExample("09_upload") # file upload wizard
# runExample("10_download") # file download wizard
# runExample("11_timer") # an automated timer
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
    ticker <- input$ticker
    if (!(ticker %in% names(cache))) {
      message("Data not in cache. Retrieving now.")
      cache[[ticker]] <<- stocks <- Quandl(paste0("WIKI/", ticker), collapse = "weekly") %>%
        dplyr::rename(date = Date, adj_close = `Adj. Close`) %>%
        dplyr::select(date, adj_close)
    }
    cache[[ticker]]
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

ui <- fluidPage(
  titlePanel("Flight Oracle"),
  
  sidebarPanel(
    
    selectInput("city", "Departure Airport:", 
                choices = c("Cape Town (CPT)", "Johannesburg (JNB)")),
    
    selectInput("city", "Arrival Airport:", 
                choices = c("Johannesburg (JNB)", "Cape Town (CPT)")),

    dateRangeInput("daterange", "Travel Dates:",
                   start  = Sys.Date() - 7,
                   end    = Sys.Date() + 7,
                   format = "mm/dd/yy",
                   separator = " - "),
    
    
    selectInput("ticker", "Stock", width = NULL,
                choices = c("AAPL", "AMD", "RDEN", "REV", "CYTK", "REXI", "CMA")
    ),
    checkboxInput("smooth", "Smooth", value = TRUE),
    # Display this only if smoothing is activated.
    conditionalPanel(
      condition = "input.smooth == true",
      sliderInput("span", "Smoother Span", min = 0, max = 1, value = 0.5)
    )
  ),
  mainPanel(
    plotlyOutput("stockPlot"),
    textOutput("recentQuote")
  )
)

# ---------------------------------------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)