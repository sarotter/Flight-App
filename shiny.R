library(dplyr)
library(plotly)
library(Quandl)
library(shiny)
library(shinythemes)
# ---------------------------------------------------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------------------------------------------------

cache = list()

server <- function(input, output) {
  observe({
    input$smooth
    message("Keeping track of smoothing...")
  })
   price.data <- eventReactive(input$search, {
    price <- cheapest$price
    date <- cheapest$departure_date
   })
   
  output$flights <- renderPlotly({
    flightPrice <- price.data()
    p <- plot_ly(cheapest[1:as.numeric(difftime(input$date, Sys.Date())),], x=mean_until_departure, y=median_price, name = "raw") %>% 
      layout(
        showLegend = F,
        xaxis = list(title = "Departure Date"),
        yaxis = list(title = "Price")
      )
  })
  output$recentQuote <- renderText({
    print('COMING SOON')
  })
}

# ---------------------------------------------------------------------------------------------------------------------
# INTERFACE
# ---------------------------------------------------------------------------------------------------------------------


ui <- shinyUI(navbarPage(theme = shinytheme("united"), "Travel Oracle",
                         tabPanel("Price Watch", fluidPage(
                           titlePanel("Flight Oracle"),
                           
                           sidebarPanel(
                             
                             selectInput("origin", "Departure Airport:", 
                                         choices = c("Cape Town (CPT)", "Johannesburg (JNB)")),
                             
                             selectInput("destination", "Arrival Airport:", 
                                         choices = c("Johannesburg (JNB)", "Cape Town (CPT)")),
                             
                             dateInput("date", "Travel Date:",
                                           value = Sys.Date()+1
                             ),
                             
                             actionButton("search", "Search")),
                           mainPanel(
                             plotlyOutput("flights")
                           )
                         )),
                         tabPanel("Airline Comparison",
                                  mainPanel(textOutput("recentQuote")),
                         tags$h3(class="header")
)))





# ---------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
