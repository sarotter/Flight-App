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
    if(input$airlines == "SA") {
      p <- plot_ly(sa_cheapest[1:as.numeric(difftime(input$date, Sys.Date())),], x=departure_date, y=median_price, name = "raw") %>% 
        layout(
          showLegend = F,
          xaxis = list(title = "Departure Date"),
          yaxis = list(title = "Price")
        )
    }
    else if(input$airlines == "BA") {
      p <- plot_ly(ba_cheapest[1:as.numeric(difftime(input$date, Sys.Date())),], x=departure_date, y=median_price, name = "raw") %>% 
        layout(
          showLegend = F,
          xaxis = list(title = "Departure Date"),
          yaxis = list(title = "Price")
        )
    }
    else {
    p <- plot_ly(cheapest[1:as.numeric(difftime(input$date, Sys.Date())),], x=departure_date, y=median_price, name = "raw") %>% 
      layout(
        showLegend = F,
        xaxis = list(title = "Departure Date"),
        yaxis = list(title = "Price")
      )
    }
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
                             wellPanel(
                               radioButtons("airlines", "Airlines:", c("BA", "SA", "Both"))
                             ),
                             actionButton("search", "Search")),
                           mainPanel(
                             plotlyOutput("flights")
                           )
                         )),
                         tabPanel("Price Alert",fluidPage(
                           titlePanel("Price Alert"), 
                           
                           p("Receive alerts when you desired flight drops below a certain price"),
                           
                           fluidRow(column(6,textInput("text", label = h3("Email"), value = "Enter Email")),
                                    
                                    column(6,sliderInput("slider2", label = h3("Price Range"), min = 0, 
                                                         max = 1500, value = c(40, 60)))),
                           
                           fluidRow(column(6,checkboxGroupInput("checkGroup", label = h3("Carrier"),inline=TRUE,
                                                                choices = list("BA" = 1, "SA" = 2),
                                                                selected = 1)),
                                    
                                    column(6,dateInput("date", label = h3("Flight Date"), value = "2014-01-01"))),
                           actionButton("action", label = "Set Alert")
                           
                         ))))






# ---------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
