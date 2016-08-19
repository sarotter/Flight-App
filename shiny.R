library(dplyr)
library(plotly)
library(Quandl)
library(shiny)
library(shinythemes)
library(DT)
# ---------------------------------------------------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  airlinesInput <- reactive({
    switch(input$airlines,
           "BA" = ba_shiny,
           "SA" = sa_shiny,
           "Both" = both_shiny)
  })
  
  price.data <- eventReactive(input$search, {
    price <- data$price
    date <- data$departure_date
  })
   
   # Show the first "n" observations
   output$view <- renderTable({
     flight <- price.data()
       date <- input$date
     sorted <- airlinesInput()
     head(sorted %>% filter(departure_date == date), n = 5)
   })
   
   output$flights <- renderPlotly({
     flightPrice <- price.data()
     if(input$airlines == "SA") {
       p <- plot_ly(sa_cheapest[(as.numeric(difftime(input$date, Sys.Date())) + 2):(as.numeric(difftime(input$date_2, Sys.Date())) + 2),], x=departure_date, y=median_price, name = "raw") %>% 
         layout(
           showLegend = F,
           xaxis = list(title = "Departure Date"),
           yaxis = list(title = "Price")
         )
     }
     else if(input$airlines == "BA") {
       p <- plot_ly(ba_cheapest[(as.numeric(difftime(input$date, Sys.Date())) + 2):(as.numeric(difftime(input$date_2, Sys.Date())) + 2),], x=departure_date, y=median_price, name = "raw") %>% 
         layout(
           showLegend = F,
           xaxis = list(title = "Departure Date"),
           yaxis = list(title = "Price")
         )
     }
     else {
       p <- plot_ly(cheapest[(as.numeric(difftime(input$date, Sys.Date())) + 2):(as.numeric(difftime(input$date_2, Sys.Date())) + 2),], x=departure_date, y=median_price, name = "raw") %>% 
         layout(
           showLegend = F,
           xaxis = list(title = "Departure Date"),
           yaxis = list(title = "Price")
         )
     }
   })
   
   predict.data <- eventReactive(input$predict, {
     price <- data$price
     date <- data$departure_date
   })
   
   #prediction graph
   output$prediction <- renderPlotly({
     predict <- predict.data()
     if(input$airlines_2 == "SA") {
       p <- plot_ly(sa.prediction[1:as.numeric(difftime(input$date3, Sys.Date())),], x=Days_until_departure, y=Predicted_price, name = "raw") %>% 
         layout(
           showLegend = F,
           xaxis = list(title = "Days until Departure"),
           yaxis = list(title = "Predicted Price")
         )
     }
     else if(input$airlines_2 == "BA") {
       p <- plot_ly(ba.prediction[1:as.numeric(difftime(input$date3, Sys.Date())),], x=Days_until_departure, y=Predicted_price, name = "raw") %>% 
         layout(
           showLegend = F,
           xaxis = list(title = "Days until Departure"),
           yaxis = list(title = "Predicted Price")
         )
     }
     else {
       p <- plot_ly(both.prediction[1:as.numeric(difftime(input$date3, Sys.Date())),], x=Days_until_departure, y=Predicted_price, name = "raw") %>% 
         layout(
           showLegend = F,
           xaxis = list(title = "Days until Departure"),
           yaxis = list(title = "Predicted Price")
         )
     }
   })
   
   send.mail <- eventReactive(input$action, {
     if(input$carriers == "BA" & ba_cheapest[input$date2,]$min_price <= input$slider2) {
       from <- isolate('saul.rotter@gmail.com')
       to <- isolate(input$email)
       subject <- "FLIGHT TO JOHANNESBURG"
       msg <- str_c("Your flight to JNB has fallen to ", input$slider2, " you should buy it now.")
       sendmail(from, to, subject, msg)
     }
     else if(input$carriers == "SA" & sa_cheapest[input$date2,]$min_price <= input$slider2) {
       from <- isolate('saul.rotter@gmail.com')
       to <- isolate(input$email)
       subject <- "FLIGHT TO JOHANNESBURG"
       msg <- str_c("Your flight to JNB has fallen to ", input$slider2, " you should buy it now.")
       sendmail(from, to, subject, msg)
     }
     else if(input$carriers == "BOTH" & cheapest[input$date2,]$min_price <= input$slider2) {
       from <- isolate('saul.rotter@gmail.com')
       to <- isolate(input$email)
       subject <- "FLIGHT TO JOHANNESBURG"
       msg <- str_c("Your flight to JNB has fallen to ", input$slider2, " you should buy it now.")
       sendmail(from, to, subject, msg)
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
                           p("Figure out when's the best date to depart. Being flexible pays off!"),
                           sidebarPanel(
                             
                             selectInput("origin", "Departure Airport:", 
                                         choices = c("Cape Town (CPT)", "Johannesburg (JNB)")),
                             
                             selectInput("destination", "Arrival Airport:", 
                                         choices = c("Johannesburg (JNB)", "Cape Town (CPT)")),
                             
                             dateInput("date", "Earliest Travel Date:",
                                       value = Sys.Date()+1
                             ),
                             dateInput("date_2", "Latest Travel Date:",
                                       value = Sys.Date() + 2),
                             wellPanel(
                               radioButtons("airlines", "Airlines:", c("BA", "SA", "Both"))
                             ),
                             actionButton("search", "Search")),
                           mainPanel(
                             plotlyOutput("flights"),
                             tableOutput("view")
                           )
                         )),
                         tabPanel("Prediction Graph",fluidPage(
                           titlePanel("Prediction Graph"),
                           p("Predict the optimal date to buy your ticket. Don't overpay for flight tickets again!"),
                           
                           sidebarPanel(
                             
                             selectInput("origin", "Departure Airport:", 
                                         choices = c("Cape Town (CPT)", "Johannesburg (JNB)")),
                             
                             selectInput("destination", "Arrival Airport:", 
                                         choices = c("Johannesburg (JNB)", "Cape Town (CPT)")),
                             
                             dateInput("date3", "Travel On:",
                                       value = Sys.Date()+1
                             ),
                             wellPanel(
                               radioButtons("airlines_2", "Airlines:", c("BA", "SA", "Both"))
                             ),
                             actionButton("predict", "Predict")),
                           mainPanel(
                             plotlyOutput("prediction")
                           )
                           )),
                         tabPanel("Price Alert",fluidPage(
                           titlePanel("Price Alert"), 
                           
                           p("Receive alerts when you desired flight drops below a certain price"),
                           
                           fluidRow(column(6,textInput("email", label = h3("Email"), value = "Enter Email")),
                                    
                                    column(6,sliderInput("slider2", label = h3("Price Range"), min = 0, 
                                                         max = 1500, value = 500))),
                           
                           fluidRow(column(6,radioButtons("carriers", "Airlines:", c("BA", "SA", "Both")),
                                           selected = 1)),
                           
                           column(6,dateInput("date2", label = h3("Flight Date"), value = Sys.Date()))),
                           actionButton("action", label = "Set Alert")
                         )))

# ---------------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
