library(googleflights)
library(jsonlite)
library(readr)
library(stringr)
devtools::install_github("rweyant/googleflights")
library(googleflights)

Hashim.key <- "AIzaSyAcJIcRVze_Bq0sHQ4uO3rV6eUEUpi8nVk"
Saul.key <- "AIzaSyBcyOApaEifsmwW1zIaKBIVfw16gGGV4Z8"
set_apikey(Saul.key)

#results <- search(origin="CPT",dest = "JNB", startDate = Sys.Date()+1, returnDate = Sys.Date() + 8)
results.json
results.json <- toJSON(results)


n <- 200

# airlines <- sapply(1:length(results$trips$data$carrier), function(x){
#   results$trips$data$carrier[[x]]$name
# })

# get the roundtrip price
flights_price_roundtrip <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$pricing[[1]]$saleTotal
})

# change the class to integer
flights_price_roundtrip <- sapply(1:length(flights_price_roundtrip), function(x) {gsub("^ZAR","", flights_price_roundtrip[x])})
flights_price_roundtrip <- as.integer(flights_price_roundtrip)

fareCalculation <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$pricing[[1]]$fareCalculation
})

# separate the price to outbound and inbound
outbound_price <- sapply(1:length(fareCalculation), function(x) {
  numbers = regmatches(fareCalculation[x], gregexpr("[[:digit:]]+", fareCalculation[x]))
  out.price <- str_c(numbers[[1]][1],".",numbers[[1]][2])
  as.integer(out.price)
})


inbound_price <- sapply(1:length(fareCalculation), function(x) {
  numbers = regmatches(fareCalculation[x], gregexpr("[[:digit:]]+", fareCalculation[x]))
  ifelse(numbers[[1]][3] == "1",
         in.price <- str_c(numbers[[1]][4]),
         in.price <- str_c(numbers[[1]][3]))
  as.integer(in.price)
})


flights_carrier_outbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[1]]$segment[[1]]$flight$carrier
})


flights_code_outbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[1]]$segment[[1]]$flight$number
})

flights_info_outbound <- sapply(1:n, function(x){str_c(flights_carrier_outbound[x],flights_code_outbound[x])})  

flights_deptime_outbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[1]]$segment[[1]]$leg[[1]]$departureTime
})

flights_deptime_outbound <- sapply(1:length(flights_deptime_outbound), function(x) {gsub("[[:alpha:]]","", flights_deptime_outbound[x])})
flights_deptime_outbound <- sapply(1:length(flights_deptime_outbound), function(x) {gsub("[-,:]","", flights_deptime_outbound[x])})
flights_deptime_outbound <- sapply(1:length(flights_deptime_outbound), function(x) {gsub("[[:punct:]].*","", flights_deptime_outbound[x])})

flights_deptime_outbound <- as.POSIXct(flights_deptime_outbound, format = "%Y%m%d%H%M")



flights_carrier_inbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[2]]$segment[[1]]$flight$carrier
})

flights_code_inbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[2]]$segment[[1]]$flight$number
})

flights_info_inbound <- sapply(1:n, function(x){str_c(flights_carrier_inbound[x],flights_code_inbound[x])})  


flights_deptime_inbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[2]]$segment[[1]]$leg[[1]]$departureTime
})

flights_deptime_inbound <- sapply(1:length(flights_deptime_inbound), function(x) {gsub("[[:alpha:]]","", flights_deptime_inbound[x])})
flights_deptime_inbound <- sapply(1:length(flights_deptime_inbound), function(x) {gsub("[-,:]","", flights_deptime_inbound[x])})
flights_deptime_inbound <- sapply(1:length(flights_deptime_inbound), function(x) {gsub("[[:punct:]].*","", flights_deptime_inbound[x])})

flights_deptime_inbound <- as.POSIXct(flights_deptime_inbound, format = "%Y%m%d%H%M")




# create the dataframe
flight.data <- data.frame(outbound_price = outbound_price,
                         flights_info_outbound = flights_info_outbound,
                         flights_deptime_outbound = flights_deptime_outbound,
                         inbound_price = inbound_price,
                         flights_info_inbound = flights_info_inbound,
                         flights_deptime_inbound = flights_deptime_inbound,
                         Query_time = Sys.time()) 
View(flight.data)



dbSendQuery(db,"INSERT INTO tb_flights VALUES()")
flights <- dbReadTable(db,"tb_flights")

head(flights)
sapply(1:nrow(results.db), function(x) {
  code <- results.db[x,"flights_code_outbound"]
  print(code)
  carrier <- results.db[x,"flights_carrier_outbound"]
  print(carrier)
  flights[flights$flight_code== code & flights$carrier== carrier,1]
})

priceQuery <- sprintf("(%d,'%s')", results.db$outbound_price,results.db$Query_time, results.db$flights_deptime_outbound)


dbSendQuery(conn = db,
            "CREATE TABLE tb_flight_prices
            (id INTEGER PRIMARY KEY AUTOINCREMENT,
            flight_ID INTEGER,
            price INTEGER,
            query DATETIME,
            departure DATETIME,
            FOREIGN KEY (flight_id) REFERENCES flights(flight_id)
            )")
unique(flights_code_outbound)

View(results.db)
class(results.db)




