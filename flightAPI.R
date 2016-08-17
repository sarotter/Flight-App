library(googleflights)
library(jsonlite)
library(readr)
library(stringr)
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

#data frame in the order to push to the SQL table
try.data <- data.frame(
                       outbound_price = outbound_price, 
                       Query_time = as.character(Sys.time()), 
                       flights_deptime_outbound = as.character(flights_deptime_outbound), 
                       flights_info_outbound = flights_info_outbound)
try.data <- unique(try.data)

#dbSendQuery(db,"INSERT INTO tb_flights VALUES()")


priceQuery <- sprintf("(%d,'%s','%s','%s')", flight.data$outbound_price , flight.data$Query_time , flight.data$flights_deptime_outbound , flight.data$flights_info_inbound)
priceQuery <- paste(priceQuery,collapse = ",")


dbSendQuery(db,str_c("INSERT INTO tb_flights(price,query,departure,flight_code) VALUES ", priceQuery))





trypriceQuery <- "(110,convert(datetime,'2016-08-17 19:10:48',120),convert(datetime,'2016-08-18 21:05:00',120),'SA2133')"

dbSendQuery(db,"INSERT INTO tb_flights(price,query,departure,flight_code) VALUES(110,'2016-08-17 19:10:48','2016-08-18 21:05:00','SA2133')")





trypriceQuery <- "(110,convert(datetime,'2016-08-17 19:10:48',120),convert(datetime,'2016-08-18 21:05:00',120),'SA2133')"

dbSendQuery(db,"INSERT INTO tb_flights(price,query,departure,flight_code) VALUES(110,'2016-08-17 19:10:48','2016-08-18 21:05:00','SA2133')")
dbSendQuery(db, "DROP TABLE tb_flights")
dbWriteTable(db, "tb_flights", try.data, append = TRUE, row.names = FALSE)
dbDisconnect(db)
head(dbReadTable(db, "tb_flights"))

