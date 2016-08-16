library(googleflights)
library(jsonlite)
library(readr)
library(stringr)
devtools::install_github("rweyant/googleflights")
Hashim.key <- "AIzaSyAcJIcRVze_Bq0sHQ4uO3rV6eUEUpi8nVk"
Saul.key <- "AIzaSyBcyOApaEifsmwW1zIaKBIVfw16gGGV4Z8"
set_apikey(Saul.key)
#results <- search(origin="CPT",dest = "JNB", startDate = Sys.Date()+1, returnDate = Sys.Date() + 8)

results.json

results.json <- toJSON(results)

n <- 200

airlines <- sapply(1:length(results$trips$data$carrier), function(x){
  results$trips$data$carrier[[x]]$name
})


flights_price_roundtrip <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$pricing[[1]]$saleTotal
})

# change the class to integer
flights_price_roundtrip <- sapply(1:length(flights_price_roundtrip), function(x) {gsub("^ZAR","", flights_price_roundtrip[x])})
flights_price_roundtrip <- as.integer(flights_price_roundtrip)


flights_carrier_outbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[1]]$segment[[1]]$flight$carrier
})


flights_code_outbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[1]]$segment[[1]]$flight$number
})

flights_deptime_outbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[1]]$segment[[1]]$leg[[1]]$departureTime
})


flights_carrier_inbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[2]]$segment[[1]]$flight$carrier
})

flights_code_inbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[2]]$segment[[1]]$flight$number
})

flights_deptime_inbound <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$slice[[2]]$segment[[1]]$leg[[1]]$departureTime
})

fareCalculation <- sapply(1:n, function(x){
  results$trips$tripOption[[x]]$pricing[[1]]$fareCalculation
})

# create the dataframe
results.db <- data.frame(round_trip_price = flights_price_roundtrip,
                         flights_carrier_outbound = flights_carrier_outbound,
                         flights_code_outbound = flights_code_outbound,
                         flights_deptime_outbound = flights_deptime_outbound,
                         flights_carrier_inbound = flights_carrier_inbound,
                         flights_code_inbound = flights_code_inbound,
                         flights_deptime_inbound = flights_deptime_inbound) 


View(results.db)

#str_c("data", Sys.Date(), "@",Sys.time(),".txt")
#write_csv(results.db, str_c)





