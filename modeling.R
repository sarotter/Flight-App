## Heterogeneous ensembles model 
library(caret)
library(dplyr)
library(ggplot2)
library(mlbench)
library(rpart)
library(ipred)
library(e1071)
library(rattle)
library(vcdExtra)
library(RSQLite)
db <- dbConnect(dbDriver("SQLite"), dbname = "Flights.db")

theme_set(theme_minimal())
# REFERENCE MODEL -----------------------------------------------------------------------------------------------------
View(data)
flights_data <- dbReadTable(db, "tb_flights")
flights_data <- flights_data %>% mutate (
  query = as.POSIXct(query),
  departure = as.POSIXct(departure),
  until_departure = as.double(departure - query),
  departure_date = as.Date(departure))

# get the cheapest flight everyday
cheapest <- flights_data %>% group_by(departure_date) %>% dplyr::summarize(
  min_price = min(price), mean_price = mean(price), median_price = median(price), mean_until_departure = mean(until_departure/24)
)

# linear regression model
(flights.lm <- train(median_price ~ min_price + mean_price + mean_until_departure , data = cheapest, method = "lm"))
summary(flights.lm)
predict(flights.lm)
points.ci <- predict(flights.lm, interval = "confidence", level = 0.95)
points.pi <- predict(flights.lm, interval = "prediction", level = 0.95)

test.accuracy(predict(flights.lm, cheapest))

# decision tree model
(flights.rpart <- train(median_price ~ min_price + mean_price + mean_until_departure , data = cheapest, method = "rpart"))

test.accuracy <- function(prediction) {
  sum(prediction >= cheapest$median_price - 30 & prediction <= cheapest$median_price + 30) / nrow(cheapest)
}

fancyRpartPlot(flights.rpart$finalModel)

# Assess accuracy of reference model.
#
test.accuracy(predict(flights.rpart, cheapest))

# bagging model
(flights.bagging <- bagging(median_price ~ min_price + mean_price + mean_until_departure , data = cheapest, coob = TRUE, nbagg = 50))

test.accuracy(predict(flights.bagging, cheapest))

# random forest model
(flights.forest <- train(median_price ~ min_price + mean_price + mean_until_departure , data = cheapest, method = "rf", ntree = 100,
                          tuneGrid = expand.grid(mtry = 2^(1:3))))

test.accuracy(predict(flights.forest, cheapest)) # 68% for both
both.prediction <- as.data.frame(predict(flights.forest, cheapest))
both.prediction <- both.prediction %>% mutate (
  Predicted_price = predict(flights.forest, cheapest),
  Days_until_departure = 1:50)

# Naive Bayes model
TRAINCONTROL = trainControl(method = "cv")
(flights.nb <- train(median_price ~ min_price + mean_price + mean_until_departure , data = cheapest, method = "nb", trControl = TRAINCONTROL))

# SVM
(flights.svm <- train(median_price ~ min_price + mean_price + mean_until_departure , data = cheapest, method = "svmRadial",
                      trControl = trainControl(method = "cv")))
test.accuracy(predict(flights.svm, cheapest))



#PREDICTIONS BY AIRLINE
# divide the cheapest data fram by each airline
sa_flights <- flights_data[substr(flights_data$flight_code,1,2)=="SA",]
ba_flights <- flights_data[substr(flights_data$flight_code,1,2)=="BA",]
sa_cheapest <- sa_flights %>% group_by(departure_date) %>% dplyr::summarise(min_price = min(price), 
                                                                     mean_price = mean(price),
                                                                     median_price = median(price),
                                                                     mean_until_departure = mean(until_departure/24)) 
ba_cheapest <- ba_flights %>% group_by(departure_date) %>% dplyr::summarise(min_price = min(price), 
                                                                     mean_price = mean(price),
                                                                     median_price = median(price),
                                                                     mean_until_departure = mean(until_departure/24)) 

sa_shiny <- sa_flights %>% dplyr::select(flight_code,departure, price) 
sa_shiny$departure_date <-format(as.POSIXct(strptime(sa_shiny$departure,"%Y-%m-%d %H:%M")) ,format = "%Y-%m-%d")
sa_shiny$departure_time <- format(as.POSIXct(strptime(sa_shiny$departure,"%Y-%m-%d %H:%M")) ,format = "%H:%M")
sa_shiny$departure <- NULL 
sa_shiny <- sa_shiny %>% arrange(departure_date, price)

ba_shiny <- ba_flights %>% dplyr::select(flight_code,departure, price) 
ba_shiny$departure_date <-format(as.POSIXct(strptime(ba_shiny$departure,"%Y-%m-%d %H:%M")) ,format = "%Y-%m-%d")
ba_shiny$departure_time <- format(as.POSIXct(strptime(ba_shiny$departure,"%Y-%m-%d %H:%M")) ,format = "%H:%M")
ba_shiny$departure <- NULL 
ba_shiny <- ba_shiny %>% arrange(departure_date, price)

both_shiny <- flights_data %>% dplyr::select(flight_code,departure, price) 
both_shiny$departure_date <-format(as.POSIXct(strptime(both_shiny$departure,"%Y-%m-%d %H:%M")) ,format = "%Y-%m-%d")
both_shiny$departure_time <- format(as.POSIXct(strptime(both_shiny$departure,"%Y-%m-%d %H:%M")) ,format = "%H:%M")
both_shiny$departure <- NULL 
both_shiny <- both_shiny %>% arrange(departure_date, price)


# build functions to test accuracy of the model
test.sa.accuracy <- function(prediction) {
  sum(prediction >= sa_cheapest$median_price - 30 & prediction <= sa_cheapest$median_price + 30) / nrow(sa_cheapest)
}

test.ba.accuracy <- function(prediction) {
  sum(prediction >= ba_cheapest$median_price - 30 & prediction <= ba_cheapest$median_price + 30) / nrow(ba_cheapest)
}

# linear regression model
(flights.sa.lm <- train(median_price ~ mean_until_departure , data = sa_cheapest, method = "lm"))
points.ci <- predict(flights.sa.lm, interval = "confidence", level = 0.95)
points.pi <- predict(flights.sa.lm, interval = "prediction", level = 0.95)
test.sa.accuracy(predict(flights.sa.lm, sa_cheapest))

(flights.ba.lm <- train(median_price ~ mean_until_departure , data = ba_cheapest, method = "lm"))
points.ci <- predict(flights.ba.lm, interval = "confidence", level = 0.95)
points.pi <- predict(flights.ba.lm, interval = "prediction", level = 0.95)
test.ba.accuracy(predict(flights.ba.lm, ba_cheapest))

# decision tree model
(flights.sa.rpart <- train(median_price ~ mean_until_departure , data = sa_cheapest, method = "rpart"))
fancyRpartPlot(flights.sa.rpart$finalModel)
test.sa.accuracy(predict(flights.sa.rpart, sa_cheapest))

(flights.ba.rpart <- train(median_price ~ mean_until_departure , data = ba_cheapest, method = "rpart"))
fancyRpartPlot(flights.ba.rpart$finalModel)
test.ba.accuracy(predict(flights.ba.rpart, ba_cheapest)) # BA prediction of 30%



# bagging model
(flights.sa.bagging <- bagging(median_price ~ mean_until_departure , data = sa_cheapest, coob = TRUE, nbagg = 50))
test.sa.accuracy(predict(flights.sa.bagging, sa_cheapest))

(flights.ba.bagging <- bagging(median_price ~ mean_until_departure , data = ba_cheapest, coob = TRUE, nbagg = 50))
test.ba.accuracy(predict(flights.ba.bagging, ba_cheapest))

# Random Forest
(flights.sa.forest <- train(median_price ~ mean_until_departure , data = sa_cheapest, method = "rf", ntree = 100,
                         tuneGrid = expand.grid(mtry = 2^(1:3))))
test.sa.accuracy(predict(flights.sa.forest, sa_cheapest))

(flights.ba.forest <- train(median_price ~ mean_until_departure , data = ba_cheapest, method = "rf", ntree = 100,
                            tuneGrid = expand.grid(mtry = 2^(1:3))))
test.ba.accuracy(predict(flights.ba.forest, ba_cheapest)) # BA prediction of 34%
ba.prediction <- predict(flights.ba.forest, ba_cheapest)

ba.prediction <- as.data.frame(predict(flights.ba.forest, ba_cheapest))
ba.prediction <- ba.prediction %>% mutate (
  Predicted_price = predict(flights.ba.forest, ba_cheapest),
  Days_until_departure = 1:50)

# SVM 
(flights.sa.svm <- train(median_price ~ mean_until_departure , data = sa_cheapest, method = "svmRadial",
                      trControl = trainControl(method = "cv")))
test.sa.accuracy(predict(flights.sa.svm, sa_cheapest))

(flights.ba.svm <- train(median_price ~ mean_until_departure , data = ba_cheapest, method = "svmRadial",
                         trControl = trainControl(method = "cv")))
test.ba.accuracy(predict(flights.ba.svm, ba_cheapest))

sa.prediction <- as.data.frame(predict(flights.sa.svm, sa_cheapest))
sa.prediction <- sa.prediction %>% mutate (
  Predicted_price = predict(flights.sa.svm, sa_cheapest),
  Days_until_departure = 1:50)

View(sa.prediction)
