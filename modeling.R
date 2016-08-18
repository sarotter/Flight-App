## Heterogeneous ensembles model 
library(caret)
library(dplyr)
library(ggplot2)
library(mlbench)
library(rpart)
library(ipred)
library(e1071)
library(vcdExtra)

theme_set(theme_minimal())

# REFERENCE MODEL -----------------------------------------------------------------------------------------------------

flights_data <- dbReadTable(db, "tb_flights")
flights_data <- flights_data %>% mutate (
  query = as.POSIXct(query),
  departure = as.POSIXct(departure),
  departure_date = factor(as.Date(departure)),
  until_departure = as.double(departure - query))


cheapest <- flights_data %>% group_by(departure_date) %>% dplyr::summarize(
  min_price = min(price), mean_price = mean(price), median_price = median(price), mean_until_departure = mean(until_departure/24)
)

# decision tree
(flights.rpart <- train(median_price ~ ., data = cheapest, method = "rpart"))

test.accuracy <- function(prediction) {
  sum(prediction >= cheapest$median_price - 30 & prediction <= cheapest$median_price + 30) / nrow(cheapest)
}

fancyRpartPlot(flights.rpart$finalModel)

# Assess accuracy of reference model.
#
test.accuracy(predict(flights.rpart, cheapest))

# bagging 
(flights.bagging <- bagging(median_price ~ ., data = cheapest, coob = TRUE, nbagg = 50))

test.accuracy(predict(flights.bagging, cheapest))

# random forest
(flights.forest <- train(median_price ~ mean_until_departure, data = cheapest, method = "rf", ntree = 100,
                          tuneGrid = expand.grid(mtry = 2^(1:3))))

test.accuracy(predict(flights.forest, cheapest))

# Naive Bayes
TRAINCONTROL = trainControl(method = "cv")
(flights.nb <- train(median_price ~ mean_until_departure, data = cheapest, method = "nb", trControl = TRAINCONTROL))

# SVM
(flights.svm <- train(median_price ~min_price + mean_price +mean_until_departure , data = cheapest, method = "svmRadial",
                      trControl = trainControl(method = "cv")))

test.accuracy(predict(flights.svm, cheapest))

predict(flights.svm, try.frame)


try.frame <- data.frame(
                        min_price = min(cheapest$min_price), 
                        mean_price = mean(cheapest$mean_price),
                        mean_until_departure = 50 + 1)

cheapest_prediction  <- cheapest


sapply(1:50, function(x) {
  departure_date_prediction[x] = cheapest_prediction[50,1]
  min_price_prediction[x] = min(cheapest_prediction$min_price)
  cheapest_prediction[50+x,2] = min(cheapest_prediction$min_price)
  cheapest_prediction[50+x,3] = mean(cheapest_prediction$mean_price)
  cheapest_prediction[50+x,5] = 50 + x
  cheapest_prediction[50+x,4] = predict(flights.svm, cheapest_prediction[50 + x,])
})
cheapest_prediction[51,1] = cheapest_prediction[50,1]
cheapest_prediction[51,2] = min(cheapest_prediction$min_price)
head(cheapest_prediction)
