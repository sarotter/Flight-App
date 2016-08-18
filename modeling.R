## Heterogeneous ensembles model 
library(caret)
library(dplyr)
library(ggplot2)
library(mlbench)
library(rpart)
library(ipred)

theme_set(theme_minimal())

# REFERENCE MODEL -----------------------------------------------------------------------------------------------------
View(data)
flights_data <- dbReadTable(db, "tb_flights")
flights_data <- flights_data %>% mutate (
  query = as.POSIXct(query),
  departure = as.POSIXct(departure),
  until_departure = as.double(departure - query),
  departure_date = as.Date(departure))
cheapest<-flights_data %>% group_by(departure_date) %>% summarise(min = min(price), 
                                                        mean = mean(price),
                                                        median = median(price),
                                                        mean_until_departure = mean(until_departure/24)) 
# decision tree
(flights.rpart <- train(price ~ ., data = flights_data, method = "rpart"))

test.accuracy <- function(prediction) {
  sum(prediction >= flights_data$price - 30 & prediction <= flights_data$price + 30) / nrow(data)
}

fancyRpartPlot(flights.rpart$finalModel)

# Assess accuracy of reference model.
#
test.accuracy(predict(flights.ref.rpart, flights_data))

# bagging 
(flights.bagging <- bagging(price ~ ., data = flights_data, coob = TRUE, nbagg = 50))

# random forest
(flights.forest <- train(price ~ ., data = flights_data, method = "rf", ntree = 100,
                          tuneGrid = expand.grid(mtry = 2^(1:3))))

test.accuracy(predict(flights.forest, flights_data))

# Naive Bayes
TRAINCONTROL = trainControl(method = "cv")

(flights.nb <- train(price ~ ., data = flights_data, method = "nb", trControl = TRAINCONTROL))

# 