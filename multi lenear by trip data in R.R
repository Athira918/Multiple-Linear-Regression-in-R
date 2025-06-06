library(dplyr)
library(caTools)

dataset <- read.csv('C:/Users/k18at/Downloads/taxi_trip_pricing.csv')

dataset$Day_of_Week <- as.factor(dataset$Day_of_Week)

dataset <- dataset %>%
  filter(!is.na(Trip_Duration_Minutes), !is.na(Trip_Price), !is.na(Day_of_Week))

set.seed(123)
split <- sample.split(dataset$Trip_Duration_Minutes, SplitRatio = 0.66)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)
 
regressor <- lm(Trip_Duration_Minutes ~ Trip_Price + Day_of_Week, data = training_set)
y_pred <- predict(regressor, newdata = test_set)
comparison <- data.frame(
  Actual = test_set$Trip_Duration_Minutes,
  Predicted = y_pred
)
library(ggplot2)

ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Actual vs Predicted Trip Duration") +
  xlab("Actual Trip Duration (Minutes)") +
  ylab("Predicted Trip Duration (Minutes)") +
  theme_minimal()
