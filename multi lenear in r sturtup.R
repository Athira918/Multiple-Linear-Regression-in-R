dataset = read.csv('C:/Users/k18at/Downloads/50_Startups.csv')
dataset
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
regressor = lm(formula = Profit ~ .,
               data = training_set)

y_pred = predict(regressor, newdata = test_set)
library(ggplot2)
comparison <- data.frame(
  Actual = test_set$Profit,
  Predicted = y_pred
)


ggplot(comparison, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Actual vs Predicted Profit") +
  xlab("Actual Profit") +
  ylab("Predicted Profit") +
  theme_minimal()
