# Load necessary libraries
install.packages("e1071")
library(e1071)
install.packages("caret")

library(caret)


data$loan_status <- as.factor(data$loan_status)

set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

naive_bayes_model <- naiveBayes(loan_status ~ ., data = train_data)

predictions <- predict(naive_bayes_model, test_data)

conf_matrix <- table(predictions, test_data$loan_status)
conf_matrix

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix) * 100
accuracy
