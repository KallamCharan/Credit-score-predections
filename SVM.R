library(kernlab)

data <- read.csv("C:/Users/chara/Documents/Predective analysis project/Datasetoncrime.csv")
data <- na.omit(data)

data$loan_status <- as.factor(data$loan_status)

set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

svm_model_linear <- ksvm(loan_status ~ ., data = train_data, kernel = "vanilladot")
svm_model_linear

linear_predictions <- predict(svm_model_linear, test_data)

conf_matrix_linear <- table(linear_predictions, test_data$loan_status)
conf_matrix_linear

accuracy_linear <- sum(diag(conf_matrix_linear)) / sum(conf_matrix_linear) * 100
accuracy_linear

svm_model_rbf <- ksvm(loan_status ~ ., data = train_data, kernel = "rbfdot")
svm_model_rbf

rbf_predictions <- predict(svm_model_rbf, test_data)

conf_matrix_rbf <- table(rbf_predictions, test_data$loan_status)
conf_matrix_rbf

accuracy_rbf <- sum(diag(conf_matrix_rbf)) / sum(conf_matrix_rbf) * 100
accuracy_rbf
