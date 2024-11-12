library(class)

# Load the dataset
data <- read.csv("C:/Users/chara/Documents/Predective analysis project/Datasetoncrime.csv")
str(data)
View(data)

data <- na.omit(data)

nor <- function(x) { (x - min(x)) / (max(x) - min(x)) }

data_norm <- as.data.frame(lapply(data[, sapply(data, is.numeric)], nor))

set.seed(123)
train_indices <- sample(1:nrow(data_norm), 0.7 * nrow(data_norm))

train_data <- data_norm[train_indices, ]
test_data <- data_norm[-train_indices, ]

train_category <- data$loan_status[train_indices]
test_category <- data$loan_status[-train_indices]

k <- 5
predicted <- knn(train = train_data, test = test_data, cl = train_category, k = k)

conf_matrix <- table(predicted, test_category)
conf_matrix

accuracy <- function(x) { sum(diag(x)) / sum(x) * 100 }
accuracy(conf_matrix)

