library(party)
library(rpart)
library(rpart.plot)


data$loan_status <- as.factor(data$loan_status)

set.seed(1234)
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.2))
train <- data[pd == 1, ]
test <- data[pd == 2, ]
data[] <- lapply(data, function(x) if(is.character(x)) as.factor(x) else x)

set.seed(1234)
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[pd == 1, ]
test <- data[pd == 2, ]

tree_party <- ctree(loan_status ~ ., data = train, 
                    controls = ctree_control(mincriterion = 0.90, minsplit = 200))
print(tree_party)
plot(tree_party)



#this part of code in part
train_pred_party <- predict(tree_party, train)
train_table_party <- table(train_pred_party, train$loan_status)
train_error_party <- 1 - sum(diag(train_table_party)) / sum(train_table_party)
train_error_party
train_accuracy_party <- sum(diag(train_table_party)) / sum(train_table_party) * 100
train_accuracy_party

test_pred_party <- predict(tree_party, test)
test_table_party <- table(test_pred_party, test$loan_status)
test_error_party <- 1 - sum(diag(test_table_party)) / sum(test_table_party)
test_error_party
test_accuracy_party <- sum(diag(test_table_party)) / sum(test_table_party) * 100
test_accuracy_party

tree_rpart <- rpart(loan_status ~ ., data = train)
rpart.plot(tree_rpart)


#this part of code in rpart
train_pred_rpart <- predict(tree_rpart, train, type = "class")
train_table_rpart <- table(train_pred_rpart, train$loan_status)
train_error_rpart <- 1 - sum(diag(train_table_rpart)) / sum(train_table_rpart)
train_error_rpart
train_accuracy_rpart <- sum(diag(train_table_rpart)) / sum(train_table_rpart) * 100
train_accuracy_rpart


test_pred_rpart <- predict(tree_rpart, test, type = "class")
test_table_rpart <- table(test_pred_rpart, test$loan_status)
test_error_rpart <- 1 - sum(diag(test_table_rpart)) / sum(test_table_rpart)
test_error_rpart
test_accuracy_rpart <- sum(diag(test_table_rpart)) / sum(test_table_rpart) * 100
test_accuracy_rpart
tree_rpart <- rpart(loan_status ~ ., data = train)
rpart.plot(tree_rpart)
