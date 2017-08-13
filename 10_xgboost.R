# I used this tutorial to help me understaind xgboost
# https://www.kaggle.com/zhuans/xgboost-for-titanic-predction-of-surviva

#####
# Install require package and libraries
#####

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("xgboost")
ipak(packages)

test2 <- data.frame(test$Survived, test$Pclass, test$Sex, test$Age, test$SibSp,
                    test$Parch, test$Fare, test$Embarked, test$Title,
                    test$FamilySize, test$FamilyID2, stringsAsFactors=FALSE)

names(test2) <- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare",
                   "Embarked", "Title", "FamilySize", "FamilyID2")

test2$Survived <- as.numeric(test2$Survived) - 1
test2$Pclass <- as.numeric(test2$Pclass) - 1
test2$Sex <- as.numeric(test2$Sex) - 1
test2$Embarked <- as.numeric(test2$Embarked) - 1
test2$Title <- as.numeric(test2$Title) - 1
test2$FamilyID2 <- as.numeric(test2$FamilyID2) - 1

train2 <- data.frame(train$Survived, train$Pclass, train$Sex,
                     train$Age, train$SibSp, train$Parch, train$Fare,
                     train$Embarked, train$Title, train$FamilySize,
                     train$FamilyID2, stringsAsFactors=FALSE)

names(train2) <- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare",
                   "Embarked", "Title", "FamilySize", "FamilyID2")

train2$Survived <- as.numeric(train2$Survived) - 1
train2$Pclass <- as.numeric(train2$Pclass) - 1
train2$Sex <- as.numeric(train2$Sex) - 1
train2$Embarked <- as.numeric(train2$Embarked) - 1
train2$Title <- as.numeric(train2$Title) - 1
train2$FamilyID2 <- as.numeric(train2$FamilyID2) - 1

dtrain <- xgb.DMatrix(data = as.matrix(train2[, 2:ncol(train2)]),
                      label = train2$Survived)
xgboost_cv <- xgboost(data = as.matrix(train2[, 2:ncol(train2)]),
                      label = train2$Survived, nfold = 3,
                      nrounds = 15, objective = "binary:logistic")

# Fitting with the xgboost model
fit_xgboost <- xgboost(data = as.matrix(train2[, 2:ncol(train2)]),
                       label = train2$Survived, nrounds = 15,
                       objective = "binary:logistic")

importance_matrix <- xgb.importance(names(train2[,2:ncol(train2)]),
                                    model = fit_xgboost)
xgb.plot.importance(importance_matrix)


# Prediction on test and train sets
pred_xgboost_test <- predict(fit_xgboost, as.matrix(test2[, -c(1)]))
pred_xgboost_train <- predict(fit_xgboost, as.matrix(train2[, -c(1)]))

# Since xgboost gives a survival probability prediction, we need to find the best cut-off:
proportion <-
  sapply(seq(.3, .7, .01),
         function(step)
           c(step,
             sum(ifelse(pred_xgboost_train < step, 0, 1) != train[, c(1)]
)))
dim(proportion)
# Applying the best cut-off on the train set prediction for score checking
predict_xgboost_train <-
  ifelse(pred_xgboost_train < proportion[, which.min(proportion[2,])][1], 0, 1)
head(predict_xgboost_train)
score <- sum(train2[, c(1)] == predict_xgboost_train)/nrow(train2)
score

# Applying the best cut-off on the test set
predict_xgboost_test <-
  ifelse(pred_xgboost_test < proportion[, which.min(proportion[2,])][1], 0, 1)

submit <- data.frame(PassengerId = test$PassengerId,
                     Survived = predict_xgboost_test)
write.csv(submit, file = "./submit/7_xgboost.csv", row.names = FALSE)
