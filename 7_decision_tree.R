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
packages <- c("rpart", 'rpart.plot')
ipak(packages)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train_data,
             method = "class")

rpart.plot(fit)

Prediction <- predict(fit, test_data, type = "class")
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)

write.csv(submit, file = "./submit/4_decisiontree.csv", row.names = FALSE)

