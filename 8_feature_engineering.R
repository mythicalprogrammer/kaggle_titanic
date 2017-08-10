test_data$Survived <- NA
combi <- rbind(train_data, test_data)

combi$Title <- sapply(combi$Name,
                      FUN = function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

# combine Mademoiselle and Madame into a single category
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'

combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <-
  'Lady'


# Make title a factor
combi$Title <- factor(combi$Title)


# if you went by yourself without your family it will be 0 + 0 + 1
# therefore the family size is 1, just you
combi$FamilySize <- combi$SibSp + combi$Parch + 1

" Well we just thought about a large family having issues getting to lifeboats
together, but maybe specific families had more trouble than others? We could
try to extract the Surname of the passengers and group them to find families,
but a common last name such as Johnson might have a few extra non-related
people aboard. In fact there are three Johnsons in a family with size 3,
and another three probably unrelated Johnsons all travelling solo.

Combining the Surname with the family size though should remedy this concern.
No two family-Johnson’s should have the same FamilySize variable on such a
small ship."

combi$Surname <- sapply(combi$Name,
                        FUN = function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
               Embarked + Title + FamilySize + FamilyID,
             data = train,
             method = "class")
rpart.plot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "./submit/5_feature_engineering.csv", row.names = FALSE)



