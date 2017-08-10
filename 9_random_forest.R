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
packages <- c("randomForest")
ipak(packages)

# Unlike decision Trees, Random Forest cannot handle missing data
# we have to handle it

summary(combi$Age)
# shows there is  263 NA's

"So let’s grow a tree on the subset of the data with the age values available,
and then replace those that are missing"

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title +
                  FamilySize,
                data=combi[!is.na(combi$Age),],
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Make sure we really fill in the age
summary(combi$Age)

# two NA (missing values) in embarked
summary(combi$Embarked)

"Because it’s so few observations and such a large majority boarded in
Southampton, let’s just replace those two with “S”."

which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"

# make it a factor again, I know we already made Embarked a factor columns
# but when we did that we have four levels (S, C, Q, and ''), the empty level
# is the missing value. So we have to make it a factor again to do S,C,Q with 3
# levels
combi$Embarked <- factor(combi$Embarked)

# another column with NA (1 missing value)
summary(combi$Fare)
which(is.na(combi$Fare))
# we replace the NA with the median
# median is often used for prices and salary because of skewness of
# people in different classes (rich, poor, middle class, working class, etc...)
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Random Forest can only handle up to 32 levels in a factor,
# FamilyID have 61 levels

# manually reduce the number of levels to keep it under the threshold
"To do this we’ll copy the FamilyID column to a new variable, FamilyID2, and
then convert it from a factor back into a character string with as.character().
We can then increase our cut-off to be a “Small” family from 2 to 3 people.
Then we just convert it back to a factor and we’re done"

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

levels(combi$FamilyID2)
length((levels(combi$FamilyID2))) # 22 levels only sweeeeeet

train <- combi[1:891,]
test <- combi[892:1309,]

# my birthday, do yours don't steal my bday
set.seed(1030) # scoooooorpio

# Train a forest of 2000 trees!!! I'm the king of the bushes
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                      Fare + Embarked + Title + FamilySize + FamilyID2,
                    data = train,
                    importance = TRUE,
                    ntree = 2000)

# So let’s look at what variables were important
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "./submit/6_random_forest.csv", row.names = FALSE)
