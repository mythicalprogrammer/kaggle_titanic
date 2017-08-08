# NA for age that's a lotta missing data (pina colada missing data hahaha)
summary(train_data$Age)

# Since there isn't any column indicating if the row is children or not..
# The turoial guy Trevor decided to do it. EDA for the win.
train_data$Child <- 0 # Set every rows to adult
train_data$Child[train_data$Age < 18] <- 1 # now filter out everybody under 18 and set as children


# create a table with both gender and age to see the survival proportions for different subsets
aggregate(
  as.numeric(levels(Survived))[Survived] ~ Child + Sex,
  data = train_data,
  FUN = function(x) { sum(x)/length(x) })
# aggregate function does not like Survived variable as factor....
# welcome to the real world
# we're using as.numeric(levels(factor))[factor] to convert back to actual value....

# From the exploration, child or not it doesn't matter the result is the same
# Being child doesn't give you special treatment, you need to be a girl
# So child is a noninformative variable


# NEXT EDA the tutorial bin the fare to see if people paying higher fare
# have special treatment in getting on the life boats. I'm on a boat!

train_data$Fare2 <- '30+'
train_data$Fare2[train_data$Fare < 30 & train_data$Fare >= 20] <- '20-30'
train_data$Fare2[train_data$Fare < 20 & train_data$Fare >= 10] <- '10-20'
train_data$Fare2[train_data$Fare < 10] <- '<10'

aggregate(as.numeric(levels(Survived))[Survived] ~ Fare2 + Pclass + Sex,
          data = train_data, FUN = function(x) {sum(x)/length(x)})

# We found out expensive ticket with class 3 female have horrible survive rating
# Trevor speculate perhaps that's where they hit the iceburge




