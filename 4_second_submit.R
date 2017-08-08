# Read in the test dataset
test_data <- read.csv("./data/test.csv", stringsAsFactors = FALSE)

# create a column Survived and set to 0, by default we set everybody died
test_data$Survived <- 0

# Base on our EDA, ~74% of women survived
# So we get all the rows that are female and set the survived column to 1
# we're going to assume all women survived in the test data set and all men
# are dead. If I was boarding the titanic I would keep my hair long and pretend
# to be a girl. Are you even reading this?
test_data$Survived[test_data$Sex == 'female'] <- 1

# create a submit dataframe consisting of the PassengerId and Survived columns
submit <- data.frame(PassengerId = test_data$PassengerId,
                     Survived = test_data$Survived)

write.csv(submit, file = "./submit/2_onlywomensurvived.csv", row.names = FALSE)

