# Preset everybody died
test_data$Survived <- 0
# All women survived
test_data$Survived[test_data$Sex == 'female'] <- 1
# All women in class 3 that pay more than 30 dollars died
# Cuz you know iceberg probably hit that section or something I dunno
test_data$Survived[test_data$Sex == 'female' & test_data$Pclass == 3 &
                     test_data$Fare >= 20] <- 0

# create a submit dataframe consisting of the PassengerId and Survived columns
submit <- data.frame(PassengerId = test_data$PassengerId,
                     Survived = test_data$Survived)

write.csv(submit, file = "./submit/3_onlywomensurvived2.csv", row.names = FALSE)
