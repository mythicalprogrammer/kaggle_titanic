test_data <- read.csv("./data/test.csv", stringsAsFactors = FALSE)
submit <- data.frame(PassengerId = test_data$PassengerId,
                     Survived = rep(0, 418))
write.csv(submit, file = "./submit/1_theyallperish.csv", row.names = FALSE)
