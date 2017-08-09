"
Data Dictionary
Variable	Definition	Key
survival 	Survival 	0 = No, 1 = Yes
pclass 	Ticket class 	1 = 1st, 2 = 2nd, 3 = 3rd
sex 	Sex
Age 	Age in years
sibsp 	# of siblings / spouses aboard the Titanic
parch 	# of parents / children aboard the Titanic
ticket 	Ticket number
fare 	Passenger fare
cabin 	Cabin number
embarked 	Port of Embarkation 	C = Cherbourg, Q = Queenstown, S = Southampton
Variable Notes

pclass: A proxy for socio-economic status (SES)
1st = Upper
2nd = Middle
3rd = Lower

age: Age is fractional if less than 1. If the age is estimated, is it in the form of xx.5

sibsp: The dataset defines family relations in this way...
Sibling = brother, sister, stepbrother, stepsister
Spouse = husband, wife (mistresses and fiancés were ignored)

parch: The dataset defines family relations in this way...
Parent = mother, father
Child = daughter, son, stepdaughter, stepson
Some children travelled only with a nanny, therefore parch=0 for them.
"

train_data <- read.csv("./data/train.csv", stringsAsFactors = FALSE)

train_data$Survived <- as.factor(train_data$Survived)
train_data$Pclass <- as.factor(train_data$Pclass)
train_data$Sex <- as.factor(train_data$Sex)
train_data$Embarked <- as.factor(train_data$Embarked)

test_data <- read.csv("./data/test.csv", stringsAsFactors = FALSE)

test_data$Survived <- as.factor(test_data$Survived)
test_data$Pclass <- as.factor(test_data$Pclass)
test_data$Sex <- as.factor(test_data$Sex)
test_data$Embarked <- as.factor(test_data$Embarked)


