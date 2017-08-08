# We're know there's a saying that women and children first
# Let's see if this is true

summary(train_data$Sex)

# each row over total number of passengers
prop.table(table(train_data$Sex, train_data$Survived))
# we don't want a proportion by total number of passenger
# we want propportion by total number of sex

# 1 tells prop.table to use proportion by rows
# our rows are train_data$Sex, our columns is train_data$Survived
prop.table(table(train_data$Sex, train_data$Survived), 1)

# 74.2% of women survived and only 18.89% of men survived
# So it is true, the saying, "women and children first".
