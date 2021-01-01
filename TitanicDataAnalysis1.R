library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(stringr)

train <- read.csv("train.csv",header=TRUE)
test <- read.csv("test.csv",header=TRUE)

train_two <- train
test_two <- test

# create a new train set with the new variable
train_two$family_size <- train$SibSp + train$Parch
test_two$family_size <- test$SibSp + test$Parch

# Extracting the title from the name
train_two$Name <- as.character(train_two$Name)
train_two$Title <- sapply(train_two$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
train_two$Title <- sub(' ', '', train_two$Title)
train_two$Title[train_two$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
train_two$Title[train_two$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
train_two$Title <- factor(train_two$Title)


test_two$Name <- as.character(test_two$Name)
test_two$Title <- sapply(test_two$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
test_two$Title <- sub(' ', '', test_two$Title)
test_two$Title[test_two$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
test_two$Title[test_two$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
test_two$Title <- factor(test_two$Title)



# train_new and test_new are available in the workspace
str(train)
str(test)

# Create a new model `my_tree`
my_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data=train_two, method="class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_five)

# Make your prediction using `my_tree_five` and `test_two`
my_prediction <- predict(my_tree, test_two, type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)