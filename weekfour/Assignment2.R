# PROBLEM 1 - PREDICTING B OR NOT B
# Load the file letters_ABPR.csv into R, and call it letters. Then, create a new variable isB in the dataframe,
# which takes the value "TRUE" if the observation corresponds to the letter B, and "FALSE" if it does not. 
letters <- read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")
# Split the data set into a training and testing set, putting 50% of the data in the training set. Set the seed 
# to 1000 before making the split. The first argument to sample.split should be the dependent variable "letters$isB".
library(caTools)
set.seed(1000)
split <- sample.split(letters, SplitRatio = 0.5)
train <- letters[split==T,]
test <- letters[split==F,]
# Let's consider a baseline method that always predicts the most frequent outcome, which is "not B". 
# What is the accuracy of this baseline method on the test set?
table(test$isB)
# Now build a classification tree to predict whether a letter is a B or not, using the training set
library(rpart)
library(rpart.plot)
treeModel <- rpart(isB ~. - letter, data=train, method="class")
# What is the accuracy of the CART model on the test set? (Use type="class" when making predictions on the test set.)
predicts <- predict(treeModel, newdata=test, type="class")
table(test$isB,predicts)
# Now, build a random forest model to predict whether the letter is a B or not (the isB variable) using the training set.
library(randomForest)
set.seed(1000)
forestModel <- randomForest(isB ~. - letter, data=train)
predictForest <- predict(forestModel, newdata=test)
# What is the accuracy of the model on the test set?
table(test$isB,predictForest)
########################################

# PROBLEM 2 - PREDICTING THE LETTERS A, B, P, R  
# Start by converting letter in the original data set (letters) to a factor by running the following command in R:
letters$letter = as.factor( letters$letter )
# Now, generate new training and testing sets of the letters data frame using letters$letter as the first 
# input to the sample.split function. 
set.seed(2000)
split <- sample.split(letters$letter, SplitRatio=.5)
train <- letters[split==T,]
test <- letters[split==F,]
# What is the baseline accuracy on the testing set?
table(test$letter)
# Calculate the result of the table to get the answer
# Build a tree model to predict "letter", using the training set
treeModel <- rpart(letter ~. - isB, data=train, method="class")
treePredicts <- predict(treeModel, newdata=test, type="class")
# What is the test set accuracy of your CART model? 
table(test$letter, treePredicts)
# Now build a random forest model on the training data, using the same independent variables as in
# the previous problem
set.seed(1000)
forestModel <- randomForest(letter~. - isB, data=train)
forestPredicts <- predict(forestModel, newdata=test)
table(test$letter, forestPredicts)
