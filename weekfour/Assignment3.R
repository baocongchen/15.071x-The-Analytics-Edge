# PROBLEM 1 - A LOGISTIC REGRESSION MODEL
# Load the necessary data
census <- read.csv("census.csv")
library(caTools)
set.seed(2000)

split <- sample.split(census$over50k, SplitRatio=0.6)
train <- census[split==T,]
test <- census[split==F,]
# Build a logistic regression model to predict the dependent variable "over50k", 
# using all of the other variables in the dataset as independent variables. 
logisModel <- glm(over50k~. , data=train, family=binomial)
# Which variables are significant, or have factors that are significant? 
# The threshold for this is 0.1
summary(logisModel)
logisPredicts <- predict(logisModel, newdata=test, type="response")
# What is the accuracy of the model on the testing set? Use a threshold of 0.5
table(test$over50k, logisPredicts >= 0.5)
# What is the baseline accuracy for the testing set?
table(test$over50k)
# What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)
pred <- prediction(logisPredicts, test$over50k)
# What is the area-under-the-curve (AUC) for this model on the test set?
as.numeric(performance(pred,"auc")@y.values)

####################################

# PROBLEM 2 - A CART MODEL  
# Build a classification tree to predict "over50k". Use the training set to 
# build the model, and all of the other variables as independent variables.
library(rpart)
treeModel <- rpart(over50k ~. , data=train, method="class")
# Load the necessary library to split the result
library(rpart.plot)
# How many splits does the tree have in total?
rpart.plot(treeModel)
# Which variable does the tree split on at the first level 
# (the very first split of the tree)?
# Answer based on the graph above
# Which variables does the tree split on at the second level 
# (immediately after the first split of the tree)? 
# Answer based on the graph above
# What is the accuracy of the model on the testing set? Use a threshold of 0.5.
treePredicts <- predict(treeModel, newdata=test, type="class")
table(test$over50k, treePredicts)
# The CART ROC curve is less smooth than the logistic regression ROC curve. Which of the
# following explanations for this behavior is most correct?
treePredicts <- predict(treeModel, newdata=test)
treePred <- prediction(treePredicts[,2], test$over50k)
treePerf <- performance(treePred,"tpr","fpr")
plot(treePerf)
logicPerf <- performance(pred,"tpr","fpr")
plot(logicPerf)
# The nature (characteristics) of tree model explains why the have the less smooth graph.
# What is the AUC of the CART model on the test set?
as.numeric(performance(treePred, 'auc')@y.values)
################################

# PROBLEM 3 - A RANDOM FOREST MODEL
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
# Now build a random forest model to predict "over50k", using the dataset "trainSmall" 
# as the data used to build the model
library(randomForest)
set.seed(1)
forestModel <- randomForest(over50k ~. , data=trainSmall)
forestPredict <- predict(forestModel, newdata=test)
# What is the accuracy of the model on the test set, using a threshold of 0.5?
table(test$over50k, forestPredict)
vu = varUsed(forestModel, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forestModel$forest$xlevels[vusorted$ix]))
# Which of the following variables is the most important in terms of the number of splits?
# The number of times a variable is used for splitting indicates the importance of that variable
varImpPlot(forestModel)
# Which one of the following variables is the most important in terms of mean 
# reduction in impurity?
# The higher the average reduction in impurity is, the more important that variable becomes
# PROBLEM 4 - SELECTING CP BY CROSS-VALIDATION  
# Let'select the cp parameter for our CART model using k-fold cross validation, with k = 10 folds
set.seed(2)
# Load cross validation packages
library(caret)
library(e1071)
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
# Perform the cross validation
train(over50k ~., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )
# Which value of cp does the train function recommend?
# Answer based on the above function
# Fit a CART model to the training data using this value of cp. What is the prediction 
# accuracy on the test set?
newTreeModel <- rpart(over50k ~., data=train, cp=0.002, method="class")
newTreePredict <- predict(newTreeModel, newdata=test, type="class")
table(test$over50k, newTreePredict)
# Plot the CART tree for this model. How many splits are there?
prp(newTreeModel)
