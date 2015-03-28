# PROBLEM 1 - LOADING THE DATASET
parole <- read.csv("parole.csv")
# How many parolees are contained in the dataset?
str(parole)
# How many of the parolees in the dataset violated the terms of their parole?
sum(parole$violator==1)
# PROBLEM 2 - PREPARING THE DATASET
# Which variables in this dataset are unordered factors with at least three levels? 
# unordered factors are factors that cannot be compared to other factors in the same group
# , for examples Ebola, HIV, Cancer in illness group.
# How does the output of summary() change for a factor variable as compared to a numerical variable?
#########################

# PROBLEM 3 - SPLITTING INTO A TRAINING AND TESTING SET
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
# Roughly what proportion of parolees have been allocated to the training and testing sets?
# SplitRatio = 0.7 which means 70% of the population's values are true. Test it by using the following function
nrow(train)/(nrow(test) + nrow(train))
# Now, suppose you re-ran lines [1]-[5] of Problem 3.1. What would you expect?
# read the docs related to set.seed function
# If you instead ONLY re-ran lines [3]-[5], what would you expect?
# read the docs related to set.seed function
# If you instead called set.seed() with a different number and then re-ran lines [3]-[5] of Problem 3.1, 
# read the docs related to set.seed function
# what would you expect?
####################

# PROBLEM 4 - BUILDING A LOGISTIC REGRESSION MODEL  

# What variables are significant in this model?
train$state <- as.factor(train$state)
train$crime <- as.factor(train$crime)
logisModel <- glm(violator~., data=train, family="binomial")
summary(logisModel)
# What can we say based on the coefficient of the multiple.offenses variable?
# recall the formula of odds (odds = e^(b0 + b1x1 + b2x2...))
# According to the model, what is the probability this individual is a violator?
theMan <- data.frame(row.names = 1)
theMan$male	<- 1
theMan$race	<- 1
theMan$age	<- 50
theMan$state <- as.factor(1)
theMan$time.served	<- 3
theMan$max.sentence	<- 12 
theMan$multiple.offenses <- 0
theMan$crime <- as.factor(2)
theMan$violator <- NA
# According to the model, what is the probability this individual is a violator?
prob <- predict(logisModel ,type="response", newdata=theMan)
# According to the model, what are the odds this individual is a violator?
odds = prob/(1-prob)
############################

# PROBLEM 5 - EVALUATING THE MODEL ON THE TESTING SET  
# Modify data type of state and crime in test data
test$crime <- as.factor(test$crime)
test$state <- as.factor(test$state)
# What is the maximum predicted probability of a violation?
predicts <- predict(logisModel, type="response", newdata=test)
max(predicts)
# In the following questions, evaluate the model's predictions on the test set using a threshold of 0.5.
table(test$violator, predicts > 0.5)
# What is the model's sensitivity?
12/23
# What is the model's specificity?
167/179
# What is the model's accuracy?
179/202
table(test$violator)
# The job of a parole board is to make sure that a prisoner is ready to be released into free society, and 
# therefore parole boards tend to be particularily concerned with releasing prisoners who will violate their parole.
# Which of the following most likely describes their preferences and best course of action?
# Answer: they will decrease the threshold; doing so, they increase the number of parolees that are predicted to violate the 
# parole, and therefore ensure that parolee to be released will not violate.
# Which of the following is the most accurate assessment of the value of the logistic regression model with a cutoff 0.5 
# to a parole board, based on the model's accuracy as compared to the simple baseline model?
# Compare the accuracy of the logistic model to that of the baseline, then increase the threshold to see how the accuracy change
# Using the ROCR package, what is the AUC value for the model?
library(ROCR)
ROCRpred <- prediction(predicts, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
# Describe the meaning of AUC in this context.
# auc is independent of the regression cutoff selected
########################## 

# PROBLEM 6 - IDENTIFYING BIAS IN OBSERVATIONAL DATA  
# It is not possible to predict the outcome (violate or not violate) of the missing parolees, so using another tracking dataset 
# would be better than other options
