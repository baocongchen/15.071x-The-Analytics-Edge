# Load the data used in this assignment
pisa2009train <- read.csv("pisa2009train.csv")
pisa2009test <- read.csv("pisa2009test.csv")
str(pisa2009train)
# PROBLEM 1 - DATASET
# Using tapply() on pisaTrain, what is the average reading test scores of males and of females??
tapply(pisa2009train$readingScore, pisa2009train$male, mean)
# Answer based on the function above
# Which variables are missing data in at least one observation in the training set? Select all that apply.
summary(pisa2009train)
# REMOVING MISSING VALUES
pisaTrain = na.omit(pisa2009train)
pisaTest = na.omit(pisa2009test)
#################

# PROBLEM 2 - FACTOR VARIABLES  
# Which of the following variables is an unordered factor with at least 3 levels? (Select all that apply.)
# raceeth
# Which of the following variables is an ordered factor with at least 3 levels? (Select all that apply.)
# grade
# Consider the variable "raceeth" in our problem, we will select White as the reference level.
# Which binary variables will be included in the regression model? (Select all that apply.)
# All the variables but not "White"
##########################

# PROBLEM 3 - BUILDING A MODEL
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
# Build a linear regression model (call it lmScore) using the training set to predict readingScore using all
# the remaining variables.
lmScore <- lm(readingScore ~., data=pisaTrain)
# What is the Multiple R-squared value of lmScore on the training set?
summary(lmScore)
# What is the training-set root-mean squared error (RMSE) of lmScore?
sqrt(mean(lmScore$residuals^2))
# Consider two students A and B. They have all variable values the same, except that student A is in grade 
# 11 and student B is in grade 9. What is the predicted reading score of student A minus the predicted 
# reading score of student B?
summary(lmScore)
# A and B are identical in any aspect but grade, and the difference in grade between A and B is 2,
# so the difference in reading score between A and B is 2 times its coefficient
# What is the meaning of the coefficient associated with variable raceethAsian?
# Answer based on the explanation of the previous question
# Answer based on the summary of lmScore 
#################

# PROBLEM 4 - PREDICTING ON UNSEEN DATA
predTest <- predict(lmScore, newdata= pisaTest)
summary(predTest)
# What is the range between the maximum and minimum predicted reading score on the test set?
# maxValue - minValue
# What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE <- sum((predTest - pisaTest$readingScore)^2)
# What is the root-mean squared error (RMSE) of lmScore on the testing set?
RMSE <- sqrt(SSESum/length(predTest))
# What is the predicted test score used in the baseline model? Remember to compute this value using
# the training set and not the test set.
baseline <- mean(pisaTrain$readingScore)
# What is the sum of squared errors of the baseline model on the testing set? HINT: We call the sum
# of squared errors for the baseline model the total sum of squares (SST).
SST <- sum((pisaTest$readingScore - baseline)^2)
# What is the test-set R-squared value of lmScore?
1-SSE/SST
