# PROBLEM 1 - PREPARING THE DATASET 
loans <- read.csv("loans.csv")
# What proportion of the loans in the dataset were not paid in full? Please input a number between 0 and 1. 
table(loans$not.fully.paid)
1533/9578
# Which of the following variables has at least one missing observation? 
summary(loans)
# Which of the following is the best reason to fill in the missing values for these variables instead of
# removing observations with missing data? 
sum(is.na(loans))
missing <- subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | 
                      is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
table(missing$not.fully.paid)
# Eliminate wrong answers
loans_imputed <- read.csv("loans_imputed.csv")
# What best describes the process we just used to handle missing values?
# Read the note above the question carefully to get the answer
###################################

# PROBLEM 2 - PREDICTION MODELS
set.seed(144)
library(caTools)
split = sample.split(loans_imputed$not.fully.paid,  SplitRatio = 0.7)
train <- subset(loans_imputed, split == T)
test <- subset(loans_imputed, split == F)
logisModel <- glm(not.fully.paid~., data=train, family=binomial)
# Which independent variables are significant in our model?
summary(logisModel)
# Let Logit(A) be the log odds of loan A not being paid back in full, according to our logistic
# regression model, and define Logit(B) similarly for loan B. What is the value of Logit(A) - Logit(B)?
# What is the accuracy of the logistic regression model?
predicted.risk <- predict(logisModel, type="response", newdata=test)
test$predicted.risk <- predicted.risk
table(test$not.fully.paid, test$predicted.risk > 0.5)
# What is the accuracy of the baseline model? 
table(test$not.fully.paid)
2413/2873
# Use the ROCR package to compute the test set AUC.
library(ROCR)
ROCRpred <- prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)
######################

# PROBLEM 3 - A "SMART BASELINE
# Using the training set, build a bivariate logistic regression model (aka a logistic regression model with a single independent variable) 
# that predicts the dependent variable not.fully.paid using only the variable int.rate.
intModel <- glm(not.fully.paid ~ int.rate, data=train, family=binomial)
# The variable int.rate is highly significant in the bivariate model, but it is not significant at the 0.05 level in the model
# trained with all the independent variables. What is the most likely explanation for this difference?
summary(intModel)
# int.rate is probably not an independant variable
# Make test set predictions for the bivariate model. What is the highest predicted
# probability of a loan not being paid in full on the testing set?
intPredict <- predict(intModel, newdata=test, type="response")
max(intPredict)
# With a logistic regression cutoff of 0.5, how many loans would be predicted as not being paid in full on the testing set?
table(test$not.fully.paid, intPredict > 0.5)
# What is the test set AUC of the bivariate model?
ROCRintpred <- prediction(intPredict, test$not.fully.paid)
as.numeric(performance(ROCRintpred, "auc")@y.values)
######################

# PROBLEM 4 - COMPUTING THE PROFITABILITY OF AN INVESTMENT  
# How much does a $10 investment with an annual interest rate of 6% pay back after 3 years,
# using continuous compounding of interest? 
10*exp(6/100*3)
# While the investment has value c * exp(rt) dollars after collecting interest, the investor had
# to pay $c for the investment. What is the profit to the investor if the investment is paid back in full?
10*exp(6/100*3) - 10
# Now, consider the case where the investor made a $c investment, but it was not paid back in full
# What is the profit to the investor in this scenario?
-10
# PROBLEM 5 - A SIMPLE INVESTMENT STRATEGY
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)
# Each dollar makes $0.8894769 interest, so 10 dollars make $8.894769
###########################

# PROBLEM 6 - AN INVESTMENT STRATEGY BASED ON RISK  
highInterest <- subset(test, int.rate > 0.15)
# What is the average profit of a $1 investment in one of these high-interest loans 
# (do not include the $ sign in your answer)?
highInterest$profit = exp(highInterest$int.rate*3) - 1
highInterest$profit[highInterest$not.fully.paid == 1] = -1
mean(highInterest$profit)
# What proportion of the high-interest loans were not paid back in full?
table(highInterest$not.fully.paid)
110/437
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, predicted.risk <= cutoff)
# What is the profit of the investor, who invested $1 in each of these 100 loans
sum(selectedLoans$profit)
# How many of 100 selected loans were not paid back in full?
table(selectedLoans$not.fully.paid)
