# DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA 
# PROBLEM 1 - UNDERSTANDING THE DATA
FluTrain <- read.csv("FluTrain.csv")
# Which week corresponds to the highest percentage of ILI-related physician visits? 
which.max(FluTrain$ILI)
FluTrain[303, "Week"]
# Which week corresponds to the highest percentage of ILI-related query fraction?
which.max(FluTrain$Queries)
FluTrain[303, "Week"]
# What best describes the distribution of values of ILI?
hist(FluTrain$ILI)
# Answer based on the plot of the above function
# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?.
plot(log(FluTrain$ILI), FluTrain$Queries)
# Answer based on the plot of the above function
################

# PROBLEM 2 - LINEAR REGRESSION MODEL  
# Based on our understanding of the data from the previous subproblem, which model best describes our estimation problem?
FluTrend1 <- lm(formula=log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)
# Answer based on the the above function
# What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")?
cor(log(FluTrain$ILI),FluTrain$Queries)
# For a single variable linear regression model, there is a direct relationship between the R-squared and the correlation
# between the independent and the dependent variables. What is the relationship we infer from our problem?
# Answer based on actual calculation of each function provided
####################

# PROBLEM 3 - PERFORMANCE ON THE TEST SET
FluTest <- read.csv("FluTest.csv")
PredTest1 = predict(FluTrend1, newdata=FluTest)
# However, the dependent variable in our model is log(ILI), so PredTest1 would contain predictions of the log(ILI) value.
# We are instead interested in obtaining predictions of the ILI value. We can convert from predictions of log(ILI) to 
# predictions of ILI via exponentiation, or the exp() function. The new code, which predicts the ILI value, is
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
which(FluTest$Week=="2012-03-11 - 2012-03-17")
PredTest1[11]
# What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012? 
# Note that the relative error is calculated as (Observed ILI - Estimated ILI)/Observed ILI
(FluTest[11,"ILI"] - PredTest1[11])/FluTest[11,"ILI"]
# What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of 
# ILI-related physician visits, on the test set?
SSE <- sum((PredTest1-FluTest$ILI)^2)
RMSE <- sqrt(SSE/length(PredTest1))
######################

# PROBLEM 4 - TRAINING A TIME SERIES MODEL
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
# How many values are missing in the new ILILag2 variable?
sum(is.na(ILILag2))
# Use the plot() function to plot the log of ILILag2 against the log of ILI. Which best describes the relationship between
# these two variables?
plot(log(ILILag2), log(FluTrain$ILI))
# Answer based on the result of the function above
# Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable
# as well as the log of the ILILag2 variable. Call this model FluTrend2.
FluTrend2 <- lm(formula=log(ILI)~Queries + log(ILILag2), data=FluTrain )
summary(FluTrend2)
# Which coefficients are significant at the p=0.05 level in this regression model? (Select all that apply.)
# Answer based on the result of the function above
# What is the R^2 value of the FluTrend2 model?
# Answer based on the result of the function above
# On the basis of R-squared value and significance of coefficients, which statement is the most accurate?
# Due to overfitting, FluTrend2 is a weaker model then FluTrend1 on the training set.
# FluTrend2 is about the same quality as FluTrend1 on the training set. 
# FluTrend2 is a stronger model than FluTrend1 on the training set. 
#############################

# PROBLEM 5 - EVALUATING THE TIME SERIES MODEL IN THE TEST SET
# Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest data frame. 
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 <- coredata(ILILag2)
# How many missing values are there in this new variable?
sum(is.na(FluTest$ILILag2))
# We can identify how to fill in the missing values for the ILILag2 variable in FluTest.
# Which value should be used to fill in the ILILag2 variable for the first observation in FluTest?
# Use common logic to solve this problem
FluTest[c(1,2),"ILILag2"] <- FluTrain[c(416,417),"ILI"]                   
# What is the new value of the ILILag2 variable in the first row of FluTest?
FluTest[1,"ILILag2"]
# What is the new value of the ILILag2 variable in the second row of FluTest?
FluTest[2,"ILILag2"]
# Obtain test set predictions of the ILI variable from the FluTrend2 model, again remembering to call
# the exp() function on the result of the predict() function to obtain predictions for ILI instead of log(ILI).
# What is the test-set RMSE of the FluTrend2 model?
summary(FluTrend2)
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE <- sum((PredTest2 - FluTest$ILI)^2)
RMSE <- sqrt(SSE/length(PredTest2))
# Which model obtained the best test-set RMSE?
# The better RMSE is the one that has smaller value