                                ############################
                                #FORECASTING AIRLINE DELAYS#
                                ############################
# PROBLEM 1 - LOADING THE DATA
Airlines <- read.csv("AirlineDelay.csv")
set.seed(15071)

spl = sample(nrow(Airlines), 0.7*nrow(Airlines))

AirlinesTrain = Airlines[spl,]

AirlinesTest = Airlines[-spl,]
# How many observations are in the training set AirlinesTrain?
str(AirlinesTrain)
# How many observations are in the testing set AirlinesTest?
str(AirlinesTest)

# PROBLEM 2 - METHOD OF SPLITTING THE DATA
# In this class, we have frequently used the sample.split function to randomly split our data. 
#                             Why do we use a different approach here?
# Review help(sample.split) of caTools package to find the answer
                            
# PROBLEM 3 - A LINEAR REGRESSION MODEL      
# Build a linear regression model to predict "TotalDelay"
linFit <- lm(TotalDelay ~ ., data=AirlinesTrain)
# What is the model's R-squared?         
summary(linFit)

#PROBLEM 4 - CHECKING FOR SIGNIFICANCE      
# In your linear regression model, which of the independent variables are significant at the
#                             p=0.05 level (at least one star)? 
summary(linFit)

# PROBLEM 5 - CORRELATIONS          
# What is the correlation between NumPrevFlights and PrevFlightGap in the training set?
cor(AirlinesTrain$NumPrevFlights,AirlinesTrain$PrevFlightGap)
# What is the correlation between OriginAvgWind and OriginWindGust in the training set?
cor(AirlinesTrain$OriginAvgWind,AirlinesTrain$OriginWindGust)

# PROBLEM 6 - IMPORTANCE OF CORRELATIONS
# Why is it imporant to check for correlations between independent variables? Select all that apply.
# Read article on "multicollinearity" to answer the quiz

# PROBLEM 7 - COEFFICIENTS  
# In the model with all of the available independent variables, what is the coefficient for HistoricallyLate?
coef(linFit)

# PROBLEM 8 - UNDERSTANDING THE COEFFICIENTS
# The coefficient for NumPrevFlights is 1.56. What is the interpretation of this coefficient?
# Read article on "Coefficient of determination" to answer the quiz

# PROBLEM 9 - UNDERSTANDING THE MODEL
# In the linear regression model, given two flights that are otherwise identical, what is the absolute
# difference in predicted total delay given that 
# -one flight is on Thursday and the other is on Sunday?
# -one flight is on Saturday and the other is on Sunday?
# The absolute difference between the coefficients of two dates is the predicted delay

# PROBLEM 10 - PREDICTIONS ON THE TEST SET  
# What is the Sum of Squared Errors (SSE) on the test set?
predictions <- predict(linFit, newdata=AirlinesTest)
# What is the Total Sum of Squares (SST) on the test set?
SSE <- sum((AirlinesTest$TotalDelay - predictions)^2)
# What is the R-squared on the test set?
SST <- sum((AirlinesTest$TotalDelay - mean(AirlinesTrain$TotalDelay))^2)
1-SSE/SST

# PROBLEM 11 - EVALUATING THE MODEL
# Given what you have seen about this model (the R-squared on the training and test sets, the 
# significance of the coefficients, etc.), which of the following are true?
# Eliminate wrong choices. Read article on "R-squared" to answer the question

# PROBLEM 12 - A CLASSIFICATION PROBLEM
Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse
                                    (Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))
# How many flights in the dataset Airlines had no delay, minor delay, major delay respectively?
summary(Airlines$DelayClass)
Airlines$TotalDelay = NULL
# Prepare necessary data for CART model
library(caTools)
set.seed(15071)
split <- sample.split(Airlines$DelayClass, SplitRatio=0.7)
AirlinesNewTrain <- subset(Airlines, split==T)
AirlinesNewTest <- subset(Airlines, split==F)

# PROBLEM 13 - A CART MODEL
library(rpart)
library(rpart.plot)
cart <- rpart(DelayClass ~., data=AirlinesNewTrain, method="class")
# How many split are in the resulting tree?
prp(cart)

# PROBLEM 14 - UNDERSTANDING THE MODEL  
# The CART model you just built never predicts one of the three outcomes. Which one?
prp(cart)

# PROBLEM 15 - TRAINING SET ACCURACY
library(caret)
# Make predictions on the training set, and then create a confusion matrix. 
# What is the overall accuracy of the model?
cartTrainPredicts <- predict(cart, AirlinesNewTrain, type="class")
confusionMatrix(AirlinesNewTrain$DelayClass, cartTrainPredicts)

# PROBLEM 16 - A BASELINE MODEL
# What is the accuracy on the training set of a baseline model that predicts the most 
# frequent outcome (No Delay) for all observations?
obs <- max(table(AirlinesNewTrain$DelayClass))
obs/nrow(AirlinesNewTrain)

# PROBLEM 17 - TESTING SET ACCURACY  
# Make predictions on the testing set, and then create a confusion matrix. What is the 
# overall accuracy of the model on the testing set?
cartTestPredicts <- predict(cart, newdata=AirlinesNewTest, type="class")
confusionMatrix(AirlinesNewTest$DelayClass, cartTestPredicts)
