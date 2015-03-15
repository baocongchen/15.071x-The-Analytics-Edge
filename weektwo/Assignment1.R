# PROBLEM 1 - CREATING OUR FIRST MODEL
climate <- read.csv("climate_change.csv")
trainTo2006 <- subset(climate, climate$Year < 2007)
testFrom2007 <- subset(climate, climate$Year > 2006)
# build a linear regression model to predict the dependent variable Temp, using MEI, CO2, CH4, N2O, CFC.11,
# CFC.12, TSI, and Aerosols as independent variables (Year and Month should NOT be used in the model)
# Enter the model R2 (the "Multiple R-squared" value):
model <- lm(formula= Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=trainTo2006)
summary(model)
# Which variables are significant in the model? We will consider a variable signficant 
# only if the p-value is below 0.05. (Select all that apply.)
# Answer based on the above function
###################

# PROBLEM 2 - UNDERSTANDING THE MODEL  
# Current scientific opinion is that nitrous oxide and CFC-11 are greenhouse gases: gases that are able to trap
# heat from the sun and contribute to the heating of the Earth. However, the regression coefficients of both 
# the N2O and CFC-11 variables are negative, indicating that increasing atmospheric concentrations of either 
# of these two compounds is associated with lower global temperatures.
# Which of the following is the simplest correct explanation for this contradiction?
# Find the correct answer by spotting the wrong answers.
cor(trainTo2006)
# Compute the correlations between all the variables in the training set. Which of the following independent 
# variables is N2O highly correlated with (absolute correlation greater than 0.7)? Select all that apply.
# Answer based on the function above
# Which of the following independent variables is CFC.11 highly correlated with? Select all that apply.
# Answer based on the function above
##################

# PROBLEM 3 - SIMPLIFYING THE MODEL
# Given that the correlations are so high, let us focus on the N2O variable and build a model with only MEI, 
# TSI, Aerosols and N2O as independent variables. Remember to use the training set to build the model.
newModel <- lm(formula = Temp ~ N2O + MEI + TSI + Aerosols, data=trainTo2006)
summary(newModel) 
# Enter the coefficient of N2O in this reduced model:  
# Answer based on the function above
# Enter the model R2:
# Answer based on the function above
###################

# PROBLEM 4 - AUTOMATICALLY BUILDING THE MODEL  
# The step function has one argument - the name of the initial model. It returns a simplified model. Use the
# step function in R to derive a new model, with the full model as the initial model
newModel <- step(model)
summary(newModel)
# Enter the R2 value of the model produced by the step function:
# Answer based on the function above
# Which of the following variable(s) were eliminated from the full model by the step function? Select all that apply.
# Answer based on the function above
#####################

# PROBLEM 5 - TESTING ON UNSEEN DATA
# Using the model produced from the step function, calculate temperature predictions for the testing data set,
# using the predict function.
# Enter the testing set R2:
pointsPredictions <- predict(newModel, newdata=testFrom2007)
SSE = sum((pointsPredictions - testFrom2007$Temp)^2)
baseline = mean(trainTo2006$Temp, na.rm=T)
SST = sum((testFrom2007$Temp - baseline)^2)
R.squared = 1-SSE/SST
