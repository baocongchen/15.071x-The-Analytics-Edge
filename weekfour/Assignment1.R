# PROBLEM 1 - EXPLORATION AND LOGISTIC REGRESSION  
gerber <- read.csv("gerber.csv")
# What proportion of people in this dataset voted in this election?
mean(gerber$voting)
# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
neiVote <- gerber$neighbors[gerber$voting==1]
table(neiVote)/length(neiVote)
selfVote <- gerber$self[gerber$voting==1]
table(selfVote)/length(selfVote)
civicVote <- gerber$civicduty[gerber$voting==1]
table(civicVote)/length(civicVote)
hawVote <- gerber$hawthorne[gerber$voting==1]
table(hawVote)/length(hawVote)
# Which of the following coefficients are significant in the logistic regression model? Select all that apply.
logisModel <- glm(voting~ neighbors + self + civicduty + hawthorne, data=gerber)
summary(logisModel)
# Using a threshold of 0.3, what is the accuracy of the logistic regression model? 
predicts <- predict(logisModel, newdata=gerber)
table(gerber$voting, predicts > 0.3)
186479/344084
# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(gerber$voting, predicts > 0.5)
235388/344084
# Compare your previous two answers to the percentage of people who did not vote (the baseline accuracy)
# and compute the AUC of the model. What is happening here?
table(gerber$voting)
library(ROCR)
predictROC <- prediction(predicts, gerber$voting)
as.numeric(performance(predictROC,"auc")@y.values)
# AUC is low, so the predictive model is weak
########################

# PROBLEM 2 - TREES
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
# Observed no split
# What do you observe about the order of the splits?
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
# Using only the CART tree plot, determine what fraction of "Civic Duty" people voted:
CARTmodel2
# 0.3145377 or 0.31 in the tree plot
# Make a new tree that includes the "sex" variable, again with cp = 0.0. Notice that sex appears as a 
# split that is of secondary importance to the treatment group.
CARTmodel3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)
# In the control group, which gender is more likely to vote?
# Use the graph or the result of CARTmodel3 (enter CARTmodel3 in the console) to get the answer
# In the "Civic Duty" group, which gender is more likely to vote?
# Use the graph or the result of CARTmodel3 (enter CARTmodel3 in the console) to get the answer
#####################################

# PROBLEM 3 - INTERACTION TERMS  
# Let's just focus on the "Control" treatment group. Create a regression tree using just the "control" 
# variable, then create another tree with the "control" and "sex" variables, both with cp=0.0.
controlTree1 <- rpart(voting ~ control, data=gerber, cp=0.0)
controlTree2 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
# In the "control" only tree, what is the absolute value of the difference in the predicted probability of voting between 
# being in the control group versus being in a different group? 
prp(controlTree1, digits=6)
# Answer based on the above graph
# Now, using the second tree (with control and sex), determine who is affected more by NOT being in the 
# control group (being in any of the four treatment groups):
prp(controlTree2, digits=6)
# Calculate the difference in the propotions of each sex being and not being in the control group, if the difference
# is less than 0.001, we conclude that they're affected about the same 
# Going back to logistic regression now, create a model using "sex" and "control". Interpret the coefficient for "sex":
logisModel = glm(voting ~ sex + control, data=gerber, family="binomial")
summary(logisModel)
#  We see the coefficient for sex is negative which means women are less likely to vote
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logisModel, newdata=Possibilities, type="response")
# What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case?
# Answer based on the above results
logisModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(logisModel2)
# How do you interpret the coefficient for the new variable in isolation? That is, how does it relate to the dependent variable?
# The coefficient is negative, so a woman in a control group is less likely to vote
predict(logisModel2, newdata=Possibilities, type="response")
# What is the difference between the logistic regression model and the CART model for the (Woman, Control) case?
# Answer based on the above functions
# Should we always include all possible interaction terms of the independent variables when building a logistic regression model?
# Think about issue like overfitting... 