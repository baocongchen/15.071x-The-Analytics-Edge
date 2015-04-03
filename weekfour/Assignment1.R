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
# Make a new tree that includes the "sex" variable, again with cp = 0.0. Notice that sex appears as a split that is of secondary importance to the treatment group.