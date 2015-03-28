# QUICK QUESTION
# In R, create a logistic regression model to predict "PoorCare" using the independent variables "StartedOnCombination"
# and "ProviderCount". Use the training set we created in the previous video to build the model.
quality = read.csv("quality.csv")

set.seed(88)
library(caTools)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain = subset(quality, split == TRUE)

qualityTest = subset(quality, split == FALSE)

# we built a logistic regression model to predict PoorCare using the R command:
     QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog)
# What is the coefficient for "StartedOnCombination"?
# Answer based on the result of the above function
# Does this model imply that starting a patient on a combination of drugs is indicative of poor care, or good care?
# Yes because the coefficient value is positive
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
# What is the AUC of this model on the test set?
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
# In the previous video, we computed the following confusion matrix for our logistic regression model on our test set with a threshold of 0.5:
#     
#       FALSE    TRUE
# 0	    1069	   6
# 1	    187	       11
# What is the sensitivity of our logistic regression model on the test set, using a threshold of 0.5?
11/(187+11)
# What is the specificity of our logistic regression model on the test set, using a threshold of 0.5?
1069/(1069+6)