# Compute the AUC of the CART model from the previous video, using the following command in your R console:
as.numeric(performance(pred, "auc")@y.values)
# What is the AUC?
# Answer based on the above function
# First build a CART model that is similar to the one we built in Video 4, except change the minbucket
# parameter to 5. Plot the tree.
# How many splits does the tree have?
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=5)
prp(StevensTree)
# Now build a CART model that is similar to the one we built in Video 4, except change the 
# parameter to 100. Plot the tree.
# How many splits does the tree have?
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)
prp(StevensTree)
# Let's see what happens if we set the seed to two different values and create two different random forest models.
# First, set the seed to 100, and the re-build the random forest model, exactly like we did in the previous video (Video 5). 
# Then make predictions on the test set. What is the accuracy of the model on the test set?
set.seed(100)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
119/170
# Set the seed to 200 and try it again to compute the accuracy.
set.seed(200)
# .........
table(Test$Reverse, PredictForest)
119/170
# Plot the tree that we created using cross-validation. How many splits does it have?
prp(StevensTreeCV)
# In what ways do you think an analytics approach to predicting healthcare cost will improve upon the previous approach of human judgment?
# Watch the video carefully to get the answer
claimsData <- read.csv("ClaimsData/ClaimsData.csv")
# What is the average age of patients in the training set, ClaimsTrain?
mean(ClaimsTrain$age)
# What proportion of people in the training set (ClaimsTrain) had at least one diagnosis code for diabetes?
mean(ClaimsTrain$diabetes)
# What would the accuracy of this baseline method be on the test set?
table(ClaimsTest$diabetes)
# actual patients with diabetes / total patients
# What would the penalty error of this baseline method be on the test set?
penaltyMatrix <- matrix(c(0,2,4,6,8))
dataTable <- table(ClaimsTest$bucket2009, replicate(length(claimsTest$bucket2009),1))
sum(dataTable*penaltyMatrix)/nrow(ClaimsTest)
# Did the second CART model, with the loss matrix, predict bucket 1 for more or fewer of the observations, and why?
# Applying logic to this question, we know that we receive higher penalty if our prediction is less than the actual outcome;
#  that is why the model with penalty matrix predicts bucket 1 less frequently 