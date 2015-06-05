##########################
#PREDICTING SALES ON EBAY#
##########################

# PROBLEM 1 - LOADING THE DATA
load("eBay.Rdata")
str(eBay)
# What proportion of all shoes were sold?
sum(eBay$sold)/nrow(eBay)

# PROBLEM 2 - MISSING VALUES
# Which of the numerical variables has at least one missing value?
summary(eBay)

# PROBLEM 3 - MOST COMMON SHOE SIZE
# What is the most common shoe size in the dataset?
which.max(table(eBay$size))

# PROBLEM 4 - CONVERTING VARIABLES TO FACTORS
eBay$sold <- as.factor(eBay$sold)
eBay$condition <- as.factor(eBay$condition)
eBay$heel <- as.factor(eBay$heel)
eBay$style <- as.factor(eBay$style)
eBay$color <- as.factor(eBay$color)
eBay$material <- as.factor(eBay$material)
# Which of the following methods requires the dependent variable be stored as a factor variable
# when training a model for classification?
# Read article on CART and Random Forest models

# PROBLEM 5 - SPLITTING INTO A TRAINING AND TESTING SET  
set.seed(144)

library(caTools)

spl = sample.split(eBay$sold, 0.7)
training <- subset(eBay, spl==T)
testing <- subset(eBay, spl==F)
# Why do we use the sample.split() function to split into a training and testing set?
# Think about this: we want to obtain a balanced data based on the dependant variable

# PROBLEM 6 - TRAINING A LOGISTIC REGRESSION MODEL
logFit <- glm(sold ~ biddable + startprice + condition + heel + style + color + material, data=training, family=binomial)
# Which of the following characteristics of a shoe are statistically significantly 
# (p < 0.05, aka at least a * in the regression summary) associated with a lower chance of an item being sold?
summary(logFit)
# Read the coeffients and their p value to answer the question

# PROBLEM 7 - PREDICTING USING A LOGISTIC REGRESSION MODEL
# What is the predicted probability that this shoe (dummy) will be sold according to the 
# logistic regression model?
dummy <-  data.frame(biddable=0, startprice=100, condition="Pre-owned", heel="High", style="Open Toe", color="Black", material="Satin")
predict(logFit, newdata=dummy, type="response")

# PROBLEM 8 - INTERPRETING MODEL COEFFICIENTS
# What is the meaning of the coefficient labeled "styleStiletto" in the logistic regression summary output?
# Read the information related to styleStiletto in the summary to answer the question. Remember
# this is a logistic model, so we need to convert it back to normal by using exp()

# PROBLEM 9 - OBTAINING TEST SET PREDICTIONS
# On how many test set observations does your logistic regression model make a different prediction 
# than the prediction the naive baseline model would make? 
logTestPredicts <- predict(logFit, newdata=testing, method="response")
table(testing$sold)
which.max(table(training$sold))

# PROBLEM 10 - COMPUTING TEST-SET AUC
library(ROCR)
# What is the test-set AUC of the logistic regression model?
logPred <- prediction(logTestPredicts, testing$sold)
as.numeric(performance(logPred,"auc")@y.values)

# PROBLEM 11 - COMPUTING TEST-SET AUC  
# What is the meaning of the AUC?
# Read article on AUC and try to relate it with the context of this test to answer to the question.

# PROBLEM 12 - ROC CURVES  
# Which logistic regression threshold is associated with the upper-right corner of the ROC plot 
# (true positive rate 1 and false positive rate 1)?
# Read article on receiver operating characteristic (ROC) to answer the question

# PROBLEM 13 - ROC CURVES
# At roughly which logistic regression cutoff does the model achieve a true positive rate of 80% 
# and a false positive rate of 50%?
logicPerf <- performance(logPred,"tpr","fpr")
plot(logicPerf, colorize=TRUE)
abline(v=0.5,h=0.8)

# PROBLEM 14 - CROSS-VALIDATION TO SELECT PARAMETERS
# Which of the following best describes how 10-fold cross-validation works when selecting 
# between 3 different parameter values?
# In 10-fold cross validation, the model with each parameter setting will be trained on 10 90% 
# subsets of the training set.

# PROBLEM 15 - CROSS-VALIDATION FOR A CART MODEL
# What cp value maximizes the cross-validation accuracy?
set.seed(144)
library(caret)
numFolds <- trainControl(method = "cv", number = 10)
grid <- expand.grid(.cp = seq(0.001,0.05, 0.001))
train(sold ~ biddable + startprice + condition + heel + style + color + material,
      data=training, method="rpart", trControl=numFolds, tuneGrid=grid)

# PROBLEM 16 - TRAIN CART MODEL
# Build and plot the CART model trained with the parameter identified in Problem 15
# What variable is used most frequently as a split in the tree?
cart <- rpart(sold ~ biddable + startprice + condition + heel + style + color + material,
              data=training, method="class", cp=0.005)
library(rpart.plot)
prp(cart)

# PROBLEM 17 - BUILDING A CORPUS FROM ITEM DESCRIPTIONS  
# Build a corpus called "corpus" using the "description" variable from the full data frame "eBay"
library(tm)
library(stringr)

newDescriptions <- str_replace_all(eBay$description,"[^[:graph:]]", " ") 
corpus <- Corpus(VectorSource(newDescriptions))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
# Build a document-term matrix called "dtm" from the preprocessed corpus. 
dtm <- DocumentTermMatrix(corpus)
# How many unique word stems are in dtm?
dtm

# PROBLEM 18 - REMOVING SPARSE TERMS  
# Remove all terms that don't appear in at least 10% of documents in the corpus, 
# storing the result in a new document term matrix called spdtm.
spdtm <- removeSparseTerms(dtm, 0.9)
# How many unique terms are in spdtm?
spdtm

# PROBLEM 19 - EVALUATING WORD FREQUENCIES IN A CORPUS
# Convert spdtm to a data frame called descriptionText. Which word stem appears 
# the most frequently across all descriptions?
descriptionText <- as.data.frame(as.matrix(spdtm))
which.max(colSums(descriptionText))

# PROBLEM 20 - ADDING DATA FROM ORIGINAL DATA FRAME
names(descriptionText) = paste0("D", names(descriptionText))

descriptionText$sold <- eBay$sold
descriptionText$biddable <- eBay$biddable
descriptionText$startprice <- eBay$startprice
descriptionText$condition <- eBay$condition
descriptionText$heel <- eBay$heel
descriptionText$style <- eBay$style
descriptionText$color <- eBay$color
descriptionText$material <- eBay$material
set.seed(144)
spl <- sample.split(descriptionText$sold, SplitRatio=.7)
testText <- subset(descriptionText, spl==F)
trainText <- subset(descriptionText, spl==T)
str(testText)

# PROBLEM 21 - TRAINING ANOTHER LOGISTIC REGRESSION MODEL
# Using trainText, train a logistic regression model called glmText to predict the dependent variable 
# using all other variables in the data frame.
glmText <- glm(sold ~., data=trainText, family=binomial)
# How many of the word frequencies from the description text (variables beginning with the letter "D") 
# are significant at or below the p=0.05 level?
summary(glmText)

# PROBLEM 22 - TEST SET AUC OF NEW LOGISTIC REGRESSION MODEL
# What is the training-set AUC of the new logistic regression model?
trainPred <- predict(glmText)
trainPredictions <- prediction(trainPred, trainText$sold)
logicTrainPerf <- performance(trainPredictions,"auc")@y.values
# What is the test-set AUC of the new logistic regression model?
testPred <- predict(glmText, newdata= testText)
testPredictions <- prediction(testPred, testText$sold)
logicTestPerf <- performance(testPredictions, "auc")@y.values

# PROBLEM 23 - ASSESSING OVERFITTING OF NEW MODEL
# What is the most accurate description of the new logistic regression model?
# What does it mean for AUC of a training set to be bigger than that of a testing set and What
# do we often do to solve this issue? This is the key to the answer