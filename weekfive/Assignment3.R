# PROBLEM 1 - LOADING THE DATASET  

emails <- read.csv("emails.csv", stringsAsFactors=F)
# How many emails are in the dataset?
nrow(emails)
# How many of the emails are spam?
sum(emails$spam)
# Which word appears at the beginning of every email in the dataset?
head(emails$text)
# Could a spam classifier potentially benefit from including the frequency of the word 
# that appears in every email?
# The Frequency with which it appears might help us differentiate spam from ham
# How many characters are in the longest email in the dataset
max(nchar(emails$text))
# Which row contains the shortest email in the dataset?
which.min(nchar(emails$text))
#######################################

# Problem 2 - Preparing the Corpus
library(tm)
# Build a new corpus variable called corpus.
corpus <- Corpus(VectorSource(emails$text))
# Using tm_map, convert the text to lowercase.
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
# Using tm_map, remove all punctuation from the corpus.
corpus <- tm_map(corpus, removePunctuation)
# Using tm_map, remove all English stopwords from the corpus.
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Using tm_map, stem the words in the corpus.
corpus <- tm_map(corpus, stemDocument)
# Build a document term matrix from the corpus, called dtm.
dtm <- DocumentTermMatrix(corpus)
# How many terms are in dtm?
# Run dtm in the console to see the result
# To obtain a more reasonable number of terms, limit dtm to contain terms appearing in 
# at least 5% of documents
spdtm <- removeSparseTerms(dtm, 0.95)
emailsSparse <- as.data.frame(as.matrix(spdtm))
# Use the make.names function to make the variable names of emailsSparse valid
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
# What is the word stem that shows up most frequently across all the emails in the dataset?
which.max(colSums(emailsSparse))
# Add a variable called "spam" to emailsSparse containing the email spam labels.
emailsSparse$spam <- emails$spam
# How many word stems appear at least 5000 times in the ham emails in the dataset?
ham <- subset(emailsSparse, spam==0)
sum(colSums(ham) >= 5000)
# How many word stems appear at least 1000 times in the spam emails in the dataset?
spam <- subset(emailsSparse, spam==1)
sum(colSums(spam) >= 1000)
# The lists of most common words are significantly different between the spam and ham emails.
# What does this likely imply?
# They're helpful in differentiating ham and spam emails
#######################################

# PROBLEM 3 - BUILDING MACHINE LEARNING MODELS
emailsSparse$spam = as.factor(emailsSparse$spam)
# set the random seed to 123 and use the sample.split function to split emailsSparse 70/30
set.seed(123)
library(caTools)
split <- sample.split(emailsSparse$spam, SplitRatio=.7)
train <- subset(emailsSparse, split==T)
test <- subset(emailsSparse, split==F)
# Using the training set, train the following three machine learning models
# A logistic regression model called spamLog
spamLog <- glm(spam ~., data=train, family="binomial")
# A CART model called spamCART
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~., data=train, method="class")
# A random forest model called spamRF
library(randomForest)
spamRF <- randomForest(spam ~., data=train)
# For each model, obtain the predicted spam probabilities for the training set
predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2] 
# How many of the training set predicted probabilities from spamLog are less than 0.00001?
table(predict(spamLog) < 0.00001)
# How many of the training set predicted probabilities from spamLog are more than 0.99999?
table(predict(spamLog) > 0.99999)
# How many of the training set predicted probabilities from spamLog are between 0.00001 and 0.99999?
table(predict(spamLog) >= 0.00001 && predict(spamLog) <= 0.99999)
# How many variables are labeled as significant (at the p=0.05 level) in the logistic 
# regression summary output?
summary(spamLog)
# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree?
# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(train$spam, predict(spamLog) >= 0.5)
# What is the training set AUC of spamLog?
predictionTrainLog = prediction(predTrainLog, train$spam)
as.numeric(performance(predictionTrainLog, "auc")@y.values) 
# Answer based on the above function
# What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions? 
table(train$spam, predTrainCART > 0.5)
# What is the training set AUC of spamCART?
predictionTrainCART = prediction(predTrainCART, train$spam)
as.numeric(performance(predictionTrainCART, "auc")@y.values) 
# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions?
table(train$spam, predTrainRF > 0.5)
# What is the training set AUC of spamRF?
predictionTrainRF = prediction(predTrainRF, train$spam)
as.numeric(performance(predictionTrainRF, "auc")@y.values) 
# Which model had the best training set performance, in terms of accuracy and AUC?
# Look at the AUC value of the prediction of each model to figure out the answer
########################################

# Problem 4 - Evaluating on the Test Set
predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2] 
# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(test$spam, predTestLog >= .5)
# What is the testing set AUC of spamLog?
predictionTestLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values)
# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
table(test$spam, predTestCART >= .5)
# What is the testing set AUC of spamCART?
predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values) 
# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
table(test$spam, predTestRF >= 0.5)
# What is the testing set AUC of spamRF?
predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values) 
# Which model had the best testing set performance, in terms of accuracy and AUC?
# The one that has the least fluctuation in AUC and accuracy values
# Which model demonstrated the greatest degree of overfitting?
# The one that has the most fluctuation in AUC and accuracy values
