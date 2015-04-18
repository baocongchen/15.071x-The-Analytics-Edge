# Problem 1 - Loading the Data
# Sys.setlocale("LC_ALL", "C")
clinical <- read.csv("clinical_trial.csv", stringsAsFactors=F, encoding="latin1" )
# How many characters are there in the longest abstract?
max(nchar(clinical$abstract))
# How many search results provided no abstract?
sum(clinical$abstract=="")
# Find the observation with the minimum number of characters in the title (the variable "title") out of 
# all of the observations in this dataset. What is the text of the title of this article?
titleMinIndex <- which.min(nchar(clinical$title))
clinical[titleMinIndex,"title"]
###########################################

# Problem 2 - Preparing the Corpus
# Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
library(tm)
corpusTitle <- Corpus(VectorSource(clinical$title))
corpusAbstract <- Corpus(VectorSource(clinical$abstract))
# Convert corpusTitle and corpusAbstract to lowercase.
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)
# Convert to plain text
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)
# Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
# Remove the English language stop words from corpusTitle and corpusAbstract
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords('english'))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords('english'))
# Stem the words in corpusTitle and corpusAbstract
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
# Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
# Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% 
sparseTitle <- removeSparseTerms(dtmTitle, 0.95)
sparseAbstract <- removeSparseTerms(dtmAbstract, 0.95)
# Convert dtmTitle and dtmAbstract to data frames 
dtmTitle <- as.data.frame(as.matrix(sparseTitle))
dtmAbstract <- as.data.frame(as.matrix(sparseAbstract))
# How many terms remain in dtmTitle after removing sparse terms?
# run dtmTitle in the console
# run dtmAbstract in the console
# What is the most frequent word stem across all the abstracts?
which.max(colSums(dtmAbstract))
##################################

# Problem 3 - Building a model
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
# What was the effect of these functions?
# run help(paste0) to see the effect
# Using cbind(), combine dtmTitle and dtmAbstract into a single data frame called dtm
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial <- clinical$trial
# How many columns are in this combined data frame?
ncol(dtm)
# Split dtm into data frames named "train" and "test", putting 70% of the data in the training set
library(caTools)
set.seed(144)
split <- sample.split(dtm$trial, SplitRatio=.7)
train <- subset(dtm, split==T)
test <- subset(dtm, split==F)
# What is the accuracy of the baseline model on the training set?
table(train$trial)
# Build a CART model called trialCART
library(rpart)
library(rpart.plot)
treeModel <- rpart(trial ~., data=train, method="class")
treePredicts <- predict(treeModel, newdata=test)
# What is the name of the first variable the model split on?
prp(treeModel)
# Answer based on the plot
# What is the maximum predicted probability for any result?
max(predict(treeModel)[,2])
# Without running the analysis, how do you expect the maximum predicted probability 
# to differ in the testing set?
# The result would be approximately the same due to the abundant data points we have
# What is the training set accuracy, sensitivity, specificity of the CART model?
table(train$trial, predict(treeModel)[,2] >= .5)
#########################################

# Problem 4 - Evaluating the model on the testing set 
# What is the testing set accuracy, assuming a probability threshold of 0.5 for predicting 
# that a result is a clinical trial?
table(test$trial, treePredicts[,2] >= .5)
# Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)
pred <- prediction(treePredicts[,2], test$trial)
perf = performance(pred, "tpr", "fpr")
as.numeric(performance(pred, "auc")@y.values)
###########################################
# part 5: decision-maker tradeoffs

# Problem 5 - Decision-Maker Tradeoffs.
# What is the cost associated with the model in Step 1 making a false negative prediction?
# Use logic to answer the question
# What is the cost associated with the model in Step 1 making a false positive prediction?
# Use logic to answer the question