# PROBLEM 1 - BAGS OF WORDS
# Load the required data 
Sys.setlocale("LC_ALL", "C")
wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE, encoding="latin1")
# How many cases of vandalism were detected in the history of this page?
wiki$Vandal = as.factor(wiki$Vandal)
sum(wiki$Vandal==1)
# We will now use the bag of words approach to build a model.
library(tm)
corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
dtmAdded <- DocumentTermMatrix(corpusAdded)
str(dtmAdded)
# How many terms appear in dtmAdded?
dtmAdded
# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the 
# revisions, and call the new matrix sparseAdded. How many terms appear in sparseAdded?
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
# Convert sparseAdded to a data frame called wordsAdded, and then prepend all the words with the letter A
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
# Repeat all of the steps we've done so far to create a Removed bag-of-words dataframe, called wordsRemoved
corpusRemoved <- Corpus(VectorSource(wiki$Remove))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
# How many terms are in the wordsRemoved data frame?
ncol(wordsRemoved)
# Combine the two data frames into a data frame called wikiWords
wikiWords = cbind(wordsAdded, wordsRemoved)
library(caTools)
wikiWords$Vandal <- wiki$Vandal
set.seed(123)
split <-sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train <- subset(wikiWords, split==T)
test <- subset(wikiWords, split==F)
# What is the accuracy on the test set of a baseline method that always predicts "not vandalism" 
# (the most frequent outcome)?
table(train$Vandal)
# Build a CART model to predict Vandal, using all of the other variables as independent variables.
library(rpart)
library(rpart.plot)
treeModel <- rpart(Vandal~., data=train, method="class")
treePredicts <- predict(treeModel, newdata=test)
# What is the accuracy of the model on the test set, using a threshold of 0.5? 
table(test$Vandal, treePredicts[,2] >= 0.5)
# Plot the CART tree. How many word stems does the CART model use?
prp(treeModel)
# Given the performance of the CART model relative to the baseline, what is the best explanation of these results?
# Eliminate wrong answers
####################################

# PROBLEM 2 - PROBLEM-SPECIFIC KNOWLEDGE
# Create a copy of the dataframe from the previous question
wikiWords2 = wikiWords
# Make a new column in wikiWords2 that is 1 if "http" was in Added
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
# Based on this new column, how many revisions added a link?
sum(wikiWords2$HTTP)
# Make new training and testing sets
train2 = subset(wikiWords2, split==TRUE)
test2 = subset(wikiWords2, split==FALSE)
# Create a new CART model using this new variable as one of the independent variables.
treeModel2 <- rpart(Vandal ~., data=train2, method="class")
treePredicts2 <- predict(treeModel2, newdata=test2)
# What is the new accuracy of the CART model on the test set, using a threshold of 0.5?
table(test2$Vandal, treePredicts2[,2] >= 0.5)
# Sum the rows of dtmAdded and dtmRemoved and add them as new variables in the data frame wikiWords2 
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
# What is the average number of words added?
mean(wikiWords2$NumWordsAdded)
# Create the CART model again (using the training set and the default parameters).
train3 <- subset(wikiWords2, split2==T)
test3 <- subset(wikiWords2, split2==F)
treeModel3 <- rpart(Vandal ~., data=train3, method="class")
treePredicts3 <- predict(treeModel3, newdata=test3)
# What is the new accuracy of the CART model on the test set?
table(test3$Vandal, treePredicts3[,2]>=0.5)
#################################################

# Problem 3 - Using Non-Textual Data
# Make a copy of wikiWords2, and call it wikiWords3:
wikiWords3 = wikiWords2
# Then add the two original variables Minor and Loggedin to this new data frame:
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
# Build a CART model using all the training data. 
train4 <- subset(wikiWords3, split == T)
test4 <- subset(wikiWords3, split == F )
treeModel4 <- rpart(Vandal ~., data=train4, method="class")
treePredicts4 <- predict(treeModel4, newdata=test4)
# What is the accuracy of the model on the test set?
table(test4$Vandal, treePredicts4[,2] >= 0.5)
# Plot the CART tree. How many splits are there in the tree?
prp(treeModel4)
# Look at the plot to get answer

