# Visualizing Text Data Using Word CLouds
# Problem 1 - Preparing the Data
tweets <- read.csv("tweets.csv", stringsAsFactors=F)
# 1) Create a corpus using the Tweet variable
library(tm)
corpus <- Corpus(VectorSource(tweets$Tweet))
# 2) Convert the corpus to lowercase 
corpus <- tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
# 3) Remove punctuation from the corpus
corpus <- tm_map(corpus, removePunctuation)
# 4) Remove all English-language stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# 5) Build a document-term matrix out of the corpus
dtm <- DocumentTermMatrix(corpus)
# 6) Convert the document-term matrix to a data frame called allTweets
allTweets <- as.data.frame(as.matrix(dtm))
colnames(allTweets) <- make.names(colnames(allTweets))
# How many unique words are there across all the documents?
ncol(allTweets)
# What is the most compelling rationale for skipping the process of stemming words
# when visualizing text data?
# Use logic to answer the question
###########################################

# Problem 2- Building a Word Cloud
# Install and load the "wordcloud" package, which is needed to build word clouds.
install.packages("wordcloud")
library(wordcloud)
# Which function can we apply to allTweets to get a vector of the words in our dataset, 
# which we'll pass as the first argument to wordcloud()?
?wordcloud
# Which function should we apply to allTweets to obtain the frequency of each word across all tweets?
# function that return the sum of each column
# Use allTweets to build a word cloud.
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.5),min.freq=20,colors=brewer.pal(8, "Dark2"))
# Pre-process the corpus, this time removing the most frequent word
corpus <- Corpus(VectorSource(tweets$Tweet))
# 2) Convert the corpus to lowercase 
corpus <- tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
# 3) Remove punctuation from the corpus
corpus <- tm_map(corpus, removePunctuation)
# 4) Remove all English-language stopwords and the most frequent word
corpus <- tm_map(corpus, removeWords, c("apple",stopwords("english")))
# 5) Build a document-term matrix out of the corpus
dtm <- DocumentTermMatrix(corpus)
# 6) Convert the document-term matrix to a data frame called allTweets
allTweets <- as.data.frame(as.matrix(dtm))
colnames(allTweets) <- make.names(colnames(allTweets))
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(8, "Dark2"))
#######################################

# Problem 3 - Size and Color
# Which word cloud is based only on the negative tweets (tweets with Avg value -1 or less)?
allTweets$Avg <- tweets$Avg
# Avg that is negative implies that the data contains negative words 
wordcloud(colnames(allTweets[allTweets$Avg<0,1:3779]),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(9, "YlOrRd"))
# Only one word cloud was created without modifying parameters min.freq or max.words. Which word cloud is this?
# Count the amount of words each plot has. Parameters min.freg and/or max.words limit the amount of words
# Which word clouds were created with parameter random.order set to FALSE?
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(9, "YlOrRd")[c(-1,-2,-3,-4)],random.order=F)
# Look how the words position in the plot
# Which word cloud was built with a non-default value for parameter rot.per?
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(9, "YlOrRd"),rot.per=0.4)
# Compare the amount of words rotated in each plot
# For which word cloud was the parameter random.color set to TRUE? 
?wordcloud
# Read the section of random.color in help
##########################################

# Problem 4 - Selecting a Color Palette
# Install and load "RColorBrewer" package
install.packages("RColorBrewer")
library(RColorBrewer)
# Which color palette would be most appropriate for use in a word cloud for which we want to use color
# to indicate word frequency?
display.brewer.all()
# Look at the how the colors in each palette are presented. 
# Which RColorBrewer palette name would be most appropriate to use when preparing an image for a document 
# that must be in grayscale?
# Look at the how the grey colors in each palette are presented. 
# Which of the following commands addresses this issue by removing the first 4 elements of the 9-color 
# palette of blue colors?
# Experience the codes to see how they work