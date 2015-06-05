# Problem 1 - Loading the Data
# Sys.setlocale("LC_ALL", "C")
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)


library(tm) # Load tm package
corpus = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))  # Create corpus
corpus = tm_map(corpus, tolower) # Pre-process data
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm1 = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm1, 0.99)  # create sparse terms
HeadlineWords = as.data.frame(as.matrix(dtm)) # Create data frame
colnames(HeadlineWords) = make.names(colnames(HeadlineWords)) # Let's make sure our variable names are okay for R:
colnames(HeadlineWords) <- paste0("H", colnames(HeadlineWords))

## Abstract (repeat code)
corpus = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))  # Create corpus
corpus = tm_map(corpus, tolower) # Pre-process data
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm1 = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm1, 0.99)  # create sparse terms
AbstractWords = as.data.frame(as.matrix(dtm)) # Create data frame
colnames(AbstractWords) = make.names(colnames(AbstractWords)) # Let's make sure our variable names are okay for R:
colnames(AbstractWords) <- paste0("A", colnames(AbstractWords))

##  combine

CombineWords <- cbind(HeadlineWords, AbstractWords, row.names=NULL)# Now we need to split the observations back into the training set and testing set.
HeadlineWordsTrain = head(CombineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(CombineWords, nrow(NewsTest))
HeadlineWordsTrain$Popular <- NewsTrain$Popular
library(rpart)
library(rpart.plot)
cart <- rpart(Popular ~., data=HeadlineWordsTrain, method="class")
cartPredicts <- predict(cart, newdata=HeadlineWordsTest)[,2]
logis <- glm(Popular ~., data=HeadlineWordsTrain, family=binomial)
logPredicts <- predict(logis, newdata=HeadlineWordsTest)
library(randomForest)
rd <- randomForest(Popular ~., data=HeadlineWordsTrain)
rdPredict = predict(rd, type="prob")[,2] 
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = logPredicts)

write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!

# One more helpful hint:
# This dataset has a date/time field (PubDate). You might remember dealing with date and time data in some of the Unit 1 homework problems. 
# In this dataset, the following commands might be useful to you when trying to get date and time variables.

# To convert the date/time to something R will understand, you can use the following commands:

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

# The second argument tells the strptime function how the data is formatted. 
# If you opened the file in Excel or another spreadsheet software before loading it into R, you might have to adjust the format. 
# See the help page ?strptime for more information.

# Now that R understands this field, there are many different attributes of the date and time that you can extract.
# For example, you can add a variable to your datasets called "Weekday" that contains the day of the week that the article was published (0 = Sunday, 1 = Monday, etc.), by using the following commands:

NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday

# Weekday could now be used as an independent variable in your predictive models.

# For more fields that you can extract from a date/time object in R, see the help page ?POSIXlt