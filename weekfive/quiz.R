# In the previous video, we showed a list of all words that appear at least 20 times in our tweets. 
# Which of the following words appear at least 100 times? Select all that apply.
findFreqTerms(frequencies, lowfreq=100)
# Build a logistic regression model (using the training set) to predict "Negative" using all of the independent variables.
logisTweet = glm(Negative ~ ., data=trainSparse, family=binomial)
predictions = predict(logisTweet, newdata=testSparse, type="response")
# Build a confusion matrix (with a threshold of 0.5) and compute the accuracy of the model. What is the accuracy?
table(testSparse$Negative, logisPredict > 0.5)
genres = colnames(movies)
genresNum <- length(genres)
