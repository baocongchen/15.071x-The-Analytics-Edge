songs <- read.csv("songs.csv")

# PROBLEM 1 - UNDERSTANDING THE DATA
str(songs)
songsFrom2010 <- subset(songs, year >= 2010)
# How many observations (songs) are from the year 2010?
str(songsFrom2010)
# How many songs does the dataset include for which the artist name is "Michael Jackson"?
michael <- subset(songs, artistname=="Michael Jackson")
# Which songs by Michael Jackson made it to the Top 10?
michaelTop10 <- subset(michael, Top10==1)
michaelTop10$songtitle
# The variable corresponding to the estimated time signature (timesignature) is discrete, 
# meaning that it only takes integer values (0, 1, 2, 3, . . . ). What are the values of 
# this variable that occur in our dataset? 
sort(unique(songs$timesignature))
# Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)
# Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. 
# Which one is it?
index <- which.max(songs$tempo)
songs[index, "songtitle"]
###########################

# PROBLEM 2 - CREATING OUR PREDICTION MODEL
# Split the data into a training set "SongsTrain" consisting of all the observations up to and 
# including 2009 song releases, and a testing set "SongsTest", consisting of the 2010 song releases
SongsTest <- songsFrom2010
# How many observations (songs) are in the training set?
SongsTrain <- subset(songs, year < 2010)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
# Remove these variables from your training and testing sets 
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
# What is the value of the Akaike Information Criterion (AIC)?
summary(SongsLog1)
# What does the model suggest?
# answer based on the summary
# In general, if the confidence is low for the time signature, tempo, and key,
# then the song is more likely to be complex. What does Model 1 suggest in terms of complexity?
# Use logic and the answer of the previous question
# Songs with heavier instrumentation tend to be louder (have higher values in the variable "loudness")
# and more energetic (have higher values in the variable "energy").
# By inspecting the coefficient of the variable "loudness", what does Model 1 suggest?
# Use logic and the answer of the previous question
# By inspecting the coefficient of the variable "energy", do we draw the same conclusions as above?
# Use logic and the answer of the previous question
##############################

# PROBLEM 3 - BEWARE OF MULTICOLLINEARITY ISSUES!
# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$loudness,SongsTrain$energy)
# Given that these two variables are highly correlated, Model 1 suffers from multicollinearity. To avoid
# this issue, we will omit one of these two variables and rerun the logistic regression.
# Model 2:
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
# Look at the summary of SongsLog2, and inspect the coefficient of the variable "energy". What do you observe?
# answer based on the summary result
# Now, create Model 3, which should be exactly like Model 1, but without the variable "energy".
# Model 3:
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)
# Do we make the same observation about the popularity of heavy instrumentation as we did with Model 2?
# answer based on the summary result
####################

# PROBLEM 4 - VALIDATING OUR MODEL
# Make predictions on the test set using Model 3. What is the accuracy of Model 3 on the test set, using a threshold of 0.45? 
predictMod3 <- predict(SongsLog3, type="response", newdata=SongsTest)
table(SongsTest$Top10, predictMod3 >= 0.45)
(309+19)/(309+19+40+5)
# What would the accuracy of the baseline model be on the test set? 
table(SongsTest$Top10)
314/(314+59)
# How many songs does Model 3 correctly predict as Top 10 hits in 2010 (remember that all songs in 2010 went into our test set),
# using a threshold of 0.45?
table(SongsTest$Top10, predictMod3 >= 0.45)
# How many non-hit songs does Model 3 predict will be Top 10 hits (again, looking at the test set), using a threshold of 0.45?
table(SongsTest$Top10, predictMod3 >= 0.45)
# What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
19/(40+19)
# What is the specificity of Model 3 on the test set, using a threshold of 0.45?
309/(309+5)
# What conclusions can you make about our model?
# Apply logic to answer the question