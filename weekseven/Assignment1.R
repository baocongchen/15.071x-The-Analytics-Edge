# Problem 1 - Drawing a Map of the US
# Load the ggplot2, maps, and ggmap packages
library(ggplot2)
library(maps)
library(ggmap)
# Load the US map and save it to the variable statesMap
statesMap = map_data("state")
# How many different groups are there? 
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 
# We specified two colors in geom_polygon -- fill and color. Which one defined the color of the outline of the states?
# Change values of fill and color attributes to see how the graph changes
################################################

# Problem 2 - Coloring the States by Predictions
# Load the data using the read.csv function, and call it "polling". Then split the data using the subset function into 
# a training set called "Train" that has observations from 2004 and 2008, and a testing set called "Test" that has 
# observations from 2012. 
polling <- read.csv("PollingImputed.csv")
Train <- subset(polling, Year <= 2008)
Test <- subset(polling, Year > 2008)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
# For how many states is our binary prediction 1 (for 2012), corresponding to Republican?
table(TestPredictionBinary)
# What is the average predicted probability of our model (on the Test set, for 2012)?
mean(TestPredictionBinary)
# Convert the Test.State variable to lowercase
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
str(predictionMap)
# When we merged the data in the previous problem, it caused the number of observations to change. Why? 
help(merge)
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
# The states appear light blue and dark blue in this map. Which color represents a Republican prediction?
# Value 1 for Republican and 0 for Democrat 
ggplot(predictionMap, 
       aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), 
                        labels = c("Democrat", "Republican"), name = "Prediction 2012")
# Change the plot command above to instead color the states by the variable TestPrediction
ggplot(predictionMap, 
       aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
    geom_polygon(color = "black") + 
    scale_fill_gradient(low = "black", high = "white", guide = "legend", breaks= c(0,1), 
                        labels = c("Democrat", "Republican"), name = "Prediction 2012")
# Do the colors of the states in the map for TestPrediction look different from the colors of the states in 
# the map with TestPredictionBinary? Why or why not?
# TestPrediction has values between 0 and 1 while TestPredictionBinary has two values 0 and 1
######################################

# Problem 3 - Understanding the Predictions
# In the 2012 election, the state of Florida ended up being a very close race. It was ultimately won by the
# Democratic party. Did we predict this state correctly or incorrectly?
# Apply logic to answer the question
# What was our predicted probability for the state of Florida?
predictionMap[predictionMap$Test.State=="Florida",]
# What does this imply?
# Though our prediction for the case of Florida is wrong, our model is generally reliable
##################################

# Problem 4 - Parameter Settings
# What is the name of the parameter we set to have value 3 to create plot (1)? 
help(linetype)
# What is the name of the parameter we set to have value 3 to create plot (2)?
help(size)
# Plot (3) was created by changing the value of a different geom_polygon parameter to have value 0.3. 
# Which parameter did we use?
help(alpha)