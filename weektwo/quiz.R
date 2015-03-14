# In R, use the dataset wine.csv to create a linear regression model to predict Price using HarvestRain 
# and WinterRain as independent variables. Using the summary output of this model, answer the following questions:

data <- read.csv("wine.csv")
model <- lm(Price ~ HarvestRain + WinterRain, data=data)
summary(model)
# What is the "Multiple R-squared" value of your model?
# Answer based on the above function
# What is the coefficient for HarvestRain?
# Answer based on the above function
# What is the intercept coefficient?
# Answer based on the above function

# Use the dataset wine.csv to create a linear regression model to predict Price using HarvestRain and WinterRain
# as independent variables, like you did in the previous quick question. Using the summary output of this model,
# answer the following questions:
model <- lm(Price ~ HarvestRain + WinterRain, data=data)
summary(model)
# Is the coefficient for HarvestRain significant?
# Answer based on the result of the above function. The number of asterisks indicates how significate the coefficients are
# Is the coefficient for WinterRain significant?
# Answer based on the result of the above function. The number of asterisks indicates how significate the coefficients are
# Using the data set wine.csv, what is the correlation between HarvestRain and WinterRain?
cor(data$HarvestRain, data$WinterRain)

# If a baseball team scores 713 runs and allows 614 runs, how many games do we expect the team to win?
# Using the linear regression model constructed during the lecture, enter the number of games we expect the team to win:
baseball <- read.csv("baseball.csv")
baseball2001 <- subset(baseball,baseball$Year < 2002 & baseball$Year >1995)
baseball2001$RD <- baseball2001$RS - baseball2001$RA
str(baseball2001)
baseball2001[]
WinsReg <- lm(formula = W ~ RD, data= baseball2001)
summary(WinsReg)
RD <- baseball2001[baseball2001$RA==614, "RD"]
# Apply RD in the formula and we got the answer 

# If a baseball team's OBP is 0.311 and SLG is 0.405, how many runs do we expect the team to score?
# Using the linear regression model constructed during the lecture (the one that uses OBP and SLG as independent variables)
# , enter the number of runs we expect the team to score:
WinsReg1 <- lm(formula = RS ~ OBP + SLG, data= baseball2001)
summary(WinsReg1)
# Apply WinsReg1 in the formula and we get the answer 
WinsReg2 <- lm(formula = RA ~ OOBP + OSLG, data= baseball2001)
summary(WinsReg2)
# Apply WinsReg2 in the formula and we get the answer 

# Suppose you are the General Manager of a baseball team, and you are selecting TWO players for your team. You have a budget
# of $1,500,000, and you have the choice between the following players:
# Player Name       OBP	    SLG	    Salary
# Eric Chavez	    0.338	0.540	$1,400,000
# Jeremy Giambi	    0.391	0.450	$1,065,000
# Frank Menechino	0.369	0.374	$295,000
# Greg Myers	    0.313	0.447	$800,000
# Carlos Pena	    0.361	0.500	$300,000
# Apply WinsReg1 in the table together with the salary and we get the answer

teamRank = c(1,2,3,3,4,4,4,4,5,5)
# In this quick question, we'll see how well these rankings correlate with the regular season wins of the teams. 
# Create another vector in R called wins2012, that has the wins of each team in 2013, in order of rank (the vector should have 10 numbers).
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)
# What is the correlation between teamRank and wins2012?
cor(teamRank,wins2012)
# What is the correlation between teamRank and wins2013?
cor(teamRank,wins2013)