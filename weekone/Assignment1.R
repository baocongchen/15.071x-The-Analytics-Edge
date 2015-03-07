Sys.setlocale("LC_ALL", "C")
# PROBLEM 1 - LOADING THE DATA

mvt <- read.csv("mvtWeek1.csv")
# How many variables are in this dataset?
# How many rows of data (observations) are in this dataset?
str(mvt)
# Using the "max" function, what is the maximum value of the variable "ID"?
max(mvt$ID)
# What is the minimum value of the variable "Beat"?
min(mvt$Beat)
# How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
sum(mvt$Arrest == TRUE)
# How many observations have a LocationDescription value of ALLEY?
sum(mvt$LocationDescription == "ALLEY")

# PROBLEM 2 - UNDERSTANDING DATES IN R 
# In what format are the entries in the variable Date?
mvt[1,"Date"]
# What is the month and year of the median date in our dataset?
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
# In which month did the fewest motor vehicle thefts occur?
which.min(table(mvt$Month))
# On which weekday did the most motor vehicle thefts occur?
which.max(table(mvt$Weekday))

# Which month has the largest number of motor vehicle thefts for which an arrest was made?
arrest <- subset(mvt, mvt$Arrest==T) 
which.max(table(arrest$Month))

# PROBLEM 3 - VISUALIZING CRIME TRENDS

hist(mvt$Date, breaks=100)
# In general, does it look like crime increases or decreases from 2002 - 2012?
# Answer based on the histogram
# In general, does it look like crime increases or decreases from 2005 - 2008?
# Answer based on the histogram
# In general, does it look like crime increases or decreases from 2009 - 2011?
# Answer based on the histogram
# Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half
# of the time period? (Note that the time period is from 2001 to 2012, so the middle of the time period is the beginning of 2007.)
boxplot(mvt$Date~mvt$Arrest)
# For what proportion of motor vehicle thefts in 2001 was an arrest made?
mvt$Year = format(DateConvert,"%Y")
table(mvt$Arrest,mvt$Year)
# For what proportion of motor vehicle thefts in 2007 was an arrest made?
# Answer based on the table
# For what proportion of motor vehicle thefts in 2012 was an arrest made?
# Answer based on the table

# PROBLEM 4 - POPULAR LOCATIONS
# Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category? You should select 5 of the following options.
sort(table(mvt$LocationDescription))
# Answer based on the table.
# How many observations are in Top5?
locdes = mvt$LocationDescription
Top5 <- subset(mvt,locdes=="GAS STATION" | locdes=="ALLEY" | locdes=="STREET" | locdes=="PARKING LOT/GARAGE(NON.RESID.)" | locdes=="DRIVEWAY - RESIDENTIAL")
Top5$LocationDescription = factor(Top5$LocationDescription)
# One of the locations has a much higher arrest rate than the other locations. Which is it?
table(Top5$Arrest,Top5$LocationDescription)
#Answer based on the table
# On which day of the week do the most motor vehicle thefts at gas stations happen?
gasSta <- subset(Top5, Top5$LocationDescription=="GAS STATION")
which.max(table(gasSta$Weekday))
# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
resDri <- subset(Top5, Top5$LocationDescription=="DRIVEWAY - RESIDENTIAL")
which.min(table(resDri$Weekday))
