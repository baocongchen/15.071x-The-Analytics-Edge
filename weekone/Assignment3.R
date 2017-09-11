# PROBLEM 1 - LOADING AND SUMMARIZING THE DATASET
CPS <- read.csv("CPSData.csv")
# How many interviewees are in the dataset?
str(CPS)
# Answer based on the result of the above function
# Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? Please enter the name exactly how you see it.
which.max(table(CPS$Industry))
# Which state has the fewest interviewees?
which.min(table(CPS$State))
# Which state has the largest number of interviewees?
which.max(table(CPS$State))
# What proportion of interviewees are citizens of the United States?
table(CPS$Citizenship)
# Answer based on the result of the above function.
hispanic <- subset(CPS,CPS$Hispanic==T)
table(hispanic$Race)
# Answer based on the result of the above function.

# PROBLEM 2 - EVALUATING MISSING VALUES
# Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPS)
table(CPS$Region, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
# Determine if there is a pattern in the missing values of the Married variable.
# Answer based on the results of the above functions.
# How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)?
# For this question, treat the District of Columbia as a state (even though it is not technically a state).
# How many states had all interviewees living in a metropolitan area? Again, treat the District of Columbia as a state.
table(CPS$State, is.na(CPS$MetroAreaCode))
# Answer based on the result of the above function.
# Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))
# Answer based on the result of the above function.
# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
# Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?
tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)
# Answers based on the results of the above function.

# PROBLEM 3 - INTEGRATING METROPOLITAN AREA DATA
MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryOfBirthCode <- read.csv("CountryCodes.csv")
# How many observations (codes for metropolitan areas) are there in MetroAreaMap?
str(MetroAreaCode)
# How many observations (codes for countries) are there in CountryMap?
str(CountryOfBirthCode)
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
# What is the name of the variable that was added to the data frame by the merge() operation?
summary(CPS)
str(CPS)
# Review the new version of the CPS data frame with the summary() and str() functions. What is the name of the variable
# that was added to the data frame by the merge() operation?
# Answer based on the result of the above function.
# How many interviewees have a missing value for the new metropolitan area variable? Note that all of these interviewees
# would have been removed from the merged data frame if we did not include the all.x=TRUE parameter.
# Answer based on the result of the above function.
# Which of the following metropolitan areas has the largest number of interviewees?
sort(table(CPS$MetroArea))
# Answer based on the result of the above function.
# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? Hint: Use tapply() with 
# mean, as in the previous subproblem. Calling sort() on the output of tapply() could also be helpful here.
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean, na.rm = T))
# Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an interviewee is Asian, determine the
# number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean, na.rm=T))
# Answer based on the result of the above function.
# Which metropolitan area has the smallest proportion of interviewees who have received no high school diploma.
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=T))

# PROBLEM 4 - INTEGRATING COUNTRY OF BIRTH DATA
CPS <- merge(CPS, CountryOfBirthCode, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
# What is the name of the variable added to the CPS data frame by this merge operation?
# Answer based on the result of the above function.
# How many interviewees have a missing value for the new country of birth variable?
sum(is.na(CPS$Country))
# Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$CountryOfBirthCode))
# What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have
# a country of birth that is not the United States? 
 tapply(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA" , CPS$Country.y != "United States" , mean, na.rm = TRUE)
# Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India?
sort(tapply(CPS$Country=="India", CPS$MetroArea, sum, na.rm=T))
# in Brazil
sort(tapply(CPS$Country=="Brazil", CPS$MetroArea, sum, na.rm=T))
# in Somalia
sort(tapply(CPS$Country=="Somalia", CPS$MetroArea, sum, na.rm=T))
