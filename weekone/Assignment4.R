# PROBLEM 1 - LOADING AND SUMMARIZING THE DATASET  
poll <- read.csv("AnonymityPoll.csv")
# How many people participated in the poll?
nrow(poll)
# How many interviewees responded that they use a smartphone?
sum(poll$Smartphone==1, na.rm=T)
# How many interviewees responded that they don't use a smartphone?
sum(poll$Smartphone==0, na.rm=T)
# How many interviewees did not respond to the question, resulting in a missing value, or NA, in the summary() output?
summary(poll$Smartphone)
sum(is.na(poll$Smartphone))
# Which of the following are states in the Midwest census region?
# Which was the state in the South census region with the largest number of interviewees?
table(poll$State, poll$Region)

# PROBLEM 2 - INTERNET AND SMARTPHONE USERS 
table((poll$Smartphone), (poll$Internet.Use))
# How many interviewees reported neither Internet use nor smartphone use?
# Answer based on the result of the above function
# How many interviewees reported both Internet use and smartphone use?
# Answer based on the result of the above function
# How many interviewees reported Internet use but no smartphone use?
# Answer based on the result of the above function
# How many interviewees reported smartphone use but no Internet use?
# Answer based on the result of the above function
# How many interviewees have a missing value for their Internet use?
sum(is.na(poll$Internet.Use))
# How many interviewees have a missing value for their smartphone use?
sum(is.na(poll$Smartphone))
# Use the subset function to obtain a data frame called "limited", which is limited to interviewees who
# reported Internet use or who reported smartphone use. How many interviewees are in the new data frame?
limited <- subset(poll, poll$Internet.Use==1 | poll$Smartphone==1)
str(limited)

# PROBLEM 3 - SUMMARIZING OPINIONS ABOUT INTERNET PRIVACY  
# Which variables have missing values in the limited data frame? 
summary(limited)
# Answer based on the result of the above function
# What is the average number of pieces of personal information on the Internet, according to the Info.On.Internet variable?
mean(limited$Info.On.Internet)
# How many interviewees reported a value of 0 for Info.On.Internet?
table(limited$Info.On.Internet)
# How many interviewees reported the maximum value of 11 for Info.On.Internet?
# Answer based on the result of the above function
# What proportion of interviewees who answered the Worry.About.Info question worry about how much information
# is available about them on the Internet?
sum(limited$Worry.About.Info==1, na.rm=T)/nrow(limited -2) # 2 is the number of NA in this column
# What proportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet?
sum(limited$Anonymity.Possible==1, na.rm=T)/(nrow(limited)-39) # 39 is the number of NA in this column
# What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the Internet?
sum(limited$Tried.Masking.Identity==1, na.rm=T)/(nrow(limited)-8) # 8 is the number of NA in this column
# What proportion of interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective?
sum(limited$Privacy.Laws.Effective==1, na.rm=T)/(nrow(limited)-65) # 65 is the number of NA in this column

# PROBLEM 4 - RELATING DEMOGRAPHICS TO POLLING RESULTS  
# Build a histogram of the age of interviewees. What is the best represented age group in the population?
hist(limited$Age)
# Answer based on the result of the above function
# What is the largest number of interviewees that have exactly the same value in their Age variable AND the same value in their Info.On.Internet variable? 
plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info.On.Internet)
# Experimenting with the command jitter(c(1, 2, 3)), what appears to be the functionality of the jitter command?
# Answer based on the result of jitter function.
# Now, plot Age against Info.On.Internet with plot(jitter(limited$Age), jitter(limited$Info.On.Internet)). 
# What relationship to you observe between Age and Info.On.Internet?
# Answer based on the result of the above function.
# Use the tapply() function to obtain the summary of the Info.On.Internet value, broken down by whether an interviewee is a smartphone user.
tapply(limited$Info.On.Internet, limited$Smartphone, mean)
# What is the average Info.On.Internet value for smartphone users?
# Answer based on the result of the above function.
# What is the average Info.On.Internet value for non-smartphone users?
# Answer based on the result of the above function.
tapply(limited$Tried.Masking.Identity, limited$Smartphone, mean, na.rm=T)
# What proportion of smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
# Answer based on the result of the above function.
# What proportion of non-smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
# Answer based on the result of the above function.