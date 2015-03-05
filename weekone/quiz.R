WHO <- read.csv("WHO.csv")
names(WHO)
# What is the mean value of the "Over60" variable?
mean(WHO$Over60)
# Which country has the smallest percentage of the population over 60?
WHO[which.min(WHO$Over60),"Country"]
# Which country has the largest literacy rate?
WHO[which.max(WHO$LiteracyRate),"Country"]

# Use the tapply function to find the average child mortality rate of countries in each region.
# Which region has the lowest average child mortality rate across all countries in that region?
tapply(WHO$FertilityRate, WHO$Region, min, na.rm=T)
