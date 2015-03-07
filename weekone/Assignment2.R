# PROBLEM 1 - SUMMARY STATISTICS

IBM <- read.csv("IBMStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
GE <- read.csv("GEStock.csv")
Boeing <- read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
###################
# Our five datasets all have the same number of observations. How many observations are there in each data set?
# Answer based on the result of str(IBM) 
# What is the earliest year in our datasets?
min(IBM$Date)
# What is the latest year in our datasets?
max(IBM$Date)
# What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice)
# What is the minimum stock price of General Electric (GE) over this time period?
min(GE$StockPrice)
# What is the maximum stock price of Coca-Cola over this time period?
max(CocaCola$StockPrice)
# What is the median stock price of Boeing over this time period?
median(Boeing$StockPrice)
# What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)

# PROBLEM 2 - VISUALIZING STOCK DYNAMICS
# Around what year did Coca-Cola has its highest stock price in this time period?
# Around what year did Coca-Cola has its lowest stock price in this time period?
plot(CocaCola$Date,CocaCola$StockPrice, col="red")
# Answers based on the plot.
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
# In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, 
# which company's stock dropped more?
abline(v=as.Date(c("2000-03-01")), lwd=2)
# Answer based on the plot.
# Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) was going up, while the
# other was going down. Which one was going up?
abline(v=as.Date(c("1983-06-15")), lwd=2)
# Answer based on the plot.
# In the time period shown in the plot, which stock generally has lower values?
# Answer based on the plot.

# PROBLEM 3 - VISUALIZING STOCK DYNAMICS 1995-2005
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="yellow")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="grey")
# Which stock fell the most right after the technology bubble burst in March 2000?
abline(v=as.Date("2000-03-01"))
# Answer based on the plot.
# Which stock reaches the highest value in the time period 1995-2005?
# Answer based on the plot.
# In October of 1997, there was a global stock market crash that was caused by an economic crisis in Asia. Comparing
# September 1997 to November 1997, which companies saw a decreasing trend in their stock price? 
abline(v=as.Date("1997-09-01"))
abline(v=as.Date("1997-11-01"))
# Answer based on the plot.
# In the last two years of this time period (2004 and 2005) which stock seems to be performing the best, in terms of increasing stock price?
abline(v=as.Date("2004-01-01"))
abline(v=as.Date("2005-01-01"))
# Answer based on the plot.

# PROBLEM 4 - MONTHLY TRENDS
# For IBM, compare the monthly averages to the overall average stock price. In which months has IBM historically had a
# higher stock price (on average)? Select all that apply.
tapply(IBM$StockPrice, months(IBM$Date), mean)
# Answer based on the result of the function
# General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?
tapply(GE$StockPrice, months(GE$Date), mean)
# Answer based on the result of the function
# For the months of December and January, every company's average stock is higher in one month and lower in the other.
# In which month are the stock prices lower?
# Answer based on the result of the previous function