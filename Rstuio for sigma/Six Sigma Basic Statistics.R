# SIX SIGMA - BASIC STATISTICS
# Coursera Guided Project
# Instructor - Moses Gummadi

# This is an R Script file
# You can open this in R Studio
# Use # symbol for comments in R Script file

# R Studio is already downloaded in your Rhyme Desktop
# Use this link https://rstudio.com/products/rstudio/
# Choose the RStudio Desktop Free Option

# Load Custom R Functions Script
# "R Functions for Six Sigma.R"

source("https://pastebin.com/raw/YSanir6B")


# Data Types - Integers & Numeric

a = 7
b = 3.14159

a + b
a - b

a*b
a/b

1/a
a^b

# Data Types - Character (String)

c = "Six"
d = "Sigma"

c + d

cat(c,d)

cat(c,d, sep = "")

# Remove Variables from the Environment

rm(a,b,c,d)

# Clear Console
cat("\f")

#   Vectors

NumVector = c(1,3,5,7)
NumVector; NumVector + 3
NumVector; NumVector - 2

NumVector[2]  # access the 2nd element of vector NumVector
NumVector[4]  # access the 4th element of vector NumVector

#  Generate Vector Data

1:10 # a vector of integers from 1 to 10

seq(1,10,2) # seq(from, to, by)

runif(10)   # 10 values uniformly distributed between 0 and 1

runif(10, 10, 20)   # runif(n, min, max)

sample(1:10) # sample digits randomly from 1 to 10

sample(1:10, 3) # sample any 3 digits randomly from 1 to 10


#------   Task 1 - Load Data Into RStudio --------------------------------
dt = data # to copy the data frame into "dt"

dt = read.csv("~/data.csv")

View(dt) 
dt 
class(dt)
nrow(dt)
head(dt)  
tail(dt)  
dt[1:10,]  
dt[101:120,] 
dt1 = subset(dt, Category == "Minor Injury")
head(dt1)
dt$TimeToAdmit[21:30]


#------   Task 2 - Calculate Centering & Spread  ------------------------
# Centering - Mean, Median, Mode

x = dt$TimeToAdmit  # copy TimeToAdmit column into x

n = length(dt$TimeToAdmit) # number of elements in the vector

mean(x)   # average value of TimeToAdmit
mean(x, na.rm = TRUE) # average after removing any "NA" values

round(mean(x),2)   # rounded to 2 decimals

median(x)   # median (middle value) in TimeToAdmit

Mode(x)    # mode (most frequent value) of TimeToAdmit

# Spread - Standard Deviation, Coefficient of Variability (CV)

y = x - mean(x)     # deviations from the mean

y[1:20]

Variance = sum(y^2)/(n - 1)  # Variance is sum of deviations from the mean

sqrt(Variance)  # Standard Deviation is suqare root of variance

sd(x)     # standard deviation

# Spread - Percentile, Inter Quartile Range, SPAN

quantile(x, 0.2)
quantile(x, 0.6)

IQR = as.numeric(quantile(x, 0.75) - quantile(x, 0.25))
SPAN = as.numeric(quantile(x, 0.95) - quantile(x, 0.05))
RANGE = max(x) - min(x)

IQR; SPAN; RANGE

# Data Summary

summary(x)
summary(dt)
summary(dt[c(2,3)])

# Summary Stats - Custom Function
SummaryStats(dt$TimeToAdmit)


#------   Task 3 - Statistical Sampling  ------------------------
# Sample 50 patients out of 15,000 (population) to calculate Mean

source("https://pastebin.com/raw/YSanir6B")
dt = read.csv("~/data.csv")
x = dt$TimeToAdmit

mean(x); sd(x)

y = sample(x,50)
mean(y); sd(y)


# Collect a sample size of 30 a 1000 times, out of the population

p = SampledMeans(x,50,1000)
p

mean(p); mean(x)
sd(p); sd(x)

# Standard Error of the Mean
se = sd(x)/sqrt(50)

# Confidence Intervals for Mean (95% for 50 sample size)
mean(x) - 1.96 * se 
mean(x) + 1.96 * se

# Confidence Intervals for Mean (90% for 50 sample size)
mean(x) - 1.64 * se 
mean(x) + 1.64 * se

# Confidence Intervals for Mean (99% for 50 sample size)
mean(x) - 2.58 * se 
mean(x) + 2.58 * se

#------   Task 4 - Histogram, Boxplot, Pareto Chart --------------

source("https://pastebin.com/raw/YSanir6B")
dt = read.csv("~/data.csv")
x = dt$TimeToAdmit

# Histogram 
hist(x)
hist(x, col = "red")
hist(x, col = "gold2")
hist(x, col = "gold2", freq = FALSE)

# For color names in R 
# visit http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# Histogram Overlayed with Normal Curve
hist(x, col = "gold2", freq = FALSE, 
     xlab = "Time To Admit",
     main = "Histogram of 'Time To Admit'")
curve(dnorm(x, mean = mean(x), sd = sd(x)), 
      col="red", lwd=2, add=TRUE)

# Boxplot of "Time to Admit" (x) 
boxplot(x, col = "gold2")

# Boxplot of "Time to Admit" (x) - Horizontal
boxplot(x, col = "gold2", horizontal = TRUE)

# Boxplot of "Time to Admit" (x) by Category
boxplot(TimeToAdmit ~ Category, dt, col = "gold2")

# Boxplot of "Time to Admit" (x) by Category - Alternarive
a = subset(dt, Category == "Major A&E")$TimeToAdmit
b = subset(dt, Category == "Speciality")$TimeToAdmit
c = subset(dt, Category == "Minor Injury")$TimeToAdmit

boxplot(a,b,c, 
        names = c("Major A&E", "Speciality", "Minor Injury"),
        main = "Box Plots - Time To Admit By Category",
        col = "gold2")

# Pareto Chart (Input Category Column, Name)
ParetoPlot(dt$Category, "A&E Category")

#------   Task 5 - Generate Distributions  --------------

# Uniform Distribution
runif(30)       # 30 samples between 0 and 1
runif(30,5,10)  # 30 samples between 5 and 10
hist(runif(30,5,10), col = "gold2")
hist(runif(30000,5,10), col = "gold2")
hist(runif(30000000,5,10), col = "gold2")


# Normal Distribution
rnorm(20)       # 20 samples, Mean = 0 and SD = 1
rnorm(20,10,2)  # 20 samples, Mean = 10 and SD = 2
mean(rnorm(20,10,2))
sd(rnorm(20,10,2))

mean(rnorm(2000000,10,2))
sd(rnorm(2000000,10,2))

hist(rnorm(20,10,2), col = "gold2")
hist(rnorm(2000000,10,2), col = "gold2", breaks = 100)
help(rnorm)

# Exponential Distribution
# The parameter lambda = 1/Mean
rexp(20)        # 20 samples, Mean = 1
rexp(20, 1/5)   # 20 samples, Mean = 5; Lambda = 1/5
hist(rexp(2000000, 1/5), col = "gold2", breaks = 100)

# Log Normal Distribution
# If Z is normally distributed (Mean, SD)
# X = exp(MeanLog + SDLog * Z) is Log Normally distributed
# Parameters - MeanLog, SDLog
# LogNormal distribution is skewed to the left 

rlnorm(20)     # 20 samples, MeanLog = 0, SDLog = 1

# GetLogMeanSD is custom R function 
# To get MeanLog and SDLog for a given Mean and SD

GetLogMeanSD(50,20)
MeanLog = GetLogMeanSD(50,20)[1]
SDLog   = GetLogMeanSD(50,20)[2]

x = rlnorm(1000000, MeanLog, SDLog)
hist(x, col = "gold2", breaks = 50)

mean(x); sd(x)


#------   Task 6 - Fit Distributions  --------------

require(fitdistrplus)
require(nortest)

source("https://pastebin.com/raw/YSanir6B")
dt = read.csv("~/data.csv")
x = dt$TimeToAdmit


# Fit distribution (Normal) - p > 0.05 for fit
# Anderson Darling Test (library = nortest)
ad.test(x)

# Fit distribution (Normal - Kolmogorov-Smirnov test)
d = fitdist(x,"norm")
summary(d)
plot(d)

# Fit distribution (Exponential)
d = fitdist(x, "exp")
summary(d)
plot(d)

# Fit distribution (Log-Normal)
d = fitdist(x,"lnorm")
summary(d)
plot(d)

# Anderson Darling Normality after Log transform
ad.test(log(x))

