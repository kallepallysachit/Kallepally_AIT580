#AIT580 ASIGNMENT 11 
#SRIVEN SACHIT KALLEPALLY
#G01354525

#TASK 2
#a.
#installations
library(tidyverse)
library(dplyr)
library(ggplot2)

#Setting working directory
getwd()

#Code:
data <- read.csv("employeeattrition.csv", header = TRUE, sep = ",")
hist(data$Age)

#Explaination : the following plot is the histogram for age and the above is the code for the histogram.

#b.
#Code:
u <- data$Age
v <- data$MonthlyIncome
plot(u, v, main = "The relationship between Age and Monthly_Income", xlab = "Age", ylab = "Monthly_Income", pch = 19)

#Explaination : the following plot is the Scatter Plot for age and the above is the code for the Scatter Plot.

