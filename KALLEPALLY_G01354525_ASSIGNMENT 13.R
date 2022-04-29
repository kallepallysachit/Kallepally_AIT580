###------------------
###Regression and Clustering
###------------------

###Students Name: SRIVEN SACHIT KALLEPALLY
###GNumber: G01354525


rm(list=ls())

#Installing the packages
install.packages(ggplot2)

library(ggplot2)
library(ggplot2)
library(cluster)
library(ClusterR)


data <- read.csv('EmployeeAttrition.csv', header = TRUE, sep = ",")
data

# Your answers here...
#1.a.
with(cars, scatter.smooth(data$TotalWorkingYears, data$MonthlyIncome,col='blue',lpars = list(col = "blue", lwd = 3, lty = 3)))
#The monthly income also increased with the increase in totsl working years and this observation is positively correlated

#1.b.
with(cars, scatter.smooth(data$Age, data$DistanceFromHome,col='red', lpars = list(col = "blue", lwd = 3, lty = 3)))
#It is not clear whether the plot is increasing or decreasing value of variables and there isno relation between the Age and Distance.

#1.c
x <- cor.test(data$TotalWorkingYears, data$MonthlyIncome, method = "pearson")
y <- cor.test(data$Age, data$DistanceFromHome, method = "pearson")
x
y
#The correlation between the MonthlyIncome and the TotalWorkingYears is equal to 0.7728932 and it is correlated
#The correlation between the Age and Distance from Home is equal to -0.00168612

#1.d.
employee = lm(data$TotalWorkingYears ~ data$MonthlyIncome)
print(employee)
summary(employee)
# The p-value less than 0.05 is significant. Strong evidence is indicated against the null hypothesis and here the p-Value is < 2.2e-16 and relationship is significant.


#2.a.
q <- data['HourlyRate']+data['TotalWorkingYears']
print(q)
#Hourly Rate ranges between 1 to 55 by using the 3 clusters that is the cluster 2 from 55-80 and the cluster 3 ranges from 80-100.


#2.b
km <- kmeans(a,centers = 3, nstart = 20)
km$cluster
# The plot has splitted into different HourlyRate as clusters by using the 5 clusters.
#The HourlyRate ranges between 10 to 50, 50-68, 68 to 82, 75 to 100 and 83 to 100.
