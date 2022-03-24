###--------------------------------------
#Student Name: SRIVEN SACHIT KALLEPALLY
#GNumber: G01354525
###--------------------------------------

rm(list=ls())

#part 1 EmployeeAttrition.csv

#checking working directory
getwd()

#reading the dataset
data <- read.csv("EmployeeAttrition.csv", header = TRUE, sep = ",") #print
data #print

# this is just for testing to use "print" statement.
print(data[1,])

# a. Find the number of rows and columns in the dataset (5 points)
nrow(data) #print for finding number of rows
ncol(data) #print for finding number of columns


# b. Find the maximum Age in the dataset (5 points)
max(data$Age) #print for maximum age in the dataset
min(data$Age) #print for minimum age in the dataset


# c. Find the minimum DailyRate in the dataset (5 points)
min(data$DailyRate) #print for minimum DailyRate in the dataset


# d. Find the average/mean MontlyIncome in the dataset (5 points)
mean(data$MonthlyIncome) #print for average/mean MonthlyIncome in the dataset


# e. How many employees rated WorkLifeBalance as 1 (5 points)
length(which( data$WorkLifeBalance == 1)) #print for number of employees rated WorkLifeBalance as 1


# f. What percent of total employees have TotalWorkingYears less than equal to 5? Also calculate the percentage for TotalWorkingYears greater than 5 (5 points)
length(which(data$TotalWorkingYear<=5))/sum(data$TotalWorkingYear) #print for number of employees who have TotalWorkingYears less than equal to 5.
length(which(data$TotalWorkingYear>5))/sum(data$TotalWorkingYear) #print to calculate the percentage for TotalWorkingYears greater than 5.


# g. Print EmployeeNumber, Department and MaritalStatus for those employees whose Attrition is Yes and RelationshipSatisfaction is 1 and YearsSinceLastPromotion is greater than 3 (10 points)
data <- subset(data, RelationshipSatisfaction == 1 & Attrition == 'Yes'& YearsSinceLastPromotion > 3, select = c(EmployeeNumber, Department, MaritalStatus))
print(data) 

#install package
library(tidyverse)

# h. Find the mean, median, mode, standard deviation and frequency distribution of EnvironmentSatisfaction for males and females separately. (Hint: For frequency distribution use table() function (10 points)
data1<- data %>% filter(Gender == "Male")
data2<- data %>% filter(Gender == "Female")
Environmental1<- data1$EnvironmentSatisfaction
Environmental2<- data2$EnvironmentSatisfaction

# mean of the environment satisfaction data for males
mean(Environmental1)

# mean of the environment satisfaction data for females
mean(Environmental2)

# median of the environment satisfaction data for males
median(Environmental1)

# median of the environment satisfaction data for females
median(Environmental2)

# standard deviation of the environment satisfaction data for males
sqrt(var(Environmental1))

# standard deviation of the environment satisfaction data for females
sqrt(var(Environmental2))

# frequency distribution of the environment satisfaction data for males
table(Environmental1)

# frequency distribution of the environment satisfaction data for females
table(Environmental2)

# mode of the environment satisfaction data for males
mode_male<- function(Environmental1) {
  unique1 <- unique(Environmental1)
  tab <- tabulate(match(Environmental1, unique1))
  unique1[tab == max(tab)]
}
mode_male(Environmental1)

# mode of the environment satisfaction data for females
mode_female <- function(Environmental2) {
  unique2<- unique(Environmental2)
  tab <- tabulate(match(Environmental2, unique2))
  unique2[tab == max(tab)]
}
mode_female(Environmental2)


#part 2 Acme.csv

getwd()

#reading the dataset
data <- read.csv("Acme.csv", header = TRUE, sep = ",") #print
data

# 1. Identify data types for each attribute in the dataset (5 points)
class(data$Years)
class(data$StSalary)
class(data$Gender)
class(data$Degree)

# 2. Produce a summary statistic for each attribute in the dataset (5 points)
summary(data$Years)
summary(data$StSalary)
summary(data$Gender)
summary(data$Degree)


# 3. Produce visualizations for each attribute (Hint: use hist() function) (5 points)
hist(data$Years)
hist(data$StSalary)
hist(data$Gender)
hist(data$Degree)

# 4a. Years of Experience and Starting Salary for all employees (5 points)
plot(data$Years,data$StSalary)
lines(lowess(data$Years,data$StSalary),col= "black")

# 4b. Years of Experience and Starting Salary for each gender (5 points)
library(dplyr) #installpackage

datam<- data %>% filter(Gender == "M")
dataf<- data %>% filter(Gender == "F")

Yearsm<-data$Years
Salarym<-data$StSalary
plot(Yearsm,Salarym)
lines(lowess(Yearsm,Salarym),col= "black")

Yearsf<- data$Years
Salaryf<-data$StSalary
plot(Yearsf,Salaryf)
lines(lowess(Yearsf,Salaryf),col= "black")

# 4c. Years of Experience and Starting Salary for each degree (5 points)
data1<- data %>% filter(Degree == "BS")
data2<- data %>% filter(Degree == "MS")
data3<- data %>% filter(Degree == "PhD")


data1Years<-data1$Years
data1Salary<-data1$StSalary
plot(data1Years,data1Salary)
lines(lowess(data1Years,data1Salary),col= "black")



data2Years<-data2$Years
data2Salary<-data2$StSalary
plot(data2Years,data2Salary)
lines(lowess(data2Years,data2Salary),col= "black")


data3Years<-data3$Years
data3Salary<-data3$StSalary
plot(data3Years,data3Salary)
lines(lowess(data3Years,data3Salary),col= "black")

# 5. Find the correlation between Starting Salary and Years of Experience? (5 points)
correlation <- cor.test(data$StSalary, data$Years, method = "pearson")
correlation

# 5a. Is the correlation different for each gender? (5 points)
#male
correlationm<-cor.test(Salarym,Yearsm,method = "pearson")
correlationm

#female
correlationf<-cor.test(Salaryf,Yearsf,method = "pearson")
correlationf
#hence we can clearly see that it is different for males and females

# 5b. Is the correlation different for each degree? (5 points)

#data1 BS
correlation<-cor.test(data1Salary,data1Years,method = "pearson")
correlation

#data2 MS
correlation<-cor.test(data2Salary,data2Years,method = "pearson")
correlation

#data3PhD
correlation<-cor.test(data3Salary,data3Years,method = "pearson")
correlation

# 6. What can you conclude about Acme with respect to gender bias after your overall analysis? (5 points)
We can conclude that the females earn less than the average of what the males earn where the experience
of the males and the females are same as 1-6 years. We can also conclude that, with more than 6 years of 
experience, there are 14 male employees and there is only one female employee with the same amount of
experience. Hence, this is the conclution about the acme with respect to the gender bias after the overall
analysis.
                                                                                            


