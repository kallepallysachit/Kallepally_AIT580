###------------------
###Hypothesis Testing
###------------------

###Students Name: Sriven Sachit Kallepally
###GNumber: G01354525

#Setting the Working Directory
setwd("~/Desktop/AIT580/data")

rm(list=ls())

#Reading the dataset
data <- read.csv("EmployeeAttrition.csv", header = TRUE, sep = ",")
head(data)

# Your hypothesis testings here...

#Solutions

#1. If the MonthlyIncome of Males is greater than Females
#Null Hypothesis: The male MonthlyIncome is less than or equal to female
#Atlernative Hypothesis: The male MonthlyIncome is greater to female

Male <- as.numeric(unlist(subset(data, Gender == "Male",select = MonthlyIncome)))
head(Male)
Female <- as.numeric(unlist(subset(data, Gender == "Female",select = MonthlyIncome)))
head(Female)
wilcox.test(Male, Female ,paired = FALSE,alternative = "greater")

#Explaination:We accept the null hypothesis as the p-value - 0.9558 is greater than 0.05
#Clearly the MonthlyIncome of male is not greater than female


#2. If the WorkLifeBalance of Males is less than Females 
#Null Hypothesis:The male WorkLifeBalance is greater than or equal to female
#Alternative hypothesis:The male WorkLifeBalance is lesser to female

Male1 <- as.numeric(unlist(subset(data,Gender == "Male", select = WorkLifeBalance)))
Male2 <- as.numeric(unlist(subset(data,Gender == "Female", select = WorkLifeBalance)))
t.test(Male1, Male2, paired = FALSE, alternative = "less")

#Explaination:We accept the null hypothesis as the p-value - 0.4577 is greater than 0.05
#Therefore the male WorkLifeBalance is not lesser than the female


#3. If the YearsAtCompany of Single is less than Married
#Null Hypothesis:The Years of a company of a single is greater than or equal to married
#Alternative hypothesis:The Years of a company of a single is lesser than married

Year1 <- as.numeric(unlist(subset(data, MaritalStatus == "Single", select = YearsAtCompany)))
Year2 <- as.numeric(unlist(subset(data, MaritalStatus == "Married", select = YearsAtCompany)))
wilcox.test(Year1, Year2, paired = FALSE, alternative = 'less')

#Explaination:We reject the NullHypothesis as the p-value - 0.001047 is less than 0.05
#Therefore the Years at a Company of a Single is less than that of the married.


#4. If the EnvironmentalSatisfaction of Attrition = Yes is less than Attrition = No 
#Null Hypothesis:EnviromentalSatisfcation of an attrition Yes is greater than or equal to No
#Alternative hypothesis:EnviromentalSatisfcation of an attrition Yes is less than No

Environment1 <- as.numeric(unlist(subset(data, Attrition == "Yes", select = EnvironmentSatisfaction)))
Environment2 <- as.numeric(unlist(subset(data, Attrition == "No", select = EnvironmentSatisfaction)))
t.test(Environment1, Environment2, paired = FALSE, alternative = "less")

#Explaination:We reject the null hypothesis as the p-value - 0.0001046 is less than 0.05
#EnvironmentalSatisfation of an attrition = 'Yes' less than to attrition = 'No'


#5. If the MonthlyIncome of Manager is greater than Laboratory Technician 
#Null Hypothesis:The MonthlyIncome of a manager is less than or equal to the LaboratoryTechnician
#Alternative hypothesis:The MonthlyIncome of Manager is greater than the LaboratoryTechnician

Monthly1 <- as.numeric(unlist(subset(data,JobRole=="Manager",select = MonthlyIncome)))
Monthly2 <- as.numeric(unlist(subset(data,JobRole=="Laboratory Technician",select = MonthlyIncome)))
wilcox.test(Monthly1, Monthly2, paired = FALSE,alternative = "greater")

#Explaination:We reject the null hypothesis as the p-value - 2.2e-16 is less than 0.05
#Therefore The MonthlyIncome of a manager is greater than the LaboratoryTechnician


# 6. If YearsAtCompany and DailyRate are correlated with each other
#Null Hypothesis:There is no specific correlation between the Years at a Company and DailyRate
#Alternative hypothesis:There is a correlation between the Years at a Company and DailyRate

cor.test(data$YearsAtCompany, data$DailyRate, method = "pearson")

#Explaination:We accept the null hypothesis as the p-value - 0.1919 is greater than 0.05


#7. If YearsAtCompany and MonthlyIncome are correlated with each other
#Null Hypothesis: There is no specific correlation between the Years at a Company and Monthly Income
#Alternative hypothesis: There is a correlation between Years at a Company and Monthly Income

cor.test(data$MonthlyIncome, data$YearsAtCompany, method = "pearson")

#Explaination: We reject the null hypothesis as the p-value - 2.2e-16 is less than 0.05


#8. If YearsAtCompany varies depending on individual’s MaritalStatus
#Null Hypothesis:The Years at a Company does not vary depending on the Marital Status
#Alternative hypothesis:The Years at a Company does vary depending on the Marital Status

summary(aov(data$YearsAtCompany~data$MaritalStatus))

#Explaination:We reject the null hypothesis as the p-value - 0.0247 is less than 0.05


#9. If MonthlyIncome varies depending on individual’s PerformanceRating 
#Null Hypothesis: The MonthlyIncome does not vary depending on the PerformanceRating
#Alternative hypothesis: The MonthlyIncome does vary depending on the PerformanceRating

summary(aov(data$MonthlyIncome~data$PerformanceRating))

#Explaination: We accept the null hypothesis as the p-value - 0.512 is greater than 0.05


#10. If MonthlyIncome varies depending on individual’s WorkLifeBalance
#Null Hypothesis:The MonthlyIncome does not depend on the WorklifeBalance
#Alternative hypothesis: The MonthlyIncome does depend on the WorklifeBalance

summary(aov(data$MonthlyIncome~factor(data$WorkLifeBalance)))

#Explaination: We accept the null hypothesis as the p-value - 0.607 is greater than 0.05


