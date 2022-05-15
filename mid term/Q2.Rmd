# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# MIDTERM Q2   

rm(list=ls())

#Importing csv to r
midterm <- read.csv("IBM_Attrition_v3.csv", header = TRUE, sep = ",", na.strings = c("?"))
midterm

#Q2:
#I. Summarizing each column (e.g. min, max, mean )
summary(midterm)

File1 <- data.frame(midterm)
File1

is.na(midterm)

#II. Identifying missing values
NAdata<-File1[which(is.na(midterm[4])),]
View(NAdata)
print('Total number of missing values:')
nrow(NAdata)


#III. Replacing the numerical missing values with the “mean” of the corresponding columns
WithoutNA <-midterm[-which(is.na(midterm['MonthlyIncome'])),]
WithoutNA
meanwithNA <-mean(midterm$MonthlyIncome, na.rm = TRUE)
midterm[is.na(midterm)] <-meanwithNA
print(midterm)


#IV. Displaying the scatter plot of “Age”, “MonthlyIncome” and “YearsAtCompany”, one pair at a time
Scatterplit_df <- data.frame(midterm['Age'], midterm['MonthlyIncome'], midterm['YearsAtCompany'])
scatterplot<-plot(Scatterplit_df[1:3],  main="Scatterplot for IBM_attrition", pch=10, col = "orange")


#V. Showing box plots for columns: “Age”, “MonthlyIncome”, and “YearsAtCompany” 
boxplot(midterm[c(1,4,5)], main='Box Plot for “Age”, “MonthlyIncome”, and “YearsAtCompany”', col=rgb(0,0.8,1,alpha = 0.5))
