#Savleen Kaur
#HW_02_EDA
#CWID- 10476867

rm(list=ls())

#Importing csv to r
HW_02_EDA <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_02_EDA

#I. Summarizing each column (e.g. min, max, mean )
summary(HW_02_EDA)

File1 <- data.frame(HW_02_EDA)
File1

is.na(HW_02_EDA)

#II. Identifying missing values
#F6 column only has missing values(? is present in F6)
NAdata<-File1[which(is.na(HW_02_EDA[7])),]
View(NAdata)
print('Total number of missing values:')
nrow(NAdata)


#III. Replacing the missing values with the “mean” of the column.

WithoutNA <-HW_02_EDA[-which(is.na(HW_02_EDA['F6'])),]
WithoutNA
meanwithNA <-mean(HW_02_EDA$F6, na.rm = TRUE)
HW_02_EDA[is.na(HW_02_EDA)] <-meanwithNA
print(HW_02_EDA)


#IV. Displaying the frequency table of “Class” vs. F6
install.packages('plyr')
library(plyr)
freqtable<-ddply(HW_02_EDA,.(HW_02_EDA$Class, HW_02_EDA$F6), nrow)
names(freqtable)<-c("Class", "F6", "Frequency") # names of the columns of the frequency table
View(freqtable)
print(freqtable)

#V. Displaying the scatter plot of F1 to F6, one pair at a time
scatterplot<-plot(HW_02_EDA[2:5], main="Scatterplot for Breast Cancer Data", pch=10, col = "orange")

#VI. Show histogram box plot for columns F7 to F9
#histogram box plot
boxplot(HW_02_EDA$F7~HW_02_EDA$F9,main='Histogram Box Plot for F7-F9', xlab='X-axis', ylab='Y-axis', col = "yellow",border = "blue")

# Histogram plot
hist(HW_02_EDA$F7,main='Histogram Plot for F7', xlab='X-axis', ylab='Y-axis', col = "yellow",border = "blue")
hist(HW_02_EDA$F8,main='Histogram Plot for F8', xlab='X-axis', ylab='Y-axis', col = "yellow",border = "blue")
hist(HW_02_EDA$F9,main='Histogram Plot for F9', xlab='X-axis', ylab='Y-axis', col = "yellow",border = "blue")


#2. Delete all the objects from your R- environment. Reload the “breast-cancer-wisconsin.data.csv” 
#from canvas into R. Remove any row with a missing value in any of the columns.

rm("HW_02_EDA")

HW_02_EDA_Reload <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_02_EDA_Reload
HW_02_EDA_omit <-na.omit(HW_02_EDA_Reload)
HW_02_EDA_omit
