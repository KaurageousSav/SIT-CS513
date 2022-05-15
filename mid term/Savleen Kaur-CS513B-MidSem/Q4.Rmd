# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# MIDTERM Q4  

#Construct a Naïve Bayes model to classify attrition (attrition=’yes’) based on the 
#other variables. Predict attrition for a random sample (30%) of the data (test 
#dataset). Measure the accuracy of the model.

rm(list=ls())

install.packages("e1071")
library(e1071)
library(class) 

#Importing csv to r
midtermq4 <- read.csv("IBM_attrition_v2.CSV", header = TRUE, sep = ",", na.strings = c("?"))
midtermq4

#Remove Missing Values
midtermq4_notmissing<-na.omit(midtermq4)
 
#categorizing MonthlyIncome
for(i in seq(from=1, to=nrow(midtermq4_notmissing), by=1))
{
  
  if(midtermq4_notmissing$MonthlyIncome[i] < 3000) 
  {
    midtermq4_notmissing$MonthlyIncome [i] <- "up to 3000";
  } 
  else if(midtermq4_notmissing$MonthlyIncome[i] >= 3000 && midtermq4_notmissing$MonthlyIncome[i] < 5000) 
  {
    midtermq4_notmissing$MonthlyIncome [i] <- "3000 up to 5,000.00";
  } 
  
  else if(midtermq4_notmissing$MonthlyIncome[i] >= 5000 && midtermq4_notmissing$MonthlyIncome[i] < 8500) 
  {
    midtermq4_notmissing$MonthlyIncome [i] <- "5000 up to 8,500";
  } 
  
  if(midtermq4_notmissing$MonthlyIncome[i] >= 8500) 
  {
    midtermq4_notmissing$MonthlyIncome [i] <- "8500 or more";
  } 
  
}

#categorizing Age
midtermq4_notmissing[midtermq4_notmissing$Age<31,"Age"]<-"<31"
midtermq4_notmissing[midtermq4_notmissing$Age>=31 & midtermq4_notmissing$Age<38,"Age"]<-"31-38"
midtermq4_notmissing[midtermq4_notmissing$Age>=38 & midtermq4_notmissing$Age<=48,"Age"]<-"38-48"
midtermq4_notmissing[midtermq4_notmissing$Age>48,"Age"]<-">48"
View(midtermq4_notmissing)


#factoring remaining column
midtermq4_notmissing$Attrition <- factor(midtermq4_notmissing$Attrition, labels = c("yes","no"))
midtermq4_notmissing
is.factor(midtermq4_notmissing$Attrition)


#splitting training and test data
cst <- sort(sample(nrow(midtermq4_notmissing),as.integer(.70*nrow(midtermq4_notmissing))))
training <- midtermq4_notmissing[cst,]
test <- midtermq4_notmissing[-cst,]


# Naive Bayes classification
nBayes_class <- naiveBayes(formula = Attrition~., data =training)
category_class<-predict(nBayes_class,test)


a4 <-table(NBayes=category_class,Class=test$Attrition)
a4

NB_wrong<-sum(category_class!=test$Attrition)


#Accuracy
accuracy_a4 <- {sum(diag(a4)/(sum(rowSums(a4)))) * 100}
accuracy_a4

#calculating error rate
NB_error_rate<-NB_wrong/length(category_class)
NB_error_rate
