#Savleen Kaur
#HW_04
#CWID- 10476867

#Use the Naïve Bayes methodology to develop a classification model for the 
#Diagnosis. Important: make sure your categories are represented by the “factor” data 
#type in R and delete the rows with missing values. Use 30% test 70% training data

rm(list=ls())

install.packages("e1071")
library(e1071)
library(class) 

#Importing csv to r
HW_04 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_04

#Remove Missing Values
HW_04_notmissing<-na.omit(HW_04)


#Convert Class Column to Factor
HW_04_notmissing$Class<-factor(HW_04_notmissing$Class, levels = c(2,4), labels = c("benign","malignment"))
View(HW_04_notmissing$Class)
is.factor(HW_04_notmissing$Class)


#30% test data & 70% training data
idx<-sort(sample(nrow(HW_04_notmissing),as.integer(.70*nrow(HW_04_notmissing))))
idx
training<-HW_04_notmissing[idx,-1]
nrow(training)
test<-HW_04_notmissing[-idx,-1]
nrow(test)


# Naive Bayes classification
nBayes_class <- naiveBayes(formula = Class~., data =training)
category_class<-predict(nBayes_class,test)

head(cbind(category_class, test))

a4 <-table(NBayes=category_class,Class=test$Class)
a4

NB_wrong<-sum(category_class!=test$Class)


#Accuracy
accuracy_a4 <- {sum(diag(a4)/(sum(rowSums(a4)))) * 100}
accuracy_a4

#calculating error rate
NB_error_rate<-NB_wrong/length(category_class)
NB_error_rate


