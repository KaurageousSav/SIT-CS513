# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 1076867
##HW_03_knn

##Use the knn methodology (k=3,5 and 10) to develop a classification models for 
##the Diagnosis. 
##Important: make sure your categories are represented by the “factor” data 
##type in R and delete the rows with missing value. Use 30% test 70% training data.


## remove all objects
rm(list=ls())

HW_03_knn <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
View(HW_03_knn)


#Remove Missing Values
HW_03_notmissing<-na.omit(HW_03_knn)


#Convert Class Column to Factor
HW_03_notmissing$Class<-factor(HW_03_notmissing$Class, levels = c(2,4), labels = c("benign","malignment"))
View(HW_03_notmissing$Class)
is.factor(HW_03_notmissing$Class)


#30% test data & 70% training data
idx<-sort(sample(nrow(HW_03_notmissing),as.integer(.70*nrow(HW_03_notmissing))))
idx
training<-HW_03_notmissing[idx,-1]
nrow(training)
test<-HW_03_notmissing[-idx,-1]
nrow(test)


#Use the R library("kknn") 
library(kknn)

#when the value of k=3
predict_k3<-kknn(formula = Class~., training, test, k=3, kernel = "triangular")
fit <- fitted(predict_k3)
n3 <- table(Actual=test$Class,Fitted=fit)
n3


#Accuracy
accuracy_k3 <- {sum(diag(n3)/(sum(rowSums(n3)))) * 100}
accuracy_k3


#when the value of k=5
predict_k5<-kknn(formula = Class~., training, test, k=5, kernel = "triangular")
fit <- fitted(predict_k5)
n5 <- table(Actual=test$Class,Fitted=fit)
n5

#Accuracy
accuracy_k5 <- {sum(diag(n5)/(sum(rowSums(n5)))) * 100}
accuracy_k5


#when the value of k=10
predict_k10<-kknn(formula = Class~., training, test, k=10, kernel = "triangular")
fit <- fitted(predict_k10)
n10 <- table(Actual=test$Class,Fitted=fit)
n10

#Accuracy
accuracy_k10 <- {sum(diag(n10)/(sum(rowSums(n10)))) * 100}
accuracy_k10

