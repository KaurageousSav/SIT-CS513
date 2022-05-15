# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# MIDTERM Q6   

rm(list=ls())

#Importing csv to r
midtermq6 <- read.csv("IBM_attrition_v2.csv", header = TRUE, na.strings = c("?"))
midtermq6


#Remove Missing Values
midtermq6_notmissing<-na.omit(midtermq6)


#Convert Class Column to Factor
midtermq6_notmissing$Attrition <- factor(midtermq6_notmissing$Attrition, labels = c("yes","no"))
midtermq6_notmissing
is.factor(midtermq6_notmissing$Attrition)


#30% test data & 70% training data
idx<-sort(sample(nrow(midtermq6_notmissing),as.integer(.70*nrow(midtermq6_notmissing))))
idx
training<-midtermq6_notmissing[idx,]
nrow(training)
test<-midtermq6_notmissing[-idx,]
nrow(test)


#Use the R library("kknn") 
library(kknn)

#when the value of k=3
predict_k3<-kknn(formula = Attrition~., training, test, k=3, kernel = "triangular")
fit <- fitted(predict_k3)
n3 <- table(Actual=test$Attrition,Fitted=fit)
n3


#Accuracy
accuracy_k3 <- {sum(diag(n3)/(sum(rowSums(n3)))) * 100}
accuracy_k3

