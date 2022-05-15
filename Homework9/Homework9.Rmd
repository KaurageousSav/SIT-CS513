# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# Project     : SVM


remove(list=ls())

#Importing csv to r
HW_09 <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, sep = ",", na.strings = c("?"))
View(HW_09)

#Removing rows with missing values
HW_09<-na.omit(HW_09)

View(HW_09)

HW_09$diagnosis<- factor(HW_09$diagnosis)
is.factor(HW_09$diagnosis)

#Dividing dataset into training and test data
index<-sort(sample(nrow(HW_09 ),as.integer(.70*nrow( HW_09))))
training<- HW_09[index,]
test<- HW_09[-index,]

library(e1071)

#Running the SVM model
svm.model <- svm(diagnosis~., data =training  )
svm.pred <- predict(svm.model,  test )

#Table
a<-table(actual=test$diagnosis,svm.pred)
a

#Error Rate
SVM_wrong<- (test$diagnosis!=svm.pred)
rate<-sum(SVM_wrong)/length(SVM_wrong)
rate

#Accuracy
Accuracy <-(sum(diag(a))/(sum(rowSums(a)))*100) 
Accuracy

