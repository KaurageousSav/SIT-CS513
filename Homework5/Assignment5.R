# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# Homework    : HW_05_Dtree


rm(list=ls())

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

#Importing csv to r
HW_05 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_05


#Convert Class Column to Factor
HW_05$Class<-factor(HW_05$Class, levels = c(2,4), labels = c("benign","malignment"))
View(HW_05$Class)
is.factor(HW_05$Class)

#get same data
set.seed(111)


#30% test data & 70% training data
idx<-sort(sample(nrow(HW_05),as.integer(.70*nrow(HW_05))))
idx
training<-HW_05[idx,]
nrow(training)
test<-HW_05[-idx,]
nrow(test)

#Growing the tree
dev.off()

#plot the tree
CART_class<-rpart(Class~.,data=training[,-1])
rpart.plot(CART_class)
summary(CART_class)


CART_predict2<-predict(CART_class, test, type="class") 

#Creating the Table
table(Actual=test[,11],CART=CART_predict2) #diagonals are misclassified 


#Percentage accuracy
match<-(test[,11]==CART_predict2)*100
accuracy<-sum(match)/length(match)
accuracy

#error rate
wrong<- sum(test[,11]!=CART_predict2)
c_rate<-wrong/length(test[,11])
c_rate


prp(CART_class)
fancyRpartPlot(CART_class)
