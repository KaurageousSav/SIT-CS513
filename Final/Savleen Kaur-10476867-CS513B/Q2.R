# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# Question    : 2

rm(list=ls())
install.packages('randomForest')


#Importing csv to r
Q2 <- read.csv("Attrition_Modified.csv", header = TRUE, sep = ",", na.strings = c("?"))
View(Q2)


#Convert Class Column to Factor
Q2$Attrition<-factor(Q2$Attrition, labels = c("Yes","No"))
View(Q2$Attrition)
is.factor(Q2$Attrition)

#get same data
set.seed(111)

#test data & training data
idx<-seq(1,nrow(Q2),by=4)
idx
training<-Q2[-idx,]
nrow(training)
test<-Q2[idx,]
nrow(test)


#Random Forest
library(randomForest)

fit <- randomForest( Attrition~., data=training, importance=TRUE, ntree=1000)

importance(fit)
varImpPlot(fit)
dev.off()
RPrediction <- predict(fit, test)
a<-table(actual=test$Attrition,RPrediction)
a

#Error rate
wrong<- (test$Attrition!=RPrediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

#Accuracy
Accuracy <-(sum(diag(a))/(sum(rowSums(a)))*100) 
Accuracy


