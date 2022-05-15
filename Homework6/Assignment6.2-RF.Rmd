# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# Homework    : HW_06_RF


rm(list=ls())

install.packages('randomForest')

#Importing csv to r
HW_06 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_06

#Remove Missing Values
HW_06_notmissing<-na.omit(HW_06)

#Convert Class Column to Factor
HW_06_notmissing$Class<-factor(HW_06_notmissing$Class, levels = c(2,4), labels = c("benign","malignment"))
View(HW_06_notmissing$Class)
is.factor(HW_06_notmissing$Class)

#get same data
set.seed(111)

#30% test data & 70% training data
idx<-sort(sample(nrow(HW_06_notmissing),as.integer(.70*nrow(HW_06_notmissing))))
idx
training<-HW_06_notmissing[idx,]
nrow(training)
test<-HW_06_notmissing[-idx,]
nrow(test)


#Random Forest
library(randomForest)

fit <- randomForest( Class~., data=training, importance=TRUE, ntree=1000)

importance(fit)
varImpPlot(fit)
dev.off()
RPrediction <- predict(fit, test)
a<-table(actual=test$Class,RPrediction)
a

#Error rate
wrong<- (test$Class!=RPrediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

#Accuracy
Accuracy <-(sum(diag(a))/(sum(rowSums(a)))*100) 
Accuracy


