# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# Homework    : HW_06_C50


rm(list=ls())

#install.packages("C50", repos="http://R-Forge.R-project.org")
install.packages("C50")
library('C50')

#Importing csv to r
HW_06 <- read.csv("breast-cancer-wisconsin.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_06


#Convert Class Column to Factor
HW_06$Class<-factor(HW_06$Class, levels = c(2,4), labels = c("benign","malignment"))
View(HW_06$Class)
is.factor(HW_06$Class)

#get same data
set.seed(111)


#30% test data & 70% training data
idx<-sort(sample(nrow(HW_06),as.integer(.70*nrow(HW_06))))
idx
training<-HW_06[idx,]
nrow(training)
test<-HW_06[-idx,]
nrow(test)

#C50
C50_class <- C5.0(Class~.,training[,-1] )
summary(C50_class )
dev.off()
plot(C50_class)

C50_predict<-predict( C50_class ,test[,-1] , type="class" )
str(C50_predict)
a<-table(actual=test[,11],C50=C50_predict)

#Error rate
wrong<- (test[,11]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,11])
c50_rate

#Accuracy
Accuracy <-(sum(diag(a))/(sum(rowSums(a)))*100) 
Accuracy

