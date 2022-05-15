# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# Question    : 3

rm(list=ls())
install.packages("C50")


#Importing csv to r
Q3 <- read.csv("Attrition_Modified.csv", header = TRUE, sep = ",", na.strings = c("?"))
View(Q3)


#Convert Class Column to Factor
Q3$Attrition<-factor(Q3$Attrition, labels = c("Yes","No"))
View(Q3$Attrition)
is.factor(Q3$Attrition)

#get same data
set.seed(111)

#test data & training data
idx<-seq(1,nrow(Q3),by=4)
idx
training<-Q3[-idx,]
nrow(training)
test<-Q3[idx,]
nrow(test)


library('C50')

#C50
C50_class <- C5.0(Attrition~.,data=training )
summary(C50_class )
dev.off()
plot(C50_class)

C50_predict<-predict( C50_class ,test, type="class" )
str(C50_predict)
a<-table(actual=test$Attrition,C50=C50_predict)
a

#Accuracy
Accuracy <-(sum(diag(a))/(sum(rowSums(a)))*100) 
Accuracy

#Error rate
e<-100-Accuracy
e
