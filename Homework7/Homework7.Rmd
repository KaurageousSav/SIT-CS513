# Course : CS513-B Knowledge Discovery and Data Mining
# Name   : Savleen Kaur
# Id     : 10476867
# HW_07_ANN

rm(list=ls())

install.packages("neuralnet")
library("neuralnet")

#Importing csv to r
HW_07 <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, na.strings = c("?"))
HW_07

#Convert Class Column to Factor
HW_07$diagnosis<-factor(HW_07$diagnosis, labels = c("M","B"))
View(HW_07$diagnosis)
is.factor(HW_07$diagnosis)

#Convert Class Column to Factor
table(HW_07$diagnosis)
HW_07<-data.frame(lapply(na.omit(HW_07),as.numeric))
View(HW_07)

idx<-sort(sample(nrow(HW_07),as.integer(.70*nrow(HW_07))))
idx
training<-HW_07[idx,]
nrow(training)
test<-HW_07[-idx,]
nrow(test)
?neuralnet()

model<- neuralnet(diagnosis~.,training[-1], hidden=5, threshold=0.01)

#Plot the neural network
plot(model)

#Test should have only the input colum
ann <-compute(model,test)
ann$net.result 

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)
length(test$diagnosis)
table(ann_cat,test$diagnosis)

wrong<- (test$diagnosis!=ann_cat)
errorRate<-sum(wrong)/length(wrong)
errorRate

Accuracy<- 1- errorRate
Accuracy


