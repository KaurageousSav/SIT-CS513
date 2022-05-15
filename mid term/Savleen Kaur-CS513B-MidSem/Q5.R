# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# MIDTERM Q5  

#Construct a CART model to classify attrition (attrition=’yes’) based on the other 
#variables. Predict attrition for a random sample (30%) of the data (test dataset). 
#Measure the accuracy of the model.

rm(list=ls())

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

#Importing csv to r
midtermq5 <- read.csv("IBM_attrition_v2.CSV", header = TRUE, sep = ",", na.strings = c("?"))
midtermq5

#Remove Missing Values
midtermq5_notmissing<-na.omit(midtermq5)
nrow(midtermq5_notmissing)


for(i in seq(from=1, to=nrow(midtermq5_notmissing), by=1))
{
  
  if(midtermq5_notmissing$MonthlyIncome[i] < 3000) 
  {
    midtermq5_notmissing$MonthlyIncome [i] <- "up to 3000";
  } 
  else if(midtermq5_notmissing$MonthlyIncome[i] >= 3000 && midtermq5_notmissing$MonthlyIncome[i] < 5000) 
  {
    midtermq5_notmissing$MonthlyIncome [i] <- "3000 up to 5,000.00";
  } 
  
  else if(midtermq5_notmissing$MonthlyIncome[i] >= 5000 && midtermq5_notmissing$MonthlyIncome[i] < 8500) 
  {
    midtermq5_notmissing$MonthlyIncome [i] <- "5000 up to 8,500";
  } 
  
  if(midtermq5_notmissing$MonthlyIncome[i] >= 8500) 
  {
    midtermq5_notmissing$MonthlyIncome [i] <- "8500 or more";
  } 
  
}

#categorizing Age
midtermq5_notmissing[midtermq5_notmissing$Age<31,"Age"]<-"<31"
midtermq5_notmissing[midtermq5_notmissing$Age>=31 & midtermq5_notmissing$Age<38,"Age"]<-"31-38"
midtermq5_notmissing[midtermq5_notmissing$Age>=38 & midtermq5_notmissing$Age<=48,"Age"]<-"38-48"
midtermq5_notmissing[midtermq5_notmissing$Age>48,"Age"]<-">48"
View(midtermq5_notmissing)

#get same data
set.seed(111)


#factoring remaining column
midtermq5_notmissing$Attrition= factor(midtermq5_notmissing$Attrition, labels = c("yes","no"))
midtermq5_notmissing
is.factor(midtermq5_notmissing$Attrition)

#splitting training and test data
cst <- sort(sample(nrow(midtermq5_notmissing),as.integer(.70*nrow(midtermq5_notmissing))))
training <- midtermq5_notmissing[cst,]
nrow(training)
test <- midtermq5_notmissing[-cst,]
nrow(test)


#Growing the tree
dev.off()

#plot the tree
CART_class<-rpart(Attrition~.,data=training)

rpart.plot(CART_class)
summary(CART_class)

CART_predict2<-predict(CART_class, test, type="class") 

#Creating the Table
a <-table(Actual=test$Attrition,CART=CART_predict2) #diagonals are misclassified 
a

#Percentage accuracy
match<-(test$Attrition==CART_predict2)*100
accuracy<-sum(match)/length(match)
accuracy


#error rate
wrong<- sum(test$Attrition!=CART_predict2)
c_rate<-wrong/length(test$Attrition)
c_rate


prp(CART_class)
fancyRpartPlot(CART_class)
