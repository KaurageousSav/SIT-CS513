# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# Question    : 1

rm(list=ls())


#Importing csv to r
Q1 <- read.csv("IBM_Attrition_v3.csv", header = TRUE, sep = ",", na.strings = c("?"))
View(Q1)


#I. Summarizing each column (e.g. min, max, mean )
summary(Q1)

is.na(Q1)

#Remove Missing Values
Q1_withoutNA<-na.omit(Q1)
View(Q1_withoutNA)

Attrition_Modified <- data.frame(Q1_withoutNA)
Attrition_Modified

is.na(Attrition_Modified)

AgeLabel <- Attrition_Modified$Age
Attrition_Modified['AgeLabel'] <- AgeLabel

MonthlyIncomeLabel <- Attrition_Modified$MonthlyIncome
Attrition_Modified['MonthlyIncomeLabel'] <- MonthlyIncomeLabel

YearsAtCompanyLabel <- Attrition_Modified$YearsAtCompany
Attrition_Modified['YearsAtCompanyLabel'] <- YearsAtCompanyLabel

View(Attrition_Modified)

#categorizing MonthlyIncome
attach(Q1)
Attrition_Modified$MonthlyIncomeLabel[MonthlyIncomeLabel <= 2900 ] <- "Income 1"
Attrition_Modified$MonthlyIncomeLabel[MonthlyIncomeLabel > 2900 & MonthlyIncomeLabel <= 5000] <- "Income 2"
Attrition_Modified$MonthlyIncomeLabel[MonthlyIncomeLabel > 5000 & MonthlyIncomeLabel <= 8500] <- "Income 3"
Attrition_Modified$MonthlyIncomeLabel[MonthlyIncomeLabel > 8500] <- "Income 4"
detach(Attrition_Modified)
View(Attrition_Modified)


#categorizing YearsAtCompany
attach(Attrition_Modified)
Attrition_Modified$YearsAtCompanyLabel[YearsAtCompanyLabel <= 6] <- "Not-Senior"
Attrition_Modified$YearsAtCompanyLabel[YearsAtCompanyLabel > 6] <- "Senior"
detach(Attrition_Modified)
View(Attrition_Modified)


#categorizing Age
for(i in seq(from=1, to=nrow(Attrition_Modified), by=1))
{
  
  if(Attrition_Modified$AgeLabel[i] < 38) 
  {
    Attrition_Modified$AgeLabel [i] <- "young";
  } 
  else if(Attrition_Modified$AgeLabel[i] > 37) 
  {
    Attrition_Modified$AgeLabel[i] <- "mature";
  } 
}
View(Attrition_Modified)


Attrition_Modified <- Attrition_Modified[ -c(1,4:5) ]
View(Attrition_Modified)

write.csv(Attrition_Modified,"/Users/savleenkaur/Desktop/SIT/kdd/Savleen/Final/Attrition_Modified.csv",row.names = FALSE)

