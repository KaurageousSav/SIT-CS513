# Course : CS513-B Knowledge Discovery and Data Mining
# Name   : Savleen Kaur
# Id     : 10476867
# HW_08_Cluster

rm(list=ls())

library(cluster)
install.packages("fpc")
library(fpc)

#Importing csv to r
HW_08 <- read.csv("wisc_bc_ContinuousVar.csv", header = TRUE, sep = ",", na.strings = c("?"))
HW_08
nrow(HW_08)

table(HW_08$diagnosis)

#removing missing values
HW_08<-na.omit(HW_08)
View(HW_08)
nrow(HW_08)


HW_08<-HW_08[-1]
HW_08_dist<-dist(HW_08[,-1])
View(HW_08_dist)


#Hcluster
final_hclust<-hclust(HW_08_dist)

plot(final_hclust)

dev.off()

hclust_2<-cutree(final_hclust,2)
hclust_2
test<-table(hclust_2,HW_08[,1])

#Accuracy
Accuracy <-(sum(diag(test))/(sum(rowSums(test)))*100) 
Accuracy


