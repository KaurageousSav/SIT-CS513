# Course : CS513-B Knowledge Discovery and Data Mining
# Name   : Savleen Kaur
# Id     : 10476867
# HW_08_Cluster-K means

rm(list=ls())

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

#kmeans
kmeans_A<- kmeans(HW_08[,-1],2,nstart = 10)
kmeans_A$cluster
plotcluster(HW_08[,-1], kmeans_A$cluster)
kmeans_A$centers
table(kmeans_A$cluster,HW_08[,1])


