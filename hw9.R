#Samir Shah
#10445681
#Assignment 9 Hclust and K means
rm(list=ls())
#load dataset
wisconsin<-read.csv("C://Users/samir/Downloads/wisc_bc_ContinuousVar.csv",na.strings = '?')#Change the path accordingly.
View(wisconsin)
summary(wisconsin)
table(wisconsin$diagnosis)
#Remove rows with missing values
cancer<-na.omit(wisconsin)
wisconsin<-wisconsin[-1]
wisconsin_dist<-dist(wisconsin[,-1])
#Use hclust
hclust_results<-hclust(wisconsin_dist)
plot(hclust_results)
hclust_1<-cutree(hclust_results,2)
#Fit to table
table(hclust_1,wisconsin[,1])


rm(list=ls())
#load dataset
wisconsin<-read.csv("C://Users/samir/Downloads/wisc_bc_ContinuousVar.csv",na.strings = '?')#Change the path accordingly.
View(wisconsin)
summary(wisconsin)
table(wisconsin$diagnosis)
#Omit missing values
wisconsin<-na.omit(wisconsin)
wisconsin<-wisconsin[-1]
#use k means
kmeans_2<- kmeans(wisconsin[,-1],2,nstart = 10)
kmeans_2$cluster
#Fit to table
table(kmeans_2$cluster,wisconsin[,1])