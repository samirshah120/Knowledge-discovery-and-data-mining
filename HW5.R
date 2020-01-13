rm(list=ls())
#load libraries
library(rpart)
library(rpart.plot)  		
library(rattle)          
library(RColorBrewer) 
#load dataset
wisconsin<-read.csv("C:/Users/samir/Downloads/breast-cancer-wisconsin.data.csv",na.strings = '?')
View(wisconsin)
wisconsin$Class <- factor(wisconsin$Class, levels = c(2,4),labels = c("Benign", "Malignant"))

id <- sample(2,nrow(wisconsin),prob = c(0.7,0.3),replace = TRUE)
#Create training and test dataset
training <- wisconsin[id == 1,]
test <- wisconsin[id == 2,]

dev.off()
model<-rpart(Class~.,training[,-1])
rpart.plot(model)
prediction<-predict(model,test[,-1],type="class") 
#Confusion Matrix
table(test$Class,prediction)
str(prediction)
wrong<-sum(test$Class!=prediction)
errorRate<-wrong/length(test$Class)
#Error rate
errorRate


library(rpart.plot)
prp(model)


# Fancy Plot
fancyRpartPlot(model)