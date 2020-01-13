rm(list=ls())
#Load Libraries 
library(C50)
wisconsin<-read.csv("C:/Users/samir/Downloads/breast-cancer-wisconsin.data.csv",na.strings = '?')
View(wisconsin)
wisconsin$Class <- factor(wisconsin$Class, levels = c(2,4),labels = c("Benign", "Malignant"))

id <- sample(2,nrow(wisconsin),prob = c(0.7,0.3),replace = TRUE)
#Create training and test dataset
training <- wisconsin[id == 1,]
test <- wisconsin[id == 2,]

dev.off()
model<-C5.0(Class~.,training[,-1])
summary(model)
plot(model)
prediction<-predict(model,test[,-1],type="class") 
#Confusion Matrix
table(test$Class,prediction)
str(prediction)
wrong<-sum(test$Class!=prediction)
errorRate<-wrong/length(test$Class)
#Error rate
errorRate


