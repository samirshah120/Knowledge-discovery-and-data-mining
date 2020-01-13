#Name : Samir Jesal Shah
# CWID : 10445681

#Clear Objects from memory
rm(list = ls())
#Read CSV
wisconsin <- read.csv("C://Users/samir/Downloads/breast-cancer-wisconsin.data.csv",na.strings = '?')
wisconsin1 <- na.omit(wisconsin)
#Factorizing Data
wisconsin1$Class <- factor(wisconsin1$Class, levels = c(2,4),labels = c("Benign", "Malignant"))
View(wisconsin1)


str(wisconsin1)
samp <- sample(1:nrow(wisconsin1), size=round(0.7*nrow(wisconsin1)), replace=FALSE)

#Create training and testing dataset
train <- wisconsin1[samp,]  #Only takes rows that are in samp
test <- wisconsin1[-samp,]


install.packages('e1071', dependencies = TRUE)
library(e1071)
library(class)


##Naive Bayes Algo
nBayes <- naiveBayes(Class~., data=train)
category <- predict(nBayes, test)

table(NBAYES=category,Class=test$Class)
not_match<-sum(category!=test$Class)
error_Rate<-not_match/length(category)

#Accuracy of Naive Bayes
accuracy <- 1-error_Rate
accuracy








