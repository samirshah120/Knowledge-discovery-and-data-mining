#Samir Shah
#10445681
#Random Forest 
rm(list = ls())
#load dataset
wisconsin <- read.csv("C://Users/samir/Downloads/breast-cancer-wisconsin.data.csv",na.strings = '?')
View(wisconsin)
summary(wisconsin)
#Factor the data
wisconsin$Class <- factor(wisconsin$Class, levels = c(2,4),labels = c("Benign","Malignant") )
View(wisconsin)
wisconsin <- na.omit(wisconsin)
install.packages('randomForest')
library(randomForest)
#Divide dataset into test and training 
index <- sample(1:nrow(wisconsin),size = (0.7*nrow(wisconsin)),replace = FALSE)
training <- wisconsin[index,]
test <-wisconsin[-index,]
#implement random forest
rforest <- randomForest( Class~., data=training[-1], importance=TRUE, ntree=1000)
importance(rforest)
varImpPlot(rforest)
Prediction <- predict(rforest, test)
#Fit to table
table(actual=test$Class,Prediction)
wrong<- (test$Class!=Prediction )
#Error rate 
error<-sum(wrong)/length(wrong)
error 
