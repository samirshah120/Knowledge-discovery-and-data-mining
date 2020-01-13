rm(list=ls())
library(neuralnet)
wisconsin<-read.csv("C://Users/samir/Downloads/wisc_bc_ContinuousVar.csv",na.strings = '?')#Change the path accordingly.
View(wisconsin)
table(wisconsin$diagnosis)
#factor the data set
wisconsin<-data.frame(lapply(na.omit(wisconsin),as.numeric))

# To split the data set into test and testing 
index<-sort(sample(nrow(wisconsin),as.integer(.70*nrow(wisconsin))))
training<-wisconsin[index,]
test<-wisconsin[-index,]
dev.off()
?neuralnet()
model<- neuralnet(diagnosis~.,training[-1], hidden=5, threshold=0.01)

#Plot the neural network
plot(model)

# compute
ann <-compute(model,test)
ann$net.result 

ann_1<-ifelse(ann$net.result <1.5,1,2)
length(ann_1)
length(test$diagnosis)
table(ann_1,test$diagnosis)

wrong<- (test$diagnosis!=ann_1)
error_rate<-sum(wrong)/length(wrong)
#Error rate
error_rate
