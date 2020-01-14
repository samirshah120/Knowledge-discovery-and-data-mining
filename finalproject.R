https://www.kaggle.com/mlg-ulb/creditcardfraud#creditcard.csv
#clean memory
remove  (list=ls());
#Load data
filename<- file.choose()
CCFD<-read.csv(filename,na.strings = "?")

View(CCFD)
#data("iris")
#View(iris)
summary(CCFD)

#load library
library(class)
library(kknn)
library(confusionMatrix)

library(e1071)
install.packages('caret', dependencies=TRUE)
library(caret)
library(class) 
library(ROCR)
library(gplots)
install.packages('corrplot',dependencies = TRUE)
library(corrplot)
library(pROC)
install.packages('randomForest')
install.packages('gridExtra',dependencies = TRUE)
library(randomForest)
library(grid)
library(ggplot2)
library(gridExtra)
library(gdata)
library(corrplot)
# clean dataset
str(CCFD)
str(test$Class)
levels(pred)
levels(test$Class)
CCFD<-data.frame(lapply(na.omit(CCFD),as.numeric))
summary(CCFD)


## factor data
CCFD$Class<-factor(CCFD$Class)


##Define max-min normalization function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

CCFD_normalized<-as.data.frame (         
  cbind( V1=mmnorm(CCFD[,1],min(CCFD[,1]),max(CCFD[,1])),
         V2=mmnorm(CCFD[,2],min(CCFD[,2]),max(CCFD[,2])),
         V3=mmnorm(CCFD[,3],min(CCFD[,3]),max(CCFD[,3])),
         V4=mmnorm(CCFD[,4],min(CCFD[,4]),max(CCFD[,4])),
         V5=mmnorm(CCFD[,5],min(CCFD[,5]),max(CCFD[,5])),
         V6=mmnorm(CCFD[,6],min(CCFD[,6]),max(CCFD[,6])),
         V7=mmnorm(CCFD[,7],min(CCFD[,7]),max(CCFD[,7])),
         V8=mmnorm(CCFD[,8],min(CCFD[,8]),max(CCFD[,8])),
         V9=mmnorm(CCFD[,9],min(CCFD[,9]),max(CCFD[,9])),
         V10=mmnorm(CCFD[,10],min(CCFD[,10]),max(CCFD[,10])),
         V11=mmnorm(CCFD[,11],min(CCFD[,11]),max(CCFD[,11])),
         V12=mmnorm(CCFD[,12],min(CCFD[,12]),max(CCFD[,12])),
         V13=mmnorm(CCFD[,13],min(CCFD[,13]),max(CCFD[,13])),
         V14=mmnorm(CCFD[,14],min(CCFD[,14]),max(CCFD[,14])),
         V15=mmnorm(CCFD[,15],min(CCFD[,15]),max(CCFD[,15])),
         V16=mmnorm(CCFD[,16],min(CCFD[,16]),max(CCFD[,16])),
         V17=mmnorm(CCFD[,17],min(CCFD[,17]),max(CCFD[,17])),
         V18=mmnorm(CCFD[,18],min(CCFD[,18]),max(CCFD[,18])),
         V19=mmnorm(CCFD[,19],min(CCFD[,19]),max(CCFD[,19])),
         V20=mmnorm(CCFD[,20],min(CCFD[,20]),max(CCFD[,20])),
         V21=mmnorm(CCFD[,21],min(CCFD[,21]),max(CCFD[,21])),
         V22=mmnorm(CCFD[,22],min(CCFD[,22]),max(CCFD[,22])),
         V23=mmnorm(CCFD[,23],min(CCFD[,23]),max(CCFD[,23])),
         V24=mmnorm(CCFD[,24],min(CCFD[,24]),max(CCFD[,24])),
         V25=mmnorm(CCFD[,25],min(CCFD[,25]),max(CCFD[,25])),
         V26=mmnorm(CCFD[,26],min(CCFD[,26]),max(CCFD[,26])),
         V27=mmnorm(CCFD[,27],min(CCFD[,27]),max(CCFD[,27])),
         V28=mmnorm(CCFD[,28],min(CCFD[,28]),max(CCFD[,28])),
         V29=mmnorm(CCFD[,29],min(CCFD[,29]),max(CCFD[,29])),
         V30=mmnorm(CCFD[,30],min(CCFD[,30]),max(CCFD[,30])),
         V31=mmnorm(CCFD[,31],min(CCFD[,31]),max(CCFD[,31]))
  )
)

View(CCFD_normalized)
summary(CCFD_normalized)

# test and tain data
index <- seq(1,nrow(CCFD),by=5)
test<-CCFD[index,]
training <-CCFD[-index,]

####
predict_k5 <- kknn(formula=Class~., training, test, k=5,kernel ="triangular" )
fit <- fitted(predict_k5)
table(test$Class,fit)

# Navie Bayes
NB<- naiveBayes(Class~., data =training ,laplace =1)
NB$apriori


test$Class <-factor(test$Class)
pred<-predict(NB,test)
caret::confusionMatrix(pred,test$Class,positive = "1") ##error

#naive Bayes fact comparision with knn
rawpred<- predict(NB,test,type = "raw")
ptest<-prediction(rawpred[,2],test$Class)
perf<-performance(ptest,"tpr","fpr")
plot(perf,colorize=T)
performance(ptest,"auc")@y.values

# Random Forest
raw<-read.csv(filename,na.strings = "?")
raw.data<- raw
sprintf("Rows: %d Columns: %d",nrow(raw.data), length(names(raw.data)))

nrows <- nrow(raw.data)
set.seed(314)
indexT <- sample(1:nrow(raw.data), 0.7 * nrows)

trainset = raw.data[indexT,]
verset =   raw.data[-indexT,]

n <- names(trainset)
rf.form <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))

trainset.rf <- randomForest(rf.form,trainset,ntree=100,importance=T)

varimp <- data.frame(trainset.rf$importance)



vi1 <- ggplot(varimp, aes(x=reorder(rownames(varimp),IncNodePurity), y=IncNodePurity)) +
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (IncNodePurity)", x="Variable", y="Variable importance (IncNodePurity)")

vi2 <- ggplot(varimp, aes(x=reorder(rownames(varimp),X.IncMSE), y=X.IncMSE)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (%IncMSE)", x="Variable", y="Variable importance (%IncMSE)")

arrangeGrob(vi1,vi2)
grid.arrange(vi1, vi2, ncol=2)

verset$predicted <- predict(trainset.rf ,verset)

plot_confusion_matrix(verset, "Random Forest with 100 trees")



plot_confusion_matrix <- function(verset, sSubtitle) {
  tst <- data.frame(round(verset$predicted,0), verset$Class)
  opts <-  c("Predicted", "True")
  names(tst) <- opts
  cf <- plyr::count(tst)
  cf[opts][cf[opts]==0] <- "Not Fraud"
  cf[opts][cf[opts]==1] <- "Fraud"
  
  ggplot(data =  cf, mapping = aes(x = True, y = Predicted)) +
    labs(title = "Confusion matrix", subtitle = sSubtitle) +
    geom_tile(aes(fill = freq), colour = "grey") +
    geom_text(aes(label = sprintf("%1.0f", freq)), vjust = 1) +
    scale_fill_gradient(low = "lightblue", high = "blue") +
    theme_bw() + theme(legend.position = "none")
  
  
  
}


#roc <- calculate_roc(verset, 1, 10, n = 100)
#c50
idex<-sort(sample(nrow(CCFD), as.integer((.70*nrow(CCFD)))))

library('C50')
C55<-C5.0(factor(Class)~., data=training)
summary(C55)
plot(C55)


C50_prediction <- predict(C55, test, type="class")

table(actual=test[,11],C50_prediction)

#accuracy
match<-(test[,11]==C50_prediction)*100
accuracy<-sum(match)/length(match)
accuracy

#error
eerr<-(test[,11]!=C50_prediction)
err<-sum(eerr)/length(eerr)
err


# corelation
correlations <- cor(CCFD,method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")
