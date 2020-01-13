#Name : Samir Jesal Shah
# CWID : 10445681

#Clear Objects from memory
rm(list = ls())

#Read CSV
wisconsin <- read.csv("C://Users/samir/Downloads/breast-cancer-wisconsin.data.csv",na.strings = '?')
wisconsin1 <- na.omit(wisconsin)
View(wisconsin1)
table(wisconsin1$Class)

#Factorizing Data
wisconsin1$Class <- factor(wisconsin1$Class, levels = c(2,4),labels =
                         c("Benign", "Malignant"))

#Normalizing Data
normalize <- function(x,minx,maxx){z<- ((x-minx)/(maxx-minx))
  return(z);
}

wisconsin_normalized <- as.data.frame(
   cbind(F1 = normalize(wisconsin1[,2],min(wisconsin1[,2]),max(wisconsin1[,2])),
         F2 = normalize(wisconsin1[,3],min(wisconsin1[,3]),max(wisconsin1[,3])),
         F3 = normalize(wisconsin1[,4],min(wisconsin1[,4]),max(wisconsin1[,4])),
         F4 = normalize(wisconsin1[,5],min(wisconsin1[,5]),max(wisconsin1[,5])),
         F5 = normalize(wisconsin1[,6],min(wisconsin1[,6]),max(wisconsin1[,6])),
         F6 = normalize(wisconsin1[,7],min(wisconsin1[,7]),max(wisconsin1[,7])),
         F7 = normalize(wisconsin1[,8],min(wisconsin1[,8]),max(wisconsin1[,8])),
         F8 = normalize(wisconsin1[,9],min(wisconsin1[,9]),max(wisconsin1[,9])),
         F9 = normalize(wisconsin1[,10],min(wisconsin1[,10]),max(wisconsin1[,10])),
         Class=as.character(wisconsin1[,11])
        )
)

View(wisconsin_normalized)
summary(wisconsin_normalized)

#KNN with normalized data
id <- sort(sample(nrow(wisconsin_normalized),as.integer(0.7*nrow(wisconsin_normalized))))
#Create training and test dataset
test_dataset <- wisconsin_normalized[id,]
training_dataset <- wisconsin_normalized[-id,]
View(test_dataset)
View(training_dataset)
library(kknn)
#K = 3
predict_k3 <- kknn(formula=Class~., training_dataset, test_dataset, k=3,kernel ="triangular" )
fit <- fitted(predict_k3)
table(test_dataset$Class,fit)
#K = 5
predict_k5 <- kknn(formula=Class~., training_dataset, test_dataset, k=5,kernel ="triangular" )
fit <- fitted(predict_k5)
table(test_dataset$Class,fit)
#K = 10
predict_k10 <- kknn(formula=Class~., training_dataset, test_dataset, k=10,kernel ="triangular" )
fit <- fitted(predict_k10)
table(test_dataset$Class,fit)


#KNN without normalized data
id1 <- sort(sample(nrow(wisconsin1),as.integer(0.7*nrow(wisconsin1))))
#Create training and test dataset
test_dataset1 <- wisconsin1[id,]
training_dataset1 <- wisconsin1[-id,]

#KKNN algorithm execution
#K = 3
predict_k31 <- kknn(formula=Class~., training_dataset1, test_dataset1, k=3,kernel ="triangular" )
fit <- fitted(predict_k31)
table(test_dataset1$Class,fit)
#K = 5
predict_k51 <- kknn(formula=Class~., training_dataset1, test_dataset1, k=5,kernel ="triangular" )
fit <- fitted(predict_k51)
table(test_dataset$Class,fit)
#K = 10
predict_k101 <- kknn(formula=Class~., training_dataset1, test_dataset1, k=10,kernel ="triangular" )
fit <- fitted(predict_k101)
table(test_dataset$Class,fit)











