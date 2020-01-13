## Name : Samir Jesal Shah
## CWID : 10445681

## clears all objects in memory
rm(list = ls())

## Reads CSV file and replaces ? with NA
wisconsin <- read.csv("C://Users/samir/Downloads/breast-cancer-wisconsin.data.csv",na.strings = '?')

##Summarizes each column
summary(wisconsin)
## Create a data frame
data_frame <- data.frame(wisconsin)
##View data frame
View(data_frame)
##Summary of data frame 
summary(data_frame)

##Identifying missing values
##Calculates the number of missing data ie calculates number of NA
sum(is.na(data_frame))
##Displays NA column wise
colSums(is.na(data_frame))

##Calculate mean of Column F6
m1 <- mean(data_frame$F6,na.rm = TRUE)
##Replaces NA with with mean of Column F^ 
data_frame[is.na(data_frame)] <- m1

##Frequency table of Class vs F6
table(data_frame$Class,data_frame$F6)
##Scatter plot for F1 to F6
pairs(data_frame[,2:7], upper.panel = NULL)
##Box plot for F7,F8 and F9
boxplot(data_frame$F7,data_frame$F8,data_frame$F9)
##Histogram for F7,F8 and F9
hist(data_frame$F7,xlab = "Values of F7",main = "Histogram for F7")
hist(data_frame$F8,xlab = "Values of F8",main = "Histogram for F8")
hist(data_frame$F9,xlab = "Values of F9",main = "Histogram for F9")

rm(list = ls())

## Reads CSV file and replaces ? with NA
wisconsin_missing <- read.csv("C://Users/samir/Downloads/breast-cancer-wisconsin.data.csv",na.strings = '?') 
View(wisconsin_missing)

## Removes rows with missing columns 
cancer_data_missing<-na.omit(wisconsin_missing)
View(cancer_data_missing)
summary(cancer_data_missing)







