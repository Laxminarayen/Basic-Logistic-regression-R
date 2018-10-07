setwd("c://Users//AADHI//Documents//Laxmi")
set.seed(6)
library(stats)
library(dplyr)
library(magrittr)
library(caret)
A <- read.csv("E:\\titanic disaster management\\train.csv")
colSums(is.na(A))
A <- A[,-c(11,9,4)]
#install.packages("mice")#for predictive mean modelling
#install.packages("e1071")#For confusion matrix
library(mice)
B <- mice(A, m = 5,method = "pmm")
c <- complete(B,1)
colSums(is.na(c))
Survived <- A[,2]
A <- A[,-2]
B <- mice(A, m = 5,method = "pmm")
c <- complete(B,1)
L <- cbind(c,Survived)
L <- L[,-1]
#d <- createDataPartition(L$Survived,p =0.80,list = FALSE) %>% c()
#train <- L[d,]
#test <- L[-d,]
#library(rpart)
#library(randomForest)
model <- randomForest(factor(Survived)~.,L[,-c(5,6,7)],mtree = 3,ntree = 200) 
#L <- predict(model,test,"prob")
#class <-ifelse(L[,2]>0.6,1,0)
#confusionMatrix(factor(test$Survived),factor(class))
test <- read.csv("E:\\titanic disaster management\\test.csv")
colSums(is.na(test))
test1 <- test[,-c(3,8,10)]
Z <- mice(test1,m=5,method = "pmm")
Y <- complete(Z,1)
L <- predict(model,Y,"prob")
Survived <- ifelse(L[,2] <0.6,0,1)
test <- cbind(test,factor(Survived))
View(test)
write.csv(test,"test4.csv")
