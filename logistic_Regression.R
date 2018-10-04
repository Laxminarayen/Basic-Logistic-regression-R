setwd("c://Users//AADHI//Documents//Laxmi")
set.seed(6)
library(stats)
library(dplyr)
library(magrittr)
A <- read.csv("E:\\train.csv")
colSums(is.na(A))
A <- A[,-c(11,9,4)]
install.packages("mice")#for predictive mean modelling
install.packages("e1071")#For confusion matrix
library(mice)
B <- mice(A, m = 5,method = "pmm")
c <- complete(B,1)
colSums(is.na(c))
C <- A[,2]
A <- A[,-2]
B <- mice(A, m = 5,method = "pmm")
c <- complete(B,1)
L <- cbind(c,C)
L <- L[,-1]
d <- createDataPartition(L$C,p =0.9,list = FALSE) %>% c()
train <- L[d,]
test <- L[-d,]
model5 <- glm(C~.,L[,-c(5,6,7)],family = binomial) 
summary(model5)
f <- predict(model5,test,"response")
class <- ifelse(f<0.4,0,1)
confusionMatrix(factor(test$C),factor(class))
