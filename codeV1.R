library(data.table)
library(xgboost)
library(dplyr)

train <- fread('Train.csv')
test <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Test.csv')
first <- fread('First_Health_Camp_Attended.csv')
second <- fread('Second_Health_Camp_Attended.csv')
third <- fread('Third_Health_Camp_Attended.csv')

train$Outcome = 0

keys<-join.keys(train,first,c("Patient_ID","Health_Camp_ID"))
matches<-match(keys$y,keys$x,nomatch=(keys$n+1))
train[matches,]$Outcome = 1

keys2<-join.keys(train,second,c("Patient_ID","Health_Camp_ID"))
matches2<-match(keys2$y,keys2$x,nomatch=(keys2$n+1))
train[matches2,]$Outcome = 1

keys3<-join.keys(train,third,c("Patient_ID","Health_Camp_ID"))
matches3<-match(keys3$y,keys3$x,nomatch=(keys3$n+1))
train[matches3,]$Outcome = 1


