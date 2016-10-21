setwd('/Users/khanh94/Documents/Kaggle/Knocktober')
library(data.table)
library(xgboost)
library(dplyr)

train <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Train/Train.csv')
test <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Test.csv')
first <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Train/First_Health_Camp_Attended.csv')
second <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Train/Second_Health_Camp_Attended.csv')
third <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Train/Third_Health_Camp_Attended.csv')

Patient_ID = test$Patient_ID
Health_Camp_ID = test$Health_Camp_ID

train$Outcome = 0

keys<-join.keys(train,first,c("Patient_ID","Health_Camp_ID"))
matches<-match(keys$y,keys$x,nomatch=(keys$n+1))
train[matches,]$Outcome = 1

keys2<-join.keys(train,second,c("Patient_ID","Health_Camp_ID"))
matches2<-match(keys2$y,keys2$x,nomatch=(keys2$n+1))
train[matches2,]$Outcome = 1

keys3<-join.keys(train,third[third$Number_of_stall_visited > 0,],c("Patient_ID","Health_Camp_ID"))
matches3<-match(keys3$y,keys3$x,nomatch=(keys3$n+1))
train[matches3,]$Outcome = 1

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}


for(i in 1:length(train$Registration_Date)){
  if(train$Registration_Date[[i]] == ""){
    train$Registration_Date[[i]] = "06-Jun-06"
  }
}

for(i in 1:length(test$Registration_Date)){
  if(test$Registration_Date[[i]] == ""){
    test$Registration_Date[[i]] = "06-Jun-06"
  }
}


train$Registration_Date[nchar(train$Registration_Date) == 8] <- paste0("0", train$Registration_Date[nchar(train$Registration_Date) == 8])
train$Registration_Date <- paste0(substr(train$Registration_Date,1,7), "20", substr(train$Registration_Date,8,9))

test$Registration_Date[nchar(test$Registration_Date) == 8] <- paste0("0", test$Registration_Date[nchar(test$Registration_Date) == 8])
test$Registration_Date <- paste0(substr(test$Registration_Date,1,7), "20", substr(test$Registration_Date,8,9))

train <- train[, ":="(Patient_ID = NULL,
                      Health_Camp_ID = NULL, 
                      #Var1 = NULL,
                      Registration_Date = as.Date(train$Registration_Date, format="%d-%b-%Y")
)]

test <- test[, ":="(Patient_ID = NULL,
                      Health_Camp_ID = NULL, 
                      #Var1 = NULL,
                      Registration_Date = as.Date(test$Registration_Date, format="%d-%b-%Y")
)]

train$Season = getSeason(train$Registration_Date)
test$Season = getSeason(test$Registration_Date)

train$Season = as.numeric(factor(train$Season, levels=c("Winter", "Spring", "Fall", "Summer"), ordered=TRUE))
test$Season = as.numeric(factor(test$Season, levels=c("Winter", "Spring", "Fall", "Summer"), ordered=TRUE))

train$day <- as.factor(weekdays(train$Registration_Date))
test$day <- as.factor(weekdays(test$Registration_Date))

train$day = as.numeric(train$day)
test$day = as.numeric(test$day)

#train$month = month(train$Registration_Date)
#test$month = month(test$Registration_Date)

train$Registration_Date = NULL
test$Registration_Date = NULL

target = train$Outcome
train$Outcome = NULL

model_xgb_cv <- xgb.cv(data=as.matrix(train), 
                       label=as.matrix(target), 
                       nfold=5, 
                       objective="binary:logistic", 
                       nrounds=2000, 
                       eta=0.04, 
                       max_depth=3, 
                       subsample=0.75, 
                       colsample_bytree=0.8, 
                       min_child_weight=1, 
                       eval_metric="auc")

preds = rep(0, nrow(test))

model_xgb<- xgboost(data=as.matrix(train), 
                       label=as.matrix(target), 
                       objective="binary:logistic", 
                       nrounds=2000, 
                       eta=0.04, 
                       max_depth=3, 
                       subsample=0.75, 
                       colsample_bytree=0.8, 
                       min_child_weight=1, 
                       eval_metric="auc")

preds <- predict(model_xgb, as.matrix(test))

submission <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/sample_submission.csv')
final_sub = data.frame(Patient_ID = Patient_ID, Health_Camp_ID = Health_Camp_ID, Outcome=preds)
write.csv(final_sub, file='final_sub.csv', row.names=FALSE)

