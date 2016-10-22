setwd('/Users/khanh94/Documents/Kaggle/Knocktober')
library(data.table)
library(xgboost)
library(dplyr)

train <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Train/Train.csv')
test <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Test.csv')
first <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Train/First_Health_Camp_Attended.csv')
second <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Train/Second_Health_Camp_Attended.csv')
third <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Train/Third_Health_Camp_Attended.csv')
profile <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/Train/Patient_Profile.csv')
Patient_ID = test$Patient_ID
Health_Camp_ID = test$Health_Camp_ID

train <- as.data.table(inner_join(train, profile))
test <- as.data.table(inner_join(test, profile))

train$Outcome = 0

#Joining the training sets together
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




#Transforming the dates to readable formats
train$Registration_Date[nchar(train$Registration_Date) == 8] <- paste0("0", train$Registration_Date[nchar(train$Registration_Date) == 8])
train$Registration_Date <- paste0(substr(train$Registration_Date,1,7), "20", substr(train$Registration_Date,8,9))

test$Registration_Date[nchar(test$Registration_Date) == 8] <- paste0("0", test$Registration_Date[nchar(test$Registration_Date) == 8])
test$Registration_Date <- paste0(substr(test$Registration_Date,1,7), "20", substr(test$Registration_Date,8,9))

train$First_Interaction[nchar(train$First_Interaction) == 8] <- paste0("0", train$First_Interaction[nchar(train$First_Interaction) == 8])
train$First_Interaction <- paste0(substr(train$First_Interaction,1,7), "20", substr(train$First_Interaction,8,9))

test$First_Interaction[nchar(test$First_Interaction) == 8] <- paste0("0", test$First_Interaction[nchar(test$First_Interaction) == 8])
test$First_Interaction <- paste0(substr(test$First_Interaction,1,7), "20", substr(test$First_Interaction,8,9))

train <- as.data.table(inner_join(train, detail, by="Health_Camp_ID"))
test <- as.data.table(inner_join(test, detail, by="Health_Camp_ID"))


train$Camp_Start_Date[nchar(train$Camp_Start_Date) == 8] <- paste0("0", train$Camp_Start_Date[nchar(train$Camp_Start_Date) == 8])
train$Camp_Start_Date <- paste0(substr(train$Camp_Start_Date,1,7), "20", substr(train$Camp_Start_Date,8,9))

test$Camp_Start_Date[nchar(test$Camp_Start_Date) == 8] <- paste0("0", test$Camp_Start_Date[nchar(test$Camp_Start_Date) == 8])
test$Camp_Start_Date <- paste0(substr(test$Camp_Start_Date,1,7), "20", substr(test$Camp_Start_Date,8,9))

train$Camp_End_Date[nchar(train$Camp_End_Date) == 8] <- paste0("0", train$Camp_End_Date[nchar(train$Camp_End_Date) == 8])
train$Camp_End_Date <- paste0(substr(train$Camp_End_Date,1,7), "20", substr(train$Camp_End_Date,8,9))

test$Camp_End_Date[nchar(test$Camp_End_Date) == 8] <- paste0("0", test$Camp_End_Date[nchar(test$Camp_End_Date) == 8])
test$Camp_End_Date <- paste0(substr(test$Camp_End_Date,1,7), "20", substr(test$Camp_End_Date,8,9))

#Data Processing 
train <- train[, ":="(Patient_ID = NULL,
                      Health_Camp_ID = NULL, 
                      #Var1 = NULL,
                      Registration_Date = as.Date(train$Registration_Date, format="%d-%b-%Y"),
                      Income = NULL, #as.numeric(train$Income), 
                      #Education_Score = NULL, 
                      Age = as.numeric(train$Age), 
                      First_Interaction = as.Date(train$First_Interaction, format="%d-%b-%Y"), 
                      Camp_Start_Date = as.Date(train$Camp_Start_Date, format="%d-%b-%Y"),
                      Camp_End_Date = as.Date(train$Camp_End_Date, format="%d-%b-%Y"),
                      City_Type = as.numeric(as.factor(train$City_Type)), 
                      Employer_Category = NULL, 
                      Education_Score = as.numeric(train$Education_Score),
                      Category1 = as.numeric(as.factor(train$Category1)), 
                      Category2 = as.numeric(as.factor(train$Category2)),
                      Category3 = train$Category3
)]

test <- test[, ":="(Patient_ID = NULL,
                    Health_Camp_ID = NULL, 
                    #Var1 = NULL,
                    Registration_Date = as.Date(test$Registration_Date, format="%d-%b-%Y"),
                    Income = NULL, #as.numeric(test$Income), 
                    #Education_Score = NULL, 
                    Age = as.numeric(test$Age), 
                    First_Interaction = as.Date(test$First_Interaction, format="%d-%b-%Y"), 
                    Camp_Start_Date = as.Date(test$Camp_Start_Date, format="%d-%b-%Y"),
                    Camp_End_Date = as.Date(test$Camp_End_Date, format="%d-%b-%Y"),
                    City_Type = as.numeric(as.factor(test$City_Type)), 
                    Employer_Category = NULL, 
                    Education_Score = as.numeric(test$Education_Score),
                    Category1 = as.numeric(as.factor(test$Category1)),
                    Category2 = as.numeric(as.factor(test$Category2)), 
                    Category3 = test$Category3
)]

#Feature Engineering - Taking lengths of camps and Waiting Times as variables to be included
train$Length_Of_Camp <- as.numeric(train$Camp_End_Date - train$Camp_Start_Date)
train$Length_Of_Camp <- as.numeric(train$Camp_End_Date - train$Camp_Start_Date)

test$Length_Of_Camp <- as.numeric(test$Camp_End_Date - test$Camp_Start_Date)
test$Length_Of_Camp <- as.numeric(test$Camp_End_Date - test$Camp_Start_Date)

train$Gap_Of_Interaction <- as.numeric(train$First_Interaction - train$Registration_Date)
test$Gap_Of_Interaction <- as.numeric(test$First_Interaction - test$Registration_Date)

train$Waiting_Time <- as.numeric(train$Camp_Start_Date - train$First_Interaction)
test$Waiting_Time <- as.numeric(test$Camp_Start_Date - test$First_Interaction)


#train$Season = getSeason(train$Registration_Date)
#test$Season = getSeason(test$Registration_Date)

#train$Season = as.numeric(factor(train$Season, levels=c("Winter", "Spring", "Fall", "Summer"), ordered=TRUE))
#test$Season = as.numeric(factor(test$Season, levels=c("Winter", "Spring", "Fall", "Summer"), ordered=TRUE))

train$Registration_Date = NULL
test$Registration_Date = NULL

train$First_Interaction = NULL
test$First_Interaction = NULL

train$Camp_End_Date = NULL
test$Camp_End_Date = NULL

train$Camp_Start_Date = NULL
test$Camp_Start_Date = NULL

target = train$Outcome
train$Outcome = NULL

train[is.na(train)] <- -400
test[is.na(test)] <- -400

#Model Crossvalidation
model_xgb_cv <- xgb.cv(data=as.matrix(train), 
                       label=as.matrix(target), 
                       nfold=10, 
                       objective="binary:logistic", 
                       nrounds=1, 
                       eta=0.015, 
                       max_depth=3, 
                       subsample=0.75, 
                       colsample_bytree=0.8, 
                       min_child_weight=1, 
                       eval_metric="auc")

preds = rep(0, nrow(test))

#Model building and ensembling
#for (z in 1:50){
  #set.seed(z + 80)
  model_xgb<- xgboost(data=as.matrix(train), 
                      label=as.matrix(target), 
                      objective="binary:logistic", 
                      nrounds=1000, 
                      eta=0.01, 
                      max_depth=3, 
                      subsample=0.75, 
                      colsample_bytree=0.8, 
                      min_child_weight=1, 
                      eval_metric="auc")
  
  preds <- preds + predict(model_xgb, as.matrix(test))
#}

#preds = preds/50.0

preds = preds*0.9  

submission <- fread('/Users/khanh94/Documents/Kaggle/Knocktober/sample_submission.csv')
final_sub = data.frame(Patient_ID = Patient_ID, Health_Camp_ID = Health_Camp_ID, Outcome=preds)
write.csv(final_sub, file='final_sub.csv', row.names=FALSE)
