setwd("C:\\Users\\Sohom Ghosh\\Desktop\\BigDataMart")
train<-read.csv("Train_UWu5bXk.csv")
test<-read.csv("Test_u94Q5KV.csv")
#item_op_sales<-train['Item_Outlet_Sales']
#mean_item_op_sales<-mean(item_op_sales[,1])

#soln<-cbind(test[,'Item_Identifier'],test[,'Outlet_Identifier'],data.frame(x=rep(mean_item_op_sales,5681)))

#write.csv(soln[,2:4],file="submission.csv")
#str(train)

#random forest
#SVM
#Deep Learning h2o
#XGBOOST
#ensemble above 4
table(train[,3])
#LF low fat Low Fat     reg Regular
#Recode(x, "1:2='A'; 3='B'")
item_fat_con <- train[,3]
library(car)
item_fat_con_recoded <-  recode(item_fat_con,"'LF'=1;'low fat'=1;'Low Fat'=1;'reg'=2;'Regular'=2" )
table(item_fat_con_recoded)
train_response <- train['Item_Outlet_Sales']

table(train[,5])
item_type_numeric <- as.numeric(train[,5])

table(train[,7])
out_iden_numeric <- as.numeric(train[,7])

out_old<-2016-as.numeric(train[,8])

table(train[,9])
out_size_recoded <- recode(train[,9],"'High'=1; 'Medium'=2;  'Small'=3; ''=NA")


table(train[,10])
out_location_type_recoded <- recode(train[,10],"'Tier 1'=1; 'Tier 2'=2; 'Tier 3'=3")

table(train[,11])
out_type_recoded <- as.numeric(train[,11])


train_df <- data.frame(cbind(train[,2],item_fat_con_recoded,train[,4],item_type_numeric,train[,6],out_iden_numeric,out_old,out_size_recoded,out_location_type_recoded,out_type_recoded))

for (i in (1:10)){
  print (table(is.na(train_df[,i])))
  print(i)
  print ("\n")
}

#1,8
mn<-mean(train_df[,1],na.rm=T)

med<-median(train_df[,8],na.rm=T)

[train_df[,1]==NA]
#****Treat NA by diffrent techniques&&&&&&&&&$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

train_df[, 1][is.na(train_df[, 1])]<-mn

train_df[, 8][is.na(train_df[, 8])]<-med
train_df_no_na <- data.frame(cbind(train_df[,1],item_fat_con_recoded,train[,4],item_type_numeric,train[,6],out_iden_numeric,out_old,train_df[,8],out_location_type_recoded,out_type_recoded))

##^^^^After One hot encoded try$$$$$$$$$$$$$$$$$$$$$$$$&&&&&&&&&&&&&&&$$$$$$$$$$$$$

library(randomForest)
model_rf<-randomForest(train_df_no_na,train[,12],importance=TRUE, ntree=100)
importance(model_rf)

###Same transformation to test_set
item_fat_con_test <- test[,3]

item_fat_con_test_recoded <-  recode(item_fat_con_test,"'LF'=1;'low fat'=1;'Low Fat'=1;'reg'=2;'Regular'=2" )
table(item_fat_con_recoded)


table(test[,5])
item_type_test_numeric <- as.numeric(test[,5])

table(test[,7])
out_iden_test_numeric <- as.numeric(test[,7])

out_old_test<-2016-as.numeric(test[,8])

table(test[,9])
out_size_test_recoded <- recode(test[,9],"'High'=1; 'Medium'=2;  'Small'=3; ''=NA")


table(test[,10])
out_location_type_test_recoded <- recode(test[,10],"'Tier 1'=1; 'Tier 2'=2; 'Tier 3'=3")

table(test[,11])
out_type_test_recoded <- as.numeric(test[,11])


test_df <- data.frame(cbind(test[,2],item_fat_con_test_recoded,test[,4],item_type_test_numeric,test[,6],out_iden_test_numeric,out_old_test,out_size_test_recoded,out_location_type_test_recoded,out_type_test_recoded))

for (i in (1:10)){
  print (table(is.na(test_df[,i])))
  print(i)
  print ("\n")
}

#1,8
mn_test<-mean(test_df[,1],na.rm=T)

med_test<-median(test_df[,8],na.rm=T)


#****Treat NA by diffrent techniques&&&&&&&&&$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

test_df[, 1][is.na(test_df[, 1])]<-mn_test

test_df[, 8][is.na(test_df[, 8])]<-med_test
test_df_no_na <- data.frame(cbind(test_df[,1],item_fat_con_test_recoded,test[,4],item_type_test_numeric,test[,6],out_iden_test_numeric,out_old_test,test_df[,8],out_location_type_test_recoded,out_type_test_recoded))







model_rf<-randomForest(train_df_no_na,train[,12])
model_rf_pred<-predict(model_rf,test_df_no_na)








train_df_new<-data.frame(cbind(train_df,train[,12]))
model_rf<-randomForest(train_df_new[,11] ~ ., data=train_df_new, ntree=100)
model_rf_pred<-predict(model_rf,test_df_no_na)

# test_response TO BE FOUND OUT































item_fat_con <- test[,3]

item_fat_con_recoded <-  recode(item_fat_con,"'LF'=1;'low fat'=1;'Low Fat'=1;'reg'=2;'Regular'=2" )
#table(item_fat_con_recoded)


#table(test[,5])
item_type_numeric <- as.numeric(test[,5])

#table(test[,7])
out_iden_numeric <- as.numeric(test[,7])

out_old<-2016-as.numeric(test[,8])

#table(test[,9])
out_size_recoded <- recode(test[,9],"'High'=1; 'Medium'=2;  'Small'=3; ''=NA")


#table(test[,10])
out_location_type_recoded <- recode(test[,10],"'Tier 1'=1; 'Tier 2'=2; 'Tier 3'=3")

#table(test[,11])
out_type_recoded <- as.numeric(test[,11])


test_df <- data.frame(cbind(test[,2],item_fat_con_recoded,test[,4],item_type_numeric,test[,6],out_iden_numeric,out_old,out_size_recoded,out_location_type_recoded,out_type_recoded))
'''
for (i in (1:10)){
  print (table(is.na(test_df[,i])))
  print(i)
  print ("\n")
}
'''
#1,8
mn_test<-mean(test_df[,1],na.rm=T)

med_test<-median(test_df[,8],na.rm=T)


#****Treat NA by diffrent techniques&&&&&&&&&$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

test_df[, 1][is.na(test_df[, 1])]<-mn_test

test_df[, 8][is.na(test_df[, 8])]<-med_test
test_df_no_na <- data.frame(cbind(test_df[,1],item_fat_con_recoded,test[,4],item_type_numeric,test[,6],out_iden_numeric,out_old,test_df[,8],out_location_type_recoded,out_type_recoded))

model_rf<-randomForest(train_df_no_na,train[,12])
model_rf_pred<-predict(model_rf,test_df_no_na)

soln<-cbind(test[,'Item_Identifier'],test[,'Outlet_Identifier'],data.frame(model_rf_pred))
write.csv(soln,file="submission_rf.csv")

#only RAndom Forest score:- 1170.34918702

###SVM
library(e1071)
model_svm<-svm(train_df_no_na,train[,12])
model_svm_pred<-predict(model_svm,test_df_no_na)
soln<-cbind(test[,'Item_Identifier'],test[,'Outlet_Identifier'],data.frame(model_svm_pred))
write.csv(soln,file="submission_svm.csv")

#only SVM score:- 1171.44987853






prediction_table<-train_df_no_na
library(deepnet)
x <- as.matrix(prediction_table)
y <- as.numeric(train[,12])
nn <- dbn.dnn.train(x,y,hidden = c(10),
                    activationfun = "sigm",numepochs = 300,learningrate = 0.1,momentum = 0.5)

prediction_table<-test_df_no_na
x_test<- as.matrix(prediction_table)

nn_predict_test <- nn.predict(nn,x_test)

soln<-cbind(test[,'Item_Identifier'],test[,'Outlet_Identifier'],data.frame(nn_predict_test))
write.csv(soln,file="submission_DNN.csv")

#Only deep net 2802.07550613

library(xgboost)
bst <- xgboost(data = as.matrix(train_df_no_na), label = train[,12], max.depth = 6,
               eta = .4, nthread = 2, nround = 15)
pred <- predict(bst, as.matrix(test_df_no_na))
Item_Identifier <- test[,'Item_Identifier']
Outlet_Identifier<- test[,'Outlet_Identifier']
Item_Outlet_Sales<-data.frame(pred)
soln<-cbind(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(soln,file="submission_XGBoost3.csv",row.names=FALSE)
#XG boost-3 reloaded as above 1169.35465908
#Only XG boost -2   1194.74521024


bst <- xgboost(data = as.matrix(train_df_no_na), label = train[,12], max.depth = 6, eta = .4, nthread = 2, nround = 10)
pred <- predict(bst, as.matrix(test_df_no_na))
Item_Identifier <- test[,'Item_Identifier']
Outlet_Identifier<- test[,'Outlet_Identifier']
Item_Outlet_Sales<-data.frame(pred)
soln<-cbind(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(soln,file="submission_XGBoost3.csv",row.names=FALSE,col.names=c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales'))
#c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales')
#Above output 1163.75401429

bst <- xgboost(data = as.matrix(train_df_no_na), label = train[,12], max.depth = 6, eta = .3, nthread = 2, nround = 10)
pred <- predict(bst, as.matrix(test_df_no_na))
Item_Identifier <- test[,'Item_Identifier']
Outlet_Identifier<- test[,'Outlet_Identifier']
Item_Outlet_Sales<-data.frame(pred)
soln<-cbind(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(soln,file="submission_XGBoost4.csv",row.names=FALSE,col.names=c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales'))
#1162.59797717

bst <- xgboost(data = as.matrix(train_df_no_na), label = train[,12],nround = 10)
pred <- predict(bst, as.matrix(test_df_no_na))
Item_Identifier <- test[,'Item_Identifier']
Outlet_Identifier<- test[,'Outlet_Identifier']
Item_Outlet_Sales<-data.frame(pred)
soln<-cbind(Item_Identifier,Outlet_Identifier,Item_Outlet_Sales)
write.csv(soln,file="submission_XGBoost5.csv",row.names=FALSE,col.names=c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales'))
#1162.59797717


#Regression
linear_fit <- lm(train[,12]~train_df_no_na[,1]+train_df_no_na[,2]+train_df_no_na[,3]+train_df_no_na[,4]+train_df_no_na[,5]+train_df_no_na[,6]+train_df_no_na[,7]+train_df_no_na[,8]+train_df_no_na[,9]+train_df_no_na[,10])
#pred<-predict(linear_fit,train_df_no_na[,1]+train_df_no_na[,2]+train_df_no_na[,3]+train_df_no_na[,4]+train_df_no_na[,5]+train_df_no_na[,6]+train_df_no_na[,7]+train_df_no_na[,8]+train_df_no_na[,9]+train_df_no_na[,10])
pred<-predict(linear_fit,test_df_no_na)
summary(pred)





# x is a matrix of multiple predictions coming from different learners
#y is a vector of all output flags
#x_test is a matrix of multiple predictions on an unseen sample

model_rf_pred_train<-predict(model_rf,train_df_no_na)
model_svm_pred_train<-predict(model_svm,train_df_no_na)
prediction_table<-cbind(model_rf_pred_train,model_svm_pred_train)
library(deepnet)
x <- as.matrix(prediction_table)
y <- as.numeric(train[,12])
nn <- dbn.dnn.train(x,y,hidden = c(10),
                    activationfun = "sigm",numepochs = 300,learningrate = 0.1,momentum = 0.5)

prediction_table<-cbind(model_rf_pred,model_svm_pred)
x_test<- as.matrix(prediction_table)

nn_predict_test <- nn.predict(nn,x_test)
  
soln<-cbind(test[,'Item_Identifier'],test[,'Outlet_Identifier'],data.frame(nn_predict_test))
write.csv(soln,file="submission_ensembleDNN___RFandSVM.csv")

#Ensemble 2802.07550613


train_df_no_na_with_y<-data.frame(cbind(train_df_no_na,train[,12]))
write.csv(train_df_no_na,file="train_df_noNA.csv",row.names=F)
write.csv(test_df_no_na,file="test_df_noNA.csv",row.names=F)