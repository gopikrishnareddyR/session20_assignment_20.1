#session20_assignment_20.1



#1. Use the below given data set 
library(readr)
library(data.table)
library(foreach)
library(dplyr)
library(psych)
getwd()
p<-"C:/Users/Swapna/Documents/R files test"
setwd(p)
wlee_data<-read.csv("C:/Users/Swapna/Documents/R files test/weight_lifting_exercises.csv", sep=",")
View(wlee_data)

class(wlee_data)
Data<-wlee_data[,colSums(is.na(wlee_data)) == 0]
View(Data)
dim(Data)


library(dplyr)
data<-select(Data,-c(2:5,11:14,36:41,45:46,59:68))# removing the columns which are not required
data<-select(data,-c(28,31,43))# removing few more columns 
View(data)
pairs(data[1:10]) 
t<-describe(data)
dim(data)

# splitting of data into training and test data

set.seed(12345) 
dataTrain<-data[1:4004,] 
dataTest<-data[4005:4024,] 
head(dataTrain) 
head(dataTest) 
indexNA <- as.vector(sapply(dataTrain[,1:51],function(x) {length(which(is.na(x)))!=0})) 
dataTrain <- dataTrain[,!indexNA] 


# splitting of data 

bound<-floor((nrow(data)/4)*3)
df<-data[sample(nrow(data)),]
df.train<-data[1:bound, ]
df.test<-data[(bound+1):nrow(data), ] 

dim(df.train) 
sum(is.na(df.train))  
View(df.train)
dim(df.test)
sum(is.na(df.test))

summary(df.train)
summary(df.test)

View(df.train)



library(tree)
fit1<-tree(classe~., data = df.train) 
plot(fit1)
text(fit1)
fit1<-glm(classe~., data = df.train)

summary(fit1)
pred<-predict(fit1, df.test, type="class")
confmat<-confusionMatrix(pred, df.test$classe)
confmat



#2.  Perform the below given activities: 



#a. Create classification model using different random forest models
library(caret)
library(rpart)

train_control<- trainControl(method="cv", number=10) 

model<- train(classe ~., data=dataTrain,trControl=train_control, method="rf") 
model 
predictions<- predict(model,dataTest) 
pred<- cbind(dataTest,predictions)
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe) 
confusionMatrix 


#b. Verify model goodness of fit 
pred<- cbind(dataTest,predictions)
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe) 
confusionMatrix 


#c. Apply all the model validation techniques
library(caret)
library(rpart)
control <- trainControl(method = 'repeatedcv', 
                        number = 3, 
                        repeats = 3) 
 

set.seed(123) 
mtry <- sqrt(ncol(dataTrain)) 
tunegrid <- expand.grid(.mtry=mtry) 
rf_default <- train(classe~.,  
                    data = dataTrain, 
                    method = 'rf', 
                    tuneGrid = tunegrid, 
                    trControl = control) 
print(rf_default) 
predictions<- predict(rf_default,dataTest)
pred<- cbind(dataTest,predictions)
confusionMatrix<- confusionMatrix(pred$predictions, pred$classe)
confusionMatrix
varImp(rf_default)


#d. Make conclusions 
 #gradient boosting 
library(caret)
library(rpart)

control <- trainControl(method = 'repeatedcv', 
                        number = 1, 
                        repeats = 3, 
                        search = 'grid') 


library(C50) 
set.seed(123) 
metric <- 'Accuracy' 
gbm_mod <- train(classe~.,  
                 data = dataTrain, 
                 method = 'gbm', 
                 trControl = control) 
print(gbm_mod) 
plot(gbm_mod) 

summary(gbm_mod) 
 
predictions<- predict(gbm_mod,dataTest) 


pred<- cbind(dataTest,predictions) 

# summarize results 
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe) 
confusionMatrix 


#Grid search 
control <- trainControl(method = 'repeatedcv', 
                        number = 1, 
                        repeats = 3, 
                        search = 'grid') 
set.seed(123) 
tunegrid <- expand.grid(.mtry=c(1:80)) 
mtry <- sqrt(ncol(x)) 
rf_gridsearch <- train(classe~.,  
                       data = dataTrain[1:200,], 
                       method = 'rf', 
                      tuneGrid = tunegrid, 
                       trControl = control) 
print(rf_gridsearch) 
plot(rf_gridsearch) 

predictions<- predict(rf_gridsearch,dataTest) 

 
pred<- cbind(dataTest,predictions) 

# summarize results 
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe) 
confusionMatrix 
varImp(rf_gridsearch) 


#e. Plot importance of variables 
 
plot(varImp(rf_default))
plot(varImp(rf_gridsearch))
