#session20_assignment_20.1

getwd()
p<-"C:/Users/DELL/Documents"
setwd(p)
wle<-read.csv("C:/Users/DELL/Documents/weight_lifting_exercises.csv")
wle<-wle[,-c(2:6,11:35,49:58,68:82,86:100,102:111,124:138,140:149)]
library(psych)
t<-View(describe(wle))
sum(is.na(wle))
summary(wle)
dim(wle)
pairs(wle[,1:10])

library(ISLR)

smp_siz<-floor(0.75*nrow(wle))  # creates a value for dividing the data into train and test. In this case the value is defined as 75% of the number of rows in the dataset
smp_siz
set.seed(123)   # set seed to ensure you always have same random numbers generated
train_ind = sample(seq_len(nrow(wle)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
wle_train =wle[train_ind,] #creates the training dataset with row numbers stored in train_ind
wle_test=wle[-train_ind,]


#### using tree model
library(tree)
fit1<-tree(classe~., data = wle) 
plot(fit1)
text(fit1)
summary(fit1)
pred<-predict(fit1, wle, type="class")
confmat<-confusionMatrix(pred, wle$classe)
confmat

#1. create a classification model using different classifiers
library(caret)
library(rpart)

##using cv
train_control<- trainControl(method="cv", number=3) 

model<- train(classe ~., data=wle_train,trControl=train_control, method="rf") 
model 
predictions<- predict(model,wle_test) 
pred<- cbind(wle_test,predictions)
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe) 
confusionMatrix 

##using repeatedcv
control <- trainControl(method = 'repeatedcv', 
                        number = 3, 
                        repeats = 3) 

set.seed(123) 
mtry <- sqrt(ncol(wle_train)) 
tunegrid <- expand.grid(.mtry=mtry) 
rf_default <- train(classe~.,data = wle_train,method = 'rf',tuneGrid = tunegrid,trControl = control)  
                     
print(rf_default) 
predictions<- predict(rf_default,wle_test)
pred<- cbind(wle_test,predictions)
confusionMatrix<- confusionMatrix(pred$predictions, pred$classe)

varImp(rf_default)

plot(varImp(rf_default))

###classification model using different classifiers

control <- trainControl(method = 'repeatedcv', 
                        number = 3, 
                        repeats = 3)
metric<-'Accuracy'
preProcess=c("center", "scale")


# Linear Discriminant Analysis
set.seed(7)
fit.lda <- train(classe~.,data = wle_train, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(7)
fit.glm <- train(classe~.,data = wle_train, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(7)
fit.glmnet <- train(classe~.,data = wle_train, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(7)
fit.svmRadial <- train(classe~.,data = wle_train, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(7)
fit.knn <- train(classe~.,data = wle_train, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(7)
fit.nb <- train(classe~.,data = wle_train, method="nb", metric=metric, trControl=control)
# CART
set.seed(7)
fit.cart <- train(classe~.,data = wle_train, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(7)
fit.c50 <- train(classe~.,data = wle_train, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(7)
fit.treebag <- train(classe~.,data = wle_train, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(classe~.,data = wle_train, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(7)
fit.gbm <- train(classe~.,data = wle_train, method="gbm", metric=metric, trControl=control, verbose=FALSE)


#### classifiers results comparision

results <- resamples(list(lda=fit.lda,glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)



# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)


#2.verify model goodness of fit

library(C50)
fitt<-glm(classe~gyros_forearm_x,data = wle_train, family = binomial(link = 'logit'))
fitt

library(ResourceSelection)
tes<-hoslem.test(fitt$y,fitted(fitt))

cbind(tes$observed,tes$expected)

#plot the fitted model
plot(fitt$fitted.values)

x <- wle_train$classe
y <- wle_train$total_accel_belt
library(MASS)
boxcox(y~x)
plot(1/y^2~x)

#check for multicollinearity
library(car)
vif(fitt)
vif(step_fit)

pred <- predict(fitt,newdata = wle_test,type = 'response')

#check the AUC curve
library(pROC)
g <- roc(classe~gyros_forearm_x, data = wle_test)
g
plot(g)

#3.Apply all the model validation techniques
library(caret)
library(rpart)

control <- trainControl(method = 'repeatedcv', 
                        number = 5, 
                        repeats = 3, 
                        search = 'grid') 


library(C50) 
set.seed(123) 
metric <- 'Accuracy' 
gbm_mod <- train(user_name~.,  
                 data = wle_train, 
                 method = 'gbm',
                 metric = metric,
                 trControl = control) 
print(gbm_mod) 
plot(gbm_mod) 

summary(gbm_mod) 

predictions<- predict(gbm_mod,wle_test) 


pred<- cbind(wle_test,predictions) 

# summarize results 
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe) 
confusionMatrix 


 ####Grid search 


control <- trainControl(method = 'repeatedcv', 
                        number = 5, 
                        repeats = 3, 
                        search = 'grid') 
set.seed(123) 
tunegrid <- expand.grid(.mtry=c(1:80)) 
mtry <- sqrt(ncol(x)) 
rf_gridsearch <- train(classe~.,  
                       data = wle_train[1:200,], 
                       method = 'rf', 
                       tuneGrid = tunegrid, 
                       trControl = control) 
print(rf_gridsearch) 
plot(rf_gridsearch) 

predictions<- predict(rf_gridsearch,wle_test) 


pred<- cbind(wle_test,predictions) 

# summarize results
library(caret)

confusionMatrix<- confusionMatrix(pred$predictions,pred$classe) 
confusionMatrix 

varImp(rf_gridsearch) 

#d. Make conclusions 
#### classifiers results comparision

results <- resamples(list(lda=fit.lda,glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)



# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)



#e. Plot importance of variables 

plot(varImp(rf_default))
plot(varImp(rf_gridsearch))






 




