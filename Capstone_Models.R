#Model 1: Decision Tree Model (J48) with 10 fold cross validation; RWeka library
#Follow method as outlined here: https://cran.r-project.org/web/packages/RWeka/RWeka.pdf
#https://rdrr.io/cran/RWeka/man/Weka_control.html

#create training model and evaluate
set.seed(123)
DT <- J48(as.factor(class)~., data = Train, control=Weka_control(M=5))


#10 fold cross validation
EV<-evaluate_Weka_classifier(DT,numFolds = 10)
EV
EV$details

#predict using J48
predDT<-predict(DT, Test, type="class")

#DT confusion matrix
confDT<-table(Test$class, predDT, dnn=c("Actual", "Predicted"))
confDT

#evaluate model
confusionMatrix(as.factor(Test$class), as.factor(predDT))




#Model 2: multivariate logistic regression - same parameters as example
#https://stackoverflow.com/questions/39550118/cross-validation-function-for-logistic-regression-in-r
#https://www.youtube.com/watch?v=fDjKa7yWk1U; nnet library

#set up train/test &  10 fold cross validation
LR_train<-Train
LR_test<-Test
set.seed(123)
tc <- trainControl(method = "cv", number = 10)


# Training the multinomial model
MN_model <- multinom(class ~ .,
                     data = LR_train,
                     method = 'glm',
                     trControl = tc,
                     family = binomial()
)

# Checking the model
summary(MN_model)

#convert coefficients to odds
exp(coef(MN_model))

#top observations
head(round(fitted(MN_model), 2))


#Prediction
# Predicting the values for train dataset
LR_train$Pred <- predict(MN_model, newdata = LR_train, "class")


# Building classification table
tab <- table(LR_train$class, LR_train$Pred)

#confusion matrix for training model
CM_train<-confusionMatrix(tab)
CM_train

#misclassification error
1-sum(diag(tab))/sum(tab)


# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

# Predicting the class for test dataset
LR_test$Pred <- predict(MN_model, newdata = LR_test, "class")
# Building classification table
tab2 <- table(LR_test$class, LR_test$Pred)
tab2

CM_mod<-confusionMatrix(tab2)
CM_mod

# Calculating accuracy of predictive model - sum of diagonal elements divided by total obs
round((sum(diag(tab2))/sum(tab2))*100,2)



#Model 3: Naive Bayes with 10 fold cross validation:Balanced Data
#https://www.geeksforgeeks.org/naive-bayes-classifier-in-r-programming/
#https://rpubs.com/maulikpatel/224581
#https://www.analyticsvidhya.com/blog/2021/03/introduction-to-k-fold-cross-validation-in-r/; naive bayes package
NB_train<-Train
NB_test<-Test

set.seed(100)
trctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
nb_fit <- train(as.factor(class) ~., data = NB_train, method = "naive_bayes", trControl=trctrl, tuneLength = 0)
nb_fit


#predict based on the NB Model
y_predNB <- predict(nb_fit, newdata = NB_test)

#NB Confusion Matrix & evaluation
cmNB <- table(NB_test$class, y_predNB, dnn = c("Actual", "Predicted"))
cmNB


confusionMatrix(as.factor(NB_test$class), as.factor(y_predNB))

#Model 4: Random Forest
#https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

control <- trainControl(method = "cv", number = 10, savePredictions = "final",
                        classProbs = F)
mtry <- sqrt(ncol(RF_Train))
tunegrid <- expand.grid(.mtry=mtry)
rf_mod <- train(class~., data=RF_Train, method="rf", metric='Accuracy', tuneGrid=tunegrid, trControl=control)
#Training time of 61 mins

print(rf_mod)

#Confusion Matrix Train w/cross validation

rf_mod$pred
rf_mod$pred[order(rf_mod$pred$rowIndex),2]
confusionMatrix(rf_mod$pred[order(rf_mod$pred$rowIndex),2], RF_Train$class)



# Prediction using test dataset
RF_Test$Pred <- predict(rf_mod, newdata = RF_Test, "raw")

# Building classification table
tab2 <- table(RF_Test$class, as.factor(RF_Test$Pred))
tab2

CM_mod<-confusionMatrix(tab2)
CM_mod

# Calculating accuracy of predictive model - sum of diagonal elements divided by total obs
round((sum(diag(tab2))/sum(tab2))*100,2)


#Model 5 kNN
#https://stats.stackexchange.com/questions/318968/knn-and-k-folding-in-r
#https://dataaspirant.com/knn-implementation-r-using-caret-package/; 4 hrs
knn_Train<-TrainingData
knn_Test<-TestingData
set.seed(123)
knn_control2 <- trainControl(method  = "repeatedcv",
                             number  = 10, repeats = 3, savePredictions = "final",
                             classProbs = F)

knn_mod2 <- train(as.factor(class) ~ .,
                  data = knn_Train,
                  method     = "knn",
                  tuneGrid   = expand.grid(k = 1:8),
                  trControl  = knn_control2,
                  metric     = "Accuracy",
                  preProcess = c('center', 'scale'),
                  tuneLength = 10)


knn_mod2

plot(knn_mod2)


# Predicting the values for train dataset
knn_Train$Pred <- predict(knn_mod2, newdata = knn_Train, "raw")


# Building classification table
kNN_tab <- table(knn_Train$class, knn_Train$Pred)

#confusion matrix for training model
CM_kNNtrain<-confusionMatrix(kNN_tab)
CM_kNNtrain


# Predicting the class for test dataset
knn_Test$Pred <- predict(knn_mod2, newdata = knn_Test, "raw")

# Building classification table
knn_tab2 <- table(knn_Test$class, knn_Test$Pred)
knn_tab2

CM_knn2<-confusionMatrix(knn_tab2)
CM_knn2


#code from rweka pdfhttps://cran.r-project.org/web/packages/RWeka/RWeka.pdf
kNN_mod<-IBk(class~., data=knn_Train, control=Weka_control(K=3, X = TRUE)) 
evaluate_Weka_classifier(kNN_mod, numFolds = 10)

kNN_pred<-predict(kNN_mod, kn_Test, type="class")  
kNN_cm<-table(knn_Test$class, kNN_pred, dnn=c("Actual", "Predicted"))
confusionMatrix(knn_Test$class, knn_pred) #evaluate model
