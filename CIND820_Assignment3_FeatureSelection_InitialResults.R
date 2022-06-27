#Feature Selection, Initial Results & Code

#Figure 27; final review of dataset, removal of redundant or non-predictive variables
str(MCI_db)

#FEATURE SELECTION

#Check for correlation among variables
set.seed(123)
correlationMatrix <- cor(MCI_db[,1:10])

#Figure 28
print(correlationMatrix)

#Figure 29; Display correlation visually
corrplot(correlationMatrix, method="circle")

#Feature Selection method 1: Boruta
#Boruta method of show variable importance; http://r-statistics.co/Variable-Selection-and-Importance-With-R.html; 
#https://www.machinelearningplus.com/machine-learning/feature-selection/
#https://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/
#https://towardsdatascience.com/boruta-explained-the-way-i-wish-someone-explained-it-to-me-4489d70e154a
#libraries: Boruta, mlbench, randomforest

set.seed(123)
boruta.train <- Boruta(as.factor(class)~., data = MCI_db, doTrace = 2)
print(boruta.train)

# Get significant variable
boruta_signif <- getSelectedAttributes(boruta.train, withTentative = TRUE)
print(boruta_signif) 

# Variable Importance Scores
imps <- attStats(boruta.train)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Figure 30: Plot variable importance
plot(boruta.train, cex.axis=.7, las=2, xlab="", main="Variable Importance")  



#Feature Selection Method 2: Stepwise forward and backward elimination; caret and random forest libraries
#https://www.machinelearningplus.com/machine-learning/feature-selection/#4stepwiseforwardandbackwardselection
#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

#Stepwise
set.seed(123)

train.control <- trainControl(method = "cv", number = 10)

step.model <- train(MCI ~., data = MCI_db,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:10),
                    trControl = train.control
)

#summary results
step.model$results

#Dispay model has the lowest RMSE
step.model$bestTune

#Figure 31: Summary showing the optimal set of variables
summary(step.model$finalModel)


#Backwards
set.seed(123)

train.control2 <- trainControl(method = "cv", number = 10)

step.model2 <- train(MCI ~., data = MCI_db,
                     method = "leapBackward", 
                     tuneGrid = data.frame(nvmax = 1:10),
                     trControl = train.control
)

#summary results
step.model2$results

#Display model with the lowest RMSE
step.model2$bestTune

#Figure 32: Summary showing the optimal set of variables
summary(step.model2$finalModel)

#Further reduce features for final classification dataset
MCI_Final<-MCI_db[-c(1,3,7,10)]
str(MCI_Final)


#Table 2: check relative proportions of classes; data set quite imbalanced
MCI_prop<-table(MCI_Final$MCI)
MCI_prop
round(100*prop.table(MCI_prop))


#Addressing class imbalance
#Apply SMOTE method to balance the dataset.  Use smote family library
set.seed(123)
smote<-SMOTE(MCI_Final[,-11], MCI_Final$MCI)
smote=smote$data
Smote_prop<-table(smote$MCI)
Smote_prop
round(100*prop.table(Smote_prop))

#write to csv
write.csv(smote,"D:/Ryerson Big Data/CIND820 Big Data Analytics Project/Assignment3/MCI_mod.csv", row.names = FALSE)

#Rename dataset then split the data: Train & Test
MCI_mod<-smote
#library(caTools)
set.seed(123)
TrainInd<-sample(1:nrow(MCI_mod), 0.8*nrow(MCI_mod))
Train<-MCI_mod[TrainInd,]
Test<-MCI_mod[-TrainInd,]

write.csv(Train,"D:/Ryerson Big Data/CIND820 Big Data Analytics Project/Assignment3/TrainingData.csv", row.names = FALSE)
write.csv(Test,"D:/Ryerson Big Data/CIND820 Big Data Analytics Project/Assignment3/TestingData.csv", row.names = FALSE)


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

#set up traint/test &  10 fold cross validation
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

                