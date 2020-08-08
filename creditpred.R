
#----------------------------------------------------------------- Predictive Analytics Assignment --------------------------------------------------------------


# *********** 1) Data reading and formatting (5 points): ************

# Reading the file credit_card_train.csv into mydata

mydata<-read.csv("Credit_Card_train.csv")

#  Formatting imported data into data.frame
newdata<-data.frame(mydata)

# Checking the dimension of the imported data
dim(newdata)

# Analysing the imported data with the help of R summary function
summary(newdata)

# Setting the seed to fixed value, so the value wont change =
set.seed(768)


library(glmnet)

# Data cleaning or Data preprocessing

## Analysing, the Missing Values of AGE variables in the imported data and index are being stored in the variable ma,
# for futher analysis
ma<-which(is.na(newdata$AGE))
# Converting the data format into matrix
m<-as.matrix(ma)
head(ma)
# Extracting records with missang AGE variables into new dataset named missing_age for the prediction
missing_age=newdata[m,2:25]
# Converting the varaible into proper data_frame for statistical analysis
missing_age=as.data.frame(missing_age)
# analysing the missing age data frame
head(missing_age)



## Analysing, if there is any missing values in the whole dataset
mv<-which(is.na(newdata))
head(mv)
print(newdata[79558 ,])
dim(mv)

# Removing all the records with the empty records and storing the new dataset as clean,data
clean.data <- newdata[!apply(is.na(newdata),1,any),]
# Checking, if there is any empty records in clean.data
which(is.na(clean.data))
which(is.na(clean.data$AGE))


# Dividing the dataset into train and validation with the ratio of 70:30
splitratio <- 0.7;
N <- nrow(clean.data)
# Random sampling of index numbers
train.ind <- sample.int(N,floor(splitratio*N))
# Assigning dataset as train
train<-clean.data[train.ind,2:25]
# Assigning the data set a validation
validation<-clean.data[-train.ind,2:25]

# Analysing the train and validation dataset with the help of summary function
summary(train)
summary(validation)

# Performing standardisation of large varying variables to make it mean close to zero
# Hence,improving the convergence rate and making the penality lambda fair for ridge and lasso regularisation

#*************************************** Standardisation of variables ****************************************
limit_mean= mean(train$LIMIT_BAL)
limit_sd= sd(train$LIMIT_BAL)
train$LIMIT_BAL=(train$LIMIT_BAL-limit_mean)/limit_sd
validation$LIMIT_BAL=(validation$LIMIT_BAL-limit_mean)/limit_sd
missing_age$LIMIT_BAL=(missing_age$LIMIT_BAL-limit_mean)/limit_sd

bill_1m=mean(train$BILL_AMT1)
bill_1sd=sd(train$BILL_AMT1)
train$BILL_AMT1=(train$BILL_AMT1-bill_1m)/bill_1sd
validation$BILL_AMT1=(validation$BILL_AMT1-bill_1m)/bill_1sd
missing_age$BILL_AMT1=(missing_age$BILL_AMT1-bill_1m)/bill_1sd


bill_2m=mean(train$BILL_AMT2)
bill_2sd=sd(train$BILL_AMT2)
train$BILL_AMT2=(train$BILL_AMT2-bill_2m)/bill_2sd
validation$BILL_AMT2=(validation$BILL_AMT2-bill_2m)/bill_2sd
missing_age$BILL_AMT2=(missing_age$BILL_AMT2-bill_2m)/bill_2sd


bill_3m=mean(train$BILL_AMT3)
bill_3sd=sd(train$BILL_AMT3)
train$BILL_AMT3=(train$BILL_AMT3-bill_3m)/bill_3sd
validation$BILL_AMT3=(validation$BILL_AMT3-bill_3m)/bill_3sd
missing_age$BILL_AMT3=(missing_age$BILL_AMT3-bill_3m)/bill_3sd


bill_4m=mean(train$BILL_AMT4)
bill_4sd=sd(train$BILL_AMT4)
train$BILL_AMT4=(train$BILL_AMT4-bill_4m)/bill_4sd
validation$BILL_AMT4=(validation$BILL_AMT4-bill_4m)/bill_4sd
missing_age$BILL_AMT4=(missing_age$BILL_AMT4-bill_4m)/bill_4sd

bill_5m=mean(train$BILL_AMT5)
bill_5sd=sd(train$BILL_AMT5)
train$BILL_AMT5=(train$BILL_AMT5-bill_5m)/bill_5sd
validation$BILL_AMT5=(validation$BILL_AMT5-bill_5m)/bill_5sd
missing_age$BILL_AMT5=(missing_age$BILL_AMT5-bill_5m)/bill_5sd


bill_6m=mean(train$BILL_AMT6)
bill_6sd=sd(train$BILL_AMT6)
train$BILL_AMT6=(train$BILL_AMT6-bill_6m)/bill_6sd
validation$BILL_AMT6=(validation$BILL_AMT6-bill_6m)/bill_6sd
missing_age$BILL_AMT6=(missing_age$BILL_AMT6-bill_6m)/bill_6sd

p_1m=mean(train$PAY_AMT1)
p_1sd=sd(train$PAY_AMT1)
train$PAY_AMT1=(train$PAY_AMT1-p_1m)/p_1sd
validation$PAY_AMT1=(validation$PAY_AMT1-p_1m)/p_1sd
missing_age$PAY_AMT1=(missing_age$PAY_AMT1-p_1m)/p_1sd

p_2m=mean(train$PAY_AMT2)
p_2sd=sd(train$PAY_AMT2)
train$PAY_AMT2=(train$PAY_AMT2-p_2m)/p_2sd
validation$PAY_AMT2=(validation$PAY_AMT2-p_2m)/p_2sd
missing_age$PAY_AMT2=(missing_age$PAY_AMT2-p_2m)/p_2sd


p_3m=mean(train$PAY_AMT3)
p_3sd=sd(train$PAY_AMT3)
train$PAY_AMT3=(train$PAY_AMT3-p_3m)/p_3sd
validation$PAY_AMT3=(validation$PAY_AMT3-p_3m)/p_3sd
missing_age$PAY_AMT3=(missing_age$PAY_AMT3-p_3m)/p_3sd

p_4m=mean(train$PAY_AMT4)
p_4sd=sd(train$PAY_AMT4)
train$PAY_AMT4=(train$PAY_AMT4-p_4m)/p_4sd
validation$PAY_AMT4=(validation$PAY_AMT4-p_4m)/p_4sd
missing_age$PAY_AMT4=(missing_age$PAY_AMT4-p_4m)/p_4sd


p_5m=mean(train$PAY_AMT5)
p_5sd=sd(train$PAY_AMT5)
train$PAY_AMT5=(train$PAY_AMT5-p_5m)/p_5sd
validation$PAY_AMT5=(validation$PAY_AMT5-p_5m)/p_5sd
missing_age$PAY_AMT5=(missing_age$PAY_AMT5-p_5m)/p_5sd



p_6m=mean(train$PAY_AMT6)
p_6sd=sd(train$PAY_AMT6)
train$PAY_AMT6=(train$PAY_AMT6-p_6m)/p_6sd
validation$PAY_AMT6=(validation$PAY_AMT6-p_6m)/p_6sd
missing_age$PAY_AMT6=(missing_age$PAY_AMT6-p_6m)/p_6sd

# *************************************************************************** End of Standardisation ****************************************************************************

# Analysing the train, validation and missing age data frame, after the standardisation
summary(train)
summary(validation)
summary(missing_age)


# **************************************** 2. Missing value handling (20 points): *********************************************
# Extracting all the variables from train except 'AGE' and formatting it as matrix with dummy variables
x <- model.matrix(~.-AGE,train)
head(x)
dim(x)

# Extracting the AGE from train as response variables
y <- train$AGE

# Implementing the ridge model
models.ridge <- cv.glmnet(x,y,family="gaussian",alpha=0)
# Extracting all other data values except age from the missing age data frame for age prediction using ridge model
t<-missing_age[,-5]
# converting into desired format using model.matrix function
test<-model.matrix(~.,t)
dim(test)
head(test)

# Predicting Missing Age witht the help of ridge model,
# Note: As studied in class, the ridge model performance better than the lasso in general. And no doubt,
# that it peforms better than the simple linear model with all predictor, which often leads to overfitting.

pred <-predict(models.ridge,test,type='response')
# Converting the predicted age as interger
age<-as.integer(pred)
print(" Predicted ")
print(age)

# Assigning the predicted age to the missing age data frame
missing_age$AGE<-age
# Analysing the missing age data frame after updating the missing age
head(missing_age)



# Updating the train data set by addinng updata missing age data frame with the help of rbind
train<-rbind(train,missing_age)
# checking, if the data had been added properly or not
dim(train)

# checking if there is any missing values in the updatad file of train data frame
which(is.na(train))
head(train)
####

## standardising the age to scale down the age to the mean of 0
age_m=mean(train$AGE)
age_sd=sd(train$AGE)
train$AGE=(train$AGE-age_m)/age_sd
validation$AGE=(validation$AGE-age_m)/age_sd

summary(train)
summary(validation)



#************************************************ 3.1  Variable selection using lasso feature selection  *****************************************************

# Extracting all the variables except default.payment.next.month from train data frame to x1
x1 <- model.matrix(~.-default.payment.next.month,train)
head(x1)
dim(x1)
# Extracting only default.payment.next.month from train for lasso modelling
y1 <- train$default.payment.next.month
head(y1)

# Lasso modelling with cv.glmnet()
models.lasso <- cv.glmnet(x1,y1,family='binomial',alpha=1)

# Printing the coefficient of lasoo model, as we know lasso converges the coefficient to zero and retaining only important variables
coef(models.lasso)

# The lasso feature selection selects total of 7 important variables from the list of 24 variables, they are LIMIT_BAL , PAY_0 , PAY_2, PAY_3, PAY_5, BILL_AMT1, PAY_AMT1 .
# Those 7 selected variables will be used as the key predictor for modeling classifier for the upcoming task.




#***************************** 3.2.1) First modeling with logistic regression for the prediction of default.payment.next.month K-fold cross validation *****************************************

library(caret)

# converting the default next month payment into factor for making response variable more discrete than continuos value between 0-1
train$default.payment.next.month<-as.factor(train$default.payment.next.month)

# define training control with repeaed cross validation with the size of 10 and repetition of 6
train_control <- trainControl(method="repeatedcv", number=10, repeats=4)

stm<-proc.time()

# ****************** 3.3.1 Cross validation ,parameter slection and model performance **************************
# Training the logistic regression with the k-fold cross validation of size 10
fit <- train(default.payment.next.month ~ LIMIT_BAL  + PAY_0 + PAY_2 + PAY_3 + PAY_5 + BILL_AMT1 + PAY_AMT1 ,
          data=train, trControl = train_control,method = "glm",family=binomial('logit'))
lg_tr<-proc.time()-stm
cat("Training time :",lg_tr[1])
print(fit)
summary(fit)


## Converting defaul payment as factors
validation$default.payment.next.month<-as.factor(validation$default.payment.next.month)

## Generating Confusion matrix of the logistic model
train_res <- confusionMatrix(predict(fit,train[,-24]),train[,24])
cat('Accuracy on trainning set:',train_res$overall[1])


res <- confusionMatrix(predict(fit,validation[,-24]),validation[,24])
cat('Accuracy on validation set:',res$overall[1])


# ******************* Tesing model complexity Vs model performance Tradeoff ********************************************************************
fit1 <- train(default.payment.next.month ~ LIMIT_BAL  + I(PAY_0^2) + I(PAY_2^2) + I(PAY_3^2) + I(PAY_5^2) + BILL_AMT1 + PAY_AMT1, data=train, trControl = train_control,method = "glm",family=binomial('logit'))

train_res <- confusionMatrix(predict(fit1,train[,-24]),train[,24])
cat('Accuracy on trainning set:',train_res$overall[1])


res <- confusionMatrix(predict(fit1,validation[,-24]),validation[,24])
cat('Accuracy on validation set:',res$overall[1])

#************************************* end of test  ******************************************************************************************

#********************************************** Efficacy and model complexity trade of disscussion *******************************************

# After cross validation of logistic regression classifier, the cross validation accuracy with single degree is about 80% and validation accuracy is 79%. In general, We can futher improve the CV accuracy by
# increasing the model complexity by introducing higher polynomial degree. However, it may effects the generalisation of validation set . Indeed, in this particular scenario , the increase in model complexity didnt yeild a
# better accuracy on test and validation. Futhermore, the increase in polinomial degree had a adverse effect on model performance and droped it performance by almost 2% on both sets. To conclude, the current model with single degree is optimum.



#*************************************************************************** 3.2.2 Second model with SVM classification ********************************************************

# importing svm library
library(e1071)
# Tuning the hyperparameter gamma and cost on training set for the svm
stm1<-proc.time()

# ****************** 3.2.2 Cross validation, hyperparameter slection and model performance **************************
models <- tune(svm,default.payment.next.month ~ LIMIT_BAL + PAY_0 + PAY_2 + PAY_3 + PAY_5 + BILL_AMT1 + PAY_AMT1 , data = train,
               ranges = list(gamma = 2^(-1:2), cost = 2^(1:3),kernel = c('radial')),
               tunecontrol = tune.control(sampling = "fix"))
svm_tr<-proc.time()-stm

cat(" SVM Training time :",svm_tr[1])

# Analysing the tuned SVM with different hyperparameters and their error rates. The below info gives an overview of efficacy and model complexity trade-off
# We choose the hyper-parameter with the least error rates.
summary(models)
# Selecting the best SVM model
svmmodel <- models$best.model
print(svmmodel)

# Using the best model prediction for accuracy test on training set
svm_tr<- table(train$default.payment.next.month,predict(svmmodel,newdata=train))
cat('SVM training accuracy:',sum(diag(svm_tr))/nrow(train))

# Using the best model prediction for accuracy test on validation set
svm_val <- table(validation$default.payment.next.month,predict(svmmodel,newdata=validation))
cat('SVM validation accuracy:',sum(diag(svm_val))/nrow(validation))


# \***************************************************** Efficay and model complexity trade off test *************************
svmmodel1<-svm(default.payment.next.month ~ LIMIT_BAL + PAY_0 + PAY_2 + PAY_3 + PAY_5 + BILL_AMT1 + PAY_AMT1 , data = train,
               ranges = list(gamma = 4, cost = 8,kernel = c('radial')),
               tunecontrol = tune.control(sampling = "fix"))

print(svmmodel1)

svm_test<- table(train$default.payment.next.month,predict(svmmodel1,newdata=train))
cat('SVM training accuracy:',sum(diag(svm_test))/nrow(train))

# Using the best model prediction for accuracy test on validation set
svm_val_test <- table(validation$default.payment.next.month,predict(svmmodel1,newdata=validation))
cat('SVM validation accuracy:',sum(diag(svm_val_test))/nrow(validation))

# ******************************************************** Bonus- Efficay trade-off discussion ***********************

# The current model with gamma 0.5 and cost 2 is selected hyperparamter as a best model with the 5099 supoort vectors , as the gamma is set to 4 and cost to 8, the number of support vector reduces to
# 4636 and also ultimately, affecting the model performance. Thus, the trade off between model complexity and performace, has to be conducted to get the optimum
# model performance. After the conducting the model performance measure with the model complexity, the best hyperparameter is chosen to yield best model. However,
# the  tuning of hyperparame




#********************************************************************** 3.2.3 Third model with Feed Forward Neural Network classification ******************************

# Defining prednn cu
prednn <- function(nnetclsmodel,newdata) predict(nnetclsmodel,newdata=newdata,type='class')

library(nnet)
stm<-proc.time()
# ****************** 3.2.3 Parameter slection and model performance **************************
nnmodels <- tune(nnet, default.payment.next.month ~ LIMIT_BAL  + PAY_0 + PAY_2 + PAY_3 + PAY_5 + BILL_AMT1 + PAY_AMT1 , data = train,
                 ranges = list(size = 2:6,decay=2^(-(c(1:10)))),
                 tunecontrol = tune.control(sampling = "fix"),predict.func = prednn)

ffn_tr<-proc.time()-stm
cat(" FFNN Training time :",ffn_tr[1])
# Slecting the best model
# The following weights are selected as the optimum parameter for thit FFNN
summary(nnmodels$best.model)
# the another hyperparmater decay that had been selected is 0.0009765625
print( nnmodels$best.model)
# Assigning the best model to a variable
nnetclsmodel <- nnmodels$best.model
#

nnpred_test <- table(train$default.payment.next.month,predict(nnetclsmodel,newdata=train[,-24],type='class'))
cat('Feed Forward Neural Network accuracy on training set:',sum(diag(nnpred_test))/nrow(train))

nnpred_val <- table(validation$default.payment.next.month,predict(nnetclsmodel,newdata=validation[,-24],type='class'))
cat('Feed Forward Neural Network accuracy on validation set:',sum(diag(nnpred_val))/nrow(validation))






#******************************************************************** Fourth modeling wiht Random forest  *********************************************

#  Reasons for doing fourth modeling with Random Forest
#  1) While, benchmarking the above three models their performance measures are almost equivalent. In order, to get better performing model, the Random Forest classifier had been adopted
#  2) Since, We know that the Random Forest takes the number of week trees and aggreagate their output to give the final one.
#  3) Indeed, the random forest hyperparameter are tuned using a for loop and the performance are recoreded. The hyper parameter with the highest model performance is selected.
#  4) Finally, the adoption of Random forest classfier proved worthwhile and perfomed better than logistic regression, SVM and FFNN in this particular task and gave the validation accuracy of 96%
#  The model hyper parmater is ntr = number of trees and mtry = number of random sampled variables
# importing random forest library
library(randomForest)

# Declaring vector variable to store random forest accuracy
a=c()
i=5
# Using for-loop to iterate random forest model with varying numbers of random sampling numbers
stm<-proc.time()
# ****************** 3.3 Hyperparameter slection and model performance **************************
for (i in 3:8){
    # Default the size of number of tree is set to 500
    random_model <- randomForest( default.payment.next.month ~ LIMIT_BAL  + PAY_0 + PAY_2 + PAY_3 + PAY_5 + BILL_AMT1 + PAY_AMT1, data = validation, ntree = 500, mtry = i, importance = TRUE)
    # Predicting the default payment on train set
    predValid <- predict(random_model, train[,-24], type = "class")
    a[i-2] = mean(predValid == train[,24])}

print(a)
plot(3:8,a, main = "Training Accuracy over number of random sampled variables", ylab = " Training Accuracy", xlab = "Number of Radom sampled data", type = "l", col="blue")
points(3,a[1], cex=5 ,pch=4, col="red")

rdf_tr<-proc.time()-stm
cat(" Random Forest Training time :",rdf_tr[1])

# The hyperparamer that had been choosen for this model is mtry=3 and ntree=500 as it had highest validation accuracy
# Hence, the model with the highest accuracy rate is selected
random_model1 <- randomForest(default.payment.next.month ~ LIMIT_BAL  + PAY_0 + PAY_2 + PAY_3 + PAY_5 + BILL_AMT1 + PAY_AMT1, data = validation, ntree = 500, mtry = 3, importance = TRUE)

predVAldation <- predict(random_model1, validation, type = "class")

Pred_Random <- table(validation$default.payment.next.month,predict(random_model1, validation, type = "class"))
cat('Random Forest testing accuracy:',sum(diag(Pred_Random))/nrow(validation))


# ******************************************************* Bonus- Efficacy and model complexity trade-off ************************************************

# From the plot of CV accuracy and number of random sample, we can cleary draw that, with the increase of mtry "Number of random sampled data " , the accuracy of model varies and
# often, the with the increased of hyperparameters and model complexity the performance model varies and optimum hyperparameter should be choosen to have best model
# The hyperparameter with the hihgest training accuracy had been choosen to test on validation set.


### Printing all the accuracy of different models
#******************************************** Model with Validation Accuracy *********************************************************************
paste('Logistic regression, validation Accuracy : ',res$overall[1])
paste('Support Vector Machine, validation accuracy:',sum(diag(svm_val))/nrow(validation))
paste('Feed Forward Neural Network accuracy on validation set:',sum(diag(nnpred_val))/nrow(validation))
paste('Random Forest Validation accuracy:',sum(diag(Pred_Random))/nrow(validation))

# *************************** Barplot******************************************

acc<-c(res$overall[1],sum(diag(svm_val))/nrow(validation),sum(diag(nnpred_val))/nrow(validation),sum(diag(Pred_Random))/nrow(validation))

print(acc)
acc<-as.matrix(acc,nrow(1))
dim(acc)

print(acc)
barplot(acc,beside = TRUE,col = blues9,legend.text =c("Logistic regression","Support Vector Machine","Feed Forward Neural Network","Random Forest Validation accuracy"),
       space = 0.15, main = " Model Accuracy")


# ********************************************************* 4 Final, model selection ******************************

# Finally, after the comparison of all model performance on validation set . The Random Forest Classifier stands out from the rest of model and has highest
# validation accuracy rate. Hence, the Random Forest classifier is choosen as the ultimate model for predicting the default.nextmont.payment .
