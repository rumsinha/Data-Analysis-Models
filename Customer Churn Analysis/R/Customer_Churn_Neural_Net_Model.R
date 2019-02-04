library("openxlsx")
library(ggplot2)
library(dplyr)
library(caret)
library(nnet)
library(ROCR)

#reading the customer data
customer_churn_data <-
  read.xlsx(
    "Telco Churn.xlsx",
    sheet = "WA_Fn-UseC_-Telco-Customer-Chur",
    startRow = 1,
    colNames = TRUE
  )


dim(customer_churn_data)  #7043 rows   21 variables
names(customer_churn_data)

names(customer_churn_data)
# [1] "customerID"       "gender"           "SeniorCitizen"    "Partner"          "Dependents"
# [6] "tenure"           "PhoneService"     "MultipleLines"    "InternetService"  "OnlineSecurity"
# [11] "OnlineBackup"     "DeviceProtection" "TechSupport"      "StreamingTV"      "StreamingMovies"
# [16] "Contract"         "PaperlessBilling" "PaymentMethod"    "MonthlyCharges"   "TotalCharges"
# [21] "Churn"

str(customer_churn_data)
summary(customer_churn_data)

customer_churn_data <- customer_churn_data[,-1]
customer_churn_data$gender <- as.factor(customer_churn_data$gender)
customer_churn_data$SeniorCitizen <- as.factor(customer_churn_data$SeniorCitizen)
customer_churn_data$Partner <- as.factor(customer_churn_data$Partner)
customer_churn_data$Dependents <- as.factor(customer_churn_data$Dependents)
customer_churn_data$PhoneService <- as.factor(customer_churn_data$PhoneService)
customer_churn_data$MultipleLines <- as.factor(customer_churn_data$MultipleLines)
customer_churn_data$InternetService <- as.factor(customer_churn_data$InternetService)
customer_churn_data$OnlineSecurity <- as.factor(customer_churn_data$OnlineSecurity)
customer_churn_data$OnlineBackup <- as.factor(customer_churn_data$OnlineBackup)
customer_churn_data$DeviceProtection <- as.factor(customer_churn_data$DeviceProtection)
customer_churn_data$TechSupport <- as.factor(customer_churn_data$TechSupport)
customer_churn_data$StreamingTV <- as.factor(customer_churn_data$StreamingTV)
customer_churn_data$StreamingMovies <- as.factor(customer_churn_data$StreamingMovies)
customer_churn_data$Contract <- as.factor(customer_churn_data$Contract)
customer_churn_data$PaperlessBilling <- as.factor(customer_churn_data$PaperlessBilling)
customer_churn_data$PaymentMethod <- as.factor(customer_churn_data$PaymentMethod)
customer_churn_data$Churn <- as.factor(customer_churn_data$Churn)

customer_churn_data$TotalCharges <- as.numeric(customer_churn_data$TotalCharges)

customer_churn_data <-
  subset(customer_churn_data,
         !is.na(customer_churn_data$TotalCharges))

sum(is.na(customer_churn_data))

#Spliting training set into two parts based on outcome: 70% and 30%
index <-
  createDataPartition(customer_churn_data$Churn,
                      p = 0.70,
                      list = FALSE)
trainSet <- customer_churn_data[index, ]
testSet <- customer_churn_data[-index, ]

#Checking the structure of trainSet
str(trainSet)

predictors <- c(
  "gender",
  "SeniorCitizen",
  "Partner" ,
  "Dependents" ,
  "PhoneServiceYes",
  "tenure",
  "InternetService" ,
  #"DeviceProtection"   ,   
 # "TechSupport"  ,
 # "StreamingTV"   ,
  #"StreamingMovies"   ,
  "Contract"  ,  
  "PaperlessBilling"   ,
  "PaymentMethod" ,
  "MonthlyCharges"
)

outcomeName <- "Churn"

model_glm <-
  train(trainSet[, names(trainSet) %in% c(predictors)], trainSet[, outcomeName], method =
          'glm')

#Predictions
predictions <-
  predict.train(object = model_glm, testSet[, names(testSet) %in% c(predictors)], type =
                  "raw")

confusionMatrix(predictions, testSet[, outcomeName])

#misclassification rate
1-0.7965  #Accuracy

#prepare the model for ROC Curve
test.glm = predict(model_glm, type = 'prob', newdata = testSet[!names(testSet) %in% ("Churn")])
glmpred = prediction(test.glm[,2], testSet$Churn)
glmperf = performance(glmpred, 'tpr', 'fpr')

perf_AUC=performance(glmpred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
AUC # 0.8348087

# finding the model coefficients for the GLM model
model_glm$finalModel$coefficients

# model building with the caret train function
# The summaryFunction argument is used to pas in a function that takes the observed and predicted
# will compute measures specific to two-class problems, such as the area under the ROC curve, the sensitivity and specificity. 
# Since the ROC curve is based on the predicted class probabilities (which are not computed automatically), another option is required. 
# The classProbs = TRUE option is used to include these calculations. values and estimate some measure of performance
# verboseIter = TRUE to print the training log
# thresh : a cutoff for the cumulative percent of variance to be retained by PCA
# pcaComp the specific number of PCA components to keep. If specified, this over-rides thresh
# k the number of nearest neighbors from the training set to use for imputation
numFolds <- trainControl(method = 'cv', number = 10, classProbs = TRUE, 
                         verboseIter = TRUE, summaryFunction = twoClassSummary, 
                         preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))

#Grid Search: Manual Grid
#The second way to search algorithm parameters is to specify a tune grid manually
#the search of a manual tune grid with hidden neurons 5 and 1 hidden layer and threshold value specified as 0.1
cust.churn.nn <- train(Churn ~ ., data = trainSet, method = 'nnet', 
                       trControl = numFolds, tuneGrid=expand.grid(size=c(10,5), decay=c(0.1)))

#train data
pred<-predict(cust.churn.nn)
pred
caret::confusionMatrix(xtabs(~pred+trainSet$Churn))

#test data
pred<-predict(cust.churn.nn, newdata=testSet[!names(testSet) %in% ("Churn")])
pred
caret::confusionMatrix(xtabs(~pred+testSet$Churn))

#prepare the model for ROC Curve
test.nnet = predict(cust.churn.nn, type = 'prob', newdata = testSet[!names(testSet) %in% ("Churn")])
nnetpred = prediction(test.nnet[,2], testSet$Churn)
nnetperf = performance(nnetpred, 'tpr', 'fpr')

perf_AUC=performance(nnetpred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
AUC 




