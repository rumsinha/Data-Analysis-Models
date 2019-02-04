library("openxlsx")
library(ggplot2)
library(dplyr)
library(caret)

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
customer_churn_data <-
  subset(customer_churn_data,
         !is.na(customer_churn_data$TotalCharges))

sum(is.na(customer_churn_data))

# Churn
prop.table(table(customer_churn_data$Churn))

customer_churn_data$tenureCategory <-
  cut(customer_churn_data$tenure, seq(0, 75, 5), right = FALSE)

customer_churn_data$tenureCategoryLabels <-
  cut(
    customer_churn_data$tenure,
    seq(0, 75, 5),
    right = FALSE,
    labels = c(1:15)
  )

customer_churn_databkup <- customer_churn_data


customer_churn_data <-
  customer_churn_data[, !names(customer_churn_data) %in% c("customerID", "tenure", "MonthlyCharges", "TotalCharges")]

#Now, creating dummy variables using one hot encoding:

#Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = customer_churn_data, fullRank = T)
customer_churn_transformed <-
  data.frame(predict(dmy, newdata = customer_churn_data))

#Checking the structure of transformed train file
str(customer_churn_transformed)

sum(is.na(customer_churn_transformed))

customer_churn_transformed$MonthlyCharges <-
  customer_churn_databkup$MonthlyCharges

head(customer_churn_transformed)
tail(customer_churn_transformed)

sum(is.na(customer_churn_transformed))

names(customer_churn_transformed)

#Converting the dependent variable back to categorical
customer_churn_transformed$ChurnYes <-
  as.factor(customer_churn_transformed$ChurnYes)

findLinearCombos(customer_churn_transformed)
# "InternetServiceNo"                    "OnlineSecurityNo.internet.service"  Linear Combos
# "OnlineBackupNo.internet.service"      "InternetServiceNo"
# "DeviceProtectionNo.internet.service"  "InternetServiceNo"
# "TechSupportNo.internet.service"       "InternetServiceNo"
# "StreamingTVNo.internet.service"       "InternetServiceNo"
# "StreamingMoviesNo.internet.service"   "InternetServiceNo"
#
# Because of Linear Combination we will not include the below columns:
#
# OnlineSecurityNo.internet.service,     OnlineBackupNo.internet.service,
# DeviceProtectionNo.internet.service,   TechSupportNo.internet.service
# StreamingTVNo.internet.service,        StreamingMoviesNo.internet.service
# "tenureCategoryLabels.2"               "tenureCategoryLabels.3"
# "tenureCategoryLabels.4"               "tenureCategoryLabels.5"
# "tenureCategoryLabels.6"               "tenureCategoryLabels.7"
# "tenureCategoryLabels.8"               "tenureCategoryLabels.9"
# "tenureCategoryLabels.10"              "tenureCategoryLabels.11"
# "tenureCategoryLabels.12"              "tenureCategoryLabels.13"
# "tenureCategoryLabels.14"              "tenureCategoryLabels.15"    

#Spliting training set into two parts based on outcome: 70% and 30%
index <-
  createDataPartition(customer_churn_transformed$Churn,
                      p = 0.70,
                      list = FALSE)
trainSet <- customer_churn_transformed[index, ]
testSet <- customer_churn_transformed[-index, ]

#Checking the structure of trainSet
str(trainSet)

#Feature selection using rfe in caret
control <- rfeControl(
  functions = rfFuncs,
  method = "repeatedcv",
  repeats = 3,
  verbose = FALSE
)
outcomeName <- 'ChurnYes'
predictors <- names(trainSet)[!names(trainSet) %in% outcomeName]
Churn_Pred_Profile <-
  rfe(trainSet[, predictors], trainSet[, outcomeName],
      rfeControl = control)

Churn_Pred_Profile

#Taking only the top 5 predictors
predictors <-
  c(
    "ContractOne.year",
    "ContractTwo.year",
    "MonthlyCharges",
    "TechSupportYes",
    "InternetServiceFiber.optic"
  )

model_glm <-
  train(trainSet[, names(trainSet) %in% c(predictors)], trainSet[, outcomeName], method =
          'glm')

#Checking variable importance for GLM
varImp(object = model_glm)
#glm variable importance

#Plotting Variable importance for GLM
plot(varImp(object = model_glm), main = "GLM - Variable Importance")

#Predictions
predictions <-
  predict.train(object = model_glm, testSet[, names(trainSet) %in% c(predictors)], type =
                  "raw")

confusionMatrix(predictions, testSet[, outcomeName])

#missclassification rate
(225+242)/2112   # 22%

predictors <- c(
  "genderMale", "SeniorCitizen","PartnerYes" , "DependentsYes" ,"PhoneServiceYes",
  #"MultipleLinesNo.phone.service",
  # "MultipleLinesYes",
  "InternetServiceFiber.optic" , "InternetServiceNo" ,
  #"OnlineSecurityNo.internet.service",
  #"OnlineSecurityYes",
  #"OnlineBackupNo.internet.service"  ,
  #"OnlineBackupYes" ,
  # "DeviceProtectionNo.internet.service" ,
  "DeviceProtectionYes"   ,               
  #"TechSupportNo.internet.service",
  "TechSupportYes"  ,
  # "StreamingTVNo.internet.service"     ,
  "StreamingTVYes"   ,
  #"StreamingMoviesNo.internet.service"  ,
  "StreamingMoviesYes"   ,"ContractOne.year"  ,"ContractTwo.year"       ,
  "PaperlessBillingYes"   ,"PaymentMethodCredit.card..automatic." ,
  "PaymentMethodElectronic.check"  ,"PaymentMethodMailed.check",
   "tenureCategory..5.10."   ,             "tenureCategory..10.15."   ,
    "tenureCategory..15.20."   ,            "tenureCategory..20.25." ,
   "tenureCategory..25.30."   ,            "tenureCategory..30.35." ,
   "tenureCategory..35.40."  ,             "tenureCategory..40.45."  ,
   "tenureCategory..45.50."   ,            "tenureCategory..50.55."    ,
    "tenureCategory..55.60."   ,            "tenureCategory..60.65." ,
    "tenureCategory..65.70."    ,           "tenureCategory..70.75."  ,
  # "tenureCategoryLabels.2"    ,
  # "tenureCategoryLabels.3"   ,
  # "tenureCategoryLabels.4"    ,
  # "tenureCategoryLabels.5"  ,
  # "tenureCategoryLabels.6"     ,
  # "tenureCategoryLabels.7" ,
  # "tenureCategoryLabels.8"      ,
  # "tenureCategoryLabels.9" ,
  # "tenureCategoryLabels.10"     ,
  # "tenureCategoryLabels.11" ,
  # "tenureCategoryLabels.12"    ,
  # "tenureCategoryLabels.13" ,
  # "tenureCategoryLabels.14"    ,
  # "tenureCategoryLabels.15" ,
  "MonthlyCharges"
)

model_glm <-
  train(trainSet[, names(trainSet) %in% c(predictors)], trainSet[, outcomeName], method =
          'glm')

#Checking variable importance for GLM
varImp(object = model_glm)
#glm variable importance

#Plotting Variable importance for GLM
plot(varImp(object = model_glm), main = "GLM - Variable Importance")

#Predictions
predictions <-
  predict.train(object = model_glm, testSet[, names(testSet) %in% c(predictors)], type =
                  "raw")

confusionMatrix(predictions, testSet[, outcomeName])

#misclassification rate
(148+276)/2112 #0.20


#CART
model_rpart <-
  train(trainSet[, names(trainSet) %in% c(predictors)], trainSet[, outcomeName], method =
          'rpart')

#Checking variable importance for GLM
varImp(object = model_rpart)
#glm variable importance

#Plotting Variable importance for GLM
plot(varImp(object = model_rpart), main = "rpart - Variable Importance")

#Predictions
predictions <-
  predict.train(object = model_rpart, testSet[, names(testSet) %in% c(predictors)], type =
                  "raw")

confusionMatrix(predictions, testSet[, outcomeName])

#misclassification rate
(148+321)/2112

#Random Forest
model_rf <-
  train(trainSet[, names(trainSet) %in% c(predictors)], trainSet[, outcomeName], method =
          'rf')

#Checking variable importance for RF
varImp(object = model_rf)
#rf variable importance

#Plotting Variable importance for GLM
plot(varImp(object = model_rf), main = "rf - Variable Importance")

#Predictions
predictions <-
  predict.train(object = model_rf, testSet[, names(testSet) %in% c(predictors)], type =
                  "raw")

confusionMatrix(predictions, testSet[, outcomeName])

#misclassification rate
(114+338)/2112 


#taking top 20 features from the feature importance using random forest
predictors <- c(
  "MonthlyCharges",       
  "ContractTwo.year",                   
  "InternetServiceFiber.optic",           
  "PaymentMethodElectronic.check",         
  "ContractOne.year",                      
  "TechSupportYes",                        
  "InternetServiceNo",               
  "PaperlessBillingYes",                  
  "PartnerYes"  ,
  "tenureCategory..70.75.",    
  "DependentsYes",            
  "SeniorCitizen"  ,
  "DeviceProtectionYes",
  "PaymentMethodCredit.card..automatic.",
  "tenureCategory..60.65.",
  "StreamingMoviesYes",
  "tenureCategory..5.10.", 
  "StreamingTVYes", 
  "tenureCategory..10.15." ,
  "genderMale " 
)

model_glm <-
  train(trainSet[, names(trainSet) %in% c(predictors)], trainSet[, outcomeName], method =
          'glm')

#Predictions
predictions <-
  predict.train(object = model_glm, testSet[, names(testSet) %in% c(predictors)], type =
                  "raw")

confusionMatrix(predictions, testSet[, outcomeName])

#misclassification rate
(194+257)/2112 


#neural net model
model_nnet <-
  train(trainSet[, names(trainSet) %in% c(predictors)], trainSet[, outcomeName], method =
          'nnet')

#Predictions
predictions <-
  predict.train(object = model_nnet, testSet[, names(testSet) %in% c(predictors)], type =
                  "raw")

confusionMatrix(predictions, testSet[, outcomeName])

#misclassification rate
(172+272)/2112 


#SVM
model_svm <-
  train(trainSet[, names(trainSet) %in% c(predictors)], trainSet[, outcomeName], method =
          'svmLinear')

#Predictions
predictions <-
  predict.train(object = model_svm, testSet[, names(testSet) %in% c(predictors)], type =
                  "class")

confusionMatrix(predictions, testSet[, outcomeName])

#misclassification rate
(253+214)/2112 

library(ROCR)
#prepare the model for ROC Curve
test.nnet = predict(model_nnet, type = 'prob', newdata = testSet[, names(testSet) %in% c(predictors,outcomeName)])
nnetpred = prediction(test.nnet[,2], testSet$ChurnYes)
nnetperf = performance(nnetpred, 'tpr', 'fpr')

perf_AUC=performance(nnetpred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
AUC # 0.835651

test.glm = predict(model_glm, type = 'prob', newdata = testSet[, names(testSet) %in% c(predictors,outcomeName)])
glmpred = prediction(test.glm[,2], testSet$ChurnYes)
glmperf = performance(glmpred, 'tpr', 'fpr')

perf_AUC=performance(glmpred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
AUC # 0.8333372

# finding the model coefficients for the GLM model
model_glm$finalModel$coefficients

