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

customer_churn_data$tenureCategory <-
  cut(customer_churn_data$tenure, seq(0, 75, 5), right = FALSE)

customer_churn_data$MonthlyChargesCategory <-
  cut(customer_churn_data$MonthlyCharges, seq(0, 120,20), right = FALSE)

customer_churn_data$TotalChargesCategory <-
  cut(customer_churn_data$TotalCharges, seq(0, 9000,1000), right = FALSE)

customer_churn_data <-
  customer_churn_data[, !names(customer_churn_data) %in% c("customerID", "tenure", "MonthlyCharges", "TotalCharges")]

str(customer_churn_data)

library(arules) 
# rules with rhs containing "Churn" only
rules <- apriori(customer_churn_data,parameter = list(minlen=2, supp=0.040, conf=0.8),
                 appearance = list(rhs=c("Churn=No","Churn=Yes"),default="lhs"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect((rules.sorted) [1:30])

subset_rules <- ((rules.sorted) [1:20])

library(arulesViz)
library(visNetwork)
library(colorspace)
plot(subset_rules, method="graph", control=list(type="items"))
     