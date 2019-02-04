library("openxlsx")
library(ggplot2)
library(dplyr)
library(sqldf)

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
customer_churn_data$TotalCharges <- as.numeric(customer_churn_data$TotalCharges) 
summary(customer_churn_data)
customer_churn_data <-
  subset(customer_churn_data,
         !is.na(customer_churn_data$TotalCharges))

sum(is.na(customer_churn_data))

# Churn
prop.table(table(customer_churn_data$Churn))

customer_churn_data$tenureCategory <-
  cut(customer_churn_data$tenure, seq(0, 75, 5), right = FALSE)

customer_churn_data$MonthlyChargesCategory <-
  cut(customer_churn_data$MonthlyCharges, seq(0, 120,20), right = FALSE)

customer_churn_data$TotalChargesCategory <-
  cut(customer_churn_data$TotalCharges, seq(0, 9000,1000), right = FALSE)

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

## kmeans cluster
#how many clusters??
library(cluster)
library(fpc)

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
data <- customer_churn_transformed
wss <- sapply(1:k.max,
              function(k) {
                kmeans(data, k, nstart = 50, iter.max = 15)$tot.withinss
              })

wss
plot(
  1:k.max,
  wss,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Total within-clusters sum of squares"
)

rm(data)

#4
kmeans_fit <- kmeans(customer_churn_transformed, 4, nstart = 25)
#kmeans_fit$centers
kmeans_fit$size

customer_churn_transformed$cluster <- kmeans_fit$cluster

customer_churn_data$cluster <- kmeans_fit$cluster

ggplot(data=customer_churn_data, aes(x=cluster,fill=Churn))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=gender))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=Partner))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=Dependents))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=PhoneService))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=MultipleLines))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=InternetService))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=OnlineSecurity))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=OnlineBackup))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=DeviceProtection))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=TechSupport))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=StreamingTV))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=StreamingMovies))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=Contract))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=PaperlessBilling))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=PaymentMethod))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=tenureCategory))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=MonthlyChargesCategory))+ geom_bar(position = "dodge")
ggplot(data=customer_churn_data, aes(x=cluster,fill=TotalChargesCategory))+ geom_bar(position = "dodge")
