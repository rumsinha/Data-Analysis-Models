library("openxlsx")
library(ggplot2)
library(dplyr)

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
str(customer_churn_data)
customer_churn_data$TotalCharges <- as.numeric(customer_churn_data$TotalCharges) 
summary(customer_churn_data)
customer_churn_data <- subset(customer_churn_data, !is.na(customer_churn_data$TotalCharges))

sum(is.na(customer_churn_data))

# Churn
prop.table(table(customer_churn_data$Churn))

customer_churn_data$tenureCategory <-
  cut(customer_churn_data$tenure, seq(0, 75, 5), right = FALSE)

customer_churn_data$MonthlyChargesCategory <-
  cut(customer_churn_data$MonthlyCharges, seq(0, 120,20), right = FALSE)

customer_churn_data$TotalChargesCategory <-
  cut(customer_churn_data$TotalCharges, seq(0, 9000,1000), right = FALSE)

# customerID
length(unique(customer_churn_data$customerID))

#customer ID is unique ID field assigned to each customer. This will not help in any analytics insight

# gender
customer_churn_data$gender <- as.factor(customer_churn_data$gender)
summary(customer_churn_data$gender)

#bar plot for gender attribute
ggplot(customer_churn_data, aes(x = Churn, fill = gender)) + geom_bar(position = "dodge")

# we can see equal proportion of male and female population across churn and Non Churns

#SeniorCitizen
customer_churn_data$SeniorCitizen <- as.factor(customer_churn_data$SeniorCitizen)
summary(customer_churn_data$SeniorCitizen)

#bar plot for SeniorCitizen attribute
ggplot(customer_churn_data, aes(x = Churn, fill = SeniorCitizen)) + geom_bar(position = "dodge")

# we can see lower proportion of both churn and no churn among SeniorCitizen

#Partner
customer_churn_data$Partner <- as.factor(customer_churn_data$Partner)
summary(customer_churn_data$Partner)

#bar plot for SeniorCitizen attribute
ggplot(customer_churn_data, aes(x = Churn, fill = Partner)) + geom_bar(position = "dodge")

# No Churn has almost equal Partner and No Partner
# Churn has lower Partners and more non Partners

#Dependents
customer_churn_data$Dependents <- as.factor(customer_churn_data$Dependents)
summary(customer_churn_data$Dependents)

#bar plot for SeniorCitizen attribute
ggplot(customer_churn_data, aes(x = Churn, fill = Dependents)) + geom_bar(position = "dodge")

# No Churn has less dependents
# Churn has less dependents
# Dependents don't seem to play any role in the Customer Churn

#tenure
summary(customer_churn_data$tenure)

ggplot(customer_churn_data, aes(x = Churn, y = tenure)) +geom_boxplot()
  
## 
table(customer_churn_data$Churn,customer_churn_data$tenureCategory)

ggplot(customer_churn_data, aes(x = Churn, fill = tenureCategory)) + geom_bar(position = "dodge")

# As the tenure increases the churn counts decreases

#PhoneService

customer_churn_data$PhoneService <- as.factor(customer_churn_data$PhoneService)
summary(customer_churn_data$PhoneService)

#bar plot for PhoneService attribute
ggplot(customer_churn_data, aes(x = Churn, fill = PhoneService)) + geom_bar(position = "dodge")

# PhoneService has higher representation in both Churn and No Churn

#MultipleLines
customer_churn_data$MultipleLines <- as.factor(customer_churn_data$MultipleLines)
summary(customer_churn_data$MultipleLines)

#bar plot for PhoneService attribute
ggplot(customer_churn_data, aes(x = Churn, fill = MultipleLines)) + geom_bar(position = "dodge")

#InternetService
customer_churn_data$InternetService <- as.factor(customer_churn_data$InternetService)
summary(customer_churn_data$InternetService)

#bar plot for PhoneService attribute
ggplot(customer_churn_data, aes(x = Churn, fill = InternetService)) + geom_bar(position = "dodge")

prop.table(table(customer_churn_data$Churn,customer_churn_data$InternetService))
#churn rate more with Fiber Optic

#OnlineSecurity
customer_churn_data$OnlineSecurity <- as.factor(customer_churn_data$OnlineSecurity)
summary(customer_churn_data$OnlineSecurity)

#bar plot for PhoneService attribute
ggplot(customer_churn_data, aes(x = Churn, fill = OnlineSecurity)) + geom_bar(position = "dodge")

prop.table(table(customer_churn_data$Churn,customer_churn_data$OnlineSecurity))

#churn rate more with "No" OnlineSecurity

#OnlineBackup
customer_churn_data$OnlineBackup <- as.factor(customer_churn_data$OnlineBackup)
summary(customer_churn_data$OnlineBackup)

#bar plot for PhoneService attribute
ggplot(customer_churn_data, aes(x = Churn, fill = OnlineBackup)) + geom_bar(position = "dodge")

prop.table(table(customer_churn_data$Churn,customer_churn_data$OnlineBackup))

#churn rate more with "No" OnlineBackup

#DeviceProtection
customer_churn_data$DeviceProtection <- as.factor(customer_churn_data$DeviceProtection)
summary(customer_churn_data$DeviceProtection)

#bar plot for DeviceProtection attribute
ggplot(customer_churn_data, aes(x = Churn, fill = DeviceProtection)) + geom_bar(position = "dodge")

prop.table(table(customer_churn_data$Churn,customer_churn_data$DeviceProtection))
#churn rate more with "No" DeviceProtection

#TechSupport
customer_churn_data$TechSupport <- as.factor(customer_churn_data$TechSupport)
summary(customer_churn_data$TechSupport)

#bar plot for TechSupport attribute
ggplot(customer_churn_data, aes(x = Churn, fill = TechSupport)) + geom_bar(position = "dodge")

prop.table(table(customer_churn_data$Churn,customer_churn_data$TechSupport))
#churn rate more with "No" TechSupport

#StreamingTV
customer_churn_data$StreamingTV <- as.factor(customer_churn_data$StreamingTV)
summary(customer_churn_data$StreamingTV)

#bar plot for TechSupport attribute
ggplot(customer_churn_data, aes(x = Churn, fill = StreamingTV)) + geom_bar(position = "dodge")

prop.table(table(customer_churn_data$Churn,customer_churn_data$StreamingTV))
#churn rate more with "No" StreamingTV and "Yes" StreamingTV

#StreamingMovies
customer_churn_data$StreamingMovies <- as.factor(customer_churn_data$StreamingMovies)
summary(customer_churn_data$StreamingMovies)

#bar plot for TechSupport attribute
ggplot(customer_churn_data, aes(x = Churn, fill = StreamingMovies)) + geom_bar(position = "dodge")

prop.table(table(customer_churn_data$Churn,customer_churn_data$StreamingMovies))
#churn rate more with "No" StreamingMovies and "Yes" StreamingMovies

#Contract
customer_churn_data$Contract <- as.factor(customer_churn_data$Contract)
summary(customer_churn_data$Contract)

#bar plot for Contract attribute
ggplot(customer_churn_data, aes(x = Churn, fill = Contract)) + geom_bar(position = "dodge")

prop.table(table(customer_churn_data$Churn,customer_churn_data$Contract))
#churn rate higher for Month-to-month

#PaperlessBilling
customer_churn_data$PaperlessBilling <- as.factor(customer_churn_data$PaperlessBilling)
summary(customer_churn_data$PaperlessBilling)

#bar plot for PaperlessBilling attribute
ggplot(customer_churn_data, aes(x = Churn, fill = PaperlessBilling)) + geom_bar(position = "dodge")
# Churn rate higher for "Yes: PaperlessBilling
                 
#PaymentMethod
customer_churn_data$PaymentMethod <- as.factor(customer_churn_data$PaymentMethod)
summary(customer_churn_data$PaymentMethod)

#bar plot for PaymentMethod attribute
ggplot(customer_churn_data, aes(x = Churn, fill = PaymentMethod)) + geom_bar(position = "dodge")

prop.table(table(customer_churn_data$Churn,customer_churn_data$PaymentMethod))
# Churn rate higher for "Electronic check" PaymentMethod

#MonthlyCharges
summary(customer_churn_data$MonthlyCharges)
ggplot(customer_churn_data, aes(x = Churn, y = MonthlyCharges, fill=Churn)) + geom_boxplot()

#Churn rate higher for high Monthly Charges

#TotalCharges
#customer_churn_data$TotalCharges<- as.numeric(customer_churn_data$TotalCharges)
summary(customer_churn_data$TotalCharges)
ggplot(customer_churn_data, aes(x = Churn, y = TotalCharges, fill=Churn)) + geom_boxplot()

#Churn rate higher for low TotalCharges

cor(customer_churn_data$MonthlyCharges,customer_churn_data$TotalCharges) #0.6510648

#
ggplot(data=customer_churn_data,aes(x=MonthlyCharges, y=TotalCharges,color=Churn))+geom_point()


#churn and paymentmethod and monthlycharges
ggplot(customer_churn_data, aes(x = PaymentMethod, y = MonthlyCharges, fill=Churn)) + geom_boxplot()


#churn and PaperlessBilling and monthlycharges
ggplot(customer_churn_data, aes(x = PaperlessBilling, y = MonthlyCharges, fill=Churn)) + geom_boxplot()

# Chi Square test
chisq.test(customer_churn_data$gender,
           customer_churn_data$Churn,
           correct = FALSE) # not significant

chisq.test(customer_churn_data$SeniorCitizen,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$Partner,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$Dependents,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$PhoneService,
           customer_churn_data$Churn,
           correct = FALSE) # not significant

chisq.test(customer_churn_data$MultipleLines,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$InternetService,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$OnlineSecurity,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$OnlineBackup,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$DeviceProtection,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$TechSupport,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$StreamingTV,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$StreamingMovies,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$Contract,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$PaperlessBilling,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$PaymentMethod,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$tenureCategory,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$MonthlyChargesCategory,
           customer_churn_data$Churn,
           correct = FALSE) # significant

chisq.test(customer_churn_data$TotalChargesCategory,
           customer_churn_data$Churn,
           correct = FALSE) # significant

# Anova test for Churn and TotalCharges
aovstat <- aov(MonthlyCharges ~ Churn, data = customer_churn_data)
summary(aovstat) # significant

aovstat <- aov(tenure ~ Churn, data = customer_churn_data)
summary(aovstat) # significant

aovstat <- aov(TotalCharges ~ Churn, data = customer_churn_data)
summary(aovstat) # significant

