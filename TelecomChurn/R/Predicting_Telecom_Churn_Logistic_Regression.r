#loading the libraries
install.packages('ggplot2')
library(ggplot2)
install.packages('corrplot')
library(corrplot)
install.packages('ROCR')
library(ROCR)
install.packages('lmtest')
library(lmtest)
install.packages('pscl')
library(pscl)
install.packages('Deducer')
library(Deducer)
install.packages('caret')
library(caret)

#reading data and checking dimensions and five point summary
cellphone.df<- read.csv("C:/Users/mayank.swaroop/Downloads/cellphonedata.csv")

dim(cellphone.df)
names(cellphone.df)

str(cellphone.df)
summary(cellphone.df)

head(cellphone.df)
tail(cellphone.df)

sum(is.na(cellphone.df)) # no null data

##correlation? Highly correlated variables
bc = cor(cellphone.df) #create an object of the features
bc
corrplot.mixed(bc)

#Dataplan and datausage highly correlated
#dataplan and monthlycharge highly correlated
#datausage and monthlycharge highly correlated
#daymins and monthlycharge highly correlated

cellphone.df$Churn<- as.factor(cellphone.df$Churn)
cellphone.df$DataPlan<- as.factor(cellphone.df$DataPlan)
cellphone.df$ContractRenewal<- as.factor(cellphone.df$ContractRenewal)

#scatterplot of DayMins and MonthlyCharge
ggplot(data = cellphone.df, aes(x=DayMins,y=MonthlyCharge,colour=Churn)) +
  geom_point()

#scatterplot of DataUsage and MonthlyCharge
ggplot(data = cellphone.df, aes(x=DataUsage,y=MonthlyCharge,colour=Churn)) +
  geom_point()

hist(cellphone.df$AccountWeeks)
summary(cellphone.df$AccountWeeks)
quantile(cellphone.df$AccountWeeks)

#AccountWeeks ~ Churn not significant
#boxplot of AccountWeeks and Churn
ggplot(cellphone.df, aes(x = Churn, y = AccountWeeks, fill = Churn)) + geom_boxplot() +
  coord_flip()
ggplot(data = cellphone.df, aes(AccountWeeks, fill = Churn)) +
  geom_density()
aovstat <- aov(AccountWeeks ~ Churn, data = cellphone.df)
summary(aovstat)
TukeyHSD(aovstat) # not significant

table(cellphone.df$ContractRenewal)

table(cellphone.df$DataPlan)

ggplot(cellphone.df, aes(x = ContractRenewal, fill = Churn)) + geom_bar(position = "dodge")

ggplot(cellphone.df, aes(x = DataPlan, fill = Churn)) + geom_bar(position = "dodge")

chisq.test(cellphone.df$DataPlan,
           cellphone.df$Churn,
           correct = FALSE) # significant

chisq.test(cellphone.df$ContractRenewal,
           cellphone.df$Churn,
           correct = FALSE) # significant

chisq.test(cellphone.df$DataPlan,
           cellphone.df$ContractRenewal,
           correct = FALSE) # not significant

hist(cellphone.df$DataUsage)
summary(cellphone.df$DataUsage)
quantile(cellphone.df$DataUsage)

#DataUsage ~ Churn significant
#boxplot of DataUsage and Churn
ggplot(cellphone.df, aes(x = Churn, y = DataUsage, fill = Churn)) + geom_boxplot() +
  coord_flip()
ggplot(data = cellphone.df, aes(DataUsage, fill = Churn)) +
  geom_density()
aovstat <- aov(DataUsage ~ Churn, data = cellphone.df)
summary(aovstat)
TukeyHSD(aovstat)


hist(cellphone.df$CustServCalls)
summary(cellphone.df$CustServCalls)
quantile(cellphone.df$CustServCalls)

#CustServCalls ~ Churn significant
#boxplot of CustServCalls and Churn
ggplot(cellphone.df, aes(x = Churn, y = CustServCalls, fill = Churn)) + geom_boxplot() +
  coord_flip()
ggplot(data = cellphone.df, aes(CustServCalls, fill = Churn)) +
  geom_density()
aovstat <- aov(CustServCalls ~ Churn, data = cellphone.df)
summary(aovstat)
TukeyHSD(aovstat)

hist(cellphone.df$DayMins)
summary(cellphone.df$DayMins)
quantile(cellphone.df$DayMins)

#DayMins ~ Churn significant
#boxplot of DayMins and Churn
ggplot(cellphone.df, aes(x = Churn, y = DayMins, fill = Churn)) + geom_boxplot() +
  coord_flip()
ggplot(data = cellphone.df, aes(DayMins, fill = Churn)) +
  geom_density()
aovstat <- aov(DayMins ~ Churn, data = cellphone.df)
summary(aovstat)
TukeyHSD(aovstat)

hist(cellphone.df$DayCalls)
summary(cellphone.df$DayCalls)
quantile(cellphone.df$DayCalls)

#DayCalls ~ Churn not significant
#boxplot of DayCalls and Churn
ggplot(cellphone.df, aes(x = Churn, y = DayCalls, fill = Churn)) + geom_boxplot() +
  coord_flip()
ggplot(data = cellphone.df, aes(DayCalls, fill = Churn)) +
  geom_density()
aovstat <- aov(DayCalls ~ Churn, data = cellphone.df)
summary(aovstat)
TukeyHSD(aovstat) #not significant

hist(cellphone.df$MonthlyCharge)
summary(cellphone.df$MonthlyCharge)
quantile(cellphone.df$MonthlyCharge)

#MonthlyCharge ~ Churn significant
#boxplot of MonthlyCharge and Churn
ggplot(cellphone.df, aes(x = Churn, y = MonthlyCharge, fill = Churn)) + geom_boxplot() +
  coord_flip()
ggplot(data = cellphone.df, aes(MonthlyCharge, fill = Churn)) +
  geom_density()
aovstat <- aov(MonthlyCharge ~ Churn, data = cellphone.df)
summary(aovstat)
TukeyHSD(aovstat)

hist(cellphone.df$OverageFee)
summary(cellphone.df$OverageFee)
quantile(cellphone.df$OverageFee)

#OverageFee ~ Churn significant
#boxplot of OverageFee and Churn
ggplot(cellphone.df, aes(x = Churn, y = OverageFee, fill = Churn)) + geom_boxplot() +
  coord_flip()

ggplot(data = cellphone.df, aes(OverageFee, fill = Churn)) +
  geom_density()
aovstat <- aov(OverageFee ~ Churn, data = cellphone.df)
summary(aovstat)
TukeyHSD(aovstat)

hist(cellphone.df$RoamMins)
summary(cellphone.df$RoamMins)
quantile(cellphone.df$RoamMins)

#RoamMins ~ Churn significant
ggplot(data = cellphone.df, aes(RoamMins, fill = Churn)) +
  geom_density()
aovstat <- aov(RoamMins ~ Churn, data = cellphone.df)
summary(aovstat)
TukeyHSD(aovstat)

#boxplot of RoamMins and Churn
ggplot(cellphone.df, aes(x = Churn, y = RoamMins, fill = Churn)) + geom_boxplot() +
  coord_flip()

# split 70 percent of the data into the training dataset and 30 percent of the data
# into the testing dataset:
set.seed(123)
ind = sample(2, nrow(cellphone.df), replace = TRUE, prob=c(0.7,0.3))
train.df = cellphone.df[ind == 1,]
test.df = cellphone.df[ind == 2,]
dim(train.df)
dim(test.df)

prop.table(table(train.df$Churn)) 

prop.table(table(test.df$Churn)) 
# we see almost equal representation in both training and testing set for the dependent or response variable

#Model 1 is with all the variables
glmfit <- glm(Churn ~ ., data = train.df, family = binomial)
summary(glmfit)

# the significant coefficients are ContractRenewal, CustServCalls and RoamMins

#Likelihood ratio test
lrtest(glmfit)

#pseudo R square value
pR2(glmfit) # 20% in variance explained by all the variables

#Model 2 with only the 3 significant variables as explained by the above model
glmfit1 <- glm(Churn ~ ContractRenewal+CustServCalls+RoamMins, data = train.df, family = binomial)
summary(glmfit1)

#Likelihood ratio test
lrtest(glmfit1)

#pseudo R square value
pR2(glmfit1) # 11% in variance explained by the 3 variables


#Model 3 based on all those variables that are significant based on our ANOVA and Chi Square test
glmfit2 <- glm(Churn ~ ContractRenewal+DataPlan+CustServCalls+MonthlyCharge+DataUsage+RoamMins, data = train.df, family = binomial)
summary(glmfit2)

#Likelihood ratio test
lrtest(glmfit2)

#pseudo R square value
pR2(glmfit2) # 20% in variance explained by the 3 variables

# we will go with out final model as glmfit2
confint(glmfit2)
exp(coef(glmfit2)) ##gives the odds
exp(confint(glmfit2))

#train data
pred<-predict(glmfit2,newdata=train.df,type="response")
y_pred_num <- ifelse(pred>0.5,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- train.df$Churn

confusionMatrix(y_pred,y_act,positive="1") # on training data Accuracy : 0.8611 

##test data
pred<-predict(glmfit2,newdata=test.df,type="response")
y_pred_num <- ifelse(pred>0.5,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- test.df$Churn

confusionMatrix(y_pred,y_act,positive="1") # on test data Accuracy : 0.8601 

## AUC
rocplot(glmfit2) ##0.8141

# percentage of positive cases did we catch correctly 
25/(20+25) #56% is the Recall

# percentage of positive predictions we correctly predict
25/(25+120) # 17% is the precision or sensitivity


##upsampling the minority class
up_train <- upSample(x=train.df[, -1],y=train.df$Churn)
dim(up_train)
table(up_train$Class) ## same representation

#Model 3 based on all those variables that are significant based on our ANOVA and Chi Square test
glmfit2 <- glm(Class ~ ContractRenewal+DataPlan+CustServCalls+MonthlyCharge+DataUsage+RoamMins, data = up_train, family = binomial)
summary(glmfit2)

#Likelihood ratio test
lrtest(glmfit2)

#pseudo R square value
pR2(glmfit2) # 24.6% in variance explained by the 3 variables

# we will go with out final model as glmfit2
exp(coef(glmfit2)) ##gives the odds

##test data
pred<-predict(glmfit2,newdata=test.df,type="response")
y_pred_num <- ifelse(pred>0.5,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- test.df$Churn

confusionMatrix(y_pred,y_act,positive="1") # on test data Accuracy : 0.8601 

#precision
111/(111+188)  # 37%

#recall
111/(111+34) #76.6%

## AUC
rocplot(glmfit2) ##0.8226

glmfitbest <- step(glm(Churn~ContractRenewal+ContractRenewal*MonthlyCharge+ContractRenewal*CustServCalls+ContractRenewal*RoamMins+DataUsage+DataUsage*RoamMins+CustServCalls*RoamMins+ContractRenewal*MonthlyCharge*DataPlan+ContractRenewal*DataUsage*DataPlan+MonthlyCharge*DataUsage*DataPlan+CustServCalls+RoamMins+ContractRenewal*DataUsage*RoamMins+ContractRenewal*DataUsage*CustServCalls+ContractRenewal*MonthlyCharge*CustServCalls , data = train.df, family = binomial))
summary(glmfitbest)
lrtest(glmfitbest)

#pseudo R square value
pR2(glmfitbest) # 39.58% in variance explained by 6 variables and their interactions


exp(coef(glmfitbest)) ##gives the odds

##test data
pred<-predict(glmfitbest,newdata=test.df,type="response")
y_pred_num <- ifelse(pred>0.5,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- test.df$Churn

confusionMatrix(y_pred,y_act,positive="1") # on test data Accuracy : 0.9136
## AUC
rocplot(glmfitbest) ##0.897


