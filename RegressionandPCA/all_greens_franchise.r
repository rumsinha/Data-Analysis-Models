#read the data
data.df<- read.csv("C:/my learning journey/Great Lakes/AdvancedStatistics/GA/All Greens Franchise.csv")
data.df

#naming the dataframe columns and checking the data type
names(data.df)<- c("AnnualNetSales","SquareFootage","Inventory","AmtSpentonAdvertizing","SizeofSalesDistrict","NumberofCompetingStores")
str(data.df)
summary(data.df)

##correlation? Highly correlated variables
library(corrplot)
bc = cor(data.df) #create an object of the features
corrplot.mixed(bc)

#A vif > 10 or a 1/vif < 0.10 indicates trouble. 
library(car)
vif(lmstat)
1/vif(lmstat)

## AnnualNetSales response and rest all predictor variables
lmstat<- lm(AnnualNetSales~.,data = data.df)
summary(lmstat)

confint(lmstat)

## all the predictor are significant and overall the model is significant based on F stat and p value
## R square is 0.99 which is the variance in sales explained by all the predictor variables
##  AnnualNetSales strongly correlated with the 5 predictors

##residual analysis
data.df$residuals<- lmstat$residuals
data.df$fittedvalues <- lmstat$fitted.values

# residuals vs squarefootage plot
#no pattern
plot(data.df$SquareFootage,data.df$residuals,xlab="Square Footage",ylab="residuals")
abline(0,0) 

# residuals vs fitted values plot
# no pattern
plot(data.df$fittedvalues,data.df$residuals,xlab="Fitted Values",ylab="residuals")
abline(0,0) 

## building models with subsets of variables and checking the impact
lmstat5<- lm(AnnualNetSales~Inventory+AmtSpentonAdvertizing,data = data.df)
summary(lmstat5)

lmstat5<- lm(AnnualNetSales~Inventory+AmtSpentonAdvertizing+SizeofSalesDistrict,data = data.df)
summary(lmstat5)

