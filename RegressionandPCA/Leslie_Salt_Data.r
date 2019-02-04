#read the data
leslie.df<- read.csv("C:/my learning journey/Great Lakes/AdvancedStatistics/GA/Leslie_Salt.csv")

#the variable data types and five point summary
str(leslie.df)
summary(leslie.df) ##observation min(Price) = 1.7 and max(Price) = 37.2

##correlation? Highly correlated variables
library(corrplot)
bc = cor(leslie.df) #create an object of the features
corrplot.mixed(bc)

##log transformation to reduce the variance in the price and other variables where possible
logleslie.df<- leslie.df

hist(leslie.df$Price)

hist(log(leslie.df$Price))

logleslie.df$logprice<- log(leslie.df$Price)

hist(leslie.df$Size)

hist(log(leslie.df$Size))

logleslie.df$logsize<- log(leslie.df$Size)

logleslie.df<- logleslie.df[,-c(1,3)]

summary(logleslie.df)

##observation min(logprice) = 0.5306 and max(logprice) = 3.6163
## meaningful log transformation for Price and Size. For others when trying to perform log transformation like 
## Elevation, Sewer and Distance resulted in "-Inf" value

##correlation? Highly correlated variables
library(corrplot)
bc = cor(logleslie.df) #create an object of the features
corrplot.mixed(bc)

## County and Elevation correlated
## County and Flood correlated
## County and Distance correlated
## Size and Distance correlated
## Flood and Distance correlated
## Elevation and Log Price correlated
## Sewer and LogPrice correlated
## Date and logprice correlated
## Flood and LogPrice correlated
## County and Elevation correlated AND Elevation and Log Price correlated 
## so lets try and check out if interaction effect of County AND Elevation on logprice??

library(ggplot2)
ggplot(data=logleslie.df, aes(x=Elevation, y=logprice,color=factor(County)))+geom_point()

lmstat<- lm(logprice~.,data = logleslie.df)
summary(lmstat)
##elevation, sewer, date, flood, distance, logsize are significant
#Multiple R-squared:  0.853,	Adjusted R-squared:  0.8083 

## County and Elevation correlated
## County and Flood correlated
lmstat1<- lm(logprice~County+Elevation+Date+Flood+Sewer, data = logleslie.df)
summary(lmstat1)
##Multiple R-squared:  0.8141,	Adjusted R-squared:  0.7769 

## County and Distance correlated
lmstat2<- lm(logprice~County+Elevation+Date+Flood+Sewer+Distance, data = logleslie.df)
summary(lmstat2)
##Multiple R-squared:  0.8347,	Adjusted R-squared:  0.7934 

## County and Distance not significant hence removing these 2 terms and running the model again
lmstat2<- lm(logprice~Elevation+Date+Flood+Sewer+Distance, data = logleslie.df)
summary(lmstat2)
#removing county made distance significant
#Multiple R-squared:  0.8336,	Adjusted R-squared:  0.8003 

## County and Distance not significant hence removing these 2 terms and running the model again
lmstat2<- lm(logprice~County+Elevation+Date+Flood+Sewer, data = logleslie.df)
summary(lmstat2)
#removing distance made county significant
#Multiple R-squared:  0.8141,	Adjusted R-squared:  0.7769 

## Size and Distance correlated
## Flood and Distance correlated
lmstat3<- lm(logprice~County+Elevation+Date+Flood+Sewer+Distance+logsize, data = logleslie.df)
summary(lmstat3)
##Multiple R-squared:  0.853,	Adjusted R-squared:  0.8083 
## county and distance not significant

## Size and Distance correlated
## Flood and Distance correlated
lmstat3<- lm(logprice~County+Elevation+Date+Flood+Sewer+Distance, data = logleslie.df)
summary(lmstat3)
##Multiple R-squared:  0.8347,	Adjusted R-squared:  0.7934 
## county and distance not significant

## County and Elevation correlated AND Elevation and Log Price correlated 
## so lets try and check out if interaction effect of County AND Elevation on logprice??
lmstat4<- lm(logprice~County+Elevation+Date+Flood+Sewer+Distance+County*Elevation, data = logleslie.df)
summary(lmstat4) ## all significant intercepts

## Multiple R-squared:  0.8859,	Adjusted R-squared:  0.8512 
## can take this as out final model

logleslie.df$fittedvalues<- lmstat4$fitted.values
logleslie.df$residuals<- lmstat4$residuals

# residuals vs elevation plot
#no pattern
plot(logleslie.df$Elevation,logleslie.df$residuals,xlab="Elevation",ylab="residuals")
abline(0,0) 

# residuals vs fitted values plot
# no pattern
plot(logleslie.df$fittedvalues,logleslie.df$residuals,xlab="Fitted Values",ylab="residuals")
abline(0,0) 

library(car)
vif(lmstat)
1/vif(lmstat)

library(psych)
KMO(leslie.df) ##KMO = 0.49

bartlett.test(leslie.df)

cortest.normal(cor(leslie.df), n1=nrow(leslie.df))

####
##PCA
####
pca_data<- leslie.df[,-1]
pca_data
pca_data<- scale(pca_data)
pca_data

# Perform Principal Component Analysis
leslie.pca <- prcomp(pca_data) 
summary(leslie.pca)

plot(leslie.pca, type="l") 

print(leslie.pca$rotation) ## the loadings

print(varimax(leslie.pca$rotation)) 
## PC1 on (1,6,7), PC2 on (2,4,6,7), PC3 on (2,3,5), PC4 on 1,4, PC5 on 1,3,5, PC6 on 1,2,3,4,6, PC7 on 1,7

# Let's make a data frame with logprice in the first column, and
# the principal components in the rest of the columns
newleslie.pca = cbind(log(leslie.df[,1]),data.frame(leslie.pca$x))
colnames(newleslie.pca)[1] <- "LogPrice"

# Let's compute a full model
leslie.pcr1 <- lm(LogPrice ~., data = newleslie.pca)
summary(leslie.pcr1)

## PC1,PC2,PC3,PC4 are statistically significant

# will remove the non significant principal components
leslie.pcr2 <- lm(LogPrice ~ PC1+PC2+PC3+PC4, data = newleslie.pca)
summary(leslie.pcr2)

#regression model without log transformation

lmstat<- lm(Price~Elevation+Date+Flood+Sewer, data = leslie.df)
summary(lmstat)


lmstat<- lm(Price~., data = leslie.df)
summary(lmstat)
