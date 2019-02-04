#################################################################################################################################
### cadata.txt
#################################################################################################################################
# This dataset contains the average house value as target variable
# and the following input variables (features): average income,
# housing average age, average rooms, average bedrooms, population,
# average occupation, latitude, and longitude in that order.
# 20640 rows and 9 variables

# Download and untar data in a temporary directory
setwd("C:/my learning journey/practice/")
url <- "http://www.dcc.fc.up.pt/~ltorgo/Regression/cal_housing.tgz"
download.file(url, destfile = "cal.tar.gz")
untar("cal.tar.gz", list=TRUE)
untar("cal.tar.gz", files = "cal_housing.pkz")

# Read the data into R and provide column names
cal_housing <- read.csv("CaliforniaHousing//cal_housing.data", header = FALSE)
head(cal_housing)
cal_housing <- cal_housing[, c(9,8,3,4,5,6,7,2,1)]
names(cal_housing) <- c("HousingValue", "MedianIncome", "HouseMedianAge", "TotalRooms", 
                        "TotalBedrooms", "Population", "Households", "Latitude", 
                        "Longitude")


str(cal_housing)
summary(cal_housing)

##correlation? Highly correlated variables
library(corrplot)
bc = cor(cal_housing) #create an object of the features
corrplot.mixed(bc)

ind<- sample(seq_len(nrow(cal_housing)), size=18540)

train.housing.df<- cal_housing[ind,]
test.housing.df<- cal_housing[-ind,]
str(train.housing.df)
str(test.housing.df)

library(corrplot)
bc = cor(train.housing.df) #create an object of the features
corrplot.mixed(bc)

library(psych)
KMO(train.housing.df) ##KMO = 0.66

bartlett.test(train.housing.df)

cortest.normal(cor(train.housing.df), n1=nrow(train.housing.df))

# Perform Principal Component Analysis on the training data
train.pca.data <- scale(train.housing.df[,-1])
train.housing.pca <- prcomp(train.pca.data) 
summary(train.housing.pca)

plot(train.housing.pca, type="l") 

print(train.housing.pca$rotation) ## the loadings

##unroated loadings
train.pca2 <- principal(train.pca.data,nfactors = 4, rotate = "none")
print.psych(train.pca2, sort=T)

##roated loadings
train.pca2 <- principal(train.pca.data,nfactors = 4, rotate = "varimax")
print.psych(train.pca2, sort=T)

##lets check on the test data set whether the same conclusion holds true
KMO(test.housing.df) ##KMO = 0.66

bartlett.test(test.housing.df)

cortest.normal(cor(test.housing.df), n1=nrow(test.housing.df))

test.pca.data <- scale(test.housing.df[,-1])
test.housing.pca <- prcomp(test.pca.data) 
summary(test.housing.pca)

plot(test.housing.pca, type="l") 

print(test.housing.pca$rotation) ## the loadings

##unrotated
test.pca2 <- principal(test.pca.data,nfactors = 4, rotate = "none")
print.psych(test.pca2, sort=T)

##rotated
test.pca2 <- principal(test.pca.data,nfactors = 4, rotate = "varimax")
print.psych(test.pca2, sort=T)

## PC1 is loaded on TotalRooms, TotalBedrooms, Population,Households
## PC2 is loaded on Latitude, Longitude
## PC3 is loaded on Income
## PC4 is loaded on HouseAge
## no additional advantage on performing rotation so we will continue with our unrotated principal components

## we can have four factors from above
## FA1 composed of TotalRooms, TotalBedrooms, Population,Households can be named as HousePopulation
## FA2 composed of Longitude and Latitude can be named as geographiclocation
## FA3 composed of Income hence named as Income
## FA4 composed of HouseAge hence named as HouseAge



###REGRESSION ANALYSIS###
# Let's make a data frame with the log of the house value  in the first column, and
# the factor scores in the rest of the columns
newtrain.pca = cbind(log(train.housing.df[,1]),data.frame(train.pca2$scores))
colnames(newtrain.pca)[1] <- "LogHousingValue"
head(newtrain.pca)

# Let's compute a full model
newtrain.pcr1 <- lm(LogHousingValue ~., data = newtrain.pca)
summary(newtrain.pcr1)
newtrain.pca$predictedvalues<- newtrain.pcr1$fitted.values
newtrain.pca$residuals <- newtrain.pcr1$residuals
head(newtrain.pca)

# Let's make a data frame with the log of the house value, and
# the principal components in the rest of the columns
newtest.pca = cbind(log(test.housing.df[,1]),data.frame(test.pca2$scores))
colnames(newtest.pca)[1] <- "LogHousingValue"
test.predict <- predict.lm(newtrain.pcr1,newdata = newtest.pca[,-1])

newtest.pca$fitted_values<- test.predict
newtest.pca$residuals <- newtest.pca$LogHousingValue-newtest.pca$fitted_values
head(newtest.pca)

##residual analysis
## residuals vs Factor1 plot
plot(newtrain.pca$residuals~newtrain.pca$RC1)
abline(0,0)

## residuals vs predicted values plot
plot(newtrain.pca$residuals~newtrain.pca$predictedvalues)
abline(0,0)
