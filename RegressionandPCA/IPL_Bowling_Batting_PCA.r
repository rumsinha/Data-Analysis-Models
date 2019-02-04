##reading IPL batting and bowling data
batting.df <- read.csv("C:/my learning journey/Great Lakes/AdvancedStatistics/GA/batting_data.csv")
batting.df

bowling.df <- read.csv("C:/my learning journey/Great Lakes/AdvancedStatistics/GA/bowling_data.csv")
bowling.df

plot(batting.df[,-1])

## Correlation Matrix for 90 Batsmen
library(corrplot)
bc = cor(batting.csv[,-1]) #create an object of the features
corrplot.mixed(bc)

library(psych)

KMO(batting.df[,-1]) ##KMO = 0.64

bartlett.test(batting.df[,-1])

cortest.normal(cor(batting.df[,-1]), n1=nrow(batting.df[,-1]))

# Perform Principal Component Analysis
batting.pca.data <- scale(batting.csv[,-1]) #will exclude the name of the batsman and scale the data
batting.pca <- prcomp(batting.pca.data) 
batting.pca$sdev^2  ##The Eigenvalue
summary(batting.pca)


biplot(batting.pca)  ##the biplot
batting.pca$rotation  ##the rotations or rhe loadings
batting.pca$x  ## the scores
plot(batting.pca, type="l") ##screeplot

##since only one pricipal component has eigen value greater then 1 so we will extract the first principal component
##that explains 71% of the data
##creating a new data frame that will have the batsman name in the first column and the first principal component scores
##in the second column
batting.new.data<- NULL
batting.new.data$Batsmanname<- batting.csv[,1]
batting.new.data$PC1<-batting.pca$x[,1]
batting.new.data<- as.data.frame(batting.new.data)

##the top 10 batsmen using the first principal component
library(plyr)
head(arrange(batting.new.data,PC1), n = 10)


##bowling data

plot(bowling.df[,-1])

## Correlation Matrix for 90 Batsmen
library(corrplot)
bc = cor(bowling.df[,-1]) #create an object of the features
corrplot.mixed(bc)

KMO(bowling.df[,-1]) ##KMO = 0.43

bartlett.test(bowling.df[,-1])

cortest.normal(cor(bowling.df[,-1]), n1=nrow(bowling.df[,-1]))


# Perform Principal Component Analysis
bowling.pca.data <- scale(bowling.csv[,-1]) #will exclude the name of the batsman

bowling.pca <- prcomp(bowling.pca.data) 
bowling.pca$sdev^2 #The Eigen Value
summary(bowling.pca)


biplot(bowling.pca) # The biplot
bowling.pca$rotation # The Rotations/Loadings
bowling.pca$x # The PCA scores
plot(bowling.pca, type="l") #Scree Plot

#Since only the first principal component has Eigen value greater than 1 so we will extract 
#the first principal component that also explains 65% of the data

#We will create a new data frame that will have the bowler name in the first column and 
#the first principal component scores in the second column

bowling.new.data<- NULL
bowling.new.data$bowlers<- bowling.csv[,1]
bowling.new.data$PC1<-bowling.pca$x[,1]
bowling.new.data<- as.data.frame(bowling.new.data)

#Top ten bowlers
# Unlike the analysis for batsmen, where the coefficients in L1 of that discussion are all positive, the
# coefficient of Wkts here is negative while the remainder are positive. This makes sense since better
# bowler performance is naturally associated with higher numbers of wickets taken from batsmen
head(arrange(bowling.new.data,PC1), n = 10)

