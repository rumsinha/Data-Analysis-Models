library(ggplot2)
library(sqldf)
library(stringr)

#Reading the data

#2004 year
election_2004_data <- read.csv("Election_Data_2004.csv")
election_2004_data$Year <- '2004'
head(election_2004_data)

## turnout as numeric feature

election_2004_data$Turnout <-as.numeric(str_trim(str_replace_all(str_replace(election_2004_data$Turnout , "%", ""),",","")))
election_2004_data$Turnout <- as.numeric(election_2004_data$Turnout)
election_2004_data$Turnout[is.na(election_2004_data$Turnout)] <- mean(election_2004_data$Turnout, na.rm=TRUE)/2  ## because mean of turnout exceeding 100%

## NAs handling
election_2004_data$Party <- ifelse(election_2004_data$Party == "BJP","BJP",
                                   ifelse(election_2004_data$Party=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2004_data$Party <- as.factor(election_2004_data$Party)


election_2004_data$PrevParty <- ifelse(election_2004_data$PrevParty == "BJP","BJP",
                                       ifelse(election_2004_data$PrevParty=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2004_data$PrevParty <- as.factor(election_2004_data$PrevParty)


election_2004_data$GSDP[is.na(election_2004_data$GSDP)] <- mean(election_2004_data$GSDP, na.rm=TRUE)
sum(is.na(election_2004_data$GSDP))

election_2004_data$Criminal.Case[is.na(election_2004_data$Criminal.Case)] <- 0
sum(is.na(election_2004_data$Criminal.Case))

election_2004_data$Total.Assets[is.na(election_2004_data$Total.Assets)] <- mean(election_2004_data$Total.Assets, na.rm=TRUE)
sum(is.na(election_2004_data$Total.Assets))
election_2004_data$Total.Assets <- as.numeric(election_2004_data$Total.Assets)

election_2004_data$Liabilities[is.na(election_2004_data$Liabilities)] <- mean(election_2004_data$Liabilities, na.rm=TRUE)
sum(is.na(election_2004_data$Liabilities))
election_2004_data$Liabilities <- as.numeric(election_2004_data$Liabilities)

election_2004_data$CandidateAge <- as.numeric(as.character(election_2004_data$CandidateAge))
election_2004_data$CandidateAge[is.na(election_2004_data$CandidateAge)] <- mean(election_2004_data$CandidateAge, na.rm=TRUE)
sum(is.na(election_2004_data$CandidateAge))

election_2004_data$GrowthRateAgri <- as.numeric(election_2004_data$GrowthRateAgri)
election_2004_data$GrowthRateAgri[is.na(election_2004_data$GrowthRateAgri)] <- mean(election_2004_data$GrowthRateAgri, na.rm=TRUE)
sum(is.na(election_2004_data$GrowthRateAgri))

summary(election_2004_data)
str(election_2004_data)


#2009 year
election_2009_data <- read.csv("Election_Data_2009.csv")
election_2009_data$Year <- '2009'
head(election_2009_data)

## turnout as numeric feature

election_2009_data$Turnout <-as.numeric(str_trim(str_replace_all(str_replace(election_2009_data$Turnout , "%", ""),",","")))
election_2009_data$Turnout <- as.numeric(election_2009_data$Turnout)
election_2009_data$Turnout[is.na(election_2009_data$Turnout)] <- mean(election_2009_data$Turnout, na.rm=TRUE)/2 ## because mean of turnout exceeding 100%

## NAs handling
election_2009_data$Party <- ifelse(election_2009_data$Party == "BJP","BJP",
                                   ifelse(election_2009_data$Party=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2009_data$Party <- as.factor(election_2009_data$Party)


election_2009_data$PrevParty <- ifelse(election_2009_data$PrevParty == "BJP","BJP",
                                       ifelse(election_2009_data$PrevParty=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2009_data$PrevParty <- as.factor(election_2009_data$PrevParty)


election_2009_data$GSDP[is.na(election_2009_data$GSDP)] <- mean(election_2009_data$GSDP, na.rm=TRUE)
sum(is.na(election_2009_data$GSDP))

election_2009_data$Criminal.Case[is.na(election_2009_data$Criminal.Case)] <- 0
sum(is.na(election_2009_data$Criminal.Case))

election_2009_data$Total.Assets[is.na(election_2009_data$Total.Assets)] <- mean(election_2009_data$Total.Assets, na.rm=TRUE)
sum(is.na(election_2009_data$Total.Assets))
election_2009_data$Total.Assets <- as.numeric(election_2009_data$Total.Assets)

election_2009_data$Liabilities[is.na(election_2009_data$Liabilities)] <- mean(election_2009_data$Liabilities, na.rm=TRUE)
sum(is.na(election_2009_data$Liabilities))
election_2009_data$Liabilities <- as.numeric(election_2009_data$Liabilities)

election_2009_data$CandidateAge <- as.numeric(as.character(election_2009_data$CandidateAge))
election_2009_data$CandidateAge[is.na(election_2009_data$CandidateAge)] <- mean(election_2009_data$CandidateAge, na.rm=TRUE)
sum(is.na(election_2009_data$CandidateAge))

election_2009_data$GrowthRateAgri <- as.numeric(election_2009_data$GrowthRateAgri)
election_2009_data$GrowthRateAgri[is.na(election_2009_data$GrowthRateAgri)] <- mean(election_2009_data$GrowthRateAgri, na.rm=TRUE)
sum(is.na(election_2009_data$GrowthRateAgri))

summary(election_2009_data)
str(election_2009_data)

#Combine the data for 2004 and 2009
final_election_data <- rbind(election_2004_data,election_2009_data)
head(final_election_data)
tail(final_election_data)

## independent variables will be Turnout, Gender, Type, PrevParty, GSDP,Criminal.Case,Education,
##                               Total.Assets, Liabilities, CandidateAge ,GrowthRateAgri
##
## dependent variable as Party

# split 70 percent of the data into the training dataset and 30 percent of the data
# into the validation dataset:
set.seed(123)
ind = sample(2, nrow(final_election_data), replace = TRUE, prob=c(0.7,0.3))
train.df = final_election_data[ind == 1,]
test.df = final_election_data[ind == 2,]
dim(train.df)
dim(test.df)

prop.table(table(train.df$Party)) 

prop.table(table(test.df$Party)) 

# we see almost equal representation in both training and testing set for the dependent or response variable


#### Neural Net model with Caret and NNET packages ####
varNames <- names(train.df)

responseVarName <- "Party"
factorcolvarNames <- varNames[varNames %in% c("Type","Gender","PrevParty","Education")]
numericcolvarNames <- varNames[varNames %in% c("Turnout","GSDP","Criminal.Case","CandidateAge","GrowthRateAgri")]
toscalevarnames <- varNames[varNames %in% c("Total.Assets","Liabilities")]

# Create Vector of Column Max and Min Values
maxs <- apply(train.df[,toscalevarnames], 2, max)
mins <- apply(train.df[,toscalevarnames], 2, min)

# Use scale() and convert the resulting matrix to a data frame
train.scaled.data <- as.data.frame(scale(train.df[,toscalevarnames],center = mins, scale = maxs - mins))

traindata<- as.data.frame(cbind("Party"=train.df[,responseVarName],train.scaled.data,train.df[,numericcolvarNames],train.df[,factorcolvarNames]))
str(traindata)

# Create Vector of Column Max and Min Values
maxs <- apply(test.df[,toscalevarnames], 2, max)
mins <- apply(test.df[,toscalevarnames], 2, min)

# Use scale() and convert the resulting matrix to a data frame
test.scaled.data <- as.data.frame(scale(test.df[,toscalevarnames],center = mins, scale = maxs - mins))

testdata<- as.data.frame(cbind("Party"=test.df[,responseVarName],test.scaled.data,test.df[,numericcolvarNames],test.df[,factorcolvarNames]))
str(testdata)

library(caret)
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
                         verboseIter = TRUE, summaryFunction = multiClassSummary, 
                         preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))

#Grid Search: Manual Grid
#The second way to search algorithm parameters is to specify a tune grid manually
#the search of a manual tune grid with hidden neurons 5 and 1 hidden layer and threshold value specified as 0.1
elec.pred.nn <- train(Party ~ ., data = traindata, method = 'nnet', 
                      trControl = numFolds, tuneGrid=expand.grid(size=c(10,5), decay=c(0.1)))

#train data
pred<-predict(elec.pred.nn)
pred
caret::confusionMatrix(xtabs(~pred+traindata$Party))


#test data
pred<-predict(elec.pred.nn, newdata=testdata[,-1])
pred
caret::confusionMatrix(xtabs(~pred+testdata$Party))


## testing the model on the 2014 data
#2014 year
election_2014_data <- read.csv("Election_Data_2014.csv")
election_2014_data$Year <- '2014'
head(election_2014_data)

## turnout as numeric feature

election_2014_data$Turnout <-as.numeric(str_trim(str_replace_all(str_replace(election_2014_data$Turnout , "%", ""),",","")))
election_2014_data$Turnout <- as.numeric(election_2014_data$Turnout)
election_2014_data$Turnout[is.na(election_2014_data$Turnout)] <- mean(election_2014_data$Turnout, na.rm=TRUE)/2 ## because mean of turnout exceeding 100%

## NAs handling
election_2014_data$Party <- ifelse(election_2014_data$Party == "BJP","BJP",
                                   ifelse(election_2014_data$Party=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2014_data$Party <- as.factor(election_2014_data$Party)


election_2014_data$PrevParty <- ifelse(election_2014_data$PrevParty == "BJP","BJP",
                                       ifelse(election_2014_data$PrevParty=="INC" ,"INC","Others")) ## "Others" "INC"    "BJP" 

election_2014_data$PrevParty <- as.factor(election_2014_data$PrevParty)


election_2014_data$GSDP[is.na(election_2014_data$GSDP)] <- mean(election_2014_data$GSDP, na.rm=TRUE)
sum(is.na(election_2014_data$GSDP))

election_2014_data$Criminal.Case[is.na(election_2014_data$Criminal.Case)] <- 0
sum(is.na(election_2014_data$Criminal.Case))

election_2014_data$Total.Assets[is.na(election_2014_data$Total.Assets)] <- mean(election_2014_data$Total.Assets, na.rm=TRUE)
sum(is.na(election_2014_data$Total.Assets))
election_2014_data$Total.Assets <- as.numeric(election_2014_data$Total.Assets)

election_2014_data$Liabilities[is.na(election_2014_data$Liabilities)] <- mean(election_2014_data$Liabilities, na.rm=TRUE)
sum(is.na(election_2014_data$Liabilities))
election_2014_data$Liabilities <- as.numeric(election_2014_data$Liabilities)

election_2014_data$CandidateAge <- as.numeric(as.character(election_2014_data$CandidateAge))
election_2014_data$CandidateAge[is.na(election_2014_data$CandidateAge)] <- mean(election_2014_data$CandidateAge, na.rm=TRUE)
sum(is.na(election_2014_data$CandidateAge))

election_2014_data$GrowthRateAgri <- as.numeric(election_2014_data$GrowthRateAgri)
election_2014_data$GrowthRateAgri[is.na(election_2014_data$GrowthRateAgri)] <- mean(election_2014_data$GrowthRateAgri, na.rm=TRUE)
sum(is.na(election_2014_data$GrowthRateAgri))

summary(election_2014_data)
str(election_2014_data)


test.df = election_2014_data
# Create Vector of Column Max and Min Values
maxs <- apply(test.df[,toscalevarnames], 2, max)
mins <- apply(test.df[,toscalevarnames], 2, min)

# Use scale() and convert the resulting matrix to a data frame
test.scaled.data <- as.data.frame(scale(test.df[,toscalevarnames],center = mins, scale = maxs - mins))

testdata<- as.data.frame(cbind("Party"=test.df[,responseVarName],test.scaled.data,test.df[,numericcolvarNames],test.df[,factorcolvarNames]))
str(testdata)


#test data
pred<-predict(elec.pred.nn, newdata=testdata[,-1])
pred
caret::confusionMatrix(xtabs(~pred+testdata$Party))

election_2014_data$PredictedValue <- pred

write.csv(election_2014_data,"election_2014_data_with_predicted_value.csv")
