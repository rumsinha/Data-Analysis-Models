#reading data into R
library(data.table)
ss13husa.df <-
  fread('C:/Users/rumsinha/Downloads/csv_hus/ss13husa.csv')
str(ss13husa.df)
dim(ss13husa.df)
View(head(ss13husa.df))
View(tail(ss13husa.df))

ss13husb.df <-
  fread('C:/Users/rumsinha/Downloads/csv_hus/ss13husb.csv')
str(ss13husb.df)
dim(ss13husb.df)
View(head(ss13husb.df))
View(tail(ss13husb.df))

#combining the two files
finaldata <- rbind(ss13husa.df, ss13husb.df)
class(finaldata)
dim(finaldata)
names(finaldata)
str(finaldata)

rm(ss13husa.df)
rm(ss13husb.df)


#null value analysis and treatment

finaldata <- as.data.frame(finaldata) # data table to data frame

# VALP, value of the property is our response variable

# null values of the response variable?
sum(is.na(finaldata$VALP))

#dropping all the rows where VALP is having no values
finaldata <- subset(finaldata, !is.na(finaldata$VALP))

dim(finaldata) #859691    231

#finding out the percentage of null values for each of the column
df <- NULL
for (i in names(finaldata)) {
  df[[paste(i)]] <- sum(is.na(finaldata[[i]]))
}

df <- as.data.frame(df)

names(df) <- c("countofnulls")

df$percentageofnull <-
  (df$countofnulls / nrow(finaldata)) * 100 # percentage of the null values for each of the variable

# get all the column names with more then 15% null values
df1 <- subset(df, percentageofnull >= 15)
rownames(df1)

# get all the column names with less then 15% null values
df2 <- subset(df, percentageofnull <= 15 & percentageofnull > 0)
rownames(df2)

# columns having no null values
df3 <- subset(df, percentageofnull == 0)
rownames(df3)

##drop all those features which are having null values that are 15% or more compared to the entire data samples

finaldata <-
  finaldata[,!(colnames(finaldata) %in% c(rownames(df1)))]

dim(finaldata) ## 859691    203

# treat the remaining columns/imputation
for (i in rownames(df2)) {
  var <- paste0('finaldata', sep = "$", i)
  var <-  noquote(var)
  var1 <- noquote(paste0("summary(", var, ")"))
  print(var1)
  var2 <- noquote(paste0("unique(", var, ")"))
  print(var2)
}

summary(finaldata$ACCESS) # 12326 NAs
unique(finaldata$ACCESS) # should be factor type

summary(finaldata$ACR) #41688 NAs
unique(finaldata$ACR) # should be factor type
table(finaldata$ACR)

summary(finaldata$BUS)# 41688 NAs
unique(finaldata$BUS) # should be factor type

summary(finaldata$COMPOTHX) #12326
unique(finaldata$COMPOTHX) # should be factor type

summary(finaldata$ELEP) #12326
unique(finaldata$ELEP) # data imputation for continuous variable

summary(finaldata$FS) #12326
unique(finaldata$FS) # should be factor type

summary(finaldata$FULP) #12326
unique(finaldata$FULP) # data imputation for continuous variable

summary(finaldata$GASP) #12326
unique(finaldata$GASP)# data imputation for continuous variable

summary(finaldata$HANDHELD)#12326
unique(finaldata$HANDHELD)# should be factor type

summary(finaldata$HFL) #12326
unique(finaldata$HFL) # should be factor type

summary(finaldata$INSP) #12326
unique(finaldata$INSP) # data imputation for continuous variable

summary(finaldata$LAPTOP) #12326
unique(finaldata$LAPTOP)  # should be factor type

summary(finaldata$MRGX) #12326
unique(finaldata$MRGX) # should be factor type

summary(finaldata$TEL) #12326
unique(finaldata$TEL) # should be factor type

summary(finaldata$TEN) #12326
unique(finaldata$TEN) # should be factor type

summary(finaldata$VEH) #12326
unique(finaldata$VEH) # should be factor type

summary(finaldata$WATP)  #12326
unique(finaldata$WATP) # data imputation for continuous variable

summary(finaldata$FSMOCP) #12326
unique(finaldata$FSMOCP) # should be factor type

summary(finaldata$HHL) #12326
unique(finaldata$HHL) # should be factor type

summary(finaldata$HHT) #12326
unique(finaldata$HHT) # should be factor type

summary(finaldata$HINCP) #12326
unique(finaldata$HINCP) # data imputation for continuous variable

summary(finaldata$HUGCL)  #12326
unique(finaldata$HUGCL) # should be factor type

summary(finaldata$HUPAC)  #12326
unique(finaldata$HUPAC) # should be factor type

summary(finaldata$HUPAOC)  #12326
unique(finaldata$HUPAOC) # should be factor type

summary(finaldata$HUPARC) #12326
unique(finaldata$HUPARC) # should be factor type

summary(finaldata$LNGI) #12326
unique(finaldata$LNGI) # should be factor type

summary(finaldata$MULTG) #12326
unique(finaldata$MULTG) # should be factor type

summary(finaldata$MV) # 12328
unique(finaldata$MV)  # should be factor type

summary(finaldata$NOC)  #12326
unique(finaldata$NOC)  # should be factor type

summary(finaldata$NPP)  #12326
unique(finaldata$NPP) # should be factor type

summary(finaldata$NR)  #12326
unique(finaldata$NR) # should be factor type

summary(finaldata$NRC) #12326
unique(finaldata$NRC)  # should be factor type

summary(finaldata$OCPIP) #19048
unique(finaldata$OCPIP) # Data Imputation

summary(finaldata$PARTNER) #12326
unique(finaldata$PARTNER) # should be factor type

summary(finaldata$PSF) #12326
unique(finaldata$PSF) # should be factor type

summary(finaldata$R18) #12326
unique(finaldata$R18)  # should be factor type

summary(finaldata$R60) #12326
unique(finaldata$R60) # should be factor type

summary(finaldata$R65)  #12326
unique(finaldata$R65) # should be factor type

summary(finaldata$SMOCP) #12326
unique(finaldata$SMOCP) # Data Imputation

summary(finaldata$SSMC)  #12326
unique(finaldata$SSMC) # should be factor type

summary(finaldata$TAXP) #12326
unique(finaldata$TAXP) # Data Imputation


## dropping the factor variables having null values as without the domain expertise not
## imputing the null values with the mode of the factor variables
## we will drop the factor variables
## ACCESS, ACR, BUS, COMPOTHX, FS, HANDHELD, HFL, LAPTOP, MRGX, TEL, TEN, VEH, FSMOCP, HHL, HHT, HUGCL, HUPAC, HUPAOC,
## HUPARC,LNGI,MULTG,MV,NOC,NPP,NR,NRC,PARTNER,PSF,R18,R60,R65,SSMC,

finaldata <-
  finaldata[,!(
    colnames(finaldata) %in% c(
      "SSMC",
      "ACCESS",
      "ACR",
      "BUS",
      "COMPOTHX",
      "FS",
      "HANDHELD",
      "HFL",
      "LAPTOP",
      "MRGX",
      "TEL",
      "TEN",
      "VEH",
      "FSMOCP",
      "HHL",
      "HHT",
      "HUGCL",
      "HUPAC",
      "HUPAOC",
      "HUPARC",
      "LNGI",
      "MULTG",
      "MV",
      "NOC",
      "NPP",
      "NR",
      "NRC",
      "PARTNER",
      "PSF",
      "R18",
      "R60",
      "R65"
    )
  )]


dim(finaldata) ## 859691    171

# #data imputation for the continuous variables ELEP, FULP, GASP, INSP, WATP, HINCP, OCPIP, SMOCP, TAXP
library(mice)

finaldatamice = mice(finaldata,  meth = 'sample', seed = 111)

# obtain first imputated matrix
imputedhousedata <- complete(finaldatamice)

# long matrix with stacked complete data, including the original data
# completedataset <- complete(finaldatamice, 'long', inc=TRUE)

head(finaldata)
tail(finaldata)
nrow(finaldata)

head(imputedhousedata)
tail(imputedhousedata)
nrow(imputedhousedata)

summary(finaldata$TAXP) #12326
summary(imputedhousedata$TAXP)

summary(finaldata$SMOCP) #12326
summary(imputedhousedata$SMOCP)

# head(completedataset)
# tail(completedataset)
# nrow(completedataset)
#
# rm(completedataset)

## checking the distribution of the response variable VALP
hist(imputedhousedata$VALP) ## right skewed
qqnorm(imputedhousedata$VALP,
       ylab = "Sample Quantiles for VALP")
qqline(imputedhousedata$VALP,
       col = "red")

## log transformation
hist(log(imputedhousedata$VALP))
qqnorm(log(imputedhousedata$VALP),
       ylab = "Sample Quantiles for log(VALP")
qqline(log(imputedhousedata$VALP),
       col = "red")

# RT has only one value "H"
unique(imputedhousedata$RT)

imputedhousedata$logVALP <-
  log(imputedhousedata$VALP) ## new feature with log transformation
imputedhousedatabkup <- imputedhousedata ## bakup dataframe values
imputedhousedata <-
  imputedhousedata[,-26] ## removing the original response variable

# correlation
cor.test(imputedhousedatabkup$VALP, imputedhousedatabkup$TAXP) #0.4727348
cor.test(imputedhousedatabkup$VALP, imputedhousedatabkup$INSP) #0.4357165
cor.test(imputedhousedatabkup$VALP, imputedhousedatabkup$HINCP) #0.4202834
cor.test(imputedhousedatabkup$VALP, imputedhousedatabkup$FULP) #0.03702378
cor.test(imputedhousedatabkup$VALP, imputedhousedatabkup$ELEP) #0.1727555

#boxplot of VALP and Division
ggplot(imputedhousedatabkup, aes(x = as.factor(DIVISION), y = VALP, fill = DIVISION)) + geom_boxplot() +
  coord_flip()

#boxplot of VALP and REGION
ggplot(imputedhousedatabkup, aes(x = as.factor(REGION), y = VALP, fill = REGION)) + geom_boxplot() +
  coord_flip()

#boxplot of VALP and ST
ggplot(imputedhousedatabkup, aes(x = as.factor(ST), y = VALP, fill = ST)) + geom_boxplot() +
  coord_flip()

#boxplot of TAXP and Division
ggplot(imputedhousedatabkup, aes(x = as.factor(DIVISION), y = TAXP, fill = DIVISION)) + geom_boxplot() +
  coord_flip()

#boxplot of TAXP and REGION
ggplot(imputedhousedatabkup, aes(x = as.factor(REGION), y = TAXP, fill = REGION)) + geom_boxplot() +
  coord_flip()

#boxplot of INSP and REGION
ggplot(imputedhousedatabkup, aes(x = as.factor(REGION), y = INSP, fill = REGION)) + geom_boxplot() +
  coord_flip()

#significant VALP difference across ST
aovstat <- aov(VALP ~ as.factor(ST), data = imputedhousedatabkup)
summary(aovstat)

#significant TAXP difference across ST
aovstat <- aov(TAXP ~ as.factor(ST), data = imputedhousedatabkup)
summary(aovstat)


#clustering of the dataset on certain variables, to see how the value of property etc forming homogenous groups
clusterdata <- imputedhousedatabkup[, c(
  "VALP",
  "HINCP",
  "TAXP",
  "SMOCP",
  "OCPIP",
  "WATP",
  "RMSP",
  "ELEP",
  "FULP",
  "GASP",
  "INSP",
  "BDSP",
  "WGTP",
  "ST",
  "REGION",
  "DIVISION"
)]
scaleddata <- as.data.frame(scale(clusterdata[, 1:13]))

#how many clusters??
library(cluster)
library(fpc)

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
data <- scaleddata
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

#we will try with k=4/6/8
kmeans_fit <- kmeans(scaleddata, 4, nstart = 25)
kmeans_fit$centers
kmeans_fit$size

plotcluster(scaleddata, kmeans_fit$cluster)

rm(scaleddata)

clusterdata$cluster <- kmeans_fit$cluster

View(head(clusterdata))
View(tail(clusterdata))

clusterdata <- clusterdata[, -1]
clusterdata$logVALP <- log(clusterdata$VALP)

names(clusterdata)

## we will go with 4 clusters
## analysing the cluster membership across ST, Region and Division

#connecting to spark and loading data
library(dplyr)
library("sparklyr")
sc <-
  spark_connect(master = "local", spark_home = spark_home_dir(version = "1.6.2"))

## housedata_tbl <- copy_to(sc, imputedhousedata)

## we will go with the subset of data that we did the clustering
## the reason not going with the complete imputed dataset because majority of the columns
## are factor variables having 0/1 values in addition to ST,Region and Division
## when including them as is in the model, the coefficients don't add any value
## when change to factor then Sparklyr regression throws error
## so going with continuous variables only
housedata_tbl <- copy_to(sc, clusterdata)

# List the data frames available in Spark
src_tbls(sc)

## housedata_tbl_df <- tbl(sc, "imputedhousedata")
housedata_tbl_df <- tbl(sc, "clusterdata")
glimpse(housedata_tbl_df)


# See how big the dataset is
##dim(imputedhousedata)
dim(clusterdata)

# Examine structure of data; number of rows is not right
#glimpse(imputedhousedata)
glimpse(clusterdata)

# See how small the tibble is
library(pryr)
#object_size(imputedhousedata)
object_size(clusterdata)

# Partition the data
partition <- housedata_tbl_df %>%
  sdf_partition(train = 0.70, test = 0.30, seed = 123)

# Create table references
ml_train_tbl <- partition$train
ml_test_tbl <- partition$test

## Random Forest

# build the formula
var <- "HINCP"
for (i in names(clusterdata[,-c(1, 13, 14, 15, 16, 17)])) {
  print(i)
  var <- paste0(var, sep = "+", i)
  print(var)
}

var <- paste0('logVALP', sep = '~', var)
ml_formula <- as.formula(var)
ml_formula

ml_rf <- ml_random_forest(ml_train_tbl, ml_formula)

# variable importance with the random forest model
imp_features <- ml_tree_feature_importance(sc, ml_rf)
imp_features

imp_features_subset <-
  subset(imp_features, as.numeric(as.character(importance)) > 0.001)
imp_features_subset

# linear regression with the important variables
predictors <- as.character(imp_features_subset$feature)

ml_fit <- ml_train_tbl %>%
  ml_linear_regression(response = "logVALP", features = c(predictors))

ml_fit

summary(ml_fit)


# test the model on the test dataset
ml_test_tblfit <- ml_test_tbl %>%
  dplyr::select(logVALP,
                TAXP,
                INSP,
                SMOCP,
                HINCP,
                BDSP,
                OCPIP,
                RMSP,
                WATP)

pred <- sdf_predict(ml_fit, ml_test_tblfit) %>% collect

# Plot the predicted versus actual y
library(ggplot2)
ggplot(pred, aes(x = logVALP, y = prediction)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(x = "Actual Response value",
       y = "Predicted Response value",
       title = "Predicted vs. Actual Response value")

## lot to be improved on the model
##

## outlier removal and model building

boxplot(clusterdata$logVALP) ## with outliers

# total 43069 rows with outliers
clusterdata_with_no_outliers <-
  subset(clusterdata,
         !clusterdata$logVALP %in% boxplot.stats(clusterdata$logVALP)$out)

housedata_nooutlier_tbl <-
  copy_to(sc, clusterdata_with_no_outliers) # load into spark

hist(clusterdata_with_no_outliers$logVALP) ## after removing the outliers
boxplot(clusterdata_with_no_outliers$logVALP) ## after removing the outliers


# Partition the data
partition_nooutlier <- housedata_nooutlier_tbl %>%
  sdf_partition(train = 0.70, test = 0.30, seed = 123)

# Create table references
ml_nooutlier_train_tbl <- partition_nooutlier$train
ml_nooutlier_test_tbl <- partition_nooutlier$test

## Random Forest

# build the formula
var <- NULL
var <- "HINCP"
for (i in names(clusterdata_with_no_outliers[,-c(1, 13, 14, 15, 16, 17)])) {
  print(i)
  var <- paste0(var, sep = "+", i)
  print(var)
}

var <- paste0('logVALP', sep = '~', var)
ml_formula <- as.formula(var)
ml_formula

ml_nooutlier_rf <-
  ml_random_forest(ml_nooutlier_train_tbl, ml_formula)

# variable importance with the random forest model
imp_nooutlier_features <-
  ml_tree_feature_importance(sc, ml_nooutlier_rf)
imp_nooutlier_features

imp_nooutlier_features_subset <-
  subset(imp_nooutlier_features, as.numeric(as.character(importance)) > 0)
imp_nooutlier_features_subset

# linear regression with the important variables
predictors <- as.character(imp_nooutlier_features_subset$feature)

ml_nooutlier_fit <- ml_nooutlier_train_tbl %>%
  ml_linear_regression(response = "logVALP", features = c(predictors))

ml_nooutlier_fit

summary(ml_nooutlier_fit) ## model improved with R-Squared: 0.5522 and Root Mean Squared Error: 0.5709

# test the model on the test dataset

ml_test_nooutlier_tblfit <- ml_nooutlier_test_tbl %>%
  dplyr::select(logVALP,
                TAXP,
                SMOCP,
                INSP,
                HINCP,
                BDSP,
                RMSP,
                OCPIP,
                WGTP,
                WATP,
                GASP,
                FULP,
                ELEP)

pred <-
  sdf_predict(ml_nooutlier_fit, ml_test_nooutlier_tblfit) %>% collect

# Plot the predicted versus actual y
ggplot(pred, aes(x = logVALP, y = prediction)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(x = "Actual Response value",
       y = "Predicted Response value",
       title = "Predicted vs. Actual Response value")


#RMSE calculation on the test data
RMSE <- sqrt(mean((pred$logVALP - pred$prediction) ^ 2))
RMSE # 0.5731417

## further how to fine tune the model? Still a lot of scope to improve on this one.....
## can we try building linear model for each of the cluster??

##
## disconnect from spark
spark_disconnect(sc)
