## classification models on the churn dataset with rpart/knn/naive bayes/logistic regression/svm/neuralnet
library(C50)

data(churn)
str(churnTrain)

churnTrain <- churnTrain[,!names(churnTrain) %in% c("state", "area_code", "account_length")]

set.seed(100)

# training and test dataset

ind <- sample(2, nrow(churnTrain),replace=TRUE,prob=c(0.7,0.3))

trainset <- churnTrain[ind == 1, ]
testset <- churnTrain[ind == 2, ]

dim(trainset)
dim(testset)

split.data <- function(data, p=0.7, s=666) {
  set.seed(s)
  index <- sample(1:dim(data)[1])
  train <- data[index[1:floor(dim(data)[1]*p)],]
  test <- data[index[((ceiling(dim(data)[1] * p))+1):dim(data)[1]],]
  return(list(train=train, test=test))
}

library(rpart)

churn.rp <- rpart(churn~.,data=trainset)

churn.rp

printcp(churn.rp)

plotcp(churn.rp)

summary(churn.rp)

plot(churn.rp, margin=0.1)
text(churn.rp,all=True, use.n=TRUE)

plot(churn.rp, uniform=TRUE, branch=0.6, margin=0.1)
text(churn.rp,all=TRUE,use.n=TRUE)

predictions <- predict(churn.rp,testset,type='class')
table(testset$churn,predictions)

library(caret)
confusionMatrix(table(predictions, testset$churn))

min(churn.rp$cptable[,"xerror"])
which.min(churn.rp$cptable[,"xerror"])
churn.rp$cptable[which.min(churn.rp$cptable[,"xerror"]),"xerror"]

prune.tree <- prune(churn.rp,cp = churn.rp$cptable[which.min(churn.rp$cptable[,"xerror"]),"xerror"])

plot(prune.tree)
plot(prune.tree, margin=0.1)
text(prune.tree,all=True, use.n=TRUE)



## conditional inference tree
library(party)

ctree.model <- ctree(churn~., data=trainset)
ctree.model

plot(ctree.model)

daycharge.model <- ctree(churn~total_day_charge, data=trainset)
plot(daycharge.model)

table(predict(ctree.model, testset), testset$churn)

confusionMatrix(table(predict(ctree.model, testset), testset$churn))

library(class)

levels(trainset$international_plan) = list("0"="no", "1"="yes")
levels(trainset$voice_mail_plan) = list("0"="no", "1"="yes")
levels(testset$international_plan) = list("0"="no", "1"="yes")
levels(testset$voice_mail_plan) = list("0"="no", "1"="yes")

churn.knn <- knn(trainset[,!names(trainset) %in% c("churn")],
                 testset[,!names(testset) %in% c("churn")], trainset$churn,k=3)

summary(churn.knn)

table(testset$churn,churn.knn)

confusionMatrix(table(testset$churn,churn.knn))


#kknn


#Logistic Regression
fit <- glm(churn~.,data=trainset,family=binomial)

summary(fit)

fit <- glm(churn~international_plan+voice_mail_plan+total_intl_calls+number_customer_service_calls, data=trainset, family=binomial)
summary(fit)

pred <- predict(fit, testset, type='response')
class <- pred >0.5

summary(class)

tb <- table(testset$churn,class)
tb

churn.mod <-  ifelse(testset$churn == "yes",1,0)
pred_class <- churn.mod
pred_class[pred<= 0.5] <- 1-pred_class[pred<=0.5]
table(churn.mod, pred_class)

confusionMatrix(table(churn.mod, pred_class))

## Naive Bayes
library(e1071)

classifier <- naiveBayes(trainset[, !names(trainset) %in% c("churn")], trainset$churn)
classifier

bayes.table <- table(predict(classifier,testset[,!names(testset) %in% c("churn")]), testset$churn)
bayes.table
confusionMatrix(bayes.table)

### SVM

model <- svm(churn ~ ., data = trainset, kernel = "radial", cost=1, gamma=1/ncol(trainset))
summary(model)

# library(klaR)
# 
# ?klaR::svmlight


iris.subset <- subset(iris, select = c("Sepal.Length","Sepal.Width","Species"),
                      Species %in% c("setosa","virginica"))


## model 1
plot(x=iris.subset$Sepal.Length, y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)

svm.model <- svm(Species ~ ., data=iris.subset, kernel='linear', cost=1, scale=FALSE)

# support vectors
points(iris.subset[svm.model$index,c(1,2)], col='blue',cex=2)

#separation plane
w = t(svm.model$coefs) %*% svm.model$SV
b = -svm.model$rho
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="red",lty=5)


## model 2
plot(x=iris.subset$Sepal.Length, y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)
svm.model = svm(Species ~ ., data=iris.subset, type='C-classification',kernel='linear',cost=10000,scale=FALSE)

#support vectors
points(iris.subset[svm.model$index,c(1,2)], col='blue',cex=2)

w = t(svm.model$coefs) %*% svm.model$SV
b = -svm.model$rho
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="red",lty=5)


## visualize the built model
data(iris)
model.iris <- svm(Species~., iris)
plot(model.iris,iris,Petal.Width~Petal.Length, slice=list(Sepal.Width=3, Sepal.Length=4))


plot(model,trainset,total_day_minutes~total_intl_charge)

svm.pred <- predict(model, testset[,!names(testset) %in% c("churn")])

svm.table <- table(svm.pred, testset$churn)
svm.table

classAgreement(svm.table)

confusionMatrix(svm.table) # accuracy, sensitivity, specificity

library(car)
data(Quartet)

model.regression <- svm(Quartet$y1~Quartet$x, type="eps-regression")

predict.y <- predict(model.regression, Quartet$x)
predict.y

## plot of the training data points and predicted data points
plot(Quartet$x, Quartet$y1, pch=19)
points(Quartet$x,predict.y,pch=15,col='red')

##tuning svm
tuned = tune.svm(churn~.,data = trainset, gamma=10^(-6:-1),cost=10^(1:2))
summary(tuned)

model.tuned <- svm(churn~.,data=trainset,gamma=tuned$best.parameters$gamma,cost=tuned$best.parameters$cost)
summary(model.tuned)

svm.tuned.pred <- predict(model.tuned, testset[,!names(testset) %in% c("churn")])

svm.tuned.table <- table(svm.tuned.pred, testset$churn)
svm.tuned.table

classAgreement(svm.tuned.table)

confusionMatrix(svm.tuned.table)


## Neural Network with neuralnet
data(iris)
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7,0.3))
trainset <- iris[ind == 1,]
testset <- iris[ind == 2,]

library(neuralnet)

trainset$setosa <- trainset$Species == "setosa"
trainset$virginica <- trainset$Species == 'virginica'
trainset$versicolor <- trainset$Species == "versicolor"

set.seed(100)

network <- neuralnet(versicolor+virginica+setosa ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,trainset,hidden=3)
network

network$result.matrix

head(network$generalized.weights[[1]])

## visualizing a trained neural network

plot(network)

par(mfrow=c(2,2))
gwplot(network,selected.covariate = 'Petal.Width')
gwplot(network,selected.covariate = 'Sepal.Width')
gwplot(network,selected.covariate = 'Petal.Length')
gwplot(network,selected.covariate = 'Sepal.Width')

## predicting the labels
net.predict <- compute(network, testset[-5])$net.result

net.prediction <- c("versicolor", "virginica","setosa") [apply(net.predict,1,which.max)]

predict.table <- table(testset$Species, net.prediction)

predict.table

confusionMatrix(predict.table)

compute(network, testset[-5])


## Neural Network with nnet
library(nnet)

data(iris)
set.seed(2)

ind = sample(2,nrow(iris), replace=TRUE, prob=c(0.7,0.3))
trainset = iris[ind == 1, ]
testset = iris[ind == 2,]

iris.nn <- nnet(Species~., data=trainset, size=2, rang=0.1, decay = 5e-4, maxit=200)

summary(iris.nn)


#predicting
iris.predict<- predict(iris.nn,testset, type='class')

nn.table <- table(testset$Species, iris.predict)

confusionMatrix(nn.table)

head(predict(iris.nn, testset))
