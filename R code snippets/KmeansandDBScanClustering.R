#hierarchical clustering Agglomerative  Bottom Up approach
customerdf <- read.csv("customer.csv",sep=',', header=TRUE)
head(customerdf)

str(customerdf)

dim(customerdf)

customerdf <- scale(customerdf[,-1])

hc <- hclust(dist(customerdf, method = 'euclidean'), method = 'ward.D2')
hc

plot(hc, hng=0.01, cex=0.7)


hc2 = hclust(dist(customerdf), method='single')

plot(hc2, hng=0.01, cex=0.7)



#Hierarchical clustering Divisive clustering Top down approach
library(cluster)

dv <- diana(customerdf, metric='euclidean')
summary(dv)

plot(dv)

library(dendextend)
library('magrittr')

dend <- customerdf %>% dist %>% hclust %>% as.dendrogram

dend %>% plot(horiz = TRUE, main = "Horizontal Dendrogram")


fit <- cutree(hc, k=4)
fit

table(fit)

plot(hc)
rect.hclust(hc, k=4, border="red")

rect.hclust(hc, k=4, which=2, border="green")

dend %>% color_branches(k=4) %>% plot(horiz =TRUE, main = "Horizontal Dendrogram")
dend %>% rect.dendrogram(k=4, horiz=TRUE)
abline(v = heights_per_k.dendrogram(dend)["4"] + .1, lwd=2, lty=2,col="blue")


## KMeans

set.seed(22)
fit <- kmeans(customerdf,4)
fit

barplot(t(fit$centers),beside=TRUE, xlab="cluster",ylab="value")

plot(customerdf,col=fit$cluster)


#bivariate cluster plot

clusplot(customerdf,fit$cluster,color=TRUE,shade=TRUE)

par(mfrow=c(1,2))
clusplot(customerdf,fit$cluster,color=TRUE,shade=TRUE)
rect(-0.7,-1.7,2.2,-1.2,border="orange",lwd=2)
clusplot(customerdf,fit$cluster,color=TRUE,xlim=c(-0.7,2.2),ylim=c(-1.7,-1.2))

mds <- cmdscale(dist(customerdf),k=2)
plot(mds,col=fit$cluster)

#cluster validation
library(fpc)

single_c <- hclust(dist(customerdf), method='single')
hc_single <- cutree(single_c,k=4)

complete_c <- hclust(dist(customerdf), method="complete")
hc_complete <- cutree(complete_c, k=4)

set.seed(22)
km <- kmeans(customerdf,4)

cs <- cluster.stats(dist(customerdf), km$cluster)

cs[c("within.cluster.ss", "avg.silwidth")]

sapply(list(kmeans <- km$cluster, hc_single = hc_single, hc_complete = hc_complete),
       function(c) cluster.stats(dist(customerdf),c)[c("within.cluster.ss","avg.silwidth")])

# avg silwidth represents how closely the points in a cluster are related and how each cluster is separated properly

km$withinss
km$betweenss

#silhouette information

kms <- silhouette(km$cluster, dist(customerdf))
summary(kms)
plot(kms)

#optimum number of clusters

nk <- 2:10
set.seed(22)

wss <- sapply(nk, function(k){
  kmeans(customerdf, centers=k)$tot.withinss
})

wss

plot(nk, wss, type="l", xlab="number of k",ylab="within sum of squares")

sw <- sapply(nk, function(k){
  cluster.stats(dist(customerdf), kmeans(customerdf,centers=k)$cluster)$avg.silwidth
})
sw

plot(nk,sw,type="l",xlab="number 0f clusters", ylab="average silhoutte width")

nk[which.max(sw)]

## density based clustering
library(mlbench)
set.seed(2)
p <- mlbench.cassini(500)
plot(p$x)

ds <- dbscan(dist(p$x), 0.2, 2, countmode=NULL,method="dist")
ds

plot(ds,p$x)

#predicting

y=matrix(0,nrow=3, ncol=2)
y[1,] = c(0,0)
y[2,] = c(0,-1.5)
y[3,] = c(1,1)
y


predict(ds,p$x,y)


##clustering with the model based 

library(mclust)

mb<- Mclust(customerdf)
plot(mb)

summary(mb)


##validating the clusters externally
library(png)

img2 <- readPNG("handwriting.png",TRUE)
img3 <- img2[,nrow(img2):1]
b <- cbind(as.integer(which(img3 < -1)  %% 28), which(img3 < -1)/28)
plot(b,xlim=c(1,28), ylim=c(1,28))

set.seed(18)

fit<- kmeans(b,2)
plot(b, col=fit$cluster)
plot(b, col=fit$cluster, xlim=c(1,28), ylim=c(1,28))

ds <- dbscan(b,2)
ds
plot(ds,b, xlim=c(1,28), ylim=c(1,28))
