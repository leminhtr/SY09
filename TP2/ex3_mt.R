getwd()
setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP2")

###### Library

library(Hmisc)# describe()

library(MASS) #shepard
library(asbio)

library(mclust)# adjustedRandIndex
library(cluster) #diana


### 1. Iris

iris_scaled <- scale(iris[,-5])

fit_k2 <- kmeans(iris_scaled, 2)
fit_k3 <- kmeans(iris_scaled, 3)
fit_k4 <- kmeans(iris_scaled, 4)

x11()
par(mfrow=c(1,3))
clusplot(iris, fit_k2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main = "K=2")
clusplot(iris, fit_k3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main = "K=3")
clusplot(iris, fit_k4$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main = "K=4")

x11()
fit_k3 <- kmeans(iris_scaled, 3)
clusplot(iris, fit_k3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main = "K=3")



### Inertie intra-classe
i <-0
n <- nrow(iris)
s <- 0
for(i in nrow(fit_k3$centers))
{
  
  s <- s+(1/n)*(iris[,-5]-fit_k3$centers[i,])%*% t(iris[,-5]-fit_k3$centers[i,])
}

#Q3.a
iner_iris_k10 <- matrix(0, nrow=100, 10)
for(k in 1:10)
{
  for (i in 1:100){
    iner_iris_k10[i,k] <-kmeans(iris[,-5],k)$tot.withinss
  }
}
#iris K=3
min(iner_iris_k10[,3])

# vecteur d'inertie minimale en fonction de K
min_iner=apply(iner_iris_k10,2,min)
plot(min_iner, main="Inertie intra-classe minimale en fonction de K", type="l", xlab="K",ylab="Inertie intra-classe")

# Inertie totale = 681.3706

par(mfrow=c(1,2))
clusplot(iris[,1:4],fit_k3$cluster,label=2,color=TRUE,main="Partitions avec k-means")
clusplot(iris[,1:4],iris$Species,label=2,color=TRUE,main="Réelle")


### CRABS

### q.2 Crabs

crabs2 <- read.csv("crabs2.csv", header=T)
head(crabs2)

crabs2_quant <- crabs2[,1:4] # variables quantitatives pour ACP

x11()
fit_k2 <- kmeans(crabs2_quant,2)
clusplot(crabs2_quant,fit_k2$cluster,color=TRUE, shade=TRUE, labels=2, lines=0, main = "K=2")
# Horizontal : species
# Vertical : sexe

fit_k2 <- kmeans(crabs2_quant,4)

crabs_sp_sex<-apply(crabs2[5:6],1,paste,collapse=" ") #créer df : concatene le sexe et l'espece
par(mfrow=c(1,2))
clusplot(crabs2_quant,fit_k2$cluster,color=TRUE,  labels=2, lines=0, main = "Partition avec k-means")
clusplot(crabs2[,1:4],crabs_sp_sex,label=2,color=TRUE, lines=0,main="Réelle")


### Mutations

### 1. mutation
mut.raw <- read.csv("mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut.raw, diag=T, upper=T)

fit_aftd <- cmdscale(mut, eig=TRUE, k=5) # -1/2 Q* D^2*Q

x11()
x <- fit_aftd$points[,1]
y <- fit_aftd$points[,2]

fit <- cmdscale(mut, k=5) # -1/2 Q* D^2*Q ; AFTD
fit_k3 <- kmeans(fit,3)
clusplot(fit,fit_k3$cluster,color=TRUE,  labels=2, lines=0, main = "K=3")

fit2 <- cmdscale(mut, k=5) # -1/2 Q* D^2*Q ; AFTD
fit_k32 <- kmeans(fit,3)
clusplot(fit2,fit_k32$cluster,color=TRUE,  labels=2, lines=0, main = "K=3")


# Q.2

iner_mut_k3 <- matrix(0, nrow=100, 5)
colnames(iner_mut_k3)=c("k=1","k=2","k=3","k=4","k=5")
for(k in 1:5)
{
  for (i in 1:100){
    iner_mut_k3[i,k] <-kmeans(fit,k)$tot.withinss
  }
}

min_iner_mut=apply(iner_mut_k3,2,min)
min_iner_mut
plot(min_iner_mut, type="l", xlab="k", ylab="Inertie intra-classe")
var_iner_mut=apply(iner_mut_k3,2,var)
var_iner_mut
summary(iner_mut_k3)
describe(as.factor(iner_mut_k3))
# k=2 : toujours la même valeur => la plus stable
# k=3 : variance la plus faible après k=2

x11()
fit <- cmdscale(mut, k=5) # -1/2 Q* D^2*Q ; AFTD
fit_k2 <- kmeans(fit,2)
clusplot(fit,fit_k2$cluster,color=TRUE,  labels=2, lines=0, main = "K=2")

