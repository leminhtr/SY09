getwd()
setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP2")

###### Library

library(Hmisc)# describe()

library(MASS) #shepard
library(asbio)

library(mclust)# adjustedRandIndex
library(cluster) #diana


### 1. mutation
mut.raw <- read.csv("mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut.raw, diag=T, upper=T)

# The complete linkage method finds similar clusters
cah_mut <- hclust(mut, method="complete")
cah_mut_sgl <- hclust(mut, method="single")
# option "ward.D2" implements that criterion (Murtagh and Legendre 2014). 
# With the latter, the dissimilarities are squared before cluster updating
cah_mut_ward <- hclust(mut, method="ward.D2")
cah_mut_ctr <- hclust(mut, method="centroid")
x11()
par(mfrow=c(2,2))
plot(cah_mut, main="complete")
plot(cah_mut_sgl, main="single")
plot(cah_mut_ward, main="ward D2")
plot(cah_mut_ctr, main="centroid")


### 2. iris

data("iris")

iris_dist <- dist(iris[,-4]*iris[,-4])

cah_irs <- hclust(iris_dist, method="complete")
cah_irs_sgl <- hclust(iris_dist, method="single")
# option "ward.D2" implements that criterion (Murtagh and Legendre 2014). 
# With the latter, the dissimilarities are squared before cluster updating
cah_irs_ward <- hclust(iris_dist, method="ward.D2")
cah_irs_ctr <- hclust(iris_dist, method="centroid")
x11()
par(mfrow=c(2,2))
plot(cah_irs, main="complete")
plot(cah_irs_sgl, main="single")
plot(cah_irs_ward, main="ward D2")
plot(cah_irs_ctr, main="centroid")

cdh_irs <- diana(iris_dist)

x11()
par(mfrow=c(1,2))
plot(cdh_irs)









