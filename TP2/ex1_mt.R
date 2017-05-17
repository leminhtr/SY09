getwd()
setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP2")

###### Library

library(Hmisc)# describe()

library(MASS) #shepard
library(asbio)

library(mclust)# adjustedRandIndex



########## Exercice 1 

#### q.1 Iris
data("iris")

head(iris)

## ACP iris : 2 plot du premier plan factoriel
res.acp.iris <- princomp(iris[,-5])

x11()
par(mfrow=c(1,2))
plot(res.acp.iris$scores, main="Premier plan factoriel")
plot(res.acp.iris$scores, col=topo.colors(3)[iris[,5]])
legend("topright", c("Setosa", "Versicolor", "Virginica"), fill=topo.colors(3))
# setosa, versicolor, virginica



### q.2 Crabs

crabs2 <- read.csv("crabs2.csv", header=T)
head(crabs2)

crabs2_quant <- crabs2[,1:4] # variables quantitatives pour ACP

## ACP crabs2
res.acp.crabs <- princomp(crabs2_quant)
dev.off()

# plot premier plan factoriel : neutre, sp, sex
x11()
par(mfrow=c(1,3))
plot(res.acp.crabs$scores, main="Premier plan factoriel")
plot(res.acp.crabs$scores, col=topo.colors(3)[crabs2[,5]], main="Premier plan factoriel (sp)")
legend("topleft", c("B","O"), fill=topo.colors(3))
plot(res.acp.crabs$scores, col=topo.colors(3)[crabs2[,6]], main="Premier plan factoriel (sex)")
legend("topleft", c("F","M"), fill=topo.colors(3))
# 



### q.3 

mut.raw <- read.csv("mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut.raw, diag=T, upper=T)


describe(mut)
summary(mut)



  # eigen cmdscale AFTD : on ne peut pas interpréter des vp. negatives => on les ignore

fit <- cmdscale(mut, eig=TRUE, k=2) # -1/2 Q* D^2*Q
fit
x11()
x <- fit$points[,1]
y <- fit$points[,2]
plot(fit$points[,1],fit$points[,2], xlab="Coordinate 1", ylab="Coordinate 2", main="Metric	MDS")
text(x, y, labels = row.names(mut.raw)) # on s'intéresse à la distance des points par rapport aux autres et distinguer des groupes

Shepard(x,y,p=2)
Shep.comp(mut,d=2)
# alignés <=> représentation parfaite
# sinon : dessus ou dessous alors sur-estime /sous estime représentation


#adjustedRandIndex()

par(mfrow=c(1,3))
fit<-cmdscale(mut,eig=TRUE,k=2)
plot(Shepard(mut.raw[lower.tri(mut.raw)],fit$points), main="k=2")
fit<-cmdscale(mut,eig=TRUE,k=3)
plot(Shepard(mut.raw[lower.tri(mut.raw)],fit$points), main="k=3")
fit<-cmdscale(mut,eig=TRUE,k=5)
plot(Shepard(mut.raw[lower.tri(mut.raw)],fit$points), main="k=5")


mut2<-cmdscale(mut, k=2)
x11()

plot(mut2, col=rainbow(ncol(mut.raw)), xlab="Axe 1", ylab="Axe 2")
text(mut2[,1], mut2[,2],labels(mutmat[,1]), pos=1)
abline(h=0,v=0)


s2=Shepard(as.dist(mut),cmdscale(mut,k=2))
s3=Shepard(as.dist(mut),cmdscale(mut,k=3))
s4=Shepard(as.dist(mut),cmdscale(mut,k=4))
s5=Shepard(as.dist(mut),cmdscale(mut,k=5))

x11()

par(mfrow=c(1,4))
plot(s2, col=rainbow(ncol(mut.raw)), xlab="Dissimilarité", ylab="Distance", main="d=2")
abline(0, 1)
plot(s3, col=rainbow(ncol(mut.raw)), xlab="Dissimilarité", ylab="Distance", main="d=3")
abline(0, 1)
plot(s4, col=rainbow(ncol(mut.raw)), xlab="Dissimilarité", ylab="Distance", main="d=4")
abline(0, 1)
plot(s5, col=rainbow(ncol(mut.raw)), xlab="Dissimilarité", ylab="Distance", main="d=5")
abline(0, 1)







