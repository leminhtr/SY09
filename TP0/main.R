getwd()
setwd("Z:/SY09/TP0")

source("fonctions.R")


a <- somme(5,3)
a


x<-c(2,4,3,7,1)
A<-matrix(c(1,2,5,3,0,9),nrow=3,byrow=T)
A
max(x)
max(A)

apply(A,1,max)
apply(A,2,max)
E
apply(E,3,sum)
apply(E,2,sum)
apply(E,1,sum)
mean(x)
mean(A)
apply(A,2,mean)


var(A)
cov(A) # unbiaised par défaut => E[(X-E[X])*(Y-E[Y])]
cov.wt(A, method='unbiased') # S*² <=> l'estimateur sans biais de la variance
cov.wt(A, method='ML') # S² <=> l'EMV de la variance

#cor(X,Y)= cov(X,Y)/ (sqrt(Var(X)) * sqrt(Var(Y))) <=> covariance normalisée avec variance

x<-c(2,2,2,1,3,4,1,1)
hist(x)
x<-c(1,2,3,4,5)
y<-c(1,4,9,16,25)
plot(x,y)
plot(x,y,pch=22)
plot(x,y,pch=19,col='blue')
plot(x,y,type='l',col='blue')

x11()


diag(n)

A
centre(A)

scale(A, scale=FALSE)

prodtrans(A)

A
A - colMeans(A)
colMeans(A)
A - rep(colMeans(A), rep.int(nrow(A), ncol(A)))

# variance empirique : 1/n X_center ' %*% X_center
# variance empirique corrigée : 1/n-1 %*% X_center ' * X_center

prodtrans(centre(A)) /nrow(A) # = var(A)
var(A)

prodtrans(centre(A)) /(nrow(A)-1)



# Partie 2.2 ----------------------------------
data(iris)
class(iris)
names(iris)
head(iris)
iris[,1]
class(iris[,1])
class(iris$Species)

summary(iris)
apply(iris[,1:4],2, mean)






