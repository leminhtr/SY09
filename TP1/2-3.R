setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP1")
#setwd("/media/leminhtr/Data/School/UTC/A16-P17/P17/SY09/TD/TP1")

crabsquant.pr<-princomp(crabsquant)
crabsquant.pr$scores
plot(crabsquant.pr)
biplot(crabsquant.pr)
names(crabsquant.pr)
plot(crabsquant.pr$scores,col=crabs$sp)
legend(20,3,legend=c("O","B"),col=c(1,2),pch=c(1))
#######différent des 2
plot(crabsquant.pr$scores[,1],crabsquant.pr$scores[,2],col=crabsnew$sp_n,pch=crabsnew$sex_n,xlab="comp.1",ylab="comp.2")
legend(20,3,legend=c("O,F","B,F","O,M","B,M"),col=c('red','blue','red','blue'),pch=c(1,1,2,2))

#######traitement pour le RW
crabsquant.pr<-princomp(crabsquant[,-2])
plot(crabsquant.pr$scores,col=crabs$sp,main="Classification de sp après supprimer RW")
legend("topright",legend=c("B","O"),col=c(1,2),pch=1)
####classification seulment sex
crabsquant.pr<-princomp(crabsquant)
par(mfrow=c(1,2))
plot(crabsquant.pr$scores[crabs$sp=='B',],col=crabs$sex,main="Sachant sp='B', classification du sexe" )
legend("topright",legend=c("F","M"),col=c(1,2),pch=1)
plot(crabsquant.pr$scores[crabs$sp=='O',],col=crabs$sex,main="Sachant sp='O', classification du sexe" )     
     legend("topright",legend=c("F","M"),col=c(1,2),pch=1)

## PCA Plot 
res <- princomp(crabsquant)
fviz_pca_ind(res, col.ind="cos2") +
scale_color_gradient2(low="white", mid="blue",
high="red", midpoint=0.50) + theme_minimal()


fviz_pca_biplot(res,  geom = "text") +
theme_minimal()     