setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP1")
#setwd("/media/leminhtr/Data/School/UTC/A16-P17/P17/SY09/TD/TP1")

library("plotrix")


notes <- read.csv("sy02-p2016.csv", na.strings="", header=T)
notes$nom<-factor(notes$nom,levels=notes$nom)
notes$niveau<-factor(notes$niveau,ordered=T)
notes$resultat<-factor(notes$resultat,levels=c("F","Fx","E","D","C","B","A"),ordered=T)

# Note moyenne de median par correcteur
moy.median <- aggregate(note.median~correcteur.median, data=notes, FUN=mean)
names(moy.median) <- c("correcteur","moy.median")

# Ecart type du median par correcteur
std.median <- aggregate(note.median~correcteur.median, data=notes, FUN=sd)
names(std.median) <- c("correcteur","std.median")

# Note moyenne et Ã©cart type du mÃ©dian par correcteur
median <- merge(moy.median, std.median)

moy.final <- aggregate(note.final~correcteur.final, data=notes, FUN=mean)
names(moy.final) <- c("correcteur","moy.final")
std.final <- aggregate(note.final~correcteur.final, data=notes, FUN=sd)
names(std.final) <- c("correcteur","std.final")

# Note moyenne et Ã©cart type du final par correcteur
final <- merge(moy.final, std.final)

# Note (moyenne, Ã©cart type) du mÃ©dian/final par correcteur
correcteurs <- merge(median, final, all=T)

# correcteurs sans la ligne 2 et 3
corr.acp <- correcteurs[-c(2,8),]
corr.acp


### Rappel de cours : ACP :

# Xc tableau de donn�e centr� en colonne
# V= 1/n * Xc'*Xc
# Diagonalisation de V : eigen ; 1�re valeur : valeur propre la plus grande, 2�me valeur : 2�me valeur propre la plus grande,...
#u_alpha : alpha-i�me vecteur propre
#C_alpha= Xc*u_alpha :
#C= Xc*U
# Ordre des axes : quantit� d'inertie d�croissante : axe 1 : variance la + importante ;
# Variance des composantes principales = valeur propre



Y <- as.matrix(corr.acp[,-1])
n <- dim(Y)[1]
X <- Y- (matrix(1,n,1) %*% apply(Y,2,mean))
# X <- X/matrix(1,n,1) %*% apply(X,2,sd)

#Calcul de la matrice de covariance ou de corrélation
V <- (1/n)*t(X) %*% X

#Calcul des valeurs propres et des axes d’inertie
tmp <- eigen(V, symmetric=TRUE)
L <- diag(tmp$values)
U <- tmp$vectors

#Calcul des composantes principales des individus
C <- X %*% U
#Calcul des contributions
COR <- diag(1/apply(X^2,1,sum)) %*% C^2
CTR <- (1/n)*C^2 %*% diag(1/diag(L))


#Représentation des variables
D <- diag(1/(sqrt((n-1)/n)*apply(X, 2, sd))) %*% U %*% sqrt(L)
D
#Tracé des graphiques
plot(-1:1,-1:1,type="n",xlab="Axe 1",ylab="Axe 2")
text(D[,1],D[,2],colnames(corr.acp[,-1]));abline(h=0);abline(v=0)
curve(sqrt(1-x^2),-1,1,add=TRUE)
curve(-sqrt(1-x^2),-1,1,add=TRUE)

plot(-1:1,-1:1,type="n",xlab="Axe 1",ylab="Axe 3")
text(D[,1],D[,3],colnames(corr.acp[,-1]));abline(h=0);abline(v=0)
curve(sqrt(1-x^2),-1,1,add=TRUE)
curve(-sqrt(1-x^2),-1,1,add=TRUE)



plot(C[,1],C[,2],type="n",xlab="Composante 1", ylab="Composante 2",main="Premier plan factoriel");text(C[,1],C[,2],rownames(corr.acp[,-1]))
abline(h=0);abline(v=0)


plot(C[,1],C[,3],type="n",xlab="Composante 1", ylab="Composante 3", main="Second plan factoriel");text(C[,1],C[,3],rownames(corr.acp[,-1]))
abline(h=0);abline(v=0)


#Q.4

#X=C%*%t(U)


res <- princomp(corr.acp[,-1])
#Quelques exemple de sorties
summary(res)
(res$sdev)^2 # Les valeurs propres
res$loadings # Les vecteurs propres
res$scores # Le nouveau tableau individus-variables
plot(res)
#Utilisation de biplot.princomp
biplot(res) # Plan 1,2 avec les
biplot(res,c(1,3)) # Plan 1,3


eigenv <- tmp$values
#colnames(eigenv) <- c("1","2","3","4")
barplot(eigenv, names.arg=1:nrow(eigenv), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.decathlon2.active), 
      eig.decathlon2.active[, 2], 
      type="b", pch=19, col = "red")


# Circle plot
MCor<-t(as.matrix(corr.acp.c[,-1])/matrix(rep(sdX,6),nrow=6,byrow=TRUE))%*%Dp%*%(corr.principal/matrix(rep(sdC,6),nrow=6,byrow=TRUE))
plot(MCor[,1],MCor[,2],xlim=c(-1.2,1.2),ylim=c(-1.2,1.2),pch=1,col="blue",xlab="Comp.1",ylab="Comp.2")
text(MCor[,1],MCor[,2],c(colnames(corr.acp.c[,-1])))
arrows(0, 0,MCor[,1],MCor[,2] , angle= 30,code=2,col="red",cex=0.5)
draw.circle(0,0,1)
abline(h=0,col="green")
abline(v=0,col="green")