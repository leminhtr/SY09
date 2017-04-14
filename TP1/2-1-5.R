setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP1")

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
corr.acp <- correcteurs[,-1]

# Fill with column mean
for(i in 1:ncol(corr.acp)){
  corr.acp[is.na(corr.acp[,i]), i] <- mean(corr.acp[,i], na.rm = TRUE)
}


Y <- as.matrix(corr.acp)
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

x11()
#Représentation des variables
D <- diag(1/(sqrt((n-1)/n)*apply(X, 2, sd))) %*% U %*% sqrt(L)
D
#Tracé des graphiques
plot(-1:1,-1:1,type="n",xlab="Axe 1",ylab="Axe 2")
text(D[,1],D[,2],colnames(corr.acp));abline(h=0);abline(v=0)
curve(sqrt(1-x^2),-1,1,add=TRUE)
curve(-sqrt(1-x^2),-1,1,add=TRUE)

plot(-1:1,-1:1,type="n",xlab="Axe 1",ylab="Axe 3")
text(D[,1],D[,3],colnames(corr.acp));abline(h=0);abline(v=0)
curve(sqrt(1-x^2),-1,1,add=TRUE)
curve(-sqrt(1-x^2),-1,1,add=TRUE)



plot(C[,1],C[,2],type="n",xlab="Composante 1", ylab="Composante 2",main="Premier plan factoriel");text(C[,1],C[,2],rownames(corr.acp))
abline(h=0);abline(v=0)


plot(C[,1],C[,3],type="n",xlab="Composante 1", ylab="Composante 3", main="Second plan factoriel");text(C[,1],C[,3],rownames(corr.acp))
abline(h=0);abline(v=0)


res <- princomp(corr.acp)
#Quelques exemple de sorties
summary(res)
(res$sdev)^2 # Les valeurs propres
res$loadings # Les vecteurs propres
res$scores # Le nouveau tableau individus-variables
plot(res)
#Utilisation de biplot.princomp
biplot(res) # Plan 1,2 avec les
biplot(res,c(1,3)) # Plan 1,3


x11()

write.table(U, "q5_vectors.txt", quote=FALSE, eol="\\\\\n", sep=" & ")
write.table(C, "q5_indiv_fac.txt", quote=FALSE, eol="\\\\\n", sep=" & ")
write.table(X, "q5_indiv_data.txt", quote=FALSE, eol="\\\\\n", sep=" & ")