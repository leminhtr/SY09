getwd()
setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP3")

source("distXY.R")
source("front.ceuc.R")
source("front.kppv.R")
source("distXY.R")
source("separ1.R")
source("separ2.R")
front.ceuc <- function(mu, Xaff, zaff, discretisation=50)
{
    deltaX <- (max(Xaff[,1]) -min(Xaff[,1]))/discretisation
    deltaY <- (max(Xaff[,2]) -min(Xaff[,2]))/discretisation
    minX <- min(Xaff[,1])-deltaX
    maxX <- max(Xaff[,1])+deltaX
    minY <- min(Xaff[,2])-deltaY
    maxY <- max(Xaff[,2])+deltaY

    # grille d'affichage 
    grilleX <- seq(from=minX,to=maxX,by=deltaX)
    naffX <- length(grilleX)
    grilleY <- seq(from=minY,to=maxY,by=deltaY)
    naffY <- length(grilleY)
    grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))

    # calcul des valeurs de la fonction 
    valf <- ceuc.val(mu, grille)
    plot(Xaff, col=c("red","green","blue","magenta","orange")[zaff], asp=1)
    contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}



ceuc.app<-function(Xapp,zapp){
  p<-dim(Xapp)[2];
  n<-dim(Xapp)[1];
  mu<-matrix(rep(0,p*2),2,p);
  for (g in 1:2){
    x<-rep(0,p);
    n_g<-0;
    for (i in 1:n){
      if( zapp[i]==g){
        x<-x+Xapp[i,];
        n_g<-n_g+1;
      }
    }
    mu[g,]<-x/n_g;
  }
  mu;
}

ceuc.val <- function(mu, Xtst){
  label <- matrix(nrow=nrow(Xtst))
  distance <- distXY(mu,Xtst)
  distance
  for(i in nrow(label)){
    label[i]<-  which.min(distance[i,])
    
  }
  label
}



