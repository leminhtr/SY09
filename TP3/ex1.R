setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP3")

source("distXY.R")
source("front.ceuc.R")
source("front.kppv.R")
source("distXY.R")
source("separ1.R")
source("separ2.R")


#########classifeur euclidien
ceuc.app<-function(Xapp,zapp){
	Xapp<-as.matrix(Xapp);
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

distXY <- function(X, Y)
{
	X <- as.matrix(X)
	Y <- as.matrix(Y)
	nx <- dim(X)[1]
	ny <- dim(Y)[1]
	h.x <- rowSums(X^2)
	h.y <- rowSums(Y^2)
	ones.x <- rep(1,nx)
	ones.y <- rep(1,ny)
	D2xy <- h.x%*%t(ones.y) - 2*X %*% t(Y) + ones.x%*%t(h.y)
	D2xy;
}

ceuc.val<-function(mu,Xtst){
	Xtst<-as.matrix(Xtst);
	mu<-as.matrix(mu);
	n<-dim(Xtst)[1];
	z<-rep(0,n);
	distance<-distXY(Xtst,mu);
	for(i in 1:n){
		z[i]<-which.min(distance[i,]);		
	}	
	z;
}

################k plus proches voisins

kppv.val<-function(Xapp,zapp,K,Xtst){
	Xapp<-as.matrix(Xapp);
	Xtst<-as.matrix(Xtst);
	n_tst<-dim(Xtst)[1];
	n_app<-dim(Xapp)[1];
	d_index<-rep(0,n_app);
	g_count<-rep(0,2);
	z<-rep(0,n_tst);
	
	for(i in 1:n_tst){
		distance<-distXY(Xtst,Xapp);
		
		d_index<-order(distance[i,])[1:K];
		g_count[1]<-table(zapp[d_index])['1'];
		g_count[2]<-table(zapp[d_index])['2'];
		g_count[is.na(g_count)]<-0;
		
		if( g_count[1] >g_count[2] ){
			z[i]<-1;
		}
		else{
			z[i]<-2;
		}					
	}
	z;
}

#######tune

kppv.tune<-function(Xapp,zapp,Xval,zval,nppv){
	Xapp<-as.matrix(Xapp);
	Xval<-as.matrix(Xval);
	zpred<-rep(0,length(zval));
	n_k<-rep(0,length(nppv));
	for(k in 1:length(nppv)){
		zpred<-kppv.val(Xapp,zapp,nppv[k],Xval);
		for(i in length(zval)){
			if (zval[i]==zpred[i]){
				n_k[k]<-n_k[k]+1;
			}
		}
	}
	nppv[which.max(n_k)];
}

###########
donn<-read.csv("~/sy09/tp3/donnees-tp3/Synth1-40.csv")
X<-donn[,1:2];
z<-donn[,3]
#separ1
donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

mu <- ceuc.app(Xapp, zapp)
front.ceuc(mu, X, z, 1000)




#separ2
donn.sep <- separ2(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xval <- donn.sep$Xval
zval <- donn.sep$zval
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

Kopt <- kppv.tune(Xapp, zapp, Xval, zval,
2*(1:6)-1)
front.kppv(Xapp, zapp, Kopt, 1000)







