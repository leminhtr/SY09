getwd()
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
	X <- t(as.matrix(X))
	Y <- t(as.matrix(Y))
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
	p<-dim(Xtst)[2];
	n<-dim(Xtst)[1];
	d<-rep(0,2);
	z<-rep(0,n);
	for(i in 1:n){
		d[1]<-distXY(Xtst[i,],mu[1,]);
		d[2]<-distXY(Xtst[i,],mu[2,]);
		z[i]<-which.min(d);		
	}	
	z;
}

################k plus proches voisins

kppv.val<-function(Xapp,zapp,K,Xtst){
	Xapp<-as.matrix(Xapp);
	Xtst<-as.matrix(Xtst);
	n_tst<-dim(Xtst)[1];
	n_app<-dim(Xapp)[1];
	p<-dim(Xtst)[2];
	#g_centre<-matrix(rep(0,K*p),K,p);####to modify
	d<-rep(0,n_app);
	d_index<-rep(0,n_app);
	g_count<-rep(0,2);
	z<-rep(0,n_tst);
	
	for(i in 1:n_tst){
		d<-rep(0,n_app);
		
		for(j in 1:n_app){
			d[j]<-distXY(Xtst[i,],Xapp[j,]);
		}
		
		d_index<-order(d)[1:K];
		g_count[1]<-table(zapp[d_index])['1'];
		g_count[2]<-table(zapp[d_index])['2'];
		g_count[is.na(g_count)]<-0;
		
		if( g_count[1] >=g_count[2] ){
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
	ztst<-rep(0,length(zval));
	n_k<-rep(0,length(nppv));
	for(k in nppv){
		ztst<-kppv.val(Xapp,zapp,k,Xval);
		for(i in length(zval)){
			if (zval[i]==ztst[i]){
				n_k[i]<-n_k[i]+1;
			}
		}
	}
	nppv[which.max(n_k)];
}

###########


donn<-read.csv("Synth1-40.csv")
X<-donn[,1:2];
z<-donn[,3]
donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst


mu <- ceuc.app(X, z)
front.ceuc(mu, X, z, 1000)
Kopt <- kppv.tune(X, z, X, z,
2*(1:6)-1)
front.kppv(X, z, Kopt, 100)

###### II)

#### Synth1-40.csv
Synth140 <- read.csv("Synth1-40.csv")
Synth140

z1 <- Synth140[Synth140$z==1, "z"]
z2 <- Synth140[Synth140$z==2, "z"]


nk1 <- length(z1)
nk2 <- length (z2)

d1_40<-read.csv("Synth1-40.csv")
class1<-d1_40[d1_40[,3]==1,]
class2<-d1_40[d1_40[,3]==2,]
mean(class1[,1])#z=1
mean(class1[,2])#z=1
mu1<-c(mean(class1[,1]),mean(class1[,2]))

mean(class2[,1])#z=1
mean(class2[,2])#z=1
mu2<-c(mean(class2[,1]),mean(class2[,2]))
var1<-t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[2]


#proportion
n<-dim(d1_40)[1]
proportion1<-(dim(d1_40[d1_40[,3]==1,])[1])/n
proportion2<-(dim(d1_40[d1_40[,3]==2,])[1])/n


#### Synth1-100.csv
Synth1100 <- read.csv("Synth1-1100.csv")
Synth1100

z1 <- Synth1100[Synth1100$z==1, "z"]
z2 <- Synth1100[Synth1100$z==2, "z"]


nk1 <- length(z1)
nk2 <- length (z2)

d1_100<-read.csv("Synth1-100.csv")
class1<-d1_100[d1_100[,3]==1,]
class2<-d1_100[d1_100[,3]==2,]
mean(class1[,1])#z=1
mean(class1[,2])#z=1
mu1<-c(mean(class1[,1]),mean(class1[,2]))

mean(class2[,1])#z=1
mean(class2[,2])#z=1
mu2<-c(mean(class2[,1]),mean(class2[,2]))
var1<-t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[2]


#proportion
n<-dim(d1_100)[1]
proportion1<-(dim(d1_100[d1_100[,3]==1,])[1])/n
proportion2<-(dim(d1_100[d1_100[,3]==2,])[1])/n



#### Synth1-500.csv
Synth1500 <- read.csv("Synth1-40.csv")
Synth1500

z1 <- Synth1500[Synth1500$z==1, "z"]
z2 <- Synth1500[Synth1500$z==2, "z"]


nk1 <- length(z1)
nk2 <- length (z2)

d1_500<-read.csv("Synth1-500.csv")
class1<-d1_500[d1_500[,3]==1,]
class2<-d1_500[d1_500[,3]==2,]
mean(class1[,1])#z=1
mean(class1[,2])#z=1
mu1<-c(mean(class1[,1]),mean(class1[,2]))

mean(class2[,1])#z=1
mean(class2[,2])#z=1
mu2<-c(mean(class2[,1]),mean(class2[,2]))
var1<-t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[2]


#proportion
n<-dim(d1_500)[1]
proportion1<-(dim(d1_500[d1_500[,3]==1,])[1])/n
proportion2<-(dim(d1_500[d1_500[,3]==2,])[1])/n


#### Synth1-1000.csv
Synth11000 <- read.csv("Synth1-1000.csv")
Synth11000

z1 <- Synth11000[Synth11000$z==1, "z"]
z2 <- Synth11000[Synth11000$z==2, "z"]


nk1 <- length(z1)
nk2 <- length (z2)

d1_1000<-read.csv("Synth1-1000.csv")
class1<-d1_1000[d1_1000[,3]==1,]
class2<-d1_1000[d1_1000[,3]==2,]
mean(class1[,1])#z=1
mean(class1[,2])#z=1
mu1<-c(mean(class1[,1]),mean(class1[,2]))

mean(class2[,1])#z=1
mean(class2[,2])#z=2
mu2<-c(mean(class2[,1]),mean(class2[,2]))
var1<-t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[2]


#proportion
n<-dim(d1_1000)[1]
proportion1<-(dim(d1_1000[d1_1000[,3]==1,])[1])/n
proportion2<-(dim(d1_1000[d1_1000[,3]==2,])[1])/n


## q.2
donn.sep <- separ1(X, z)
N <- 20
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst


mu <- ceuc.app(Xapp, zapp)

zpred <-ceuc.val(mu,Xtst)

Kopt <- kppv.tune(X, z, X, z,
                  2*(1:6)-1)
front.kppv(X, z, Kopt, 100)


validation<-function(tst,pred){
  n<-length(tst);
  err<-0;
  for(i in 1:n){
    if(tst[i]!=pred[i]){
      err<-err+1;
    }
  }
  err/n;
}

N <- 20
tab_err <-matrix(ncol=1, nrow=N)
tab_err <- rep(0,20)
for (i in 1:N)
{
  donn.sep <- separ1(X, z)

  Xapp <- donn.sep$Xapp
  zapp <- donn.sep$zapp
  Xtst <- donn.sep$Xtst
  ztst <- donn.sep$ztst
  
  mu <- ceuc.app(Xapp, zapp)
  
  zpred <-ceuc.val(mu,Xtst)
  
  tab_err[i] <- validation(ztst, zpred)
  
  
}

## Synth1-100.csv

donn<-read.csv("Synth1-100.csv")
X<-donn[,1:2];
z<-donn[,3]
donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

tab_err <- rep(0,20)
for (i in 1:N)
{
  donn.sep <- separ1(X, z)
  
  Xapp <- donn.sep$Xapp
  zapp <- donn.sep$zapp
  Xtst <- donn.sep$Xtst
  ztst <- donn.sep$ztst
  
  mu <- ceuc.app(Xapp, zapp)
  
  zpred <-ceuc.val(mu,Xtst)
  
  tab_err[i] <- validation(ztst, zpred)
  
  
}
tab_err


## Synth1-500.csv

donn<-read.csv("Synth1-500.csv")
X<-donn[,1:2];
z<-donn[,3]
donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

tab_err <- rep(0,20)
for (i in 1:N)
{
  donn.sep <- separ1(X, z)
  
  Xapp <- donn.sep$Xapp
  zapp <- donn.sep$zapp
  Xtst <- donn.sep$Xtst
  ztst <- donn.sep$ztst
  
  mu <- ceuc.app(Xapp, zapp)
  
  zpred <-ceuc.val(mu,Xtst)
  
  tab_err[i] <- validation(ztst, zpred)
  
  
}
tab_err

## Synth1-1000.csv

donn<-read.csv("Synth1-1000.csv")
X<-donn[,1:2];
z<-donn[,3]
donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

tab_err <- rep(0,20)
for (i in 1:N)
{
  donn.sep <- separ1(X, z)
  
  Xapp <- donn.sep$Xapp
  zapp <- donn.sep$zapp
  Xtst <- donn.sep$Xtst
  ztst <- donn.sep$ztst
  
  mu <- ceuc.app(Xapp, zapp)
  
  zpred <-ceuc.val(mu,Xtst)
  
  tab_err[i] <- validation(ztst, zpred)
  
  
}
tab_err

## q.3

## Synth1-1000.csv

donn<-read.csv("Synth1-40.csv")
X<-donn[,1:2];
z<-donn[,3]
donn.sep <- separ2(X, z)
Xapp <- donn.sep$Xval
zapp <- donn.sep$zval
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

Kopt <- kppv.tune(X, z, X, z, 2*(1:6)-1)



#q.4

donn<-read.csv("Synth1-1000.csv")
X<-donn[,1:2];
z<-donn[,3]
donn.sep <- separ2(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

tab_err <- rep(0,20)
for (i in 1:N)
{
  donn.sep <- separ2(X, z)
  Xapp <- donn.sep$Xapp
  zapp <- donn.sep$zapp
  Xval <- donn.sep$Xval
  zval <- donn.sep$zval
  Xtst <- donn.sep$Xtst
  ztst <- donn.sep$ztst
  
  mu <- ceuc.app(Xapp, zapp)
  
  zpred <-ceuc.val(mu,Xtst)
  
  Kopt <- kppv.tune(Xapp, zapp, Xval, zval, 2*(1:6)-1)
  tab_err[i] <- validation(ztst, zpred)
  
  
  
}
tab_err

# intervalle de confiance au degré alpha*=0.05 : [mu (+-) sdtev]

#1.2.2

# 1.

pima<-read.csv("Pima.csv")
class1<-pima[pima[,8]==1,]
class2<-pima[pima[,8]==2,]
mean(class1[,1])#z=1
mean(class1[,2])#z=1
mu1<-c(mean(class1[,1]),mean(class1[,2]))

mean(class2[,1])#z=1
mean(class2[,2])#z=1
mu2<-c(mean(class2[,1]),mean(class2[,2]))
var1<-t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[2]


donn<-read.csv("Pima.csv")
X<-donn[,1:7];
z<-donn[,8]
donn.sep <- separ2(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

tab_err <- rep(0,20)
Kopt <- rep(0,20)
for (i in 1:N)
{
  donn.sep <- separ2(X, z)
  Xapp <- donn.sep$Xapp
  zapp <- donn.sep$zapp
  Xval <- donn.sep$Xval
  zval <- donn.sep$zval
  Xtst <- donn.sep$Xtst
  ztst <- donn.sep$ztst
  
  mu <- ceuc.app(Xapp, zapp)
  
  zpred <-rep(0,length(ztst))
  
  Kopt[i] <- kppv.tune(Xapp, zapp, Xval, zval, 2*(1:6)-1)
  tab_err[i] <- validation(ztst, zpred)
  
  
  
}
tab_err

