setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP3")

source("distXY.R")
source("front.ceuc.R")
source("front.kppv.R")
source("distXY.R")
source("separ1.R")
source("separ2.R")


##################1_40##############
d1_40<-read.csv("Synth1-40.csv")
class1<-d1_40[d1_40[,3]==1,]
class2<-d1_40[d1_40[,3]==2,]
mean(class1[,1])#z=1
mean(class1[,2])#z=1
mu1<-c(mean(class1[,1]),mean(class1[,2]))
mu1<-matrix(mu1,ncol=2,nrow=dim(class1)[1],byrow=TRUE)

mean(class2[,1])#z=1
mean(class2[,2])#z=1
mu2<-c(mean(class2[,1]),mean(class2[,2]))
mu2<-matrix(mu2,ncol=2,nrow=dim(class2)[1],byrow=TRUE)

var1<-t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[1]


#proportion
n<-dim(d1_40)[1]
proportion1<-(dim(class1)[1])/n
proportion2<-(dim(class2)[1])/n

################1_100#########################
d1_100<-read.csv("Synth1-100.csv")
#proportion
n<-dim(d1_100)[1]
class1<-d1_100[d1_100[,3]==1,]
class2<-d1_100[d1_100[,3]==2,]

proportion1<-(dim(class1)[1])/n
proportion2<-(dim(class2)[1])/n
mean(class1[,1])#z=1
mean(class1[,2])#z=2
mu1<-c(mean(class1[,1]),mean(class1[,2]))
mu1<-matrix(mu1,ncol=2,nrow=dim(class1)[1],byrow=TRUE)

mean(class2[,1])#z=1
mean(class2[,2])#z=2
mu2<-c(mean(class2[,1]),mean(class2[,2]))
mu2<-matrix(mu2,ncol=2,nrow=dim(class2)[1],byrow=TRUE)


var1<-t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[1]




###############1_500################################

d1_500<-read.csv("Synth1-500.csv")


class1<-d1_500[d1_500[,3]==1,]
class2<-d1_500[d1_500[,3]==2,]
mean(class1[,1])#z=1
mean(class1[,2])#z=1
mu1<-c(mean(class1[,1]),mean(class1[,2]))
mu1<-matrix(mu1,ncol=2,nrow=dim(class1)[1],byrow=TRUE)

mean(class2[,1])#z=1
mean(class2[,2])#z=1
mu2<-c(mean(class2[,1]),mean(class2[,2]))
mu2<-matrix(mu2,ncol=2,nrow=dim(class2)[1],byrow=TRUE)


var1<-t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[1]


#proportion
n<-dim(d1_500)[1]
proportion1<-(dim(class1)[1])/n
proportion2<-(dim(class2)[1])/n

#################1_1000################################
d1_1000<-read.csv("Synth1-1000.csv")

class1<-d1_1000[d1_1000[,3]==1,]
class2<-d1_1000[d1_1000[,3]==2,]
mean(class1[,1])#z=1
mean(class1[,2])#z=2
mu1<-c(mean(class1[,1]),mean(class1[,2]))
mu1<-matrix(mu1,ncol=2,nrow=dim(class1)[1],byrow=TRUE)

mean(class2[,1])#z=1
mean(class2[,2])#z=1
mu2<-c(mean(class2[,1]),mean(class2[,2]))
mu2<-matrix(mu2,ncol=2,nrow=dim(class2)[1],byrow=TRUE)

var1<-(     t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1)))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[1]


#proportion
n<-dim(d1_1000)[1]
proportion1<-(dim(class1)[1])/n
proportion2<-(dim(class2)[1])/n


#################2_1000##############################

d2_1000<-read.csv("~/sy09/tp3/donnees-tp3/Synth2-1000.csv")

class1<-d2_1000[d2_1000[,3]==1,]
class2<-d2_1000[d2_1000[,3]==2,]
mean(class1[,1])#z=1
mean(class1[,2])#z=1
mu1<-c(mean(class1[,1]),mean(class1[,2]))
mu1<-matrix(mu1,ncol=2,nrow=dim(class1)[1],byrow=TRUE)

mean(class2[,1])#z=1
mean(class2[,2])#z=1
mu2<-c(mean(class2[,1]),mean(class2[,2]))
mu2<-matrix(mu2,ncol=2,nrow=dim(class2)[1],byrow=TRUE)

var1<-t(as.matrix(class1[,1:2]-mu1))%*%(as.matrix(class1[,1:2]-mu1))/dim(class1)[1]
var2<-t(as.matrix(class2[,1:2]-mu2))%*%(as.matrix(class2[,1:2]-mu2))/dim(class2)[1]


#proportion
n<-dim(d2_1000)[1]
proportion1<-(dim(class1)[1])/n
proportion2<-(dim(class2)[1])/n

####################evalution des performance euc#################
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


donn<-d1_40
X<-donn[,1:2];
z<-donn[,3]
#separ1
donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

mu <- ceuc.app(Xapp, zapp)
zpred<-ceuc.val(mu, Xtst)


donn<-d1_1000
X<-donn[,1:2];
z<-donn[,3]
ERR_tst<-rep(0,20)
ERR_app<-rep(0,20)

for(i in 1:20){
	donn.sep <- separ1(X, z);
	Xapp <- donn.sep$Xapp;
	zapp <- donn.sep$zapp;
	Xtst <- donn.sep$Xtst;
	ztst <- donn.sep$ztst;
	
	mu <- ceuc.app(Xapp, zapp);
	zpred<-ceuc.val(mu, Xapp);
	ERR_app[i]<-validation(zapp,zpred);
	
	zpred<-ceuc.val(mu, Xtst);
	ERR_tst[i]<-validation(ztst,zpred);
}

interv_conf <- function(vector){
  interv <- c(0,0,0);
  mu <- mean(vector);
  s <- sd(vector);
  n <- length(vector);
  interv[1]<- mu
  interv[2]<-mu-1.96* s/sqrt(n)
  interv[3]<-mu+1.96* s/sqrt(n)
  
  interv
}# renvoie moyenne, borne inf, borne sup.

interv_conf(ERR_app)
interv_conf(ERR_tst)



# 3.
#########################err k voisin######################

kppv.tune<-function(Xapp,zapp,Xval,zval,nppv){
	Xapp<-as.matrix(Xapp);
	Xval<-as.matrix(Xval);
	ztst<-rep(0,length(zval));
	n_k<-rep(0,length(nppv));
	for(k in nppv){
		ztst<-kppv.val(Xapp,zapp,k,Xval);
		for(i in length(zval)){
			if (zval[i]==ztst[i]){
				n_k[k]<-n_k[k]+1;
			}
		}
	}
	nppv[which.max(n_k)];
}

#separ2
donn<-d1_40
X<-donn[,1:2];
z<-donn[,3]

# 3. Détermination de K plus proches voisins
donn.sep <- separ1(X, z);
Xapp <- donn.sep$Xapp;
zapp <- donn.sep$zapp;
Xtst <- donn.sep$Xtst;
ztst <- donn.sep$ztst;
kppv.tune(Xapp, zapp, Xapp, zapp,(2:10));

x11()
donn<-d1_100
plot(donn$V1, donn$V2, col=topo.colors(3)[donn[,3]])
dev.off()

# 4. Calcul du taux d'erreur :
donn<-d2_1000
X<-donn[,1:2];
z<-donn[,3]
ERR_tst<-rep(0,20)
ERR_app<-rep(0,20)
Kopt<-rep(0,20)

for(i in 1:20){
	donn.sep <- separ2(X, z);
	Xapp <- donn.sep$Xapp;
	zapp <- donn.sep$zapp;
	Xval <- donn.sep$Xval;
	zval <- donn.sep$zval;
	Xtst <- donn.sep$Xtst;
	ztst <- donn.sep$ztst;
	
	zpred<-rep(0,length(ztst));
	#Kopt[i]<- kppv.tune(Xapp, zapp, Xapp, zapp,2*(1:6)-1)
	Kopt[i]<- kppv.tune(Xapp, zapp, Xval, zval,(2:10));
	
	zpred<-kppv.val(Xapp,zapp,Kopt[i],Xapp);#Xtst avant
	ERR_app[i]<-validation(zapp,zpred);
	
	zpred<-kppv.val(Xapp,zapp,Kopt[i],Xtst);#Xtst avant
	ERR_tst[i]<-validation(ztst,zpred);
}

Kopt
mean(Kopt)
sd(Kopt)
interv_conf(ERR_app)
interv_conf(ERR_tst)









##### ******** PIMA *************

pima<-read.csv("Pima.csv")
class1<-pima[pima[,8]==1,]
class2<-pima[pima[,8]==2,]
donn<-pima#modify
X<-donn[,1:7];#modify
z<-donn[,8]#modify

### Classifieur euclidien

ERR_tst<-rep(0,20)
ERR_app<-rep(0,20)

for(i in 1:100){
  donn.sep <- separ1(X, z);
  Xapp <- donn.sep$Xapp;
  zapp <- donn.sep$zapp;
  Xtst <- donn.sep$Xtst;
  ztst <- donn.sep$ztst;
  
  mu <- ceuc.app(Xapp, zapp);
  zpred<-ceuc.val(mu, Xapp);
  ERR_app[i]<-validation(zapp,zpred);
  
  zpred<-ceuc.val(mu, Xtst);
  ERR_tst[i]<-validation(ztst,zpred);
}

interv_conf <- function(vector){
  interv <- c(0,0,0);
  mu <- mean(vector);
  s <- sd(vector);
  n <- length(vector);
  interv[1]<- mu
  interv[2]<-mu-1.96* s/sqrt(n)
  interv[3]<-mu+1.96* s/sqrt(n)
  
  interv
}# renvoie moyenne, borne inf, borne sup.

interv_conf(ERR_app)
interv_conf(ERR_tst)


### KPPV
donn<-pima
X<-donn[,1:7];#modify
z<-donn[,8]#modify
ERR_tst<-rep(0,20)
ERR_app<-rep(0,20)
Kopt<-rep(0,20)

for(i in 1:20){
  donn.sep <- separ2(X, z);
  Xapp <- donn.sep$Xapp;
  zapp <- donn.sep$zapp;
  Xval <- donn.sep$Xval;
  zval <- donn.sep$zval;
  Xtst <- donn.sep$Xtst;
  ztst <- donn.sep$ztst;
  
  zpred<-rep(0,length(ztst));
  #Kopt[i]<- kppv.tune(Xapp, zapp, Xapp, zapp,2*(1:6)-1)
  Kopt[i]<- kppv.tune(Xapp, zapp, Xval, zval,(2:10));
  
  zpred<-kppv.val(Xapp,zapp,Kopt[i],Xapp);#Xtst avant
  ERR_app[i]<-validation(zapp,zpred);
  
  zpred<-kppv.val(Xapp,zapp,Kopt[i],Xtst);#Xtst avant
  ERR_tst[i]<-validation(ztst,zpred);
}

Kopt
mean(Kopt)
sd(Kopt)
interv_conf(ERR_app)
interv_conf(ERR_tst)


##### Breastcancer

bcancer<-read.csv("Breastcancer.csv")
class1<-pima[pima[,10]==1,]
class2<-pima[pima[,10]==2,]
donn<-bcancer#modify
X<-donn[,1:9];#modify
z<-donn[,10]#modify

### Classifieur euclidien

ERR_tst<-rep(0,200)
ERR_app<-rep(0,200)

for(i in 1:200){
  donn.sep <- separ1(X, z);
  Xapp <- donn.sep$Xapp;
  zapp <- donn.sep$zapp;
  Xtst <- donn.sep$Xtst;
  ztst <- donn.sep$ztst;
  
  mu <- ceuc.app(Xapp, zapp);
  zpred<-ceuc.val(mu, Xapp);
  ERR_app[i]<-validation(zapp,zpred);
  
  zpred<-ceuc.val(mu, Xtst);
  ERR_tst[i]<-validation(ztst,zpred);
}

interv_conf <- function(vector){
  interv <- c(0,0,0);
  mu <- mean(vector);
  s <- sd(vector);
  n <- length(vector);
  interv[1]<- mu
  interv[2]<-mu-1.96* s/sqrt(n)
  interv[3]<-mu+1.96* s/sqrt(n)
  
  interv
}# renvoie moyenne, borne inf, borne sup.

interv_conf(ERR_app)
interv_conf(ERR_tst)


### KPPV

donn<-bcancer#modify
X<-donn[,1:9];#modify
z<-donn[,10]#modify

for(i in 1:20){
  donn.sep <- separ2(X, z);
  Xapp <- donn.sep$Xapp;
  zapp <- donn.sep$zapp;
  Xval <- donn.sep$Xval;
  zval <- donn.sep$zval;
  Xtst <- donn.sep$Xtst;
  ztst <- donn.sep$ztst;
  
  zpred<-rep(0,length(ztst));
  #Kopt[i]<- kppv.tune(Xapp, zapp, Xapp, zapp,2*(1:6)-1)
  Kopt[i]<- kppv.tune(Xapp, zapp, Xval, zval,(2:10));
  
  zpred<-kppv.val(Xapp,zapp,Kopt[i],Xapp);#Xtst avant
  ERR_app[i]<-validation(zapp,zpred);
  
  zpred<-kppv.val(Xapp,zapp,Kopt[i],Xtst);#Xtst avant
  ERR_tst[i]<-validation(ztst,zpred);
}

Kopt
mean(Kopt)
sd(Kopt)
interv_conf(ERR_app)
interv_conf(ERR_tst)






front.kppv(Xapp, zapp, Kopt, 1000)
