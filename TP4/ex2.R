setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP4")


source("anadisc.R")
source("logistic.R")
source("mvdnorm.R")
source("prob.ad.R")
source("prob.log.R")
source("prob.log2.R")

pima<-read.csv("Pima.csv")
bcw<-read.csv("bcw.csv")
spam<-read.csv("spam.csv")
d1_1000<-read.csv("Synth1-1000.csv")
d2_1000<-read.csv("Synth2-1000.csv")
d3_1000<-read.csv("Synth3-1000.csv")


library(tree)




data <- d1_1000

X <- data[,1:2]
z <- data[,3]

#treeMod <- tree(response ~ ., data = inputData)  # model the tree, including all the variables



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

validation_NA<-function(tst,pred){
  NAs <- is.na(pred)
  pred <- pred[!NAs]
  tst <- tst[!NAs]
  
  n<-length(tst);
  err<-0;
  for(i in 1:n){
    if(tst[i]!=pred[i]){
      err<-err+1;
    }
  }
  err/n;
}


separ1 <- function(X, z)
{
  g <- max(z)
  
  Xapp <- NULL
  zapp <- NULL
  Xtst <- NULL
  ztst <- NULL
  
  for (k in 1:g)
  {
    indk <- which(z==k)
    ntot <- length(indk)
    napp <- round(ntot*2/3)
    ntst <- ntot-napp
    
    itot <- sample(indk)
    iapp <- itot[1:napp]
    itst <- itot[(napp+1):ntot]
    
    Xapp <- rbind(Xapp, X[iapp,])
    zapp <- c(zapp, z[iapp])
    Xtst <- rbind(Xtst, X[itst,])
    ztst <- c(ztst, z[itst])
  }
  
  res <- NULL
  res$Xapp <- Xapp
  res$zapp <- zapp
  res$Xtst <- Xtst
  res$ztst <- ztst
  
  res
}

interv_conf <- function(vector){
  interv <- c(0,0,0);
  mu <- mean(vector);
  s <- sd(vector);
  n <- length(vector);
  interv[1]<- mu
  interv[2]<-mu-1.96* (s/sqrt(n))
  interv[3]<-mu+1.96* (s/sqrt(n))
  
  interv
}# renvoie moyenne, borne inf, borne sup.


erreur<-rep(0,20)
#d<-read.csv("~/sy09/tp4/donnees/Synth1-1000.csv")############

library(tree)
tree.fit<-tree(z~. ,data)
summary(tree.fit)
plot(tree.fit);text(tree.fit)
cv.tree(tree.fit)
tree.fit.pruned<-prune.tree(tree.fit, best=5)
plot(tree.fit.pruned);text(tree.fit.pruned)



data <- pima
X <- pima[,1:ncol(pima)-1]
z <- pima[,ncol(pima)]

Terrpim_tree<-function(X,z, N_rep)
{
  result_ERR <- c()
  result_ERR$app <- rep(0,N_rep)
  result_ERR$tst <- rep(0,N_rep)
  
  result_ERR$app_interv <- rep(0,3)
  result_ERR$tst_interv <- rep(0,3)
  
  result_ERR$app_interv_round <- rep(0,3)
  result_ERR$tst_interv_round <- rep(0,3)
  
  i=1
  m = matrix(nrow=N_rep, ncol=1)
  while(i<=N_rep){
    donn.sep <- separ1(X, z)
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    zapp <- as.factor(zapp)
    formula <- zapp ~ npreg + glu + bp + skin + bmi + ped + age
    full_tree = tree(formula, data=Xapp, control=tree.control(nobs=nrow(Xapp), mindev = 0.0001))
    best_k = cv.tree(full_tree)$size[which.min(cv.tree(full_tree)$dev)]
    pruned_tree = prune.misclass(full_tree, best=best_k)
    
    
    prediction_app = predict(pruned_tree, Xapp)
    zpred = max.col(prediction_app)
    result_ERR$app[i]= 1 - (sum(zpred==zapp)/length(zapp))
    
    prediction_tst = predict(pruned_tree, Xtst)
    zpred = max.col(prediction_tst)
    result_ERR$tst[i] <- 1 - (sum(zpred==ztst)/length(ztst))
    i<-i+1
  }
  
  
  result_ERR$app_interv <- interv_conf(result_ERR$app)
  result_ERR$tst_interv <- interv_conf(result_ERR$tst)
  
  # Renvoie les intervalles, arrondi à 10^-4
  result_ERR$app_interv_round <- signif(result_ERR$app_interv,4) 
  result_ERR$tst_interv_round <- signif(result_ERR$tst_interv,4)
  
  return(result_ERR)
}

N_err <- Terrpim_tree(X,z)

interv_conf(N_err)
validation_NA<-function(tst,pred){
  NAs <- is.na(pred)
  pred <- pred[!NAs]
  tst <- tst[!NAs]
  
  n<-length(tst);
  err<-0;
  for(i in 1:n){
    if(tst[i]!=pred[i]){
      err<-err+1;
    }
  }
  err/n;
}


TerrBCW_tree<-function(X,z)
{
  i=1
  m = matrix(nrow=100, ncol=1)
  while(i<=100){
    donn.sep <- separ1(X, z)
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    zapp <- as.factor(zapp)
    formula <- zapp ~ V2+V3+V4+V5+V6+V7+V8+V9+V10
    full_tree = tree(formula, data=Xapp, control=tree.control(nobs=nrow(Xapp), mindev = 0.0001))
    best_k = cv.tree(full_tree)$size[which.min(cv.tree(full_tree)$dev)]
    pruned_tree = prune.misclass(full_tree, best=best_k)
    prediction = predict(pruned_tree, Xtst)
    zpred = max.col(prediction)
    m[i,1] <- 1 - (sum(zpred==ztst)/length(ztst))
    i<-i+1
  }
  m
}



data <- bcw
X <- bcw[,1:ncol(bcw)-1]
z <- bcw[,ncol(bcw)]


TerrBCW_tree<-function(X,z, N_rep)
{
  result_ERR <- c()
  result_ERR$app <- rep(0,N_rep)
  result_ERR$tst <- rep(0,N_rep)
  
  result_ERR$app_interv <- rep(0,3)
  result_ERR$tst_interv <- rep(0,3)
  
  result_ERR$app_interv_round <- rep(0,3)
  result_ERR$tst_interv_round <- rep(0,3)
  
  i=1
  m = matrix(nrow=N_rep, ncol=1)
  while(i<=N_rep){
    donn.sep <- separ1(X, z)
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    zapp <- as.factor(zapp)
    formula <- zapp ~ V2+V3+V4+V5+V6+V7+V8+V9+V10
    full_tree = tree(formula, data=Xapp, control=tree.control(nobs=nrow(Xapp), mindev = 0.0001))
    best_k = cv.tree(full_tree)$size[which.min(cv.tree(full_tree)$dev)]
    pruned_tree = prune.misclass(full_tree, best=best_k)
    
    
    prediction_app = predict(pruned_tree, Xapp)
    zpred = max.col(prediction_app)
    result_ERR$app[i]= 1 - (sum(zpred==zapp)/length(zapp))
    
    prediction_tst = predict(pruned_tree, Xtst)
    zpred = max.col(prediction_tst)
    result_ERR$tst[i] <- 1 - (sum(zpred==ztst)/length(ztst))
    i<-i+1
  }
  
  
  result_ERR$app_interv <- interv_conf(result_ERR$app)
  result_ERR$tst_interv <- interv_conf(result_ERR$tst)
  
  # Renvoie les intervalles, arrondi à 10^-4
  result_ERR$app_interv_round <- signif(result_ERR$app_interv,4) 
  result_ERR$tst_interv_round <- signif(result_ERR$tst_interv,4)
  
  return(result_ERR)
}
