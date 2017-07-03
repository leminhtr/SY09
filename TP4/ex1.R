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


# Effectue "N_rep" test de prédictions sur un "dataset" avec la méthode "func" donné en paramètre
run_test <- function(dataset, func, N_rep){
  
  p <- ncol(dataset)-1
  z <- dataset[,p+1]
  X <- dataset[,1:p]
  
  #apply function
  
  
  # Renvoie un data frame avec l'erreur sur apprentissage et test
  result_ERR <- c()
  result_ERR$app <- rep(0,N_rep)
  result_ERR$tst <- rep(0,N_rep)
  
  result_ERR$app_interv <- rep(0,3)
  result_ERR$tst_interv <- rep(0,3)
  
  result_ERR$app_interv_round <- rep(0,3)
  result_ERR$tst_interv_round <- rep(0,3)
  
  
  for(i in 1:N_rep){
    
    # split data
    donn.sep <- separ1(X, z)
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    # find parameters for app data
    param <- func(Xapp,zapp)
    
    # apply prediction for app data
    out <- ad.val(param, Xapp)
    result_ERR$app[i]<-validation(out$pred, zapp)
    
    # apply prediction for test data
    out <- ad.val(param, Xtst)
    result_ERR$tst[i]<-validation(out$pred, ztst)
    
    }
  
    result_ERR$app_interv <- interv_conf(result_ERR$app)
    result_ERR$tst_interv <- interv_conf(result_ERR$tst)
  
    # Renvoie les intervalles, arrondi à 10^-4
    result_ERR$app_interv_round <- signif(result_ERR$app_interv,4) 
    result_ERR$tst_interv_round <- signif(result_ERR$tst_interv,4)
    
    return(result_ERR)
}

N=10
run_test(pima,adq.app,N)
run_test(pima,adl.app,N)
run_test(pima,nba.app,N)

run_test(bcw,adq.app,N)
run_test(bcw,adl.app,N)
run_test(bcw,nba.app,N)

data <- scale(spam[,2:59], scale=FALSE)
m <- run_test(data,nba.app,1)




run_test_logit <- function(dataset, func, N_rep,orig){
  
  p <- ncol(dataset)-1
  z <- dataset[,p+1]
  X <- dataset[,1:p]
  
  #apply function
  
  
  # Renvoie un data frame avec l'erreur sur apprentissage et test
  result_ERR <- c()
  result_ERR$app <- rep(0,N_rep)
  result_ERR$tst <- rep(0,N_rep)
  
  result_ERR$app_interv <- rep(0,3)
  result_ERR$tst_interv <- rep(0,3)
  
  result_ERR$app_interv_round <- rep(0,3)
  result_ERR$tst_interv_round <- rep(0,3)
  
  
  for(i in 1:N_rep){
    
    # split data
    donn.sep <- separ1(X, z)
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    # find parameters for app data
    param <- func(Xapp,zapp,orig,0.1^(-6))## Xapp, zapp, intr, epsi
    
    # apply prediction for app data
    out <- log.val(param$beta, Xapp)
    result_ERR$app[i]<-validation(out$pred, zapp)
    
    # apply prediction for test data
    out <- log.val(param$beta, Xtst)
    result_ERR$tst[i]<-validation(out$pred, ztst)
    
  }
  
  result_ERR$app_interv <- interv_conf(result_ERR$app)
  result_ERR$tst_interv <- interv_conf(result_ERR$tst)
  
  # Renvoie les intervalles, arrondi à 10^-4
  result_ERR$app_interv_round <- signif(result_ERR$app_interv,4) 
  result_ERR$tst_interv_round <- signif(result_ERR$tst_interv,4)
  
  return(result_ERR)
}




run_test_logit_quadra <- function(dataset, func, N_rep,orig){
  
  p <- ncol(dataset)-1
  z <- dataset[,p+1]
  X <- dataset[,1:p]
  
  #apply function
  
  
  # Renvoie un data frame avec l'erreur sur apprentissage et test
  result_ERR <- c()
  result_ERR$app <- rep(0,N_rep)
  result_ERR$tst <- rep(0,N_rep)
  
  result_ERR$app_interv <- rep(0,3)
  result_ERR$tst_interv <- rep(0,3)
  
  result_ERR$app_interv_round <- rep(0,3)
  result_ERR$tst_interv_round <- rep(0,3)
  
  
  for(i in 1:N_rep){
    
    # split data
    donn.sep <- separ1(X, z)
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst

    
    # find parameters for app data
    Xapp <- quadratique(Xapp)
    param <- log.app(Xapp,zapp,orig,0.1^(-6))## Xapp, zapp, intr, epsi
    
    # apply prediction for app data
    out <- log.val(param$beta, Xapp)
    result_ERR$app[i]<-validation(out$pred, zapp)
    
    # apply prediction for test data
    Xtst <- quadratique(Xtst)
    out <- log.val(param$beta, Xtst)
    result_ERR$tst[i]<-validation(out$pred, ztst)
    
  }
  
  result_ERR$app_interv <- interv_conf(result_ERR$app)
  result_ERR$tst_interv <- interv_conf(result_ERR$tst)
  
  # Renvoie les intervalles, arrondi à 10^-4
  result_ERR$app_interv_round <- signif(result_ERR$app_interv,4) 
  result_ERR$tst_interv_round <- signif(result_ERR$tst_interv,4)
  
  return(result_ERR)
}



spam_sc <- scale(spam)
run_test(spam_sc[,2:ncol(spam_sc)],nba.app,1)
