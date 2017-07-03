setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP4")


source("anadisc.R")
source("logistic.R")
source("mvdnorm.R")
source("prob.ad.R")
source("prob.log.R")
source("prob.log2.R")

library("factoextra")

pima<-read.csv("Pima.csv")
bcw<-read.csv("bcw.csv")
spam<-read.csv("spam.csv")
d1_1000<-read.csv("Synth1-1000.csv")
d2_1000<-read.csv("Synth2-1000.csv")
d3_1000<-read.csv("Synth3-1000.csv")

library(Hmisc)# describe()


library(tree)


data <- d1_1000

X <- data[,1:2]
z <- data[,3]

treeMod <- tree(response ~ ., data = inputData)  # model the tree, including all the variables



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





Terrpim_tree<-function(X,z)
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
    formula <- zapp ~ npreg + glu + bp + skin + bmi + ped + age
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



interv_conf(N_err)

spam_quant <- spam[,2:58]
spam_quant <- scale(spam_quant)
res.pca <- princomp(spam_quant)


acp.score <- res.acp.spam$scores
plot(res.acp.spam)
x11()
biplot(res.acp.spam, c(1,2))

var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
}

var <- get_pca_var(res.pca)
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
head(var.coord[, 1:4])


eig.val <- get_eigenvalue(res.pca)
head(eig.val)

barplot(eig.val$variance.percent, names.arg=names(res.pca$scale), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
plot(eig.val$cumulative.variance.percent)
barplot(eig.val$cumulative.variance.percent, names.arg=names(res.pca$scale), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
#plot(eig.val$cumulative.variance.percent)



fviz_pca_var(res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=50) + theme_minimal()

fviz_pca_var(res.pca, col.var="contrib")

data <- spam
z <- data[,59]
X <- data[,2:58]

donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst


param <- adq.app(X,z)

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

    NAs <- is.na(out$pred)
    pred <- out$pred[!NAs]
    zp <- zapp[!NAs]
    
    result_ERR$app[i]<-validation(pred, zp)
    
    # apply prediction for test data
    out <- ad.val(param, Xtst)
    
    NAs <- is.na(out$pred)
    pred <- out$pred[!NAs]
    zt <- ztst[!NAs]
    
    result_ERR$tst[i]<-validation(pred, zt)
    
  #  result_ERR$tst[i]<-validation_NA(out$pred, ztst)
    
  }
  
  result_ERR$app_interv <- interv_conf(result_ERR$app)
  result_ERR$tst_interv <- interv_conf(result_ERR$tst)
  
  # Renvoie les intervalles, arrondi à 10^-4
  result_ERR$app_interv_round <- signif(result_ERR$app_interv,4) 
  result_ERR$tst_interv_round <- signif(result_ERR$tst_interv,4)
  
  return(result_ERR)
}

run_test(spam,adq.app,1)



