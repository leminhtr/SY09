log.app <- function(Xapp, zapp, intr, epsi)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]

	Xapp <- as.matrix(Xapp)

	if (intr == T)
	{
		Xapp <- cbind(rep(1,n),Xapp)
		p <- p + 1
	}

	targ <- matrix(as.numeric(zapp),nrow=n)
	targ[which(targ==2),] <- 0
	tXap <- t(Xapp)

	beta <- matrix(0,nrow=p,ncol=1)

	conv <- F
	iter <- 0
	while (conv == F)
	{
		iter <- iter + 1
		bold <- beta

		prob <- postprob(beta, Xapp)
		MatW <- diag(prob,n)%*%diag((1-prob),n)

		det(tXap%*%MatW%*%Xapp);
		beta <- bold+solve(tXap%*%MatW%*%Xapp)%*%tXap%*%(targ-prob)

		if (norm(beta-bold)<epsi)
		{
			conv <- T
		}
	}

	prob <- postprob(beta, Xapp)
	out <- NULL
	out$beta <- beta
	out$iter <- iter
	out$logL <- sum(targ*log(prob)+(1-targ)*log(1-prob))

	out
}

log.val <- function(beta, Xtst)
{
	m <- dim(Xtst)[1]
	p <- dim(beta)[1]
	pX <- dim(Xtst)[2]

	Xtst <- as.matrix(Xtst)

	if (pX == (p-1))
	{
		Xtst  <- cbind(rep(1,m),Xtst)
	}

	prob <- postprob(beta,Xtst)
	pred <- max.col(cbind(prob,1-prob))

	out <- NULL
	out$prob <- prob
	out$pred <- pred

	return(out)
}

postprob <- function(beta, X)
{
	X <- as.matrix(X)
	n<-nrow(X)
	beta<-as.matrix(beta)
	Mbeta<-matrix(rep(t(beta),n),nrow=n,byrow=T)
	tmp<-exp(rowSums(Mbeta*X))
	prob <- tmp/(1+tmp)
	prob
}


quadratique<-function(X){
	X<-as.matrix(X)
	i=1;
	j=1;
	n<-dim(X)[1]
	p<-dim(X)[2]
	c<-p+1
	Xnew<-matrix(0,ncol=p*p/2+3*p/2,nrow=n)
	Xnew[,1:p]<-X
	for(i in 1:p){
		for(j in i:p){
			Xnew[,c]<-X[,i]*X[,j]
			c<-c+1			
		}
	}
	Xnew
}
