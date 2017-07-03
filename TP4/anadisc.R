library(MASS)


adq.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	
	param <- NULL
	param$MCov <- array(0, c(p,p,g))# g tableaux de matrices de covariance de dim : p x p : 
	                                # on a donc 1 tableau de matrice de covariance par classe
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	mean_vector<- rep(0,p)

	
	for (k in 1:g)# calculer paramètre pour chaque classe
	{
	 
		indk <- which(zapp==k)# index des individus de la classe k
		
		class_k<- as.matrix(0,nrow=length(indk), ncol=p)
		
		Xindiv_k <- Xapp[indk,]
		
		for(i in 1:p)# calculer moyenne de chaque var pour les individus de la classe k
		{
		  class_k <- Xapp[indk,];
		  mean_vector[i] <-mean(class_k[,i])
		  
		}

		mean_matrix_k<-matrix(mean_vector,ncol=p,nrow=length(indk),byrow=TRUE) # matrice p*nombre d'individu de la classe k
		                                                                      # réplication en ligne de mean_vector
		
		covar <- t(as.matrix(Xindiv_k-mean_matrix_k))%*%(as.matrix(Xindiv_k-mean_matrix_k))/length(indk)
		covar_corr <- covar*(length(indk)/(length(indk)-1))
		
		param$MCov[,,k] <- covar_corr
		param$mean[k,] <- mean_vector;
		param$prop[k] <- length(indk)/n;
	}

	param
}

adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	MCov <- array(0, c(p,p))
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	mean_vector<- rep(0,p)
	
	for (k in 1:g)
	{
		indk <- which(zapp==k)

		class_k<- as.matrix(0,nrow=length(indk), ncol=p)
		
		Xindiv_k <- Xapp[indk,]
		
		for(i in 1:p)# calculer moyenne de chaque var pour les individus de la classe k
		{
		  class_k <- Xapp[indk,];
		  mean_vector[i] <-mean(class_k[,i])
		  
		}
		
		mean_matrix_k<-matrix(mean_vector,ncol=p,nrow=length(indk),byrow=TRUE) # matrice p*nombre d'individu de la classe k
		
		V_k <- t(as.matrix(Xindiv_k-mean_matrix_k))%*%(as.matrix(Xindiv_k-mean_matrix_k))/length(indk)

		n_k <- length(indk)

		V_k_corr <- (n_k*V_k)/(n_k-1) # Variance de classe corrigée
		
		MCov <- MCov+ V_k_corr*(n_k-1)
		param$mean[k,] <- mean_vector
		param$prop[k] <- length(indk)/n
	}# Fin pour chaque classe
	
	MCov <- MCov/(n-g)
	
	  
	for (k in 1:g)
	{
		param$MCov[,,k] <- MCov
	};

	param
} #adl.app

nba.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	mean_vector<- rep(0,p)
	
	for (k in 1:g)
	{
		indk <- which(zapp==k)

		class_k<- as.matrix(0,nrow=length(indk), ncol=p)
		
		Xindiv_k <- Xapp[indk,]
		
		for(i in 1:p)# calculer moyenne de chaque var pour les individus de la classe k
		{
		  class_k <- Xapp[indk,];
		  mean_vector[i] <-mean(class_k[,i])
		  
		}
		
		mean_matrix_k<-matrix(mean_vector,ncol=p,nrow=length(indk),byrow=TRUE) # matrice p*nombre d'individu de la classe k
		# réplication en ligne de mean_vector
		
		covar <- t(as.matrix(Xindiv_k-mean_matrix_k))%*%(as.matrix(Xindiv_k-mean_matrix_k))/length(indk)
		
		
		
		param$MCov[,,k] <- diag(covar)* diag(1,p)
		param$mean[k,] <- mean_vector;
		param$prop[k] <- length(indk)/n;
	}

	param
}

ad.val <- function(param, Xtst) # P(w_k|x)=f_k(x)/f(x) * pi_k
{
	n <- dim(Xtst)[1]
	p <- dim(Xtst)[2]
	g <- length(param$prop)

	out <- NULL

	prob <- matrix(0, nrow=n, ncol=g)
	f_x <- matrix(0, nrow=n, ncol=1)
	f_k <- matrix(0, nrow=n, ncol=1)
	
	for (k in 1:g)# pour chaque classe, calculer f_k(x) puis construire f(x)=sum(pi_k *f_k)
	{
	  f_k <- mvdnorm(Xtst, param$mean[k,], param$MCov[,,k]) # f_k(x)
	  
	  f_x= f_x+param$prop[k]*f_k # f(x)= somme (pi_k* f_k(x))
	  
	  prob[,k] <- f_k
		
		
	};
	
	for (k in 1:g) # calcule la probabilité a posteriori : f_k/f_x
	{
	  
	  prob[,k] <- (prob[,k]*param$prop[k]) /f_x
	  
	};
	
	
	#prob <- 
	pred <- max.col(prob)

	out$prob <- prob
	out$pred <- pred

	out
}
