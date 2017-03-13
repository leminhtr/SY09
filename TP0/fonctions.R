somme <- function(a, b)
{
resultat <- a + b;
}
produit <- function(a, b)
{
resultat <- a * b;
}


prodtrans <- function(x){
	T <- t(x) %*% x
	return(T)
}



#http://gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/

centre <- function(x) {
    n = nrow(x)
    I = rep(1, n)
    C = diag(n) - (1/n) * (I %*% t(I))
    res <- C %*% x
	return(res)
}

center_colmeans <- function(x) {
    xcenter = colMeans(x)
    x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}














