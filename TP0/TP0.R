A<-matrix(1:9,nrow=3,byrow=T)
B<-matrix(c(5,3,7,4,6,3,1,6,3,2,8,5),nrow=4,byrow=T)
A
B
dim(A)
na<-dim(A)[1]
pa<-dim(A)[2]

v<-1:3
v
C<-cbind(A,v)
D<-rbind(A,B)
D

D[3,3]
diag(3)
diag(v)
A
diag(A)

B
A
B%*%A
A%*%A
A*A

# %*% : multiplication matrice
# * multiplication éléments

E <- array(1,c(4,3,1))
E

array(1,c(4,3,2))

library(abind)


abind(A,B, along=3)

F1 <- matrix(1,4,3) # <=> array(1,c(4,3,1))
F2 <- matrix(2,4,3)
F <- abind(F1, F2, along=3)
F
matrix(1,1,3)































