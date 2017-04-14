setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP1")
#setwd("/media/leminhtr/Data/School/UTC/A16-P17/P17/SY09/TD/TP1")


library(MASS)
data(crabs)
?crabs

crabsquant <- crabs[,4:8]
crabsquant

summary(crabs)
head(crabsquant)
head(crabs)


sample(crabsquant)

n_a <- is.na(crabs)
n_a
notes[!complete.cases(crabs),]
length(crasb[!complete.cases(crabs),] )
sapply(crabs, function(x) sum(is.na(x)))


boxplot(crabsquant)


t_matrix<-matrix(ncol=ncol(crabsquant),nrow=1)

#rownames(t_matrix)<-levels(unique(notes$correcteur.median))

t_matrix<-matrix(rep(0,5),nrow=5)
rownames(t_matrix)<-c("FL","RW","CL","CW","BD")
colnames(t_matrix)<-c("P-value")

t_matrix[1,1]<-t.test(crabs$FL[crabs$sp=="O"],crabs$FL[crabs$sp=="B"])$p.value
t_matrix[2,1]<-t.test(crabs$RW[crabs$sp=="O"],crabs$RW[crabs$sp=="B"])$p.value
t_matrix[3,1]<-t.test(crabs$CL[crabs$sp=="O"],crabs$CL[crabs$sp=="B"])$p.value
t_matrix[4,1]<-t.test(crabs$CW[crabs$sp=="O"],crabs$CW[crabs$sp=="B"])$p.value
t_matrix[5,1]<-t.test(crabs$BD[crabs$sp=="O"],crabs$BD[crabs$sp=="B"])$p.value

t_matrix_logic<-matrix(rep(0,5),nrow=5)
rownames(t_matrix_logic)<-c("FL","RW","CL","CW","BD")
colnames(t_matrix_logic)<-c("Dependence")
for(i in 1:5){
	if( t_matrix[i,1]<0.05)
		t_matrix_logic[i,1]="Dependent"
		
	else
		t_matrix_logic[i,1]="Independent"
}

##################
t_matrix<-matrix(rep(0,5),nrow=5)
rownames(t_matrix)<-c("FL","RW","CL","CW","BD")
colnames(t_matrix)<-c("P valeur du student test")

t_matrix[1,1]<-t.test(crabs$FL[crabs$sex=="M"],crabs$FL[crabs$sex=="F"])$p.value
t_matrix[2,1]<-t.test(crabs$RW[crabs$sex=="M"],crabs$RW[crabs$sex=="F"])$p.value
t_matrix[3,1]<-t.test(crabs$CL[crabs$sex=="M"],crabs$CL[crabs$sex=="F"])$p.value
t_matrix[4,1]<-t.test(crabs$CW[crabs$sex=="M"],crabs$CW[crabs$sex=="F"])$p.value
t_matrix[5,1]<-t.test(crabs$BD[crabs$sex=="M"],crabs$BD[crabs$sex=="F"])$p.value

t_matrix_logic<-matrix(rep(0,5),nrow=5)
rownames(t_matrix_logic)<-c("FL","RW","CL","CW","BD")
colnames(t_matrix_logic)<-c("Dependence")
for(i in 1:5){
	if( t_matrix[i,1]<0.05)
		t_matrix_logic[i,1]="Dependent"
		
	else
		t_matrix_logic[i,1]="Independent"
}




#2.

cor_matrix <- matrix(ncol=ncol(crabsquant), nrow=ncol(crabsquant))
colnames(cor_matrix)<-colnames(crabsquant)
rownames(cor_matrix)<-colnames(crabsquant)
for (i in 1:nrow(cor_matrix)){
	for(j in 1:ncol(cor_matrix)){
		t_tmp<-cor.test(crabsquant[,colnames(crabsquant[i])],crabsquant[,colnames(crabsquant[j])])
		cor_matrix[i,j]<-t_tmp$estimate;
	}
}



n_a <- is.na(crabs)
n_a
crabs[!complete.cases(crabs),]
nrow(notes[!complete.cases(crabs),] )
sapply(crabs, function(x) sum(is.na(x)))



par(mfrow=c(2,3))
boxplot(FL~sp+sex,data=crabs,xlab="sp et sex",ylab="FL",col=c("red3","mediumblue","red","dodgerblue"))
boxplot(RW~sp+sex,data=crabs,xlab="sp et sex",ylab="RW",col=c("red3","blue","red","dodgerblue"))
boxplot(CL~sp+sex,data=crabs,xlab="sp et sex",ylab="CL",col=c("red3","blue","red","dodgerblue"))
boxplot(CW~sp+sex,data=crabs,xlab="sp et sex",ylab="CW",col=c("red3","blue","red","dodgerblue"))
boxplot(BD~sp+sex,data=crabs,xlab="sp et sex",ylab="BD",col=c("red3","blue","red","dodgerblue"))


boxplot(sp~FL,data=crabs,xlab="sp et sex",ylab="FL",col=c("red3","mediumblue","red","dodgerblue","green"))
boxplot(RW~sp,data=crabs,xlab="sp et sex",ylab="RW",col=c("red3","blue","red","dodgerblue"))

crabsnew<-crabs
crabsnew$sp_n<-'blue'
crabsnew$sex_n<-1
crabsnew[crabsnew$sp=='O',9]<-'red'
crabsnew[crabsnew$sex=='M',10]<-2

plot(crabsquant,col=crabsnew$sp_n, pch=crabsnew$sex_n)
legend("topright",pch=c(1,2),lty=c(1,2))

plot(crabsquant,col=crabsnew$sp_n, pch=crabsnew$sex_n)

plot(crabsquant)

pairs(crabsquant,col=c('blue','red')[crabs$sp],main="Caracteristiques des especes de crabe B (bleu) et O (rouge)")

par(mfrow=c(1,5))
plot(crabsquant$FL, crabsquant$CL, col=c("blue","orange")[crabs$sp])
plot(crabsquant$FL, crabsquant$CW, col=c("blue","orange")[crabs$sp])
plot(crabsquant$CL, crabsquant$BD, col=c("blue","orange")[crabs$sp])
plot(crabsquant$CW, crabsquant$BD, col=c("blue","orange")[crabs$sp])

x11()

plot(crabsquant$RW, col=c("red","blue")[crabs$sex])
plot(crabsquant$FL, crabsquant$RW, col=c("red","blue")[crabs$sex])
plot(crabsquant$CL, crabsquant$RW, col=c("red","blue")[crabs$sex])
plot(crabsquant$BD, crabsquant$RW, col=c("red","blue")[crabs$sex])
plot(crabsquant$CW, crabsquant$RW, col=c("red","blue")[crabs$sex])


x11()
par(mfrow=c(1,2));
pairs(crabsquant,col=c('blue','orange')[crabs$sp],pch=1)
pairs(crabsquant,col=c('red','blue')[crabs$sex],pch=1)


