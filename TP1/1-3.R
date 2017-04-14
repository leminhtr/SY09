setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP1")
#setwd("/media/leminhtr/Data/School/UTC/A16-P17/P17/SY09/TD/TP1")

pima <- read.csv("Pima.csv", header=T)
pima$z <- factor(pima$z)

head(pima)
head(pima$z)

plot(pima)

plot(pima_quant)

pima_quant <- pima[,1:ncol(pima)-1]

### Plot
plot(pima_quant, col=rainbow(7))
plot(pima_quant, col=c("green","red")[pima$z])

par(mfrow=c(2,5))

##### Boxplot
boxplot(npreg~z,data=pima,xlab="categorie",ylab="npreg")
boxplot(glu~z,data=pima,xlab="categorie",ylab="blu")
boxplot(bp~z,data=pima,xlab="categorie",ylab="bp")
boxplot(skin~z,data=pima,xlab="categorie",ylab="skin")
boxplot(bmi~z,data=pima,xlab="categorie",ylab="bmi")
boxplot(ped~z,data=pima,xlab="categorie",ylab="ped")
boxplot(age~z,data=pima,xlab="categorie",ylab="age")


### Correlation matrix
cor_matrix <- matrix(ncol=ncol(pima_quant), nrow=ncol(pima_quant))
colnames(cor_matrix)<-colnames(pima_quant)
rownames(cor_matrix)<-colnames(pima_quant)
for (i in 1:nrow(cor_matrix)){
	for(j in 1:ncol(cor_matrix)){
		t_tmp<-cor.test(pima_quant[,colnames(pima_quant[i])],pima_quant[,colnames(pima_quant[j])])
		cor_matrix[i,j]<-t_tmp$estimate;
	}
}

cor_matrix

#export matrix LaTeX
library(gdata)
t_matrix_low <- cor_matrix
upperTriangle(t_matrix_low) <- 0
colnames(t_matrix_low)<-colnames(cor_matrix)
rownames(t_matrix_low)<-colnames(cor_matrix)
write.table(t_matrix_low, "q3_cor_mat_.txt", quote=FALSE, eol="\\\\\n", sep=" & ")


cor_matrix_bool<- matrix(ncol=ncol(pima_quant), nrow=ncol(pima_quant))
colnames(cor_matrix_bool)<-colnames(pima_quant)
rownames(cor_matrix_bool)<-colnames(pima_quant)
for (i in 1:nrow(cor_matrix_bool))
{
	for(j in 1:ncol(cor_matrix_bool)){
		if(cor_matrix[i,j]<0.90)
			cor_matrix_bool[j,i] <- 0
		else
			cor_matrix_bool[j,i] <- 1
	}
}


####### Student test
t_matrix<-matrix(rep(0,7),nrow=7)
rownames(t_matrix)<-c("npreg","glu","bp","skin","bmi","ped","age")
colnames(t_matrix)<-c("P valeurr du student test")
z_factor<-factor(c(1,2))

t_matrix[1,1]<-t.test(pima$npreg[pima$z==z_factor[1]],pima$npreg[pima$z==z_factor[2]])$p.value
t_matrix[2,1]<-t.test(pima$glu[pima$z==z_factor[1]],pima$glu[pima$z==z_factor[2]])$p.value
t_matrix[3,1]<-t.test(pima$bp[pima$z==z_factor[1]],pima$bp[pima$z==z_factor[2]])$p.value
t_matrix[4,1]<-t.test(pima$skin[pima$z==z_factor[1]],pima$skin[pima$z==z_factor[2]])$p.value
t_matrix[5,1]<-t.test(pima$bmi[pima$z==z_factor[1]],pima$bmi[pima$z==z_factor[2]])$p.value
t_matrix[6,1]<-t.test(pima$ped[pima$z==z_factor[1]],pima$ped[pima$z==z_factor[2]])$p.value
t_matrix[7,1]<-t.test(pima$age[pima$z==z_factor[1]],pima$age[pima$z==z_factor[2]])$p.value


t_matrix_logic<-matrix(rep(0,7),nrow=7)
rownames(t_matrix_logic)<-c("npreg","glu","bp","skin","bmi","ped","age")
colnames(t_matrix_logic)<-c("Dependence")
for(i in 1:7){
	if( t_matrix[i,1]<0.05)
		t_matrix_logic[i,1]="Dependent"
		
	else
		t_matrix_logic[i,1]="Independent"
}
