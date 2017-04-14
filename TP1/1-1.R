setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP1")
#setwd("/media/leminhtr/Data/School/UTC/A16-P17/P17/SY09/TD/TP1")

## Lib.
library("RColorBrewer")
library(Hmisc)

#1.

###1.1

###### 1.

#nature : nom etu; specialite/niveau,
notes <- read.csv("sy02-p2016.csv", na.strings="", header=T)
 notes$nom <- factor(notes$nom, levels=notes$nom)
 notes$niveau <- factor(notes$niveau, ordered=T)
 notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),
ordered=T)


# explore data
head(notes)
summary(notes)
plot(notes)
n_a <- is.na(notes)
n_a
notes[!complete.cases(notes),]
nrow(notes[!complete.cases(notes),] )
sapply(notes, function(x) sum(is.na(x)))

unique(notes$dernier.diplome.obtenu)
unique(notes$correcteur.median)
#notes sans N/A
notes_clean <- na.omit(notes)
summary(notes_clean)

############# GRAPH

plot(notes$specialite, notes$resultat)
#histogramme : quantitatives
#barplot : qualitatives
#boxplot : quantitative en fonction de qualitative

sd(notes_clean$note.median)
sd(notes_clean$note.total)
sd(notes_clean$note.final)


#### Boxplot :  Qualitative en fonction de quantitative (note)
notes2<- notes
notes2$spec_niv <- with(notes,paste0(specialite,niveau)) # merge colonne specialite & niveau into one
notes2 <- merge(notes,notes2)

par(mfrow=c(1,4));
boxplot(note.totale ~ specialite , data=notes,xlab="specialite",ylab="note totale")
boxplot(note.totale ~ niveau , data=notes,xlab="niveau",ylab="note totale")
boxplot(note.totale~ spec_niv,col=c("cyan","cyan","cyan","cyan","yellow","yellow","yellow","yellow","yellow","brown","brown","brown","brown","brown","orange","orange","orange","orange","brown4","brown4","brown4","brown4","green","green","green","green","white", "grey", "plum","plum"),data=notes2,xlab="specialite & niveau",ylab="note totale")
legend(bty="n",inset=0.0009,xy.coords(x=29.5,y=8), title="spec", c("GB","GI","GM", "GP","GSM","GSU","ISS2","TC"), fill=c("cyan","yellow","brown","orange","brown4","green","grey","plum"), horiz=FALSE)
boxplot(note.totale~ statut, data=notes,xlab="statut",ylab="note totale")
boxplot(note.totale ~ dernier.diplome.obtenu, data=notes,xlab="origine",ylab="note totale")
boxplot(note.median~ correcteur.median, data=notes,xlab="correcteur median",ylab="note median")
boxplot(note.final~ correcteur.final, data=notes,xlab="correcteur final",ylab="note final")
boxplot(note.totale~ correcteur.final, data=notes,xlab="correcteur final",ylab="note totale")

boxplot(note.totale~ dernier.diplome.obtenu,col=topo.colors(12),data=notes,xlab="dernier diplome obtenu",ylab="note totale")
legend(bty="n",inset=0.01,xy.coords(x=10.2,y=8.07), title="diplome", c( "AUTRE 1ER CYCLE","AUTRE 2E CYCLE", "AUTRE DIPLOME SUP.", "BAC","BTS","CPGE","DEUG","DUT","ETRANGER SECOND.","ETRANGER SUP.","INGENIEUR","LICENCE" ), fill=topo.colors(12), horiz=FALSE, cex=0.7)

factor(notes2$spec_niv)


########
table_spec_res<-table(notes$specialite,notes$resultat)

barplot(table_spec_res,beside=T,legend=rownames(table_spec_res),col=brewer.pal(9, "Set3"),args.legend=list(title="sp?cialit?"),xlab="Resultat",ylab="Nombre d'?tudiant")
	

par(mfrow=c(2,3));
boxplot(note.totale ~ specialite , data=notes,xlab="specialite",ylab="note")
boxplot(note.totale ~ niveau , data=notes,xlab="niveau",ylab="note")
boxplot(note.totale ~ dernier.diplome.obtenu, data=notes,xlab="origine",ylab="note")
boxplot(note.median~ correcteur.median, data=notes,xlab="correcteur median",ylab="note median")
boxplot(note.final~ correcteur.final, data=notes,xlab="correcteur final",ylab="note final")
########
barplot(table_spec_res,beside=T,legend=rownames(table_spec_res),col=1:9,
	args.legend=list(title="spécialité"),xlab="Resultat",ylab="Nombre d'étudiants")
	



## Correlation plot (notes)
plot(notes$note.median,notes$note.final,xlab="Note médian",ylab="Note final")
note_clean<-na.omit(notes)
cor(note_clean$note.median,note_clean$note.final)

plot(note_clean$note.median,note_clean$note.totale,xlab="Note médian",ylab="Note totale")
cor(note_clean$note.median,note_clean$note.totale)

plot(note_clean$note.final,note_clean$note.totale,xlab="Note final",ylab="Note totale")
cor(note_clean$note.final,note_clean$note.totale)



#2. : filiere, origine, niveau, correcteur, ...
### Rappel : 
#cor.test : test si correlation a une valeur significative
#test correlation 2 quantitative : cor.test
#test correlation 2 qualitative : chi2
#test correlation qualitative/quantitative : variance/test student si 2 �chantillons

head(notes)
length(notes$median)

################### TEST CHI2 : TEST IND. ENTRE VARIABLES QUALITATIVES

table_spec_res<-table(notes$specialite,notes$resultat)

chisq.test(table_spec_res)
table_diplome_res<-table(notes$dernier.diplome.obtenu,notes$resultat)
chisq.test(table_diplome_res)
table_niveau_res<-table(notes$niveau,notes$resultat)
chisq.test(table_niveau_res)

tab_corr_med_res <- table(notes$correcteur.median, notes$resultat)
tab_corr_med_res
tab_corr_fin_res <- table(notes$correcteur.final, notes$resultat)
tab_corr_fin_res
tab_dipl_res <- table(notes$dernier.diplome.obtenu, notes$resultat)
tab_dipl_res
tab_spec_res <- table(notes$specialite, notes$resultat)
tab_spec_res
tab_niv_res <- table(notes$niveau, notes$resultat)
tab_niv_res

chisq.test(tab_spec_res) #p<0.05
chisq.test(tab_niv_res) #p<0.05
chisq.test(tab_dipl_res)#p>0.05
 chisq.test(tab_corr_med_res) #p>0.05
chisq.test(tab_corr_fin_res) #p>0.05

## LABEL de la matrice
var_qual_col <- c ("specialite","niveau","dernier.diplome.obtenu","correcteur.median","correcteur.final")
var_qual_row <- c ("resultat")


###### MATRICE DES p-values test chi2 entre variable qualitatives
mat_var_qual<-matrix(ncol=length(var_qual_col),nrow=length(var_qual_row))
colnames(mat_var_qual)<-var_qual_col
rownames(mat_var_qual)<-var_qual_row

for (i in 1:length(var_qual_col))
{
	for(j in 1:length(var_qual_row)){
		table_temp <- table(notes[[var_qual_col[i]]],notes[[var_qual_row[j]]]);
		p <- chisq.test(table_temp);
		p_val <- p$p.value;
		mat_var_qual[j,i]<-p_val;
	}
}

###### MATRICE BOOLEENE test chi2 entre variable qualitatives : FALSE : REJETTE HYPOTHESE IND.
mat_var_qual_bool<-matrix(ncol=length(var_qual_col),nrow=length(var_qual_row))
colnames(mat_var_qual_bool)<-var_qual_col
rownames(mat_var_qual_bool)<-var_qual_row

for (i in 1:length(var_qual_col))
{
	for(j in 1:length(var_qual_row)){
		if(mat_var_qual[j,i]<0.05)
			mat_var_qual_bool[j,i] <- FALSE
		else
			mat_var_qual_bool[j,i] <- TRUE
	}
}

head(notes)

#which(notes$specialite=="GI") : liste l'index des personnes en GI.

############## VARIABLES QUANTITATIVES : CORRELATION

chisq.test(table_spec_res)
table_diplome_res<-table(notes$dernier.diplome.obtenu,notes$resultat)
chisq.test(table_diplome_res)
table_niveau_res<-table(notes$niveau,notes$resultat)
chisq.test(table_niveau_res)

plot(notes$note.median,notes$note.final)
note_clean<-na.omit(notes)
cor(note_clean$note.median,note_clean$note.final)

plot(note_clean$note.median,note_clean$note.totale)
cor(note_clean$note.median,note_clean$note.totale)

plot(note_clean$note.final,note_clean$note.totale)
cor(note_clean$note.final,note_clean$note.totale)

corrgram(note_clean, panel=panel.pts, upper.panel = NULL,text.panel = panel.txt)
################ VARIABLES QUALITATIVES/QUANTITATIVES : STUDENT TEST

############correcteur & notes median : MATRICE DES p-values : STUDENT TEST
t_matrix<-matrix(ncol=nlevels(unique(notes$correcteur.median)),nrow=nlevels(unique(notes$correcteur.median)))
colnames(t_matrix)<-levels(unique(notes$correcteur.median))
rownames(t_matrix)<-levels(unique(notes$correcteur.median))

correcteur<-levels(unique(notes$correcteur.median))

for (i in 1:length(correcteur)){
	for(j in 1:length(correcteur)){
		t_tmp<-t.test(notes[notes$correcteur.median==correcteur[i],"note.median"],notes[notes$correcteur.median==correcteur[j],"note.median"]);
		t_matrix[i,j]<-t_tmp$p.value;
	}
}

t_matrix

t_matrix_low <- t_matrix
upperTriangle(t_matrix_low) <- 0
colnames(t_matrix_low)<-levels(unique(notes$correcteur.median))
rownames(t_matrix_low)<-levels(unique(notes$correcteur.median))
write.table(t_matrix_low,file = "q1_student_mat.txt")
### MATRICE BOOLENNE
t_matrix_bool<-matrix(ncol=nlevels(unique(notes$correcteur.median)),nrow=nlevels(unique(notes$correcteur.median)))
colnames(t_matrix_bool)<-levels(unique(notes$correcteur.median))
rownames(t_matrix_bool)<-levels(unique(notes$correcteur.median))

for (i in 1:length(correcteur)){
	for(j in 1:length(correcteur)){
		if (t_matrix[i,j]<0.05)
			t_matrix_bool[i,j]<-0
		else
			t_matrix_bool[i,j]<-1
	}
}
t_matrix_bool


############correcteur & notes median
t_matrix<-matrix(rep(0,25),nrow=5)
colnames(t_matrix)<-c("Cor1","Cor2","Cor4","Cor5","Cor6")
rownames(t_matrix)<-c("Cor1","Cor2","Cor4","Cor5","Cor6")
correcteur<-c("Cor1","Cor2","Cor4","Cor5","Cor6")

for (i in 1:length(correcteur)){
	for(j in 1:length(correcteur)){
		t_tmp<-t.test(notes[notes$correcteur.median==correcteur[i],"note.median"],notes[notes$correcteur.median==correcteur[j],"note.median"]);
		t_matrix[i,j]<-t_tmp$p.value;
	}
}

t_matrix_new<-matrix(rep(0,25),nrow=5)
colnames(t_matrix_new)<-c("Cor1","Cor2","Cor4","Cor5","Cor6")
rownames(t_matrix_new)<-c("Cor1","Cor2","Cor4","Cor5","Cor6")
for (i in 1:length(correcteur)){
	for(j in 1:length(correcteur)){
		if (t_matrix[i,j]<0.05) 
			t_matrix_new[i,j]<-FALSE
		else
			t_matrix_new[i,j]<-TRUE
	}
}

##############correcteur & note final

t_matrix<-matrix(ncol=nlevels(unique(notes$correcteur.final)),nrow=nlevels(unique(notes$correcteur.final)))
colnames(t_matrix)<-levels(unique(notes$correcteur.final))
rownames(t_matrix)<-levels(unique(notes$correcteur.final))

correcteur<-levels(unique(notes$correcteur.final))


for (i in 1:length(correcteur)){
	for(j in 1:length(correcteur)){
		t_tmp<-t.test(notes[notes$correcteur.final==correcteur[i],"note.final"],notes[notes$correcteur.final==correcteur[j],"note.final"]);
		t_matrix[i,j]<-t_tmp$p.value;
	}
}
t_matrix

t_matrix_low <- t_matrix
upperTriangle(t_matrix_low) <- 0
colnames(t_matrix_low)<-levels(unique(notes$correcteur.final))
rownames(t_matrix_low)<-levels(unique(notes$correcteur.final))
write.table(t_matrix_low, "q1_student_mat_fin.txt", quote=FALSE, eol="\\\\\n", sep=" & ")



t_matrix_bool<-matrix(ncol=nlevels(unique(notes$correcteur.final)),nrow=nlevels(unique(notes$correcteur.final)))
colnames(t_matrix_bool)<-levels(unique(notes$correcteur.final))
rownames(t_matrix_bool)<-levels(unique(notes$correcteur.final))

for (i in 1:length(correcteur)){
	for(j in 1:length(correcteur)){
		if (t_matrix[i,j]<0.05)
			t_matrix_bool[i,j]<-0
		else
			t_matrix_bool[i,j]<- 1
	}
}
t_matrix_bool

nrow(notes)

plot(notes$note.median, notes$note.final, col=topo.colors(2), main="Scatterplot note median et note final")
legend(bty="n",inset=0.0009,xy.coords(x=0,y=20),c("note median","note final"), fill=topo.colors(2), horiz=FALSE)
abline(v=mean(notes$note.median, na.rm=TRUE), col="blue")
abline(h=mean(notes$note.final, na.rm=TRUE), col="cyan")
text(0.2,15, cex=.8, pos=4, "La ligne bleue est\nla moyenne de\nnote median")
text(0.2,13.3, cex=.8, pos=4, "La ligne cyan est\nla moyenne de\nnote final")

