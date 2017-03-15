getwd()
setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP1")

#######################1.

###########1.1

####1

#nature : nom etu; specialite/niveau,

notes <- read.csv("sy02-p2016.csv", na.strings="", header=T)
 notes$nom <- factor(notes$nom, levels=notes$nom)
 notes$niveau <- factor(notes$niveau, ordered=T)
 notes$resultat <- factor(notes$resultat, levels=c("F","Fx","E","D","C","B","A"),
ordered=T)

head(notes)
summary(notes)
plot(notes)
n_a <- is.na(notes)
n_a
notes[!complete.cases(notes),]
length(notes[!complete.cases(notes),] )
sapply(notes, function(x) sum(is.na(x)))



#notes sans N/A
notes_clean <- na.omit(notes)
summary(notes_clean)

############# GRAPH

plot(notes$specialite, notes$resultat)
#histogramme : quantitative
#barplot : qualitative
#boxplot : comparer des notes



par(mfrow=c(2,3));
boxplot(note.totale ~ specialite , data=notes,xlab="specialite",ylab="note")
boxplot(note.totale ~ niveau , data=notes,xlab="niveau",ylab="note")
boxplot(note.totale ~ dernier.diplome.obtenu, data=notes,xlab="origine",ylab="note")
boxplot(note.median~ correcteur.median, data=notes,xlab="correcteur median",ylab="note median")
boxplot(note.final~ correcteur.final, data=notes,xlab="correcteur final",ylab="note final")
########
table_spec_res<-table(notes$specialite,notes$resultat)
barplot(table_spec_res,beside=T)








#2. : filière, origine, niveau, correcteur, ...
#cor.test : test si correlation a une valeur significative
#test correlation 2 quantitative : cor.test
#test correlation 2 qualitative : chi2
#test correlation qualitative/quantitative : variance/test student si 2 échantillons

head(notes)
length(notes$median)

################### TEST CHI2 : TEST IND. ENTRE VARIABLES QUALITATIVES
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






