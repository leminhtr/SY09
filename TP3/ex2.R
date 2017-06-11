###3.  Frontière décision Bayes
setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP3")

source("distXY.R")
source("front.ceuc.R")
source("front.kppv.R")
source("distXY.R")
source("separ1.R")
source("separ2.R")

d1_40<-read.csv("Synth1-40.csv")
d1_100<-read.csv("Synth1-100.csv")
d1_500<-read.csv("Synth1-500.csv")
d1_1000<-read.csv("Synth1-1000.csv")
d2_1000<-read.csv("Synth2-1000.csv")


x11()
donn<-d1_1000

plot(donn$V1, donn$V2, col=topo.colors(3)[donn[,3]], xlab = "V1", ylab="V2")
abline(a=-2, b=-3/2, col="red", lwd=2)
text(2.2,-3.9,"x2 = -x1-3/2",srt=0.2,pos=3, col = "red")
dev.off()

plot()
abline(-1/2,-3/4)

