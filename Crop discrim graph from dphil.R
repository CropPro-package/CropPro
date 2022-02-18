+

#Crop 1.5
data<-read.csv("~/Dropbox/phd stuff/statistical and graphing CW/SPSS/Crop processing versions/All Version CW SPSS.csv")

setwd("~/Dropbox/phd stuff/statistical and graphing CW/SPSS/Crop processing versions/")

weeds<-subset(data, Version=="weeds")

a<-sum(weeds$Highest.Predicted.Group=="1")
b<-sum(weeds$Highest.Predicted.Group=="2")
c<-sum(weeds$Highest.Predicted.Group=="3")
d<-sum(weeds$Highest.Predicted.Group=="4")

df=data.frame(a,b,c,d)

dung1<-subset(data, Version=="Dung1")

a<-sum(dung1$Highest.Predicted.Group=="1")
b<-sum(dung1$Highest.Predicted.Group=="2")
c<-sum(dung1$Highest.Predicted.Group=="3")
d<-sum(dung1$Highest.Predicted.Group=="4")

df1=data.frame(a,b,c,d)

dung1.des<-subset(data, Version=="Dung1.des")
a<-sum(dung1.des$Highest.Predicted.Group=="1")
b<-sum(dung1.des$Highest.Predicted.Group=="2")
c<-sum(dung1.des$Highest.Predicted.Group=="3")
d<-sum(dung1.des$Highest.Predicted.Group=="4")
df2=data.frame(a,b,c,d)



dung1.bolb<-subset(data, Version=="Dung1.bolb")
a<-sum(dung1.bolb$Highest.Predicted.Group=="1")
b<-sum(dung1.bolb$Highest.Predicted.Group=="2")
c<-sum(dung1.bolb$Highest.Predicted.Group=="3")
d<-sum(dung1.bolb$Highest.Predicted.Group=="4")
df3=data.frame(a,b,c,d)



dung1.bolb.des<-subset(data, Version=="Dung1.bolb.des")
a<-sum(dung1.bolb.des$Highest.Predicted.Group=="1")
b<-sum(dung1.bolb.des$Highest.Predicted.Group=="2")
c<-sum(dung1.bolb.des$Highest.Predicted.Group=="3")
d<-sum(dung1.bolb.des$Highest.Predicted.Group=="4")
df4=data.frame(a,b,c,d)


dung2<-subset(data, Version=="Dung2")
a<-sum(dung2$Highest.Predicted.Group=="1")
b<-sum(dung2$Highest.Predicted.Group=="2")
c<-sum(dung2$Highest.Predicted.Group=="3")
d<-sum(dung2$Highest.Predicted.Group=="4")

df5=data.frame(a,b,c,d)


dung2.des<-subset(data, Version=="Dung2.des")
a<-sum(dung2.des$Highest.Predicted.Group=="1")
b<-sum(dung2.des$Highest.Predicted.Group=="2")
c<-sum(dung2.des$Highest.Predicted.Group=="3")
d<-sum(dung2.des$Highest.Predicted.Group=="4")

df6=data.frame(a,b,c,d)



no.des<-subset(data, Version=="No_des")
a<-sum(no.des$Highest.Predicted.Group=="1")
b<-sum(no.des$Highest.Predicted.Group=="2")
c<-sum(no.des$Highest.Predicted.Group=="3")
d<-sum(no.des$Highest.Predicted.Group=="4")

df7=data.frame(a,b,c,d)




dung<-rbind(df,df1,df2,df3,df4,df5,df6,df7)

rownames(dung)<-c("Weeds","Dung1", "Dung1.des", "Dung1.bolb", "Dung1.bolb.des", "Dung2", "Dung2.des","No.des")
colnames(dung)<-c("Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products")

write.csv(dung, file="count per stage all versions.csv")

#dung1

ethno<-subset(data, Version=="Ethno")

pdf("dung1.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung1$Function.2~dung1$Function.1, col=paste(dung1$Col), pch=as.numeric(as.character(dung1$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()
##dung1.des

ethno<-subset(data, Version=="Ethno")

pdf("dung1.des.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung1.des$Function.2~dung1.des$Function.1, col=paste(dung1.des$Col), pch=as.numeric(as.character(dung1.des$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()
## dung1.bolb

pdf("dung1.bolb.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung1.bolb$Function.2~dung1.bolb$Function.1, col=paste(dung1.bolb$Col), pch=as.numeric(as.character(dung1.bolb$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()

###dung1.bolb.des

pdf("dung1.bolb.des.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung1.bolb.des$Function.2~dung1.bolb.des$Function.1, col=paste(dung1.bolb.des$Col), pch=as.numeric(as.character(dung1.bolb.des$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()

##dung2

pdf("dung2.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung2$Function.2~dung2$Function.1, col=paste(dung2$Col), pch=as.numeric(as.character(dung2$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()
##dung2.des

pdf("dung2.des.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung2.des$Function.2~dung2.des$Function.1, col=paste(dung2.des$Col), pch=as.numeric(as.character(dung2.des$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()

# results table for each sample, different verions

data1<-merge(weeds, dung1,"CaseNumber", all=T)
data2<-merge(dung1.des, dung1.bolb,"CaseNumber",all=T)
data3<-merge(dung1.bolb.des, dung2, "CaseNumber", all=T)
data4<-merge(dung2.des,data1,"CaseNumber",all=T)
data5<-merge(data4,data2, "CaseNumber", all=T)
data6<-merge(data5,data3, "CaseNumber", all=T)

data6$Col<-NULL
data6$pch<-NULL
data6$Col.x.x<-NULL
data6$pch.x.x<-NULL
data6$Col.y.x<-NULL
data6$pch.y.x<-NULL



colnames(data6,"CaseNumber","Vd2.des", Gr.d2.des,)
write.csv(data6, "merged SPSS data.csv")



batch1 <- data.frame(dung1$CaseNumber,dung1$Highest.Predicted.Group, dung1$PGD1,dung1$Second.Highest.Group, dung1$PDG2)

names(batch1) <- c( "CaseNumber","Highestdung1","PDG1dung1","Seconddung1", "PDG2dung1")

batch2 <- data.frame(dung1.des$CaseNumber, dung1.des$Highest.Predicted.Group, dung1.des$PGD1,dung1.des$Second.Highest.Group, dung1.des$PDG2)

names(batch2) <- c( "CaseNumber","Highestdung1.des","PDG1dung1.des","Seconddung1.des", "PDG2dung1.des")

batch3 <- data.frame(dung1.bolb$CaseNumber, dung1.bolb$Highest.Predicted.Group, dung1.bolb$PGD1,dung1.bolb$Second.Highest.Group, dung1.bolb$PDG2)

names(batch3) <- c( "CaseNumber","Highestdung1.bolb","PDG1dung1.bolb","Seconddung1.bolb", "PDG2dung1.bolb")

batch4 <- data.frame(dung1.bolb.des$CaseNumber,dung1.bolb.des$Highest.Predicted.Group, dung1.bolb.des$PGD1,dung1.bolb.des$Second.Highest.Group, dung1.bolb.des$PDG2)

names(batch4) <- c( "CaseNumber","Highestdung1.bolb.des","PDG1dung1.bolb.des","Seconddung1.bolb.des", "PDG2dung1.bolb.des")


batch5 <- data.frame(dung2$CaseNumber, dung2$Highest.Predicted.Group, dung2$PGD1,dung2$Second.Highest.Group, dung2$PDG2)
names(batch5) <- c( "CaseNumber","Highestdung2","PDG1dung2","Seconddung2", "PDG2dung2")
batch6 <- data.frame(dung2.des$CaseNumber, dung2.des$Highest.Predicted.Group, dung2.des$PGD1,dung2.des$Second.Highest.Group, dung2.des$PDG2)

names(batch6) <- c( "CaseNumber","Highestdung2.des","PDG1dung2.des","Seconddung2.des", "PDG2dung2.des")

batch7 <- data.frame(weeds$CaseNumber, weeds$Highest.Predicted.Group, weeds$PGD1,weeds$Second.Highest.Group, weeds$PDG2)

names(batch7) <- c( "CaseNumber","Highestweeds","PDG1weeds","Secondweeds", "PDG2weeds")



data1<-merge(batch1, batch2,"CaseNumber", all=T)
data2<-merge(batch3, batch4,"CaseNumber",all=T)
data3<-merge(batch5, batch6, "CaseNumber", all=T)
data4<-merge(batch7,data1,"CaseNumber",all=T)
data5<-merge(data4,data2, "CaseNumber", all=T)
data6<-merge(data5,data3, "CaseNumber", all=T)

write.csv(data6, "merged SPSS dataV2.csv")


##Crop 2.0

data<-read.csv("~/Dropbox/phd stuff/statistical and graphing CW/SPSS/Crop processing versions/2mm/All Version CW SPSS v2.csv")

setwd("~/Dropbox/phd stuff/statistical and graphing CW/SPSS/Crop processing versions/")

weeds<-subset(data, Version=="weeds")

a<-sum(weeds$Highest.Predicted.Group=="1")
b<-sum(weeds$Highest.Predicted.Group=="2")
c<-sum(weeds$Highest.Predicted.Group=="3")
d<-sum(weeds$Highest.Predicted.Group=="4")

df=data.frame(a,b,c,d)

dung1<-subset(data, Version=="Dung1")

a<-sum(dung1$Highest.Predicted.Group=="1")
b<-sum(dung1$Highest.Predicted.Group=="2")
c<-sum(dung1$Highest.Predicted.Group=="3")
d<-sum(dung1$Highest.Predicted.Group=="4")

df1=data.frame(a,b,c,d)

dung1.des<-subset(data, Version=="Dung1.des")
a<-sum(dung1.des$Highest.Predicted.Group=="1")
b<-sum(dung1.des$Highest.Predicted.Group=="2")
c<-sum(dung1.des$Highest.Predicted.Group=="3")
d<-sum(dung1.des$Highest.Predicted.Group=="4")
df2=data.frame(a,b,c,d)



dung1.bolb<-subset(data, Version=="Dung1.bolb")
a<-sum(dung1.bolb$Highest.Predicted.Group=="1")
b<-sum(dung1.bolb$Highest.Predicted.Group=="2")
c<-sum(dung1.bolb$Highest.Predicted.Group=="3")
d<-sum(dung1.bolb$Highest.Predicted.Group=="4")
df3=data.frame(a,b,c,d)



dung1.bolb.des<-subset(data, Version=="Dung1.bolb.des")
a<-sum(dung1.bolb.des$Highest.Predicted.Group=="1")
b<-sum(dung1.bolb.des$Highest.Predicted.Group=="2")
c<-sum(dung1.bolb.des$Highest.Predicted.Group=="3")
d<-sum(dung1.bolb.des$Highest.Predicted.Group=="4")
df4=data.frame(a,b,c,d)


dung2<-subset(data, Version=="Dung2")
a<-sum(dung2$Highest.Predicted.Group=="1")
b<-sum(dung2$Highest.Predicted.Group=="2")
c<-sum(dung2$Highest.Predicted.Group=="3")
d<-sum(dung2$Highest.Predicted.Group=="4")

df5=data.frame(a,b,c,d)


dung2.des<-subset(data, Version=="Dung2.des")
a<-sum(dung2.des$Highest.Predicted.Group=="1")
b<-sum(dung2.des$Highest.Predicted.Group=="2")
c<-sum(dung2.des$Highest.Predicted.Group=="3")
d<-sum(dung2.des$Highest.Predicted.Group=="4")

df6=data.frame(a,b,c,d)



no.des<-subset(data, Version=="No_des")
a<-sum(no.des$Highest.Predicted.Group=="1")
b<-sum(no.des$Highest.Predicted.Group=="2")
c<-sum(no.des$Highest.Predicted.Group=="3")
d<-sum(no.des$Highest.Predicted.Group=="4")

df7=data.frame(a,b,c,d)




dung<-rbind(df,df1,df2,df3,df4,df5,df6,df7)

rownames(dung)<-c("Weeds","Dung1", "Dung1.des", "Dung1.bolb", "Dung1.bolb.des", "Dung2", "Dung2.des","No.des")
colnames(dung)<-c("Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products")

write.csv(dung, file="count per stage all versions 2mm.csv")

#dung1
setwd("~/Dropbox/phd stuff/statistical and graphing CW/SPSS/Crop processing versions/2mm/")

ethno<-subset(data, Version=="Ethno")

pdf("dung1.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung1$Function.2~dung1$Function.1, col=paste(dung1$Col), pch=as.numeric(as.character(dung1$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()
##dung1.des

ethno<-subset(data, Version=="Ethno")

pdf("dung1.des.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung1.des$Function.2~dung1.des$Function.1, col=paste(dung1.des$Col), pch=as.numeric(as.character(dung1.des$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()
## dung1.bolb

pdf("dung1.bolb.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung1.bolb$Function.2~dung1.bolb$Function.1, col=paste(dung1.bolb$Col), pch=as.numeric(as.character(dung1.bolb$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()

###dung1.bolb.des

pdf("dung1.bolb.des.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung1.bolb.des$Function.2~dung1.bolb.des$Function.1, col=paste(dung1.bolb.des$Col), pch=as.numeric(as.character(dung1.bolb.des$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()

##dung2

pdf("dung2.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung2$Function.2~dung2$Function.1, col=paste(dung2$Col), pch=as.numeric(as.character(dung2$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()
##dung2.des

pdf("dung2.des.pdf")
plot(ethno$Function.2~ethno$Function.1, col=paste(ethno$Col), pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))

#plot(ethno$Function.2~ethno$Function.1, col="black", pch=as.numeric(as.character(ethno$pch)), xlab= "Function 1", ylab= "Function 2")

par(new=TRUE)
plot(dung2.des$Function.2~dung2.des$Function.1, col=paste(dung2.des$Col), pch=as.numeric(as.character(dung2.des$pch)), xlab= "", ylab= "", xlim=c(-4.5,4.5), ylim=c(-4.5,4.5))


legend.table<- data[!duplicated(data$Actual.Group),]

 legend("bottomright", paste(legend.table$Actual.Group), col=paste(legend.table$Col), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.80, bg="white")

dev.off()

# results table for each sample, different verions

data1<-merge(weeds, dung1,"CaseNumber", all=T)
data2<-merge(dung1.des, dung1.bolb,"CaseNumber",all=T)
data3<-merge(dung1.bolb.des, dung2, "CaseNumber", all=T)
data4<-merge(dung2.des,data1,"CaseNumber",all=T)
data5<-merge(data4,data2, "CaseNumber", all=T)
data6<-merge(data5,data3, "CaseNumber", all=T)

data6$Col<-NULL
data6$pch<-NULL
data6$Col.x.x<-NULL
data6$pch.x.x<-NULL
data6$Col.y.x<-NULL
data6$pch.y.x<-NULL



colnames(data6,"CaseNumber","Vd2.des", "Gr.d2.des",)
write.csv(data6, "merged SPSS data.csv")



batch1 <- data.frame(dung1$CaseNumber,dung1$Highest.Predicted.Group, dung1$PGD1,dung1$Second.Highest.Group, dung1$PDG2)

names(batch1) <- c( "CaseNumber","Highestdung1","PDG1dung1","Seconddung1", "PDG2dung1")

batch2 <- data.frame(dung1.des$CaseNumber, dung1.des$Highest.Predicted.Group, dung1.des$PGD1,dung1.des$Second.Highest.Group, dung1.des$PDG2)

names(batch2) <- c( "CaseNumber","Highestdung1.des","PDG1dung1.des","Seconddung1.des", "PDG2dung1.des")

batch3 <- data.frame(dung1.bolb$CaseNumber, dung1.bolb$Highest.Predicted.Group, dung1.bolb$PGD1,dung1.bolb$Second.Highest.Group, dung1.bolb$PDG2)

names(batch3) <- c( "CaseNumber","Highestdung1.bolb","PDG1dung1.bolb","Seconddung1.bolb", "PDG2dung1.bolb")

batch4 <- data.frame(dung1.bolb.des$CaseNumber,dung1.bolb.des$Highest.Predicted.Group, dung1.bolb.des$PGD1,dung1.bolb.des$Second.Highest.Group, dung1.bolb.des$PDG2)

names(batch4) <- c( "CaseNumber","Highestdung1.bolb.des","PDG1dung1.bolb.des","Seconddung1.bolb.des", "PDG2dung1.bolb.des")


batch5 <- data.frame(dung2$CaseNumber, dung2$Highest.Predicted.Group, dung2$PGD1,dung2$Second.Highest.Group, dung2$PDG2)
names(batch5) <- c( "CaseNumber","Highestdung2","PDG1dung2","Seconddung2", "PDG2dung2")
batch6 <- data.frame(dung2.des$CaseNumber, dung2.des$Highest.Predicted.Group, dung2.des$PGD1,dung2.des$Second.Highest.Group, dung2.des$PDG2)

names(batch6) <- c( "CaseNumber","Highestdung2.des","PDG1dung2.des","Seconddung2.des", "PDG2dung2.des")

batch7 <- data.frame(weeds$CaseNumber, weeds$Highest.Predicted.Group, weeds$PGD1,weeds$Second.Highest.Group, weeds$PDG2)

names(batch7) <- c( "CaseNumber","Highestweeds","PDG1weeds","Secondweeds", "PDG2weeds")



data1<-merge(batch1, batch2,"CaseNumber", all=T)
data2<-merge(batch3, batch4,"CaseNumber",all=T)
data3<-merge(batch5, batch6, "CaseNumber", all=T)
data4<-merge(batch7,data1,"CaseNumber",all=T)
data5<-merge(data4,data2, "CaseNumber", all=T)
data6<-merge(data5,data3, "CaseNumber", all=T)

write.csv(data6, "merged SPSS dataV2.csv")
