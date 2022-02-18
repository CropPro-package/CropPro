library(dplyr)
library(MASS)
library(haven)
library(psych)
library(Hmisc)
library(biotools)
library(MASS)
library(rgl)
# Read in data
# Organise data so only study column and  attributes



data<-readxl::read_excel("~/OneDrive - Nexus365/Explo/FIBS/croppro/Book3.xlsx")
data<-data[,c("BHH","BFH","SHH","SHL","SFH","SFL","PROC")]

data.model<-data[data$PROC!="0",]

data.test<-data[data$PROC=="0",]


#summary by class
summary_by_class<-data.model %>%
  group_by(PROC) %>%
  summarise_each(funs(mean,sd,length))

# frequencies
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res
}
tblFun(data.model$PROC)

#shows the variables as histograms, as well as scatterplots of how the combinations seperate the two classes, and the correlation coeffiencets

pairs.panels(data.model,
             gap=0,
             bg=c("red", "green")[data.model$PROC],
             pch=21)
#correlations among functional attributes (variables) - produces a correlation matrix and the p-vaules corresponding to the significance levels of the correlations
rcorr(as.matrix(data.model[, -ncol(data.model)]), type="pearson")


# Box's test of equality of covariance matrices ( == SPSS)

boxM(data = data.frame(data.model[, -ncol(data.model)]), data.model$PROC)

# wilks test (!= SPSS) not sure if this is correct.....
x <- as.matrix(data.model[,c("BHH","BFH","SHH","SHL","SFH","SFL")])
summary(manova(x ~ data.model$PROC), test="Wilks")

discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model, CV = TRUE)
model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model)

#discrim$scaling/eigens$vectors - from websites suggests you can use a formula to change the lda scalingcolumn
??

# this is used to get eigenvalues but need to work out how to get it out of lda - also not the same as SPSS
library(ade4)
dis1 <- discrimin(dudi.pca(data.model[,c("BHH","BFH","SHH","SHL","SFH","SFL")], scan = FALSE),
                  as.factor(data.model$PROC),
                  scan = FALSE)
dis1$eig# eigen values -not the same as SPSS
dis1$fa# canonical weights



#prior probabilities
model_lda$prior
model_lda$counts# number per group
model_lda$N#total number
model_lda$scaling #### this is the same as canonical discriminant function coefficients ( unstandardized coeffients) but without the constant
discrim_cv$class
discrim_cv$posterior

groupmean<-(model_lda$prior%*%model_lda$means)
constant<-(groupmean%*%model_lda$scaling)
-constant # for result equal to SPSS

#Assess the accuracy of the prediction percent correct for each category == SPSS 'classification results' according to Ben M

confcv<-table(list(observed=data.model$PROC,predicted=discrim_cv$class))

conf<-table(list(observed=data.model$PROC,predicted=predictionmodel$class))

# total percent not correct using CV or orginal
ctv<-table(data.model$PROC,discrim_cv$class)
sum(diag(prop.table(ctv)))

ct<-table(data.model$PROC, predictionmodel$class)
sum(diag(prop.table(ct)))
# Singular values - ratio of the between- and within group standard deviations on the linear discriminant variables
# - can be used to compute the amount of between group variance that is explained by each linear discriminant ( eg 1st D explains 68%, second 27%)
# this is in the eigenvalues table in SPSS under % of Variance
prop <- model_lda$svd^2/sum(model_lda$svd^2)
#ores for the model - could also use CV posterior ? check
predictionmodel <- predict(model_lda,data.model)

#use below to get discrim function for model - BUT NOTE these are actual groups not predicted groups
dataset = data.frame(PROC = as.factor(data.model$PROC),
                     lda = predictionmodel$x,
                     Predicted_group= predictionmodel$class)
dataset<-dataset%>%
  mutate(correct=if_else(Predicted_group==PROC, "Equal", "Not Equal"))

#centroids - these are correct/same as SPSS
centroids <- dataset %>%
  group_by(PROC) %>%
  summarise(centroid1 = mean(lda.LD1),
            centroid2= mean(lda.LD2),
            centroid3=mean(lda.LD3))


#prediction classes based on above model
predictions <- predict(model_lda,data.test)
predictions<-as.data.frame(predictions)
predictions<-cbind(predictions,data.test)

#classifications - correct to ungrouped cases in the classification results

tblFun(predictions$class)


#Ploting

plot(model_lda)

mycolours<-c("royalblue1", "darkcyan", "purple", "darkred" )
dataset$colour<-mycolours[as.numeric(dataset$PROC)]

function1<-dataset[dataset$PROC==1,]
function2<-dataset[dataset$PROC==2,]
function3<-dataset[dataset$PROC==3,]
function4<-dataset[dataset$PROC==4,]

ellips1<-ellipse3d(cov(cbind(function1$lda.LD1, function1$lda.LD2, function1$lda.LD3)),
                  centre=c(mean(function1$lda.LD1), mean(function1$lda.LD2),mean(function1$lda.LD3)), level=0.95)
ellips2<-ellipse3d(cov(cbind(function2$lda.LD1, function2$lda.LD2, function2$lda.LD3)),
                   centre=c(mean(function2$lda.LD1), mean(function2$lda.LD2),mean(function2$lda.LD3)), level=0.95)
ellips3<-ellipse3d(cov(cbind(function3$lda.LD1, function3$lda.LD2, function3$lda.LD3)),
                   centre=c(mean(function3$lda.LD1), mean(function3$lda.LD2),mean(function3$lda.LD3)), level=0.95)

ellips4<-ellipse3d(cov(cbind(function4$lda.LD1, function4$lda.LD2, function4$lda.LD3)),
                   centre=c(mean(function4$lda.LD1), mean(function4$lda.LD2),mean(function4$lda.LD3)), level=0.95)


open3d()
plot3d(dataset$lda.LD1, dataset$lda.LD2, dataset$lda.LD3,
       xlab="LD1", ylab="LD2", zlab="LD3", col=dataset$colour, type="s",size=0.8 )
shapelist3d(cube3d(),x=centroids$centroid1,y=centroids$centroid2, z=centroids$centroid3,  col="black",size=0.25)
wire3d(ellips1, col="blue")
wire3d(ellips2, col="pink")
wire3d(ellips3, col="purple")
wire3d(ellips4, col="black")
points3d(predictions$x.LD1,predictions$x.LD2, predictions$x.LD3, col="black",  size=0.9)

play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )
library(magick)
# Save like gif
movie3d(
  movie="3dAnimatedScatterplot",
  spin3d( axis = c(0, 0, 1), rpm = 7),
  duration = 10,
  dir = "~/Desktop",
  type = "gif",
  clean = TRUE,
  webshot= FALSE
)

library(car)
scatter3d(dataset$lda.LD1, dataset$lda.LD2, dataset$lda.LD3, surface=F, groups=dataset$PROC, ellipsoid=T)

rgl.postscript('3dplot.pdf', fmt = 'pdf')


mycolours<-c("royalblue1", "darkcyan", "purple", "darkred" )
dataset$colour<-mycolours[as.numeric(dataset$PROC)]

mygroups<-c("Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products")
dataset$Actual.Group<-mygroups[as.numeric(dataset$PROC)]

mypch<-c("1","2", "3", "5")
dataset$pch<-mypch[as.numeric(dataset$PROC)]
quartz()
par(mar=c(10,4,4,4))
plot(x=dataset$lda.LD1, y=dataset$lda.LD2, col=dataset$colour, pch=as.numeric(as.character(dataset$pch)), ylim=c(-5.5,4), xlim=c(-5,5), xlab="", ylab="")
par(new=T)
plot(centroids$centroid1,centroids$centroid2 , col="Black", pch=20, ylim=c(-5.5,4), xlim=c(-5,5), xlab="Function 1", ylab="Function 2")
par(new=T)
plot(predictions$x.LD1,predictions$x.LD2, col="Black", pch=17,ylim=c(-5.5,4), xlim=c(-5,5), xlab="Function 1", ylab="Function 2")

legend.table<- dataset[!duplicated(dataset$Actual.Group),]
legend.table<-legend.table[,c("Actual.Group","colour", "pch")]
legend.tableadd<-c("Archaeological samples", "black", "17")
legend.tablecent<-c("Centroids", "black", "20")
legend.table<-rbind(legend.table, legend.tableadd)
legend.table<-rbind(legend.table, legend.tablecent)
par(xpd=T)
legend("bottom", paste(legend.table$Actual.Group), col=paste(legend.table$colour), pch=as.numeric(as.character(legend.table$pch)), pt.cex=1, cex=0.70, bg="white", ncol=2)

