
LDAcrop.pro<-function(x){
  library(dplyr)
  library(MASS)
  data.model<-data.frame(CP.data.model)
  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,CP.data.model, CV = TRUE)
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,CP.data.model)
  predictionmodel <- predict(model_lda,CP.data.model)
  functionalAt <- data.frame(PROC = as.factor(CP.data.model$PROC),
                             Classification= predictionmodel$class,
                             predictionmodel$x)
  centroids <- functionalAt %>%
  group_by(PROC) %>%
  summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3=mean(LD3))
  model <- cbind(as.data.frame(predict(model_lda,x)),x)

  print(model)

}


#x = lda1, y = lda2, z= lda3
# note not sure how to do the colour - do have this as a default colour that can be changed.

cropplot3dpoints<-function(x,y,z, Col){
  library(rgl)

  load(file="CP.data.model.rda")
  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,CP.data.model, CV = TRUE)
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,CP.data.model)
  predictionmodel <- predict(model_lda,CP.data.model)
  functionalAt <- data.frame(PROC = as.factor(CP.data.model$PROC),
                             Classification= predictionmodel$class,
                             predictionmodel$x)
  centroids <- functionalAt %>%
    group_by(PROC) %>%
    summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3=mean(LD3))
  open3d()
  par3d(windowRect = c(100, 100, 612, 612))


  plot3d(functionalAt$LD1,functionalAt$LD2, functionalAt$LD3, col=functionalAt$PROC, type="s",  size=0.9, xlab= "LD1", ylab="LD2", zlab="LD3")
  pch3d(x, y, z, pch=8, col=Col,cex = 0.2 )
  shapelist3d(cube3d(),x=centroids$centroid1,y=centroids$centroid2, z=centroids$centroid3,  col="black",size=0.2)
#play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )
legend3d("topright",c("Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products", "SITE", "Group centroids"), pch= c(16,16,16,16,8,15), col=c(1,2,3,4,Col,"black"), cex=1)
}



cropplot2d<-function(x,y,ylims=c(-5,5),xlims=c(-5,5),gcols=NULL,gpchs=NULL, col ='black', pch=23){
  load(file="CP.data.model.rda")
  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,CP.data.model, CV = TRUE)
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,CP.data.model)
  predictionmodel <- predict(model_lda,CP.data.model)
  dataset <- data.frame(PROC = as.factor(CP.data.model$PROC),
                             Classification= predictionmodel$class,
                             predictionmodel$x)
  centroids <- dataset %>%
    group_by(PROC) %>%
    summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3=mean(LD3))

  if(!is.null(gcols)){
    gcolours<-gcols
    dataset$colour<-gcolours[as.numeric(dataset$PROC)]
  }
  if(is.null(gcols)){
    gcolours<-c('black','black','black','black')
    dataset$colour<-gcolours[as.numeric(dataset$PROC)]
  }
  mygroups<-c("Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products")
 dataset$Actual.Group<-mygroups[as.numeric(dataset$PROC)]
  if(!is.null(gpchs)){
    mypch<-gpchs
    dataset$pch<-mypch[as.numeric(dataset$PROC)]
  }
 if(is.null(gpchs)){
    mypch<-c(1,2,3,5)
    dataset$pch<-mypch[as.numeric(dataset$PROC)]
  }

  par(mar=c(10,4,4,4))
  plot(dataset$LD1, dataset$LD2, col=paste(dataset$colour), pch=as.numeric(as.character(dataset$pch)), ylim=ylims, xlim=xlims, xlab="", ylab="")
  par(new=T)
  plot(centroids$centroid1,centroids$centroid2 , col="Black", pch=20, ylim=ylims, xlim=xlims, xlab="", ylab="")
  par(new=T)
  plot(x,y, col=col, pch=21, ylim=ylims, xlim=xlims, xlab="Function 1", ylab="Function 2")

  legend.table<- dataset[!duplicated(dataset$Actual.Group),]

  legend("bottom", c(paste(legend.table$Actual.Group), "SITE", "Group centroids"), col=c((paste(legend.table$colour)),col, "black"), pch=c((as.numeric(as.character(legend.table$pch))),pch,20), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2, inset = c(-0.3,-0.4))

}
