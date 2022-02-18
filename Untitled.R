
LDAcrop.pro<-function(x){
  library(dplyr)
  library(haven)
  library(MASS)
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
  model <- cbind(as.data.frame(predict(model_lda,x)),x)
  print(model)
}


#x = lda1, y = lda2, z= lda3
# note not sure how to do the colour - do have this as a default colour that can be changed.

cropplot3dpoints<-function(x,y,z, Col){
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
              centroid3=mean(LD3))  open3d()
  plot3d(x, y, z,
       xlab="LD1", ylab="LD2", zlab="LD3", col=Col, type="s",size=0.8 )
  shapelist3d(cube3d(),x=centroids$centroid1,y=centroids$centroid2, z=centroids$centroid3,  col="black",size=0.25)

  points3d(x,y, z, col="black",  size=0.9)

play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )
}

wire3d(ellips1, col="blue")
wire3d(ellips2, col="pink")
wire3d(ellips3, col="purple")
wire3d(ellips4, col="black")


cropplot2d<-function(x???,y???,xlims=c(-5,5),ylims=c(-5.5,4),cols=,pchs,legend){
  LDA.data<-
  centroids<-
  par(mar=c(10,4,4,4))
  plot(x=dataset$lda.LD1, y=dataset$lda.LD2, col=dataset$colour, pch=as.numeric(as.character(dataset$pch)), ylim=c(-5.5,4), xlim=c(-5,5), xlab="", ylab="")
  par(new=T)
  plot(centroids$centroid1,centroids$centroid2 , col="Black", pch=20, ylim=c(-5.5,4), xlim=c(-5,5), xlab="Function 1", ylab="Function 2")
  par(new=T)
  plot(predictions$x.LD1,predictions$x.LD2, col="Black", pch=17,ylim=c(-5.5,4), xlim=c(-5,5), xlab="Function 1", ylab="Function 2")
}
