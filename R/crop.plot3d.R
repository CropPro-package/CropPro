

#x = lda1, y = lda2, z= lda3
# note not sure how to do the colour - do have this as a default colour that can be changed.

crop.plot3D<-function(x, gcol=NULL, col="black", site="Site"){

  data.model<-data.frame(data.model)
  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model, CV = TRUE)
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model)
  predictionmodel <- predict(model_lda,data.model)
  functionalAt <- data.frame(PROC = as.factor(data.model$PROC),
                             Classification= predictionmodel$class,
                             predictionmodel$x)
  centroids <- functionalAt %>%
    group_by(PROC) %>%
    dplyr::summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3= mean(LD3))

  if(!is.null(gcol)){
    gcolours<-gcol
    functionalAt$colour<-gcolours[as.numeric(functionalAt$PROC)]
  }
  if(is.null(gcol)){
    gcolours<-c("forestgreen", "blue", "dodgerblue", "red")
    functionalAt$colour<-gcolours[as.numeric(functionalAt$PROC)]
  }
  names(x)<-gsub(x=names(x), pattern = "*", replacement="")

  open3d()
  par3d(windowRect = c(100, 100, 612, 612))
  plot3d(functionalAt$LD1,functionalAt$LD3, functionalAt$LD2,  col=functionalAt$colour, type="s",  size=0.9, xlab= "LD1", ylab="LD3", zlab="LD2")

  spheres3d(x$LD1,x$LD3,x$LD2, col=col,radius=0.14 )
  shapelist3d(cube3d(),x=centroids$centroid1,y=centroids$centroid3, z=centroids$centroid2,  col="black",size=0.2)
  #play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )

  legendtab<-tibble(labels=site,col=unique(col))

  legend3d("topright",c("Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products", site, "Group centroids"), pch= c(16,16,16,16,16,15), col=c(gcolours,legendtab$col,"black"), cex=1)
}


