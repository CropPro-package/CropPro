

#x = lda1, y = lda2, z= lda3
# note not sure how to do the colour - do have this as a default colour that can be changed.

dungplot3d<-function(data,x,y,z, gcol=NULL, col="black", site="Archaeological", forth=3){
  data.model<-data.frame(data.model)
  archdata<-data[c(11:18)]
  archdata$PROC<-"6"
  model.arch<-rbind(data.model, archdata)
  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch, CV = TRUE)
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch)
  predictionmodel <- predict(model_lda, model.arch)
  dataset <- data.frame(PROC = as.factor(model.arch$PROC),
                        Classification= predictionmodel$class,
                        predictionmodel$x)
  centroids <- dataset %>%
    group_by(PROC) %>%
    summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3= mean(LD3),
              centroid4 = mean(LD4))


  if(!is.null(gcol)){
    gcolours<-c(gcol,col)
    dataset$colour<-gcolours[as.numeric(dataset$PROC)]
  }
  if(is.null(gcol)){
    gcolours<-c("forestgreen", "blue", "dodgerblue", "red", col)
    dataset$colour<-gcolours[as.numeric(dataset$PROC)]
  }

  open3d()
  par3d(windowRect = c(100, 100, 612, 612))

  if (forth>3){
    plot3d(dataset$LD1, dataset$LD4,dataset$LD2, col=dataset$colour, type="s",  size=0.9, xlab= "LD1", ylab="LD4", zlab="LD2")

    shapelist3d(cube3d(),x=centroids$centroid1,y=centroids$centroid2, z=centroids$centroid3,  col="black",size=0.2)
    #play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )

    legend.table<- dataset[!duplicated(dataset$Actual.Group),]

    legend3d("topright",c("Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products", site, "Group centroids"), pch= c(16,16,16,16,16,15), col=c(gcolours, "black"), cex=1)

  }else{

  plot3d(dataset$LD1, dataset$LD3,dataset$LD2, col=dataset$colour, type="s",  size=0.9, xlab= "LD1", ylab="LD3", zlab="LD2")

  shapelist3d(cube3d(),x=centroids$centroid1,y=centroids$centroid2, z=centroids$centroid3,  col="black",size=0.2)
  #play3d( spin3d( axis = c(0, 0, 1), rpm = 20), duration = 10 )

  legend.table<- dataset[!duplicated(dataset$Actual.Group),]

  legend3d("topright",c("Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products", site, "Group centroids"), pch= c(16,16,16,16,16,15), col=c(gcolours, "black"), cex=1)

  }
}

