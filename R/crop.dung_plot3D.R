
#x = lda1, y = lda2, z= lda3
# note not sure how to do the colour - do have this as a default colour that can be changed.
crop.dung_plot3D<-function(data, gcols=NULL, col="black", site="Archaeological", LD=3, label=NULL, cex.lab =0.65, pos.lab=3){
  data.model<-data.frame(data.model)
  archdata<-data
  archdata$PROC<-"5"
  labels<-archdata[c(1)]
  archdata<-archdata[c("PROC","BHH","BFH", "SHH", "SHL", "SFH", "SFL")]

  model.arch<-rbind(data.model, archdata)
  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch, CV = TRUE)
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, model.arch)
  predictionmodel <- predict(model_lda, model.arch)
  dataset <- data.frame(PROC = as.factor(model.arch$PROC),
                        Classification= predictionmodel$class,
                        predictionmodel$x)
  centroids <- dataset %>%
    group_by(PROC) %>%
    dplyr::summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3= mean(LD3),
              centroid4 = mean(LD4))

  #names(data)<-gsub(x=names(data), pattern = "*", replacement="")

  sampledata<-data

  ethnodata<-dataset[dataset$PROC!=5,]

  if(!is.null(gcols)){
    gcolours<-c(gcols)
    ethnodata$colour<-gcolours[as.numeric(ethnodata$PROC)]
  }
  if(is.null(gcols)){
    gcolours<-c("forestgreen", "blue", "dodgerblue", "red")
    ethnodata$colour<-gcolours[as.numeric(ethnodata$PROC)]
  }
  mygroups<-c( "Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products")

  ethnodata$Actual.Group<-mygroups[as.numeric(ethnodata$PROC)]


  open3d()
  par3d(windowRect = c(100, 100, 612, 612))

  if (LD>3){
    plot3d(x=ethnodata$LD1, y=ethnodata$LD4,z=ethnodata$LD2, col=ethnodata$colour, type="s",  size=0.9, xlab= "LD1", ylab="LD4", zlab="LD2")
    shapelist3d(cube3d(),x=centroids$centroid1,y=centroids$centroid4, z=centroids$centroid2,  col="black",size=0.1)
    spheres3d(x=sampledata$LD1,y=sampledata$LD4, z=sampledata$LD2,  col=col,radius =0.1)
    if(!is.null(label)){
      labeltable<- sampledata[sampledata$Samples %in% c(label),]
      text3d(labeltable$LD1, labeltable$LD4, labeltable$LD2, labeltable$Samples,cex=cex.lab, pos=pos.lab)
    }
    legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
    legendtab<-tibble(labels=site,col=unique(col), pch=(21))

    legend3d("topright", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),  pch=c(21,21,21,21,legendtab$pch,15),  pt.bg=c(gcolours,legendtab$col,"black"), cex=1)

  }else{
    plot3d(x=ethnodata$LD1, y=ethnodata$LD3,z=ethnodata$LD2, col=ethnodata$colour, type="s",  size=0.9, xlab= "LD1", ylab="LD3", zlab="LD2")
    shapelist3d(cube3d(),x=centroids$centroid1,y=centroids$centroid3, z=centroids$centroid2,  col="black",size=0.1)
    spheres3d(x=sampledata$LD1,y=sampledata$LD3, z=sampledata$LD2,  col=col,radius =0.1)
    if(!is.null(label)){
      labeltable<- sampledata[sampledata$Samples %in% c(label),]
      text3d(labeltable$LD1, labeltable$LD3, labeltable$LD2, labeltable$Samples,cex=cex.lab, pos=pos.lab)
    }
    legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
    legendtab<-tibble(labels=site,col=unique(col), pch=21)
    legend3d("topright", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),  pch=c(21,21,21,21,legendtab$pch,15),  pt.bg=c(gcolours,legendtab$col,"black"), cex=1)

  }
}

