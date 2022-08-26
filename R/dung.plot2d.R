dung.plot2d<-function(data,ylims=NULL,xlims=NULL,gcols=NULL,gpchs=NULL, col ='black', pch=15, site="Archaeological"){
  data.model<-data.frame(data.model)
  archdata<-data[c(11:18)]
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

  if(!is.null(gcols)){
    gcolours<-c(gcols,col)
    dataset$colour<-gcolours[as.numeric(dataset$PROC)]
  }
  if(is.null(gcols)){
    gcolours<-c('black','black','black','black',col)
    dataset$colour<-gcolours[as.numeric(dataset$PROC)]
  }
  mygroups<-c( "Winnowing by-products", "Coarse-sieving by-products", "Fine-sieving by-products", "Fine-sieving products", site)

  dataset$Actual.Group<-mygroups[as.numeric(dataset$PROC)]

  if(is.null(gpchs)){
    mypch<-c(1,2,3,5,15)
    dataset$pch<-mypch[as.numeric(dataset$PROC)]
  }
  x.value<-unlist((data$x.LD1))
  m.value<-unlist(dataset$LD1)
  xmin<-min(x.value)
  xmax<-max(x.value)
  mmin<-min(m.value)
  mmax<-max(m.value)

  if(xmin > mmin){
    xmin<-mmin
  }else {
    xmin<-xmin
  }
  if(xmax > mmax){
    xmax<-xmax
  }else {
    xmax<-mmax
  }

  if (length(xlims)){
    xlim<-xlims
  }else {
    xlim<-c(xmin-0.5,xmax+0.5)}

  y.value<-unlist((data$x.LD2))
  ym.value<-unlist(dataset$LD2)
  ymin<-min(y.value)
  ymax<-max(y.value)
  ymmin<-min(ym.value)
  ymmax<-max(ym.value)

  if(ymin > ymmin){
    ymin<-ymmin
  }else {
    ymin<-ymin
  }
  if(ymax > ymmax){
    ymax<-ymax
  }else {
    ymax<-ymmax
  }

  if (length(ylims)){
    ylim<-ylims
  }else {
    ylim<-c(ymin-0.5,ymax+0.5)}


  par(mar=c(10,4,4,4))
  plot(dataset$LD1, dataset$LD2, col=paste(dataset$colour), pch=as.numeric(as.character(dataset$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
  par(new=T)
  plot(centroids$centroid1,centroids$centroid2 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 1", ylab="Function 2")

  legend.table<- dataset[!duplicated(dataset$Actual.Group),]

  legend("bottom", c(paste(legend.table$Actual.Group), "Group centroids"), col=c((paste(legend.table$colour)), "black"), pch=c((as.numeric(as.character(legend.table$pch))),19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2, inset = c(-0.3,-0.4))

}
