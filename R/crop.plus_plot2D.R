crop.plus_plot2D<-function(data,Func1=1, Func2=2, ylims=NULL,xlims=NULL,gcols=NULL, gbg=NULL,gpchs=NULL, col ='black', bg='black', pch=15, site="Archaeological", label=NULL, pos=c(0,-0.3)){
  PROC<-LD1<-LD2<-LD3<-LD4<-NULL
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

  #names(data)<-gsub(x=names(data), pattern = "`*`", replacement="")

  sampledata<-data
  ethnodata<-dataset[dataset$PROC!=5,]

  if(!is.null(gcols)){
    gcolours<-c(gcols)
    ethnodata$colour<-gcolours[as.numeric(ethnodata$PROC)]
  }
  if(is.null(gcols)){
    gcolours<-c('black','black','black','black')
    ethnodata$colour<-gcolours[as.numeric(ethnodata$PROC)]
  }
  if(!is.null(gbg)){
    gback<-c(gbg)
    ethnodata$bg<-gback[as.numeric(ethnodata$PROC)]
  }
  if(is.null(gbg)){
    gback<-c('black','black','black','black')
    ethnodata$bg<-gback[as.numeric(ethnodata$PROC)]
  }
  mygroups<-c( "Winnowing by-product", "Coarse sieve by-product  ", "Fine sieve by-product", "Fine sieve product")

  ethnodata$Actual.Group<-mygroups[as.numeric(ethnodata$PROC)]

  if(is.null(gpchs)){
    mypch<-c(1,2,3,5)
    ethnodata$pch<-mypch[as.numeric(ethnodata$PROC)]
  }

  if(Func1==1 & Func2==3){
    xv<-data$LD1
    x.value<-unlist((xv))
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
    yv<-data$LD3
    y.value<-unlist((yv))
    ym.value<-unlist(dataset$LD3)
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
    par(mar=c(8,4,4,4))
    plot(ethnodata$LD1, ethnodata$LD3, col=paste(ethnodata$colour),bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
    par(new=T)
    plot(sampledata$LD1, sampledata$LD3, col=paste(col),bg=paste(bg), pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
    par(new=T)
    plot(centroids$centroid1,centroids$centroid3 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 1", ylab="Function 3")
    if(!is.null(label)){
      samples<- sampledata[sampledata$Samples %in% c(label),]
      text(samples$LD1,samples$LD3+pos,labels=samples$Samples, cex=0.8)
    }

    legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
    legendtab<-tibble(labels=site,col=unique(col),bg=unique(bg), pch=unique(pch))

    par(new=TRUE)
    par(mar=c(1,4,1,1))
    plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
    legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)
  }
    else if(Func1==1 & Func2==4){
      xv<-data$LD1
      x.value<-unlist((xv))
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
      yv<-data$LD4
      y.value<-unlist(yv)
      ym.value<-unlist(dataset$LD4)
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
      par(mar=c(8,4,1,1))

      plot(ethnodata$LD1, ethnodata$LD4, col=paste(ethnodata$colour),bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD1, sampledata$LD4, col=paste(col), bg=paste(bg),pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(centroids$centroid1,centroids$centroid4 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 1", ylab="Function 4")
      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD1,samples$LD4+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col),bg=unique(bg), pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg= c((paste(legend.table$bg)),legendtab$bg,"black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }
    else if(Func1==2 & Func2==1){
      xv<-data$LD2
      x.value<-unlist((xv))
      m.value<-unlist(dataset$LD2)
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
      yv<-data$LD1
      y.value<-unlist(yv)
      ym.value<-unlist(dataset$LD1)
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
      par(mar=c(8,4,1,1))
      plot(ethnodata$LD2, ethnodata$LD1, col=paste(ethnodata$colour),bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD2, sampledata$LD1, col=paste(col),bg=paste(bg), pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(centroids$centroid2,centroids$centroid1 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 2", ylab="Function 1")

      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD2,samples$LD1+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col), bg=unique(bg),pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }
    else if(Func1==2 & Func2==3){
      xv<-data$LD2
      x.value<-unlist((xv))
      m.value<-unlist(dataset$LD2)
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
      yv<-data$LD3
      y.value<-unlist(yv)
      ym.value<-unlist(dataset$LD3)
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
      par(mar=c(8,4,1,1))
      plot(ethnodata$LD2, ethnodata$LD3, col=paste(ethnodata$colour), bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD2, sampledata$LD3, col=paste(col), bg=paste(bg), pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")

       par(new=T)
      plot(centroids$centroid2,centroids$centroid3 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 2", ylab="Function 3")
      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD2,samples$LD3+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col), bg=unique(bg),pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }
    else if(Func1==2 & Func2==4){
      xv<-data$LD2
      x.value<-unlist((xv))
      m.value<-unlist(dataset$LD2)
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
      yv<-data$LD4
      y.value<-unlist(yv)
      ym.value<-unlist(dataset$LD4)
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
      par(mar=c(8,4,1,1))
      plot(ethnodata$LD2, ethnodata$LD4, col=paste(ethnodata$colour), bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD2, sampledata$LD4, col=paste(col), bg=paste(bg), pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")

      par(new=T)
      plot(centroids$centroid2,centroids$centroid4 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 2", ylab="Function 4")
      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD2,samples$LD4+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col), bg=unique(bg), pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }
    else if(Func1==3 & Func2==4){
      xv<-data$LD3
      x.value<-unlist((xv))
      m.value<-unlist(dataset$LD3)
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
      yv<-data$LD4
      y.value<-unlist(yv)
      ym.value<-unlist(dataset$LD4)
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
      par(mar=c(8,4,1,1))
      plot(ethnodata$LD3, ethnodata$LD4, col=paste(ethnodata$colour), bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD3, sampledata$LD4, col=paste(col), bg=paste(bg), pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")

      par(new=T)
      plot(centroids$centroid3,centroids$centroid4 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 3", ylab="Function 4")
      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD3,samples$LD4+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col), bg=unique(bg), pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }
    else if(Func1==3 & Func2==2){
      xv<-data$LD3
      x.value<-unlist((xv))
      m.value<-unlist(dataset$LD3)
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
      yv<-data$LD2
      y.value<-unlist(yv)
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
      par(mar=c(8,4,1,1))
      plot(ethnodata$LD3, ethnodata$LD2, col=paste(ethnodata$colour), bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD3, sampledata$LD2, col=paste(col), bg=paste(bg), pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")

      par(new=T)
      plot(centroids$centroid3,centroids$centroid2 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 3", ylab="Function 2")
      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD3,samples$LD2+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col),bg=unique(bg), pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }
    else if(Func1==3 & Func2==4){
      xv<-data$LD3
      x.value<-unlist((xv))
      m.value<-unlist(dataset$LD3)
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
      yv<-data$LD4
      y.value<-unlist(yv)
      ym.value<-unlist(dataset$LD4)
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
      par(mar=c(8,4,1,1))
      plot(ethnodata$LD3, ethnodata$LD4, col=paste(ethnodata$colour), bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD3, sampledata$LD4, col=paste(col), bg=paste(bg),pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")

      par(new=T)
      plot(centroids$centroid3,centroids$centroid4 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 3", ylab="Function 4")
      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD3,samples$LD4+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col),bg=unique(bg),pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }
    else if(Func1==4 & Func2==1){
      xv<-data$LD4
      x.value<-unlist((xv))
      m.value<-unlist(dataset$LD4)
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
      yv<-data$LD1
      y.value<-unlist(yv)
      ym.value<-unlist(dataset$LD1)
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
      par(mar=c(8,4,1,1))
      plot(ethnodata$LD4, ethnodata$LD1, col=paste(ethnodata$colour), bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD4, sampledata$LD1, col=paste(col), bg=paste(bg), pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(centroids$centroid4,centroids$centroid1 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 4", ylab="Function 1")
      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD4,samples$LD1+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col), bg=unique(bg), pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)
    }
    else if(Func1==4 & Func2==2){
      xv<-data$LD4
      x.value<-unlist((xv))
      m.value<-unlist(dataset$LD4)
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
      yv<-data$LD2
      y.value<-unlist(yv)
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
      par(mar=c(8,4,1,1))
      plot(ethnodata$LD4, ethnodata$LD2, col=paste(ethnodata$colour), bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD4, sampledata$LD2, col=paste(col),  bg=paste(bg),pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")

      par(new=T)
      plot(centroids$centroid4,centroids$centroid2 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 4", ylab="Function 2")
      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD4,samples$LD2+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col), bg=unique(bg), pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }
    else if(Func1==4 & Func2==3){
      xv<-data$LD4
      x.value<-unlist((xv))
      m.value<-unlist(dataset$LD4)
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
      yv<-data$LD3
      y.value<-unlist(yv)
      ym.value<-unlist(dataset$LD3)
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
      par(mar=c(8,4,1,1))
      plot(ethnodata$LD4, ethnodata$LD3, col=paste(ethnodata$colour),bg=paste(ethnodata$bg),  pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
      par(new=T)
      plot(sampledata$LD4, sampledata$LD3, col=paste(col), bg=paste(bg), pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")

      par(new=T)
      plot(centroids$centroid4,centroids$centroid3 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 4", ylab="Function 3")
      if(!is.null(label)){
        samples<- sampledata[sampledata$Samples %in% c(label),]
        text(samples$LD4,samples$LD3+pos,labels=samples$Samples, cex=0.8)
      }

      legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
      legendtab<-tibble(labels=site,col=unique(col),bg=unique(bg), pch=unique(pch))

      par(new=TRUE)
      par(mar=c(1,4,1,1))
      plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
      legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }
    else {xv<-data$LD1
    x.value<-unlist((xv))
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
    yv<-data$LD2
    y.value<-unlist(yv)
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
    par(mar=c(8,4,1,1))
    plot(ethnodata$LD1, ethnodata$LD2, col=paste(ethnodata$colour),bg=paste(ethnodata$bg), pch=as.numeric(as.character(ethnodata$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
    par(new=T)
    plot(sampledata$LD1, sampledata$LD2, col=paste(col), bg=paste(bg), pch=as.numeric(as.character(pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
    par(new=T)
    plot(centroids$centroid1,centroids$centroid2 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="Function 1", ylab="Function 2")
    if(!is.null(label)){
      samples<- sampledata[sampledata$Samples %in% c(label),]
      text(samples$LD1+pos[1],samples$LD2+pos[2],labels=samples$Samples, cex=0.8)
    }
    par(new=T)
    legend.table<- ethnodata[!duplicated(ethnodata$Actual.Group),]
    legendtab<-tibble(labels=site,col=unique(col), bg=unique(bg), pch=unique(pch))

    par(new=TRUE)
    par(mar=c(1,4,1,1))
    plot(1:1, axes= FALSE,type= "n", xlab="", ylab="")
    legend("bottom", c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"),pt.bg=c((paste(legend.table$bg)),legendtab$bg, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

    }

  }
