crop.plot2D<-function(x,ylims=NULL,xlims=NULL,gcol=NULL,gpch=NULL, col ='black', pch=15, site="Site", Func1=1, Func2=2, label=NULL,pos=c(0,-0.3), lab.col="black", lab.cex=0.8){
  data.model<-data.frame(data.model)

  discrim_cv <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL,data.model,CV = TRUE)
  model_lda <- lda(PROC ~ BHH+BFH+SHH+SHL+SFH+SFL, data.model)
  predictionmodel <- predict(model_lda,data.model)
  dataset <- data.frame(PROC = as.factor(data.model$PROC),
                             Classification= predictionmodel$class,
                             predictionmodel$x)
  centroids <- dataset %>%
    group_by(PROC) %>%
    dplyr::summarise(centroid1 = mean(LD1),
              centroid2= mean(LD2),
              centroid3= mean(LD3))

  if(!is.null(gcol)){
    gcolours<-gcol
    dataset$colour<-gcolours[as.numeric(dataset$PROC)]
  }
  if(is.null(gcol)){
    gcolours<-c('black','black','black','black')
    dataset$colour<-gcolours[as.numeric(dataset$PROC)]
  }
  mygroups<-c("Winnowing by-product", "Coarse sieve by-product  ", "Fine sieve by-product", "Fine sieve product")
 dataset$Actual.Group<-mygroups[as.numeric(dataset$PROC)]
  if(!is.null(gpch)){
    mypch<-gpch
    dataset$pch<-mypch[as.numeric(dataset$PROC)]
  }
 if(is.null(gpch)){
    mypch<-c(1,2,3,5)
    dataset$pch<-mypch[as.numeric(dataset$PROC)]
 }
names(x)<-gsub(x=names(x), pattern = "*", replacement="")

 if (Func1==1 & Func2==3){
   xv<-x$LD1
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
   yv<-x$LD3
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
   par(mar=c(8,4,1,1))
   plot(dataset$LD1, dataset$LD3, col=paste(dataset$colour), pch=as.numeric(as.character(dataset$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(centroids$centroid1,centroids$centroid3 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(x$LD1, x$LD3, col=col, pch=pch, ylim=ylim, xlim=xlim, xlab="Function 1", ylab="Function 3")
   if(!is.null(label)){
     samples<- x[x$Samples %in% c(label),]
     text(samples$LD1+pos[1],samples$LD3+pos[2],labels=samples$Samples, cex=0.8)
   }
   legend.table<- dataset[!duplicated(dataset$Actual.Group),]
   legendtab<-tibble(labels=site,col=unique(col), pch=unique(pch))

   legend("bottom", inset=c(0,-0.5), c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)
 }
 else if(Func1==3 & Func2==1){
   xv<-x$LD3
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
   yv<-x$LD1
   y.value<-unlist((yv))
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
   plot(dataset$LD3, dataset$LD1, col=paste(dataset$colour), pch=as.numeric(as.character(dataset$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(centroids$centroid3,centroids$centroid1 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(x$LD3, x$LD1, col=col, pch=pch, ylim=ylim, xlim=xlim, xlab="Function 3", ylab="Function 1")
   if(!is.null(label)){
     samples<- x[x$Samples %in% c(label),]
     text(samples$LD3+pos[1],samples$LD1+pos[2],labels=samples$Samples, cex=0.8)
   }
   legend.table<- dataset[!duplicated(dataset$Actual.Group),]
   legendtab<-tibble(labels=site,col=unique(col), pch=unique(pch))

   legend("bottom", inset=c(0,-0.5),c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)
 }
 else if(Func1==3 & Func2==2){
   xv<-x$LD3
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
   yv<-x$LD2
   y.value<-unlist((yv))
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
   plot(dataset$LD3, dataset$LD2, col=paste(dataset$colour), pch=as.numeric(as.character(dataset$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(centroids$centroid3,centroids$centroid2 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(x$LD3, x$LD2, col=col, pch=pch, ylim=ylim, xlim=xlim, xlab="Function 3", ylab="Function 2")
   if(!is.null(label)){
     samples<- x[x$Samples %in% c(label),]
     text(samples$LD3+pos[1],samples$LD2+pos[2],labels=samples$Samples, cex=0.8)
   }
   legend.table<- dataset[!duplicated(dataset$Actual.Group),]
   legendtab<-tibble(labels=site,col=unique(col), pch=unique(pch))

   legend("bottom", inset=c(0,-0.5), c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)

 }
 else if (Func1==2 & Func2==3){
   xv<-x$LD2
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
   yv<-x$LD3
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
   par(mar=c(8,4,1,1))
   plot(dataset$LD2, dataset$LD3, col=paste(dataset$colour), pch=as.numeric(as.character(dataset$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(centroids$centroid2,centroids$centroid3 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(x$LD2, x$LD3, col=col, pch=pch, ylim=ylim, xlim=xlim, xlab="Function 2", ylab="Function 3")
   if(!is.null(label)){
     samples<- x[x$Samples %in% c(label),]
     text(samples$LD2+pos[1],samples$LD3+pos[2],labels=samples$Samples, cex=0.8)
   }
   legend.table<- dataset[!duplicated(dataset$Actual.Group),]
   legendtab<-tibble(labels=site,col=unique(col), pch=unique(pch))

   legend("bottom", inset=c(0,-0.5),c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)
 }
 else if (Func1==2 & Func2==1){
   xv<-x$LD2
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
   yv<-x$LD1
   y.value<-unlist((yv))
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
   plot(dataset$LD2, dataset$LD1, col=paste(dataset$colour), pch=as.numeric(as.character(dataset$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(centroids$centroid2,centroids$centroid1 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(x$LD2, x$LD1, col=col, pch=pch, ylim=ylim, xlim=xlim, xlab="Function 2", ylab="Function 1")
   if(!is.null(label)){
     samples<- x[x$Samples %in% c(label),]
     text(samples$LD2+pos[1],samples$LD1+pos[2],labels=samples$Samples, cex=0.8)
   }
   legend.table<- dataset[!duplicated(dataset$Actual.Group),]
   legendtab<-tibble(labels=site,col=unique(col), pch=unique(pch))

   legend("bottom", inset=c(0,-0.5),c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2)
 }
 else {
   xv<-x$LD1
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
   yv<-x$LD2
   y.value<-unlist((yv))
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
   plot(dataset$LD1, dataset$LD2, col=paste(dataset$colour), pch=as.numeric(as.character(dataset$pch)), ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(centroids$centroid1,centroids$centroid2 , col="Black", pch=19, ylim=ylim, xlim=xlim, xlab="", ylab="")
   par(new=T)
   plot(x$LD1, x$LD2, col=col, pch=pch, ylim=ylim, xlim=xlim, xlab="Function 1", ylab="Function 2")
   if(!is.null(label)){
     samples<- x[x$Samples %in% c(label),]
     text(samples$LD1+pos[1],samples$LD2+pos[2],labels=samples$Samples, cex=lab.cex,col=lab.col)
   }
   legend.table<- dataset[!duplicated(dataset$Actual.Group),]
   legendtab<-tibble(labels=site,col=unique(col), pch=unique(pch))

   legend("bottom",inset=c(0,-0.5), c(paste(legend.table$Actual.Group), site, "Group centroids"), col=c((paste(legend.table$colour)),legendtab$col, "black"), pch=c((as.numeric(as.character(legend.table$pch))),legendtab$pch,19), pt.cex=1, cex=0.64, bg="white",xpd=TRUE, ncol=2, )
 }

}
