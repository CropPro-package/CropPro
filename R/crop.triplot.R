#triplot

crop.triplot<-function(grain,rachis,weeds,pch=5, col="black", bg="black", sample=NULL, samplelabel="Sample",legendlabel="Samples",cpch=NULL, cbg=NULL, ccol=NULL){

data_tri<-crop.tri.data
  old.par<-par(no.readonly = TRUE)
  par(mar=c(3,2,1,3),
      mfrow=c(1,2))
  plot(NA,NA,xlim=c(0,1),ylim=c(0,sqrt(3)/2),asp=1,bty="n",axes=F,xlab="",ylab="")
  segments(0,0,0.5,sqrt(3)/2)
  segments(0.5,sqrt(3)/2,1,0)
  segments(1,0,0,0)

  text(0,0,labels="100% Weed",pos=1, xpd=NA)
  text(1,0,labels="100% Rachis",pos=1,xpd=NA)
  text(0.5,sqrt(3)/2,labels="100% Grain",pos=3,xpd=NA)
  tern2cart <- function(coord){
    coord[1]->x
    coord[2]->y
    coord[3]->z
    x+y+z->tot
    x/tot -> x
    y/tot -> y
    z/tot -> z
    (2*y + z)/(2*(x+y+z)) -> x1
    sqrt(3)*z/(2*(x+y+z)) -> y1
    return(c(x1,y1))
  }
  a<-seq(0.9,0.1, by=-0.1)
  b<-rep(0,9)
  c<-seq(0.1,0.9,by=0.1)
  grid<-data.frame(x=c(a, b, c, a, c, b),y=c(b, c, a, c, b, a),z=c(c, a, b, b, a, c))
  grid.tern<-t(apply(grid,1,tern2cart))
  grid<-cbind(grid.tern[1:27,],grid.tern[28:54,])
  apply(grid,1,function(x){segments(x0=x[1],y0=x[2],x1=x[3],y1=x[4],lty=2,col="grey80")})

  data_tri$GroupN<-data_tri$Group
  data_tri$GroupN[data_tri$GroupN=="coarse-sieve by-products"]<-1
  data_tri$GroupN[data_tri$GroupN=="winnowing by-products"]<-2
  data_tri$GroupN[data_tri$GroupN=="fine-sieve by-products"]<-3
  data_tri$GroupN[data_tri$GroupN=="cleaned products"]<-4
  if(!is.null(ccol)){
    ccolours<-ccol
    data_tri$colour<-ccolours[as.numeric(data_tri$GroupN)]
  }
  if(is.null(ccol)){
    ccolours<-c('black','black','black','black')
    data_tri$colour<-ccolours[as.numeric(data_tri$GroupN)]
  }

  if(!is.null(cpch)){
    mypch<-cpch
    data_tri$pch<-mypch[as.numeric(data_tri$GroupN)]
  }
  if(is.null(cpch)){
    mypch<-c(17,4,19,0)
    data_tri$pch<-mypch[as.numeric(data_tri$GroupN)]
  }
  if(!is.null(cbg)){
    mybg<-cbg
    data_tri$bg<-mybg[as.numeric(data_tri$GroupN)]
  }
  if(is.null(cbg)){
    mybg<-c('white','white',"white",'white')
    data_tri$bg<-mybg[as.numeric(data_tri$GroupN)]
  }


  md<-data_tri[,c("weeds", "rachis", "grain")]
  as.data.frame(t(apply(md, 1, tern2cart)) )-> md.tern
  md.tern2<-cbind(data_tri[,c("Group","colour","bg","pch")], md.tern)
  winnowing<-md.tern2[md.tern2$Group=="winnowing by-products",]
  coarse <-md.tern2[md.tern2$Group=="coarse-sieve by-products",]
  fine<-md.tern2[md.tern2$Group=="fine-sieve by-products",]
  product<-md.tern2[md.tern2$Group=="cleaned products",]
  points(winnowing$rachis,winnowing$grain, pch=winnowing$pch,col=winnowing$colour,bg=winnowing$bg, cex=1.2)
  points(coarse$rachis,coarse$grain, pch=coarse$pch,col=coarse$colour,bg=coarse$bg, cex=1.2)
  points(fine$rachis,fine$grain, pch=fine$pch,col=coarse$colour,bg=fine$bg, cex=1.2)
  points(product$rachis,product$grain, pch=product$pch,col=product$colour,bg=product$bg, cex=1.2)

  legend.table<- data_tri[!duplicated(data_tri$Group),]
  legend.table$Group[legend.table$Group=="coarse-sieve by-products"]<-"Coarse sieve by-product "
  legend.table$Group[legend.table$Group=="winnowing by-products"]<-"Winnowing by-product"
  legend.table$Group[legend.table$Group=="fine-sieve by-products"]<-"Fine sieve by-product"
  legend.table$Group[legend.table$Group=="cleaned products"]<-"Fine sieve product"

  legend("bottom", c(paste(legend.table$Group)), col=c((paste(legend.table$colour))), pch=c((as.numeric(as.character(legend.table$pch)))), pt.bg=c((paste(legend.table$bg))), pt.cex=1, cex=0.64,xpd=TRUE, ncol=2, inset=c(-0.03,-0.03))

  plot(NA,NA,xlim=c(0,1),ylim=c(0,sqrt(3)/2),asp=1,bty="n",axes=F,xlab="",ylab="")
  segments(0,0,0.5,sqrt(3)/2)
  segments(0.5,sqrt(3)/2,1,0)
  segments(1,0,0,0)

  text(0,0,labels="100% Weed",pos=1, xpd=NA)
  text(1,0,labels="100% Rachis",pos=1,xpd=NA)
  text(0.5,sqrt(3)/2,labels="100% Grain",pos=3,xpd=NA)

  tern2cart <- function(coord){
    coord[1]->x
    coord[2]->y
    coord[3]->z
    x+y+z->tot
    x/tot -> x
    y/tot -> y
    z/tot -> z
    (2*y + z)/(2*(x+y+z)) -> x1
    sqrt(3)*z/(2*(x+y+z)) -> y1
    return(c(x1,y1))
  }
  a<-seq(0.9,0.1, by=-0.1)
  b<-rep(0,9)
  c<-seq(0.1,0.9,by=0.1)
  grid<-data.frame(x=c(a, b, c, a, c, b),y=c(b, c, a, c, b, a),z=c(c, a, b, b, a, c))
  grid.tern<-t(apply(grid,1,tern2cart))
  grid<-cbind(grid.tern[1:27,],grid.tern[28:54,])
  apply(grid,1,function(x){segments(x0=x[1],y0=x[2],x1=x[3],y1=x[4],lty=2,col="grey80")})

  df<-data.frame(weeds,rachis,grain)
  as.data.frame(t(apply(df, 1, tern2cart)) )-> df.tern
  points(df.tern$rachis,df.tern$grain, pch=pch,col=col,bg=bg, cex=1.2)

  if(!is.null(sample)){
    subset<-df.tern[sample,]
    points(subset$rachis, subset$grain, pch=pch, col="red", bg="red")
    text(subset$rachis, subset$grain-0.025, labels=samplelabel, xpd=NA, cex=0.9)
  }
  legend.table<- data_tri[!duplicated(data_tri$Group),]

  legend("bottom", c(legendlabel), col=c(unique(col)), pch=c(unique(pch)), pt.bg=c(unique(bg)), pt.cex=1, cex=0.64,xpd=TRUE, ncol=2, inset=c(-0.03,-0.03))

  on.exit(par(old.par))
}

