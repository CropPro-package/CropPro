\name{crop.plot3D}
\alias{crop.plot3D}
\title{A three dimensional graph of the results of \code{\link[CropPro]{LDAcrop.pro}}
}
\description{crop.plot3D plots the linear discriminant scores obtained from \code{\link[CropPro]{LDAcrop.pro}} as a three dimensional graph
}
\usage{
crop.plot3D(x,  gcol=NULL, col= "black",site ="Site",label=NULL, cex.lab =0.65, pos.lab=3)
}

\arguments{
\item{x}{the dataframe containing the discriminant scores obtained from \code{\link[CropPro]{LDAcrop.pro}}}
\item{col}{Symbol colour of archaeobotanical data}
\item{gcol}{Symbolcolours of the ethnographical crop processing groups written as a list eg. c("red", "green"...) etc}
\item{site}{The name that the archaeobotanical data will be labelled as in the legend}
\item{label}{the name of the samples to be labeled on the graph}
\item{cex.lab}{the size of the label following text3d}
\item{pos.lab}{the position of the label following text3d}
}
\references{PUT IN PAPER REFERENCE}
\author{Elizabeth Stroud
}

\examples{
##example dataset

LD1<-runif(40, min= -0, max=3)
LD2<-runif(40, min = -2, max=4)
LD3<-runif(40, min =-4, max=-1)
Study<-sample(1:3, 40, replace=TRUE)
data<-data.frame(Study,LD1, LD2, LD3)

## use

crop.plot3D(data)

## without defaults

crop.plot3D(data, gcol = c("black", "grey", "grey48", "grey89"),
col = "red", site = "Example")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
