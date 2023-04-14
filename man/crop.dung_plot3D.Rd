\name{crop.dung_plot3D}
\alias{crop.dung_plot3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A three dimensional graph of the results of \code{\link[CropPro]{LDAcrop.dung}}
%%  ~~function to do ... ~~
}
\description{crop.dung_plot3D plots the linear discriminant scores obtained from \code{\link[CropPro]{LDAcrop.dung}} as a three dimensional graph
}
\usage{dung.plot3d(data, gcols=NULL, col="black", site="Archaeological", LD=3, label=NULL, cex.lab =0.65, pos.lab=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The output of \code{\link[CropPro]{LDAcrop.dung}}}
  \item{gcol}{The colour of the crop processing stages' symbols}
  \item{col}{The colour of the archaeobotanical samples symbols}
  \item{site}{The name for the archaeobotanical samples - to appear in the legend}
  \item{LD}{Whether ploting the first three discriminant functions or the 4th has been substituted for the 3rd function (response is LD=4)}
  \item{label}{the name of the samples to be labeled on the graph}
  \item{cex.lab}{the size of the label following text3d}
   \item{pos.lab}{the position of the label following text3d}
  }

\references{PAPER
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}

\seealso{\link[CropPro]{LDAcrop.dung} \link[CropPro]{crop.plot2D}}

\examples{
##Random data

BHH<-runif(40, min=0, max=7)
BFH<-runif(40, min=0, max=24)
SHH<-runif(40, min=1, max=13)
SHL<-runif(40, min=0.5, max=17)
SFH<-runif(40, min=1, max=22)
SFL<-runif(40, min=1, max=8)
Sample<-sample(1:40, 40, replace=F)

data<-data.frame(Sample,BHH,BFH,SHH,SHL,SFH,SFL)
results<-LDAcrop.dung(data)

## Usage

crop.dung_plot3D(results)
crop.dung_plot3D(results, LD=4)
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
