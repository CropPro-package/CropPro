\name{dung.plot3d}
\alias{dung.plot3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{3D plot for the results of \code{\link[CropPro]{LDAcrop.dung}} where the archaeobotanical samples are entered at the discrimination point.
%%  ~~function to do ... ~~
}
\description{3D plot for the results of \code{\link[CropPro]{LDAcrop.dung}} where the archaeobotanical samples are entered at the discrimination point and then everything is classified as either the crop processing stages or as an archaeological sample

}
\usage{
dung.plot3d(data, gcol=NULL, col="black", site="site", LD=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The output of \code{\link[CropPro]{LDAcrop.dung}}
}
  \item{gcol}{The colour of the crop processing stages symbols
}
  \item{col}{The colour of the archaeobotanical samples symbols}
  \item{site}{The name for the archaeobotanical samples - to appear in the legend}
  \item{LD}{Whether using the first three discriminant functions or the 4th has been substituted for the 3rd function (response is LD=4)
  }
  }
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##Random data
BHH<-runif(40, min=0, max=7)
BFH<-runif(40, min=0, max=24)
SHH<-runif(40, min=1, max=13)
SHL<-runif(40, min=0.5, max=17)
SFH<-runif(40, min=1, max=22)
SFL<-runif(40, min=1, max=8)
NO<-sample(1:40, 40, replace=F)
PROC<-runif(40,min=0,max=0)
data<-as.data.frame(cbind(NO,PROC,BHH,BFH,SHH,SHL,SFH,SFL))
results<-LDAcrop.dung(results)
## Usage
dung.plot3d(results)
dung.plot3d(results, LD=4)
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
