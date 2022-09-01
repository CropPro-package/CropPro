\name{dung.plot2d}
\alias{dung.plot2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting the output of the \code{\link[CropPro]{LDAcrop.dung}} as a 2D graph
%%  ~~function to do ... ~~
}
\description{This function plots the output of \code{\link[CropPro]{LDAcrop.dung}} as a 2D graph showing function1 (LD1) and function 2 (LD2)

}
\usage{
dung.plot2d(data, ylims = NULL, xlims = NULL, gcols = NULL, gpchs = NULL, col = "black",
pch = 15, site = "Archaeological")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The dataframe containing the results of the \code{\link[CropPro]{LDAcrop.dung}} function
}
  \item{ylims}{The limits of the y axis expressed as c(min, max)
}
  \item{xlims}{The limits of the x axis expressed as c(min, max)
}
  \item{gcols}{The colours of the symbols of the modern crop processing groups expressed as for example
  c("pink", "red", etc)
}
  \item{gpchs}{The symbols of the modern crop processing groups  expressed as for example c(1,2,3,4)
}
  \item{col}{The colour of the archaeobotanical samples
%%     ~~Describe \code{col} here~~
}
  \item{pch}{The symbol of the archaeobotanical samples
%%     ~~Describe \code{pch} here~~
}
  \item{site}{The name of the archaeobotanical samples to appear in the legend
%%     ~~Describe \code{site} here~~
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

\seealso{\code{\link{LDAcrop.dung}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Random data
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
# Use
dung.plot2d(results)

}

