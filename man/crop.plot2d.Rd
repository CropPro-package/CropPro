\name{crop.plot2d}
\alias{crop.plot2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting the output of the \code{\link[CropPro]{LDAcrop.pro}} as a 2D graph
%%  ~~function to do ... ~~
}
\description{This function plots the output of \code{\link[CropPro]{LDAcrop.pro}} as a 2D graph showing LD1 and LD2
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crop.plot2d(x, y, ylims=NULL, xlims=NULL, gcols=NULL, gpchs=NULL, col ='black', pch=15,
site="Site")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The first linear discriminant function. If using the output of \code{\link[CropPro]{LDAcrop.pro}} then this will be called x.LD1
%%     ~~Describe \code{x} here~~
}
  \item{y}{The second linear discriminant function. If using the output of \code{\link[CropPro]{LDAcrop.pro}} then this will be called x.LD2
%%     ~~Describe \code{y} here~~
}
  \item{ylims}{The limits of the y axis (expressed as c(min, max))
%%     ~~Describe \code{ylims} here~~
}
  \item{xlims}{The limits of the x axis (expressed as c(min, max))
%%     ~~Describe \code{xlims} here~~
}
  \item{gcols}{Symbol colours of the ethnographical crop processing groups written as c("red", "green"...) etc
%%     ~~Describe \code{gcols} here~~
}
  \item{gpchs}{The symbols of the ethnographical crop processing groups (expressed as c(1, 2, 3, 14, 18))
%%     ~~Describe \code{gpchs} here~~
}
  \item{col}{Symbol colour for archaeobotanical data
%%     ~~Describe \code{col} here~~
}
  \item{pch}{Symbol of the archaeobotanical data
%%     ~~Describe \code{pch} here~~
}
 \item{site}{The name the archaeobotanical data will be labelled as in key
%%     ~~Describe \code{pch} here~~
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
##Example dataset
LD1<-runif(40, min= -0, max=3)
LD2<-runif(40, min = -2, 4)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

###Use with defaults (will return a black and white graph)
crop.plot2d(data$LD1, data$LD2)

##Use with variables
crop.plot2d(data$LD1,data$LD2, xlims = c(-5, 5), ylims =c(-5,5), gcols =c("forestgreen", "blue",
"skyblue", "orange"), gpch=c(6,7,8,9), col = "darkred", pch = 20 , site ="Example")
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
