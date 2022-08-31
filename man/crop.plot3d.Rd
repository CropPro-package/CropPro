\name{crop.plot3d}
\alias{crop.plot3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ploting the results of \code{\link[CropPro]{LDAcrop.pro}} as a 3d object
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crop.plot3d(x, y, z, col, gcol, site)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Column of the dateframe containing the discriminant scores of function 1 ( if results from \code{\link[CropPro]{LDAcrop.pro}} it will be called x.LD1)
%%     ~~Describe \code{x} here~~
}
  \item{y}{Column of the dateframe containing the discriminant scores of function 2 ( if results from \code{\link[CropPro]{LDAcrop.pro}} it will be called x.LD2)
%%     ~~Describe \code{y} here~~
}
  \item{z}{Column of the dateframe containing the discriminant scores of function 3 ( if results from \code{\link[CropPro]{LDAcrop.pro}} it will be called x.LD3)
%%     ~~Describe \code{z} here~~
}
  \item{col}{Symbol colour of archaeobotanical data

}
\item{gcol}{Symbol colours of the ethnographical crop processing groups writen as c("red", "green"...) etc

}
\item{site}{The name the archaeobotanical data will be labelled as in key
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
##example dataset

LD1<-runif(40, min= -0, max=3)
LD2<-runif(40, min = -2, max=4)
LD3<-runif(40, min =-4, max=-1)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1, LD2, LD3)

## use

crop.plot3d(data$LD1, data$LD2, data$LD3)

## without defaults

crop.plot3d(data$LD1, data$LD2, data$LD3, gcol = c("black", "grey", "grey48", "grey100"), col = "red", site = "Example")
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
