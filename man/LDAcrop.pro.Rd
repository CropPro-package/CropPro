\name{LDAcrop.pro}
\alias{LDAcrop.pro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Linear discriminant analysis for crop processing
%%  ~~function to do ... ~~
}
\description{LDA of crop processing model
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LDAcrop.pro(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ archaeobtanical dataset
%%     ~~Describe \code{x} here~~
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
\author{
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
##Create random dataset for examples
BHH<-runif(40, min=0, max=7)
BFH<-runif(40, min=0, max=24)
SHH<-runif(40, min=1, max=13)
SHL<-runif(40, min=0.5, max=17)
SFH<-runif(40, min=1, max=22)
SFL<-runif(40, min=1, max=8)
Sample<-sample(1:40, 40, replace=F)
data<-as.data.frame(cbind(BHH,BFH,SHH,SHL,SFH,SFL,Sample))

#Use
LDAcrop.pro(data)

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
