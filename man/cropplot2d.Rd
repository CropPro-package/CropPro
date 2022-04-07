\name{cropplot2d}
\alias{cropplot2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ploting the output of the LDA as a 2d graph showing LD1 and LD2
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cropplot2d(x, y, ylims = ylims, xlims = xlims, gcols = NULL, gpchs = NULL, col = "black", pch = 23, site= "site")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The first linear discriminant function
%%     ~~Describe \code{x} here~~
}
  \item{y}{ the second linear discriminant function
%%     ~~Describe \code{y} here~~
}
  \item{ylims}{the limits of the y axis (expressed as c(min, max))
%%     ~~Describe \code{ylims} here~~
}
  \item{xlims}{the limits of the x axis (expressed as c(min, max))
%%     ~~Describe \code{xlims} here~~
}
  \item{gcols}{ the colours for the symbols of the modern crop processing groups ( expressed as c("pink", "red", etc))
%%     ~~Describe \code{gcols} here~~
}
  \item{gpchs}{ the symbols of the modern crop processing groups ( expressed as c( 1,2,3,14,18))
%%     ~~Describe \code{gpchs} here~~
}
  \item{col}{ The colour of the archaeobotanical data's symbol
%%     ~~Describe \code{col} here~~
}
  \item{pch}{the symbol of the archaebotanical data
%%     ~~Describe \code{pch} here~~
}
 \item{site}{the name of the archaeobotanical data showing in the legend
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
##example dataset
LD1<-runif(40, min= -0, max=3)
LD2<-runif(40, min = -2, 4)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1)

#### Use with defaults (will return a black and white graph)
cropplot2d(data$LD1, data$LD2)

##Use with variables
cropplot2d(data$LD1,data$LD2, xlims = c(-5, 5), ylims =c(-5,5), gcols =c("forestgreen", "blue", "skyblue", "orange"), gpch=c(6,7,8,9), col = "darkred", pch = 20 , site ="Example")
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
