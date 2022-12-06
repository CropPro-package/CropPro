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
crop.plot2d(x,  ylims=NULL, xlims=NULL, gcols=NULL, gpchs=NULL, col ='black', pch=15,
site="Site")
}

\arguments{
  \item{x}{dataframe containing columns with the LD1, LD2 and LD3 scores - this can be the output of \code{\link[CropPro]{LDAcrop.pro}, see details for futher information}
}
  \item{Func1}{The first linear discriminant function to be ploted, the default is 1
}
  \item{Func2}{The second linear discriminant function to be ploted (1,2 or 3), the default is 2
}
  \item{ylims}{The limits of the y axis (expressed as c(min, max))
}
  \item{xlims}{The limits of the x axis (expressed as c(min, max))
}
  \item{gcols}{Symbol colours of the ethnographical crop processing groups written as c("red", "green"...) etc. The default produces a black and white plot
}
  \item{gpchs}{The symbols of the ethnographical crop processing groups (expressed as c(1, 2, 3, 14, 18))
}
  \item{col}{Symbol colour for archaeobotanical data
}
  \item{pch}{Symbol of the archaeobotanical data
}
 \item{site}{The name the archaeobotanical data will be labelled as in the legend
}
}
\details{
The dataframe entered as argument X can be the output of \code{\link[CropPro]{LDAcrop.pro}} or from manual normalizing. However the dataframe must have column labeled LD1, LD2, LD2, and containing the linear discrimanant scores
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
LD3<-runif(40, min = 2, 4)
Study<-sample(1:3, 40, replace=T)
data<-data.frame(Study,LD1, LD2, LD3)

###Use with defaults (will return a black and white graph)
crop.plot2d(data)

###Changing discriminant function ploted
crop.plot2d(data, Func1=2, Func2=3)

##Use with colour and symbol variables
crop.plot2d(data, xlims = c(-5, 5), ylims =c(-5,5), gcols =c("forestgreen", "blue",
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
