\name{crop.triplot}
\alias{crop.triplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Triangle plot of the different proportions of weeds, grains, and rachis compared to the Amrgos data
%%  ~~function to do ... ~~
}
\description{Trianglar diagram....
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crop.triplot(grain,rachis,weeds)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grain}{the column containing the amount of free threshing cereal grain}
  \item{rachis}{the column containing the amount of free threshing cereal rachis}
  \item{weeds}{the column containing the amount of weed seeds}
  \item{pch}{the symbol for the archaeobotanical samples, default is 5}
  \item{col}{the colour for the archaeobotanical samples, default is black}
  \item{bg}{the colour the the background of the symbol, default is black}
  \item{sample}{row number of the sample/s to be labeled}
  \item{label}{label of the sample/s entered in the sample argument}
  \item{cpch}{the symbol of the model data}
  \item{cbg}{the blackground colour of the symbol of the model data}
  \item{ccol}{the colour of the symbol of the model data}
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
\references{ Jones, G. (1990) The application of present-day cereal processing studies to charred archaeobotanical remains. Circaea 6(2):91-96
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##example data
samples<-c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10")
grain<-runif(10, min= 5, max=100)
rachis<-runif(10, min= 1, max=50)
weeds<-runif(10, min= 50, max=200)
data<-data.frame(samples,grain,rachis,weeds)
##usage
crop.triplot(grain=data$grain, rachis=data$rachis, weeds=data$weeds, col="red")
## label specific sample
crop.triplot(grain=data$grain, rachis=data$rachis, weeds=data$weeds, col="red", sample=2, label="s2")

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
