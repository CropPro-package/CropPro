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
crop.triplot(grain,rachis,weeds,pch=5, col="black", bg="black", sample=NULL, cpch=NULL, cbg=NULL, ccol=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grain}{The column containing the amount of free threshing cereal grain}
  \item{rachis}{the column containing the amount of free threshing cereal rachis}
  \item{weeds}{the column containing the amount of weed seeds}
  \item{pch}{ the symbol for the archaeobotanical samples}
  \item{col}{ the colour for the archaeobotanical samples}
  \item{bg}{the colour the the background of the symbol}
  \item{label}{row number of a the sample/s to be labeled}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
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
