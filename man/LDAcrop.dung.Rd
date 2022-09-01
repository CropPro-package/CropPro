\name{LDAcrop.dung}
\alias{LDAcrop.dung}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Linear discriminant analysis based on attributes of weed seeds to classify taphonomic pathway ( processing vs dung)
%%  ~~function to do ... ~~
}
\description{This function conducts linear discriminant analysis classification using ethnographic data of crop processing sequence to classify archaeobotanical samples on the basis of weed seeds attributes. This function is a modification of \code{\link[CropPro]{LDAcrop.pro}}, in that it uses the entered archaeobotanical data as well as the ethnographic data to create the model. Data is then reclassified against that model, allowing the archaeobotanical samples to be classified as 1 of five groups: archaeological, winnowing by-products, coarse sieve by-products, fine sieve by-products, fine sieve products. The function provides the classification of each sample (class), as well as the discriminant scores of each sample (LD1, LD2, LD3) and the posterior probabilities. The function also produces a classification table showing the classification results and the percentage of samples classified for each group.
}
\usage{
LDAcrop.dung(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The archaeobotanical dataset
%%     ~~Describe \code{x} here~~
}
}
\details{The dataset needs to be organised in the way described in \code{\link[CropPro]{LDAcrop.pro}} or as the output of \code{\link[CropPro]{dataorg}}
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
## Create random dataset for example
BHH<-runif(40, min=0, max=7)
BFH<-runif(40, min=0, max=24)
SHH<-runif(40, min=1, max=13)
SHL<-runif(40, min=0.5, max=17)
SFH<-runif(40, min=1, max=22)
SFL<-runif(40, min=1, max=8)
NO<-sample(1:40, 40, replace=F)
PROC<-runif(40,min=0,max=0)
data<-as.data.frame(cbind(NO, PROC,BHH,BFH,SHH,SHL,SFH,SFL))

## function usage
LDAcrop.dung(data)

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
