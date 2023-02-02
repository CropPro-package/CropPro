\name{LDAcrop.pro}
\alias{LDAcrop.pro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Linear discriminant analysis based on attributes of weed seeds
}
\description{This function conducts linear discriminant analysis. The size, areodymics and tendancy to remain in heads of weed seeds from ethnographic crop processing data are used to create a model against which the archaeobotanical samples are classified. The function provides the classification, posterior probabilities of such classifications, and the discriminant score of the entered samples.
}
\usage{
LDAcrop.pro(x)
}
\arguments{
  \item{x}{The archaeobotanical dataset
}
}
\details{The archaeobotanical dataset needs to have been transformed and organised with columns labelled and in the order of: BHH, BFH, SHH, SHL, SFH, SFL.The first column of the dataframe should be the sample names.Transformation can be done manually following (insert reference) or through the use of \code{\link[CropPro]{crop.dataorg}} which can transform a raw archaeobotanical dataset.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Results table:(note the * asterisked items appear in console output and are used for interpretation, non-asterisked columns provide addition details regarding standardised and unstandardised components
  \item{Samples}{the archaeobotanical sample names form the first column of the entered dataset (x)}
  \item{Class_std*}{The standardised classification of the samples as either 1,2,3,or 4. 1= winnowing by-products, 2= coarse-sieving by-products, 3= fine-seieving by=products and 4= fine sieving products}
  \item{Prob.1_std*}{the standardised posterior probability of the sample being classified as group 1}
  \item{Prob.2_std*}{the standardised posterior probability of the sample being classified as group 2}
  \item{Prob.3_std*}{the standardised posterior probability of the sample being classified as group 3}
  \item{Prob.4_std*}{the standardised posterior probability of the sample being classified as group 4}
  \item{ld1_std}{the standarised linear discriminant score for funcation 1}
  \item{ld2_std}{the standarised linear discriminant score for funcation 2}
  \item{ld3_std}{the standarised linear discriminant score for funcation 3}
  \item{Class}{the unstandarised classification of the samples }
  \item{Prob.1}{the unstandardised posterior probability of the sample being classified as group 4}
  \item{Prob.2}{the unstandardised posterior probability of the sample being classified as group 4}
  \item{Prob.3}{the unstandardised posterior probability of the sample being classified as group 4}
  \item{Prob.4}{the unstandardised posterior probability of the sample being classified as group 4}
  \item{LD1*}{the unstandardised linear discriminant score for function 1}
  \item{LD2*}{the unstandardised linear discriminant score for function 2}
  \item{LD3*}{the unstandardised linear discriminant score for function 3}
Classification table: showing the count and percentage of samples classifed as one of four crop processing groups - as shown in the Class_std column
\item{winnowing by-products}{the count and percentage of samples classified as group 1}
  \item{Coarse-sieving by-products}{the count and percentage of samples classified as group 2}
  \item{Fine-sieving by-products}{the count and percentage of samples classified as group 3}
  \item{Fine-sieving products}{the count and percentage of samples classified as group 4}
  }

\references{PAPER reference
%% ~put references to the literature/web site here ~
}
\author{Eizabeth Stroud
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

BHH<-runif(20, min=0, max=7)
BFH<-runif(20, min=0, max=24)
SHH<-runif(20, min=1, max=13)
SHL<-runif(20, min=0.5, max=17)
SFH<-runif(20, min=1, max=22)
SFL<-runif(20, min=1, max=8)
Samples<-c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13",
"s14","s15","s16","s17","s18","s19","s20")
data<-data.frame(Samples,BHH,BFH,SHH,SHL,SFH,SFL)

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
