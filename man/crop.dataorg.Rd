\name{crop.dataorg}
\alias{crop.dataorg}
\title{Function to transform raw archaeobotanical data into the form required for \code{\link[CropPro]{LDAcrop.pro}} and \code{\link[CropPro]{LDAcrop.plus}}.
}
\description{The function transforms raw archaeobotanical data calculating the square root of the weed seeds percentage within each entered sample.

}
\usage{
crop.dataorg(dataframe, codes, samples)
}

\arguments{
  \item{dataframe}{The dataframe with the archaeobotanical samples}
 \item{codes}{The column containing the grouping variables of the different species (BHH, BFH etc)}
\item{samples}{The column in which the samples data starts}}
\details{The function conducts a square-root transformation of the weed seeds percentages as per Jones (1984).
}

\references{
Jones, G. 1984. Interpretation of archaeological plant remains: ethnographic models from Greece. In (eds) W. van Zeist & W.A. Casparie, \emph{Plants and Ancient Man: Studies in Palaeoethnobotany}. Rotterdam: Balkema, pp 43-61

Jones, G., 1987. A statistical approach to the archaeological identification of crop processing. \emph{Journal of Archaeological Science}, 14(3), pp.311-323.
}
\author{
Elizabeth Stroud
}

\examples{
## Example data

species<-c("Chenopodium album" , "Anthemis cotula", "Brassica rapa ssp campestris",
"Raphanus raphanistrum", "Agrostemma githago" , "Poa annua" )
category<-c("SFH", "BHH","SFH","BHH","BFH", "SFL" )
s.1246<-sample(1:3, 6, replace=TRUE)
s.46178<-sample(1:5, 6, replace=TRUE)
s.1<-sample(0:8, 6, replace=TRUE)
s.23<-sample(0:3, 6, replace=TRUE)
s.987<-sample(3:9, 6, replace=TRUE)
dataset<-data.frame(species,category,s.1246,s.46178,s.1,s.23,s.987)

## Usage

data<-crop.dataorg(dataset, codes=2, samples=3)
data
}
