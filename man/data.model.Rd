\name{data.model}
\alias{data.model}
\docType{data}
\title{The ethnobotanical dataset from Amorgos
%%   ~~ data name/kind ... ~~
}
\description{ Ethnographic data collected in Greece on the attributes of the weed seeds found in four different stages of crop processing of free-threshing crops. The samples taken from the different stages are shown under the 6 different seed attribute groupings: BHH, BFH, SHH, SHL, SFH SFL
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}

\format{
  A data frame with 216 observations on the following 8 variables.
  \describe{
    \item{\code{PROC}}{The crop proccessing stage written as a number and used as the grouping factor in \code{\link[CropPro]{LDAcrop.pro}} and \code{\link[CropPro]{LDAcrop.dung}}}. 1= Winnowing by-product, 2= Coarse sieve by-product, 3= Fine sieve by-product, 4 =Fine sieve product
    \item{\code{BHH}}{Big, headed and heavy}
    \item{\code{BFH}}{Big, free and heavy}
    \item{\code{SHH}}{Small, headed and heavy}
    \item{\code{SHL}}{Small, headed and light}
    \item{\code{SFH}}{Small, free and heavy}
    \item{\code{SFL}}{Small, free and light}
  }
}

\source{Jones, G. - maybe phd? over the paper we are publishing?
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Jones, G. 1984. Interpretation of archaeological plant remains: ethnographic models from Greece. In (eds) W. van Zeist & W.A. Casparie, \emph{Plants and Ancient Man: Studies in Palaeoethnobotany}. Rotterdam: Balkema, pp 43-61

Jones, G., 1987. A statistical approach to the archaeological identification of crop processing. \emph{Journal of Archaeological Science}, 14(3), pp.311-323.

}

\keyword{datasets}
