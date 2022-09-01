\name{data.model}
\alias{data.model}
\docType{data}
\title{The ethnobotanical dataset
%%   ~~ data name/kind ... ~~
}
\description{ The attributes of the weed seeds classified under the 5 different seed attribute groupings
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("data.model")}
\format{
  A data frame with 216 observations on the following 8 variables.
  \describe{
    \item{\code{NO}}{Running number of samples}
    \item{\code{PROC}}{The crop proccessing stage written as a number and used as the grouping factor in \code{\link[CropPro]{LDAcrop.pro}} and  \code{\link[CropPro]{LDAcrop.dung}}}. 1= Winnowing by-products, 2= Coarse-sieving by-products, 3= Fine-sieving by-products, 4 =Fine-sieving products
    \item{\code{BHH}}{Big, headed and heavy}
    \item{\code{BFH}}{Big, free and heavy}
    \item{\code{SHH}}{Small, headed and heavy}
    \item{\code{SHL}}{Small, headed and light}
    \item{\code{SFH}}{Small, free and heavy}
    \item{\code{SFL}}{Small, free and light}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{Jones, G., 1987. A statistical approach to the archaeological identification of crop processing. Journal of archaeological science, 14(3), pp.311-323.
%%  ~~ possibly secondary sources and usages ~~
}
\examples{

}
\keyword{datasets}
