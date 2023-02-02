\name{crop.triplot}
\alias{crop.triplot}

\title{Triplot of the different proportions of weeds, grains, and rachis compared to the ethnographic data
}
\description{Trianglar diagram showing the percentages of weeds to grains to rachis of the entered data. The funcation create two side by side plots, with the first showing the ethnographic data :\code{\link{crop.tri.data}} and the second the entered archaeobotanical data.
}
\usage{
crop.triplot(grain,rachis,weeds,pch=5, col="black", bg="black", sample=NULL,
samplelabel="Sample",legendlabel="Samples",cpch=NULL, cbg=NULL, ccol=NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grain}{the column containing the amount of free threshing cereal grain in each sample}
  \item{rachis}{the column containing the amount of free threshing cereal rachis in each sample}
  \item{weeds}{the column containing the amount of weed seeds in each sample}
  \item{pch}{the symbol for the archaeobotanical samples, default is 5}
  \item{col}{the colour for the archaeobotanical samples, default is black}
  \item{bg}{the colour the the background of the symbol, default is black}
  \item{sample}{row number of the sample/s to be labeled}
  \item{samplelabel}{label of the sample/s entered in the sample argument to be shown on graph}
  \item{legendlabel}{the name of the archaebotanical assemablage to appear in the legend}
  \item{cpch}{the symbols of the model data as a list}
  \item{cbg}{the blackground colours of the symbols of the model data}
  \item{ccol}{the colours of the symbols of the model data}
}

\references{ Jones, G. (1990) The application of present-day cereal processing studies to charred archaeobotanical remains. \emph{Circaea} 6(2):91-96
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
}

\examples{
##example data

samples<-c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10")
grain<-runif(10, min= 5, max=100)
rachis<-runif(10, min= 1, max=50)
weeds<-runif(10, min= 50, max=200)
data<-data.frame(samples,grain,rachis,weeds)

##usage

crop.triplot(grain=data$grain, rachis=data$rachis, weeds=data$weeds, col="blue")

## label specific sample

crop.triplot(grain=data$grain, rachis=data$rachis, weeds=data$weeds, col="blue",
sample=2, samplelabel="s2")

}
