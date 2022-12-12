\name{dataorg}
\alias{dataorg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to transform raw archaeobotanical data into the form required for \code{\link[CropPro]{LDAcrop.pro}} and \code{\link[CropPro]{LDAcrop.dung}}.
%%  ~~function to do ... ~~
}
\description{The function transforms raw archaeobotanical data, calculating the squareroot of each species number multiplied by 100, divided by the total weed species within the sample
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dataorg(dataframe, groupcol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{The dataframe with the archaeobotanical samples
}
 \item{codes}{The column containing the grouping variables of the different species (BHH, BFH etc )
}
\item{samples}{The column in which the samples data starts
}
}
\details{
}
\value{
}
\references{see - Jones, G., 1987. A statistical approach to the archaeological identification of crop processing. Journal of archaeological science, 14(3), pp.311-323.

}
\author{
Elizabeth Stroud
}

\examples{
# Example data
species<-c("Chenopodium album" , "Anthemis cotula", "Brassica rapa ssp campestris",
"Raphanus raphanistrum", "Agrostemma githago" , "Poa annua" )
category<-c("SFH", "BHH","SFH","BHH","BFH", "SFL" )
s.1246<-sample(1:3, 6, replace=T)
s.46178<-sample(1:5, 6, replace=T)
s.1<-sample(0:8, 6, replace=T)
s.23<-sample(0:3, 6, replace=T)
s.987<-sample(3:9, 6, replace=T)
dataset<-data.frame(species,category,s.1246,s.46178,s.1,s.23,s.987))
# Usage

data<-dataorg(dataset, codes=2, samples=3)

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
