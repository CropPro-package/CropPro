\name{crop.plot2D}
\alias{crop.plot2D}

\title{A two dimensional plot (xy scatterplot) of linear discriminant scores from \code{\link[CropPro]{LDAcrop.pro}}
}
\description{This function plots the linear discriminant scores from \code{\link[CropPro]{LDAcrop.pro}} as a 2D graph.

}
\usage{
crop.plot2D(x,ylims=NULL,xlims=NULL,gcol=NULL, gbg=NULL,gpch=NULL, col ='black', bg="black", pch=15,site="Site", Func1=1, Func2=2, label=NULL, pos=c(0,-0.3), lab.col= "black", lab.cex = 0.8)
}

\arguments{
  \item{x}{dataframe containing columns with the LD1, LD2 and LD3 scores - this can be the output of \code{\link[CropPro]{LDAcrop.pro}, see details for further information}}
  \item{ylims}{The limits of the y axis (expressed as c(min, max))}
  \item{xlims}{The limits of the x axis (expressed as c(min, max))}
  \item{gcol}{Symbol colours of the ethnographical crop processing groups written as c("red", "green"...) etc. The default produces a black and white plot}
  \item{gbg}{The colour of the background of the symbols of the ethnographical crop processing groups (expressed  c("red", "green"...) etc. The default produces a black and white plot)}
  \item{gpch}{The symbols of the ethnographical crop processing groups (expressed as c(1, 2, 3, 14, 18))}
  \item{col}{Symbol colour for archaeobotanical data}
  \item{bg}{The background(fill) colours for open plot symbols with a pch = 21:25, see \link[graphics]{points}}
  \item{pch}{Symbol of the archaeobotanical data}
  \item{site}{The name the archaeobotanical data will be labelled as in the legend}
  \item{Func1}{The linear discriminant function to be plotted on the x axis- the default is 1}
  \item{Func2}{The linear discriminant function to be plotted on the y axis- the default is 2}
  \item{label}{The name of the sample(s) to be labelled on the graph}
  \item{pos}{The position of the label (format c(x,y))
}
\item{lab.col}{need to fill
}
\item{lab.cex}{need to fill
}

}
\details{
The data frame entered as argument x can be the output of \code{\link[CropPro]{LDAcrop.pro}} or from manual normalizing. However the data frame must have column labelled LD1, LD2, LD3 containing the linear discriminant scores for function 1, 2 and 3
}

\references{PAPER reference
%% ~put references to the literature/web site here ~
}
\author{Elizabeth Stroud
%%  ~~who you are~~
}

\examples{
##Example dataset

LD1<-runif(40, min= -0, max=3)
LD2<-runif(40, min = -2, 4)
LD3<-runif(40, min = 2, 4)
Study<-sample(1:3, 40, replace=TRUE)
data<-data.frame(Study,LD1, LD2, LD3)

###Use with defaults (will return a black and white graph)

crop.plot2D(data)

###Changing whcih discriminant function is ploted

crop.plot2D(data, Func1=2, Func2=3)

##Use with colour and symbol variables

crop.plot2D(data, xlims = c(-5, 5), ylims =c(-5,5), gcol =c("forestgreen", "blue",
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
