\name{crop.plus_plot2D}
\alias{crop.plus_plot2D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A two dimensional plot (xy scatterplot) of linear discriminant scores from \code{\link[CropPro]{LDAcrop.plus}} in comparison to the ethnographic model's scores
}
\description{This function plots the  linear discriminant scores from \code{\link[CropPro]{LDAcrop.plus}} as a 2D graph.
}
\usage{
crop.plus_plot2D(data,Func1=1, Func2=2, ylims=NULL, xlims=NULL, gcols=NULL,gbg=NULL,
gpchs=NULL, col ='black', bg='black', pch=15, site="Archaeological", label=NULL, pos=c(0,-0.3))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The data frame containing the results of the \code{\link[CropPro]{LDAcrop.plus}} function
}
  \item{Func1}{The linear discriminant function to be plotted on the x axis
}
  \item{Func2}{The linear discriminant function to be plotted  on the y axis
}
  \item{ylims}{The limits of the y axis expressed as c(min, max)
}
  \item{xlims}{The limits of the x axis expressed as c(min, max)
}
  \item{gcols}{The colours of the symbols of the modern crop processing groups expressed as for example c("pink", "red", etc)
}
\item{gbg}{The colour of the background of the symbols of the ethnographical crop processing groups (expressed  c("red", "green"...) etc. The default produces a black and white plot)}

  \item{gpchs}{The symbols of the modern crop processing groups  expressed as for example c(1,2,3,4)
}
  \item{col}{The colour of the archaeobotanical samples
}
 \item{bg}{The background(fill) colours for open plot symbols with a pch = 21:25, see \link[graphics]{points}}
  \item{pch}{The symbol of the archaeobotanical samples
}
  \item{site}{The name of the archaeobotanical samples to appear in the legend
}
\item{label}{The name of the sample(s) to be labelled on the graph
}
\item{pos}{The position of the label (format c(x,y))
}
}
\details{The data frame entered as argument x can be the output of \code{\link[CropPro]{LDAcrop.plus}} or from manual transformation. However the data frame must have column labelled LD1, LD2, LD3 containing the linear discriminant scores
}

\references{ PAPER reference

}
\author{Elizabeth Stroud
%%  ~~who you are~~
}

\seealso{\code{\link{LDAcrop.plus}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Random data

BHH<-runif(40, min=0, max=7)
BFH<-runif(40, min=0, max=24)
SHH<-runif(40, min=1, max=13)
SHL<-runif(40, min=0.5, max=17)
SFH<-runif(40, min=1, max=22)
SFL<-runif(40, min=1, max=8)
Sample<-sample(1:40, 40, replace=FALSE)
data<-data.frame(Sample,BHH,BFH,SHH,SHL,SFH,SFL)
results<-LDAcrop.plus(data)

## Usage

crop.plus_plot2D(results)

##Plot different LD function

crop.plus_plot2D(results, Func1=2, Func2=3)

}

