\name{cropplot3dpoints}
\alias{cropplot3dpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ploting the results of the LDA as a 3 d object
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cropplot3dpoints(x, y, z, Col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{z}{
%%     ~~Describe \code{z} here~~
}
  \item{Col}{
%%     ~~Describe \code{Col} here~~
}
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
function (x, y, z, Col)
{
    library(rgl)
    load(file = "CP.data.model.rda")
    discrim_cv <- lda(PROC ~ BHH + BFH + SHH + SHL + SFH + SFL,
        CP.data.model, CV = TRUE)
    model_lda <- lda(PROC ~ BHH + BFH + SHH + SHL + SFH + SFL,
        CP.data.model)
    predictionmodel <- predict(model_lda, CP.data.model)
    functionalAt <- data.frame(PROC = as.factor(CP.data.model$PROC),
        Classification = predictionmodel$class, predictionmodel$x)
    centroids <- functionalAt \%>\% group_by(PROC) \%>\% summarise(centroid1 = mean(LD1),
        centroid2 = mean(LD2), centroid3 = mean(LD3))
    open3d()
    par3d(windowRect = c(100, 100, 612, 612))
    plot3d(functionalAt$LD1, functionalAt$LD2, functionalAt$LD3,
        col = functionalAt$PROC, type = "s", size = 0.9, xlab = "LD1",
        ylab = "LD2", zlab = "LD3")
    pch3d(x, y, z, pch = 8, col = Col, cex = 0.2)
    shapelist3d(cube3d(), x = centroids$centroid1, y = centroids$centroid2,
        z = centroids$centroid3, col = "black", size = 0.2)
    legend3d("topright", c("Winnowing by-products", "Coarse-sieving by-products",
        "Fine-sieving by-products", "Fine-sieving products",
        "SITE", "Group centroids"), pch = c(16, 16, 16, 16, 8,
        15), col = c(1, 2, 3, 4, Col, "black"), cex = 1)
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
