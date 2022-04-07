\name{cropplot2d}
\alias{cropplot2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ploting the output of the lDA as a 2 d graph showing LD1 and LD2
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cropplot2d(x, y, ylims = c(-5, 5), xlims = c(-5, 5), gcols = NULL, gpchs = NULL, col = "black", pch = 23)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{ylims}{
%%     ~~Describe \code{ylims} here~~
}
  \item{xlims}{
%%     ~~Describe \code{xlims} here~~
}
  \item{gcols}{
%%     ~~Describe \code{gcols} here~~
}
  \item{gpchs}{
%%     ~~Describe \code{gpchs} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
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
function (x, y, ylims = c(-5, 5), xlims = c(-5, 5), gcols = NULL,
    gpchs = NULL, col = "black", pch = 23)
{
    load(file = "CP.data.model.rda")
    discrim_cv <- lda(PROC ~ BHH + BFH + SHH + SHL + SFH + SFL,
        CP.data.model, CV = TRUE)
    model_lda <- lda(PROC ~ BHH + BFH + SHH + SHL + SFH + SFL,
        CP.data.model)
    predictionmodel <- predict(model_lda, CP.data.model)
    dataset <- data.frame(PROC = as.factor(CP.data.model$PROC),
        Classification = predictionmodel$class, predictionmodel$x)
    centroids <- dataset \%>\% group_by(PROC) \%>\% summarise(centroid1 = mean(LD1),
        centroid2 = mean(LD2), centroid3 = mean(LD3))
    if (!is.null(gcols)) {
        gcolours <- gcols
        dataset$colour <- gcolours[as.numeric(dataset$PROC)]
    }
    if (is.null(gcols)) {
        gcolours <- c("black", "black", "black", "black")
        dataset$colour <- gcolours[as.numeric(dataset$PROC)]
    }
    mygroups <- c("Winnowing by-products", "Coarse-sieving by-products",
        "Fine-sieving by-products", "Fine-sieving products")
    dataset$Actual.Group <- mygroups[as.numeric(dataset$PROC)]
    if (!is.null(gpchs)) {
        mypch <- gpchs
        dataset$pch <- mypch[as.numeric(dataset$PROC)]
    }
    if (is.null(gpchs)) {
        mypch <- c(1, 2, 3, 5)
        dataset$pch <- mypch[as.numeric(dataset$PROC)]
    }
    par(mar = c(10, 4, 4, 4))
    plot(dataset$LD1, dataset$LD2, col = paste(dataset$colour),
        pch = as.numeric(as.character(dataset$pch)), ylim = ylims,
        xlim = xlims, xlab = "", ylab = "")
    par(new = T)
    plot(centroids$centroid1, centroids$centroid2, col = "Black",
        pch = 20, ylim = ylims, xlim = xlims, xlab = "", ylab = "")
    par(new = T)
    plot(x, y, col = col, pch = 21, ylim = ylims, xlim = xlims,
        xlab = "Function 1", ylab = "Function 2")
    legend.table <- dataset[!duplicated(dataset$Actual.Group),
        ]
    legend("bottom", c(paste(legend.table$Actual.Group), "SITE",
        "Group centroids"), col = c((paste(legend.table$colour)),
        col, "black"), pch = c((as.numeric(as.character(legend.table$pch))),
        pch, 20), pt.cex = 1, cex = 0.64, bg = "white", xpd = TRUE,
        ncol = 2, inset = c(-0.3, -0.4))
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
