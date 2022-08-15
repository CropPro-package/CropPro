\name{dungplot2d}
\alias{dungplot2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dung plot...
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dungplot2d(data, x, y, ylims = NULL, xlims = NULL, gcols = NULL, gpchs = NULL, col = "black", pch = 15, site = "site")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ dataframe
%%     ~~Describe \code{data} here~~
}
  \item{x}{the colum to be ploted
%%     ~~Describe \code{x} here~~
}
  \item{y}{ column to be plotted
%%     ~~Describe \code{y} here~~
}
  \item{ylims}{limits
%%     ~~Describe \code{ylims} here~~
}
  \item{xlims}{ limits
%%     ~~Describe \code{xlims} here~~
}
  \item{gcols}{ col
%%     ~~Describe \code{gcols} here~~
}
  \item{gpchs}{ pch
%%     ~~Describe \code{gpchs} here~~
}
  \item{col}{col
%%     ~~Describe \code{col} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{site}{
%%     ~~Describe \code{site} here~~
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
function (data, x, y, ylims = NULL, xlims = NULL, gcols = NULL,
    gpchs = NULL, col = "black", pch = 15, site = "site")
{
    data.model <- data.frame(data.model)
    archdata <- data[c(11:18)]
    model.arch <- rbind(data.model, archdata)
    discrim_cv <- lda(PROC ~ BHH + BFH + SHH + SHL + SFH + SFL,
        model.arch, CV = TRUE)
    model_lda <- lda(PROC ~ BHH + BFH + SHH + SHL + SFH + SFL,
        model.arch)
    predictionmodel <- predict(model_lda, model.arch)
    dataset <- data.frame(PROC = as.factor(model.arch$PROC),
        Classification = predictionmodel$class, predictionmodel$x)
    centroids <- functionalAt \%>\% group_by(PROC) \%>\% summarise(centroid1 = mean(LD1),
        centroid2 = mean(LD2), centroid3 = mean(LD3), centroid4 = mean(LD4))
    if (!is.null(gcols)) {
        gcolours <- gcols
        dataset$colour <- gcolours[as.numeric(dataset$PROC)]
    }
    if (is.null(gcols)) {
        gcolours <- c("black", "black", "black", "black")
        dataset$colour <- gcolours[as.numeric(dataset$PROC)]
    }
    mygroups <- c("Archaeological", "Winnowing by-products",
        "Coarse-sieving by-products", "Fine-sieving by-products",
        "Fine-sieving products")
    dataset$Actual.Group <- mygroups[as.numeric(dataset$PROC)]
    if (!is.null(gpchs)) {
        mypch <- gpchs
        dataset$pch <- mypch[as.numeric(dataset$PROC)]
    }
    if (is.null(gpchs)) {
        mypch <- c(1, 2, 3, 5)
        dataset$pch <- mypch[as.numeric(dataset$PROC)]
    }
    x.value <- unlist((x))
    m.value <- unlist(dataset$LD1)
    xmin <- min(x.value)
    xmax <- max(x.value)
    mmin <- min(m.value)
    mmax <- max(m.value)
    if (xmin > mmin) {
        xmin <- mmin
    }
    else {
        xmin <- xmin
    }
    if (xmax > mmax) {
        xmax <- xmax
    }
    else {
        xmax <- mmax
    }
    if (length(xlims)) {
        xlim <- xlims
    }
    else {
        xlim <- c(xmin - 0.5, xmax + 0.5)
    }
    y.value <- unlist((y))
    ym.value <- unlist(dataset$LD2)
    ymin <- min(y.value)
    ymax <- max(y.value)
    ymmin <- min(ym.value)
    ymmax <- max(ym.value)
    if (ymin > ymmin) {
        ymin <- ymmin
    }
    else {
        ymin <- ymin
    }
    if (ymax > ymmax) {
        ymax <- ymax
    }
    else {
        ymax <- ymmax
    }
    if (length(ylims)) {
        ylim <- ylims
    }
    else {
        ylim <- c(ymin - 0.5, ymax + 0.5)
    }
    par(mar = c(10, 4, 4, 4))
    plot(dataset$LD1, dataset$LD2, col = paste(dataset$colour),
        pch = as.numeric(as.character(dataset$pch)), ylim = ylim,
        xlim = xlim, xlab = "", ylab = "")
    par(new = T)
    plot(centroids$centroid1, centroids$centroid2, col = "Black",
        pch = 19, ylim = ylim, xlim = xlim, xlab = "", ylab = "")
    par(new = T)
    plot(x, y, col = col, pch = pch, ylim = ylim, xlim = xlim,
        xlab = "Function 1", ylab = "Function 2")
    legend.table <- dataset[!duplicated(dataset$Actual.Group),
        ]
    legend("bottom", c(paste(legend.table$Actual.Group), site,
        "Group centroids"), col = c((paste(legend.table$colour)),
        col, "black"), pch = c((as.numeric(as.character(legend.table$pch))),
        pch, 19), pt.cex = 1, cex = 0.64, bg = "white", xpd = TRUE,
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
