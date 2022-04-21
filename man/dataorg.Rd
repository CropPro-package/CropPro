\name{dataorg}
\alias{dataorg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{function to transform raw archaeobotanical data into the form required for LDAcrop.pro.
%%  ~~function to do ... ~~
}
\description{ the function transforms raw archaeobotnical data, calculating the squareroot of each species number muliplited by 100, divided by the total weed species within the sample
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dataorg(dataframe, groupcol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{ the data to be entered
}
 \item{groupcol}{ the column containing the grouping variables of the different species (BHH, BFH etc )
}
}
\details{
}
\value{
}
\references{ see - references

}
\author{
Elizabeth Stroud
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
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
