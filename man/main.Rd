\name{DistributionFitr-package}
\alias{DistributionFitr}
\alias{DistributionFitr-package}
\docType{package}
\title{
  Fitting Multiple Parametric Distributions
}
\description{
A density (or count density) should be parametrically adapted to a univariate data set, per maximum likelihood. The user only enters the data. The program automatically searches for the best parametric family and the best parameters within a family. The program then returns three best parameter families including fitted parameters.

Examine how several theoretical distibutions describe given univariate data.

The package contains the following functions:
\itemize{
 \item \code{getFamilies} Browsing for distribution families item
\item \code{getFamily}  Find all distributions in a package item
\item \code{getParams}for a single distribution family, parameters are determined and classified, as well as upper and lower limits of parameters item
\item \code{optimParamsDiscrete}fits the parameters for a given distribution family with known names for the parameters and known value range of each parameter item
\item \code{globalfit}Determines the best parameters for a list of distribution families and recognizes discrete distributions item
\item \code{loglik} returns the loglikelihood function to be optimized item

}
}

\author{
  
}

\seealso{
 **objects to See Also as \code{\link{help}}, **~
 \code{\link{globalfit}}
 \code{\link{getFamilies}}
 \code{\link{getFamily}}
 \code{\link{loglik}}
 \code{\link{optimParamsDiscrete}}
 \code{\link{getParams}}
}

\examples{
#example for getFamily
library(stats)
getFamily('stats')

#example for getParams
parameter_characteristics <- getParams("beta")

}


\keyword{distributionfitr, distribution, parameters }
\references{
Zucchini, W. (2000). An introduction to model selection. Journal of mathematical psychology, 44(1), 41-61.
}
