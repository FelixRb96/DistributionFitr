\name{DistributionFitr-package}
\alias{DistributionFitr}
\alias{DistributionFitr-package}
\docType{package}
\title{
  Fitting Multiple Parametric Distributions
}
\description{
A density (or count density) will be fitted parametrically to a
univariate data set via maximum likelihood. The user only enters the
data. The program automatically searches for the parametric family
and its parameters within that best describe the data. The program then returns 
the ten best parameter families including the fitted parameters. 

Examine how several theoretical distibutions describe given univariate data.

The package contains the following functions:
\itemize{
 \item \code{getFamilies} Browsing for distribution families
\item \code{getFamily} Find all distributions in a package
\item \code{getParams} For a single distribution family, parameters are
determined; upper/lower limits and other chraracteristics are extracted
%\item \code{optimParamsDiscrete}: Fits the parameters for a given
%distribution family with known names for the parameters and known value
%range of each parameter item 
\item \code{globalfit} Determines the best parameters for a list of
distribution families
%\item \code{loglik} Returns the loglikelihood function to be optimized item
}
}

\author{
Borui Niklas Zhu, Benedikt Geier, Moritz Kern, Kiril Dik, Moritz Lauff, 
Manuel J. Hentschel, Adrian Heppeler, Niclas Lietzow, Till Freihaut, 
Tim Glockner, Nadine Tampe, Valentin V. Trotha, Leonardo Vela, André Ferdinand, Hélène Peter, Martin Schlather, Yiqi Li
  
Maintainer: Borui Niklas Zhu, \email{bzhu@mail.uni-mannheim.de}   
}

\seealso{
 \code{\link{globalfit}},
 \code{\link{getFamilies}},
 \code{\link{getFamily}},
% \code{\link{loglik}},
% \code{\link{optimParamsDiscrete}},
 \code{\link{getParams}}
}

\examples{ %  library(DistributionFitr) 

# example for globalfit
data <- rnorm(n = 100, mean = 70, sd = 4)
r <- globalfit(data,  cores= if (interactive()) NULL else 1,
               packages = "stats", append_packages = FALSE)
summary(r)

# example for getFamily
str(getFamily("stats"))

# example for getParams
getParams("beta", package = "stats")

}


\keyword{distribution}
\keyword{parameters}
\references{
Zucchini, W. (2000). An introduction to model selection. Journal of mathematical psychology, 44(1), 41-61.

Shao, J. (2003). Mathematical Statistics.

}
