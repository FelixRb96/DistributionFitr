\name{globalfit}
\alias{globalfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Detect continuity and fit multiple distributions to given data
}
\description{
  Given data, this function firstly tests multiple criterea for continuity and may transforms discrete data to integer values. After that it calculates for multiple distributions the best fitted parameters and information criteria.
}
\usage{
  globalfit <- function(data, continuity = NULL, method = "MLE")
  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    numerical vector
  }
  \item{continuity}{
    a logical boolean value whether data is continious or not. If no input is given, the data will be tested for continuity.
  }
  \item{method}{
    method for parameter estimation. So far only Maximum-Likelihood is implemented, thus this argument must be \code{"MLE"}.
  }
}

\details{
  This function combines the usage of multiple other functions in this package, mainly get_families and optimParam. The function getFamilies returns a list of distributions and some additional parameters. Those distributions get fitted to the data using the optimParam function.
}

\value{
  For \code{globalfit} a list of following components is returned:
  %\item{continuity}{Prediction whether the data is estimated to be continious or discrete}
  \items{familyName}{Name of the distribution}
  \item{packageName}{Name of the package that includes the distribution}
  
  \item{par}{Optimal parameters as named vector, over which it was optimized.}
  \item{value}{(Log-)Likelihood of optimal parameters}
  \item{convergence}{An integer code. 0 indicates successful completion. Refer to \code{optim} in \code{stats} for further details}
  \item{AIC}{Akaike Information Criterion of optimal parameters}
  \item{BIC}{Bayes Information Criterion of optimal parameters}
  \item{AICc}{Akaike Information Criterion of optimal parameters, corrected for small sample sizes}
}
\references{
  ** references to the literature/web site here **
}
\author{
  Moritz Lauff, Kiril Dik, Nadine Tampe
}
\note{
  **further notes**
}

% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    **objects to See Also as \code{\link{help}}, **~
  }
\examples{
  ##---- Should be DIRECTLY executable !! ----
  ##-- ==>  Define data, use random,
  ##--	or do  help(data=index)  for the standard data sets.
  
  # Example 1
  data <- rnorm(n=100, mean=70, sd= 4)
  estimation <- globalfit(data)
  
  # Alternatively, it is possible to input whether the data is continuous or discrete
  estimation <- globalfit(data, continuity = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution }