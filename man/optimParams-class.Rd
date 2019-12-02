\name{optimParams-class}
\alias{optimParams-class}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Class "optimParams"}
\description{The \code{optimParams}-class is the class to handle the fits-slot
  of the \code{\link{globalfit-class}} It contains for some given data
  the optimization results and their characteristics.}

\section{Objects from the Class}{
  Objects can be created by calls of the form 
  \code{new("optimParams", family, package, estimatedValues, AIC, BIC, AICc, sanity)}.
  More comfortably, you may use the  function \code{\link{globalfit}}.
  The list elements in the fits-slot of the result is an optimParams object.
}

\section{Slots}{
  \describe{
    \item{family}{a character indicating the family name in the package; 
                  e.g. norm.}
    \item{package}{a character indicating the package, where the family 
                   was found, e.g. stats.}
    \item{estimatedValues}{a named vector indicating the estimated 
                           parameters of the family.}
    \item{AIC}{numeric; Akaike Information Criteria.}
    \item{BIC}{numeric; Bayes Information Criteria.}
    \item{AICc}{numeric; Corrected Akaike Information Criteria.}
    \item{sanity}{named list with numeric items \code{hist_check} und 
                  \code{int_check} and boolean item \code{good} as a 
                  result of the sanity check.}
  }}
  

\author{
  Moritz Kern
}

\seealso{
  \code{\link{globalfitSummary-class}} \cr
  \code{\link{globalfit}}
}

\examples{
    r <- globalfit(rnorm(n = 1000, mean=10, sd=1))
  
    # optimParams of best fit
    r@fits[[1]]
    
}

\keyword{distribution}