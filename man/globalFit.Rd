\name{globalfit}
\alias{globalfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Detect continuity and fit multiple distributions to given data
}
\description{
  Given a numerical data vector, this function fits multiple distributions by the maximum likelihood method and
  returns a list of the best fitted parameters and information criteria.
}
\usage{
  globalfit(data, continuity = NULL, method = "MLE", progress = TRUE, cores = NULL, ...)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{data}{
    vector of data points.
  }
  \item{continuity}{
    logical; if TRUE, data is fitted with continious distributions. If no input is given, the data will be tested for continuity.
  }
  \item{method}{
    method for parameter estimation. So far only Maximum-Likelihood is implemented, thus this argument must be \code{"MLE"}.
  }
  \item{progress}{
   logical; if TRUE, always show progress
  }
  \item{cores}{
   integer; Number of CPU cores to be used in the calculations of best fitted parameters and information criteria
  }
  \item{...}{
  Further named arguments that can are passed on to optimParamsDiscrete
  }
}

\details{
  If there is no continuity input given, this function firstly tests via multiple criteria whether the data is continious or discrete distributed.
  Given that information, the related distributions from getFamilies() are fitted to the data by the maximum likelihood method and
  information criteria is calculated. For discrete data that is not in the form of integers, there is an implemented linear transformation to 
  guarantee a stable optimization.
}

\value{
  For \code{globalfit} a list of following components is returned:
  
  \item{package}{name of the R package, which contains the distribution family.}
  \item{family}{distribution family callable in the \code{package}}
  \item{estimatedVaues}{Optimal parameters as named vector, over which it was optimized.}  
  
  \item{value}{(Log-)Likelihood of optimal parameters}
  \item{convergence}{An integer code. 0 indicates sucessful completion. Refer to \code{optim} in \code{stats} for further details}
  \item{AIC}{Akaike Information Criterion of optimal parameters}
  \item{BIC}{Bayes Information Criterion of optimal parameters}
  \item{AIC}{Akaike Information Criterion of optimal parameters, corrected for small sample sizes}
  
  \item{continuousParams}{Not yet implemented}
  \item{range}{Not yet implemented}
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
  \dontrun{
  data <- rnorm(n=100, mean=70, sd= 4)
  estimation <- globalfit(data)
  
  # Alternatively, it is possible to input whether the data is continuous or discrete
  estimation <- globalfit(data, continuity = TRUE)
  }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
