\name{globalfit}
\alias{globalfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Detect continuity and fit multiple distributions to given data
}
\description{
  Given a numerical data vector, this function fits multiple distributions with the maximum likelihood method and
  returns a list of the best fitted parameters and information criteria.
}
\usage{
  globalfit(data, continuity = NULL, method = "MLE", progress = TRUE,
	    stats_only = TRUE, packages = NULL, append_packages = TRUE,
	    perform_check = TRUE, cores = NULL, max_dim_discrete, ...)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{data}{
    vector of data points.
  }
  \item{continuity}{
    logical; if \code{TRUE}, data is fitted with continuous distributions. If no input is given, the data will be tested for continuity.
  }
  \item{method}{
    method for parameter estimation. So far only Maximum-Likelihood is implemented, thus this argument must be \code{"MLE"}.
  }
  \item{progress}{
   logical; if \code{TRUE}, always show progress
  }
  \item{stats_only}{
   logical; if \code{TRUE} (default) only distributions from the \code{\link{stats}}-package will be considered. If \code{FALSE} all distributions in our standard package will be fitted over, unless following arguments below are specified otherwise.
  }
  \item{packages}{
   either a character vector with names of packages; \\
   or a list such as those returned by \code{getFamilies} (recommended)
  } 
  \item{append_packages}{
   logical; if \code{TRUE} (default) appends packages specified in the equally-named argument above to the standard search list, if \code{FALSE} \code{globalfit} will use only those packages.
  }
  \item{perform_check}{
   logical; if \code{TRUE} (default) obviously bad fits will be filtered out (DistributionFitr generally depends on other packages to supply reasonable distribution functions).
  }
  \item{max_dim_discrete}{
   non-negative integer; as a final step, all distributions with more non-continuous parameters than \code{max_dim_discrete} will not be considered. Manual setting is recommended if calculation speed has to be cut down.
  }
  \item{cores}{
   integer; Number of CPU cores to be used in the calculations of best fitted parameters and information criteria
  }
  \item{...}{
  Further named arguments that can are passed on to optimParamsDiscrete
  }
}

\details{
  If there is no continuity input given, this function first tests via multiple criteria whether the data is continuously or discretely distributed.
  Given that information, the related distributions from \code{getFamilies()} are fitted to the data with the maximum likelihood method and
  information criteria are calculated. For discrete data that is not in the form of integers only, an appropriate linear transformation is applied to 
  guarantee stable optimization.
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
  Moritz Lauff, Kiril Dik, Nadine Tampe, Borui Niklas Zhu
}
\note{
  **further notes**
}

% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    **objects to See Also as \code{\link{help}}, **~
  }
\examples{
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
