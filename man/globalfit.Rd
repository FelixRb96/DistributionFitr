\name{globalfit}
\alias{globalfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Detect continuity and fit multiple distributions to given data
}
\description{
  Given a numerical data vector, this function fits multiple distributions 
  with the maximum likelihood method and returns an object containing the best 
  fitted parameters and information criteria. Refer to \dQuote{Examples} or
  \code{\link{globalfit-class}} on how to format the results.
}
\usage{
globalfit(data, continuity = NULL, method = "MLE",
  verbose = TRUE, packages = "stats",
  append_packages = FALSE, perform_check = TRUE,
  cores = NULL, max_dim_discrete = Inf, sanity_level = 1, ...)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{data}{
    vector of data points.
  }
  \item{continuity}{
    logical; if \code{TRUE}, the data is fitted with continuous distributions.
    If no input is given, the data will be tested for continuity.
  }
  \item{method}{
    method for parameter estimation. So far only Maximum-Likelihood is 
    implemented, thus this argument must
    be \code{"MLE"}.
  }
  \item{verbose}{
   logical; if \code{TRUE}, always show progress and packages from where to fit distributions.
  }
  \item{packages}{
    default: \dQuote{stats}. \cr
    either a character vector with names of packages; \cr
    or a list such as those returned by \command{getFamilies}\cr
    or \code{NULL}, i.e. all families known by this package (recommended)   
  } 
  \item{append_packages}{
   logical; if \code{TRUE} (default) appends packages specified in the 
   argument \code{packages} to the standard search list, if \code{FALSE} 
   \code{globalfit} will use only those packages.
  }
  \item{max_dim_discrete}{
   non-negative integer; distributions with more 
   non-continuous parameters than \code{max_dim_discrete} will not be 
   considered. Manual setting is recommended if calculation speed has to
   be cut down.
  }
  \item{cores}{
   integer; Number of CPU cores to be used in the calculations of best fitted 
   parameters and information criteria
  }
  \item{sanity}{
   either a positive numeric; controls a sanity check where obviously bad fits
   are filtered out. The smaller the number, the stricter the check will be
   executed and the more potential distributions will be rejected.
   Default is 1. \cr
   or \code{FALSE}; sanity check is not carried out. \cr
   (DistributionFitr generally depends on other packages to supply reasonable 
   distribution functions)
  }
  \item{...}{
  Further named arguments that may be passed on to the underlying optimiser \code{\link{optim}},
  such as \code{parscale} or \code{fnscale}.
  }
}

\details{
  If there is no continuity input given, this function first tests via multiple 
  criteria whether the data is   continuously or discretely distributed. 
  Given that information, the related distributions from \code{getFamilies()} 
  are fitted to the data with the maximum likelihood method and information 
  criteria are calculated. For discrete data that is not in the form of 
  integers only, an appropriate linear transformation is applied to guarantee 
  stable optimization.
}

\value{
  \code{globalfit} returns an object of \code{\link{globalfit-class}}.
}

\author{
  Moritz Lauff, Kiril Dik, Nadine Tampe, Borui Niklas Zhu, 
  Benedikt Geier, Moritz Kern
}

\examples{ %  library(DistributionFitr)  ## bitte lassen 
  # Example 1
  data <- rnorm(n = 100, mean = 70, sd = 4)
  r <- globalfit(data, cores = if (interactive()) NULL else 1) 
  summary(r)

  # Example 2
  
  # Alternatively, it is possible to input whether the data is
  # continuous or discrete
  \dontrun{
  globalfit(data, continuity = TRUE, cores = if (interactive()) NULL else 1,
            packages="stats", append_packages = FALSE) 
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
