\name{install.all}
\alias{install.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Installs all distribution packages
}
\description{
  To make use of the rich parameter data already embedded in DistributionFitr's
  standard search list, this function enables users to install the R-packages
  where those distributions are from.
}
\usage{
install.all(...)
}

%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{...}{
    other arguments passed down to \code{\link{install.packages}}.
  }
}

\details{
  Essentially a wrapper for \code{\link{install.packages}}.
}

\value{
  Invisible \code{NULL}.
}

\author{
  Borui Niklas Zhu
}

\examples{ %  library(DistributionFitr)
  # Example
  install.all()
}
\keyword{DistributionFitr}
\keyword{install}
