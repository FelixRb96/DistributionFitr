\name{install.packages_DistributionFitr}
\alias{install.packages_DistributionFitr}

\title{
  Installs all packages from DistributionFitr's standard search list.
}
\description{
  DistributionFitr comes with an extensive list of distribution families
  on CRAN together with their characteristics, referred to in our manuals
  as \dQuote{standard search list}. This function is a helper to install all
  the packages which contribute to this list. Usage is exactly like
  \code{install.packages} in base R, but without the need to specify
  which packages to install.
}
\usage{
  install.packages_DistributionFitr(...)
}

\arguments{
  \item{...}{
    any argument that can be passed to base R's \code{install.packages}, except
    the argument \code{pkgs}.
  }
}

\value{
  invisible \code{NULL}.
}
\author{
  Borui Niklas Zhu
}

\examples{
   install.packages_DistributionFitr()
}

\keyword{install}
\keyword{packages}
