\name{getFamily}
\alias{getFamily}

\title{
   Find all distributions in a package
}
\description{
   Given the name of a package, this function finds all distribution families that are provided in the package.
   Distributions are identified by scanning all function names in a package for the pattern r***, p***, q***, d***,
   (*** representing at least one character)
   where at least two functions with the same remainder of the function name (the ***) must be found.
}
\usage{
getFamily(pkg)
}

\arguments{
  \item{pkg}{
     Name of the package as string
  }
}

\value{
   A list of of lists of two named elements:
  For \code{optimParam} a list with components: % MS optimParam ist hier
					% fehl am Platze
  \item{package}{The name of the package provided to the function}
  \item{family}{The name of the function that belongs to a distribution family}
}
\references{
 ~put references to the literature/web site here ~
}
\author{
   Manuel Hentschel, Valentin von Trotha
}

\examples{ %  library(DistributionFitr)  ## MS bitte lassen 
   str(getFamily("stats"))
}

\keyword{distribution}
