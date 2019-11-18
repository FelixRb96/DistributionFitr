\name{getFamily}
\alias{getFamily}
%- Also NEED an '\alias' for EACH other topic documented here.
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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{
     Name of the package as string
  }
}

\details{
  ** If necessary, more details than the description above **
}
\value{
   A list of of lists of two named elements:
  For \code{optimParam} a list with components:
  \item{package}{The name of the package provided to the function}
  \item{family}{The name of the function that belongs to a distribution family}
}
\references{
 ~put references to the literature/web site here ~
}
\author{
   Manuel Hentschel, Valentin von Trotha
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

# Example 1 for optimParam
## library(stats)
## getFamily('stats')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution }
