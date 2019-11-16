\name{getFamilies}
\alias{getFamilies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Lists all distribution families with their parameters
}
\description{
   This function returns a list of lists of all distribution families with their parameters
   
   ***INTERNAL**
   iterates over all.packages i
}
\usage{
getFamilies()

***INTERNAL***
getFamilies(all.packages, file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   **internal**
  \item{all.packages}{
      vector of package names to scan for distribution families or
     \code{TRUE} to scan all packages  or \code{FALSE} to scan base packages only
  }
}

\details{
  
}
\value{
  %description of output%
}
\references{
 ~put references to the literature/web site here ~
}
\author{
   AUTHORS
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

getFamilies()
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution }
