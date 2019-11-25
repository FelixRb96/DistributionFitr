\name{getFamilies}
\alias{getFamilies}
\title{
   Lists all distribution families with their parameters
}
\description{
   This function returns a list of lists of all distribution families with their parameters and scans packages if desired.
   
}
\usage{
getFamilies(all.packages, file="all_families.R")

}

\arguments{
   **internal**
  \item{all.packages}{
      vector of package names to scan for distribution families or
     \code{TRUE} to scan all packages  or \code{FALSE} to scan base packages only or \code{MISSING} to return families saved in file
  \item Character string. Path of where to create file.
  }
}

\details{
  This function stores all distribution families in a list of lists with following items
  \item{package} name of package
  \item{family} name of distribution family
  \item{family_info} parameters and various characteristics as given by \code{getParams}
  
}
\value{
  this function always returns the list of lists as specified. 
  Additionally, if \code{all.packages} is not missing, it is saved in an R file as \code{FamilyList <- list(list(...))}
}

\author{
   Tim Glockner, Adrian Heppeler
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

## getFamilies()

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{distribution}
