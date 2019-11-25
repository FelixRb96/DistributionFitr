\name{summary}
\alias{summary}
\alias{print}
  
  \title{Summarizing Results of global Distribution Fit}
\description{
  \code{summary} method for class "globalfit"; it returns the best results of fitting the given data to several distributions together with 
  the optimized parameters.
}

\usage{
  # S4 method for class 'globalfit'
  summary(object, which=1, count=10, ic = c('AIC'))
  
  # S4 method for class 'globalfit'
  print(object, ...)
}

\arguments{
  \item{object}{an object of class "globalfit", usually as a result of call to \link{globalfit}}
  \item{which}{vector indicating of which (best) results the parameters should be printed}
  \item{count}{integer, determines how many results are turned back as best fits}
  \item{ic}{which information criteria used for the ranking, requires one of the following \code{AIC}, \code{BIC}, \code{AICc}}
  \item{...}{Further named arguments}
}

\details{
  %TBD: currently only placeholder
  Returns a summary of the optimisation results stored in globalfit.
    
} 
  
\value{
  An object of class \code{summary.globalfit} which contains the optimisation results and can be printed in a nice form.
  % description of output%
  % needs to be coded
  % afterwards docu added
}

\author{
   Niclas Lietzow, Moritz Kern
}
\note{
  **further notes**
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 **objects to See Also as \code{\link{help}}, **~
}
\examples{
##--	or do  help(data=index)  for the standard data sets.

% TODO:  to slow so far, maybe just stats package?
\dontrun{
  r <- globalfit(rnorm(n = 1000, mean=10, sd=1))
  summary(r)  
  summary(r, count=5, which=2)
  }
}
\keyword{distribution}
