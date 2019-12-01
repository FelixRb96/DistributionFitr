\name{summary}
\alias{summary,globalfit-method}% MS
\alias{sort,globalfit-method}% MS
\alias{print,globalfit-method}% MS
\alias{hist,globalfit-method}% MS
\alias{print,globalfitSummary-method}% MS
\alias{show,globalfitSummary-method}% MS
 
  \title{Summarizing Results of global Distribution Fit}
\description{
  \code{summary} method for class "globalfit"; it returns the best results of fitting the given data to several distributions together with 
  the optimized parameters.
}

\usage{% MS komplett neu
  \S4method{summary}{globalfit}(object, count=10, ic = 'AIC')
  \S4method{print}{globalfit}(x)
  \S4method{sort}{globalfit}(x, decreasing=FALSE, ic='AIC') 
  \S4method{show}{globalfitSummary}(object)

}

\arguments{
 
  \item{object,x}{an object of class "globalfit", usually as a result of
    call to \link{globalfit}}
  \item{decreasing}{always \code{TRUE}} % MS
  \item{count}{integer, determines how many results are turned back as best fits}
  \item{ic}{character. It indicates which information criteria used for the ranking, requires one of the following \code{AIC}, \code{BIC}, \code{AICc}}
}

\details{
  %TBD: currently only placeholder
  Returns a summary of the optimisation results stored in globalfit.
    
} 
  
\value{
  An object of class \code{globalfitSummary} which contains the optimisation results and can be printed in a nice form.
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
\examples{ %  library(DistributionFitr)  ## MS bitte lassen 
##--	or do  help(data=index)  for the standard data sets.

r <- globalfit(rnorm(n = 100, mean=10, sd=1))
summary(r)  
summary(r, count=5, ic='BIC')
}
\keyword{distribution}
