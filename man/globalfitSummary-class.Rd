\name{globalfitSummary-class}
\alias{globalfitSummary-class}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Class "globalfitSummary"}
\description{The \code{globalfitSummary}-class is the class to handle the 
  entries of the \code{\link{globalfit-class}} and assemble them together
  for summary.}

\section{Objects from the Class}{
  Objects can be created by calls of the form 
  \code{new("globalfitSummary", data, continuity, method, fits, ic)}.
  More comfortably, you may use  apply the summary-method to an object 
  of \code{\link{globalfit-class}}, its result is an 
  \code{globalfitSummary}-object.
}

\section{Slots}{
  \describe{
    \item{data}{vector of data points}
    \item{continuity}{logical; if \code{TRUE}, indicating that the data 
          points come from a continuous distribution;
      if \code{FALSE}, indicating thate these come from a discrite distribtion}
    \item{character}{the method used for the fit.}
    \item{fits}{data frame; sorted by the selected \code{ic}, 
                with the columns family, package and ic}
    \item{ic}{character; indicates in which order \code{fits} is sorted}    
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(x = "globalfitSummary")}: display the object}
    \item{print}{\code{signature(x = "globalfitSummary")}: calls \code{show}}
    }
}
  
\author{
  Moritz Kern
}

\seealso{
  \code{\link{globalfitSummary-class}} \cr
  \code{\link{globalfit}} \cr
  \code{\link{optimParams-class}}
}

\examples{
    r <- summary(globalfit(rnorm(n = 1000, mean=10, sd=1)))
    
    show(r)
    print(r)
}

\keyword{distribution}