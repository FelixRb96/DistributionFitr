\name{globalfit-class} 
\alias{globalfit-class} 
\alias{sort,globalfit-method}
\alias{summary,globalfit-method}
\alias{print,globalfit-method}
\alias{AIC,globalfit-method}
\alias{BIC,globalfit-method}
\alias{hist,globalfit-method}

\title{Class "globalfit"}
\description{The \code{globalfit}-class is the class to handle return objects 
  from \link{globalfit}. It contains for some given data a list of fitted 
  distributions, their estimated parameters and supplementary information.}
\section{Objects from the Class}{
  Objects can be created by calls of the form 
  \code{new("globalfit", data, continuity, method, fits)}.
  More comfortably, you may use the  function \code{\link{globalfit}}.
  The result of these calls is a globalfit object.
}

\section{Slots}{
  \describe{
    \item{data}{vector of data points}
    \item{continuity}{logical; if \code{TRUE}, indicating that the data 
          points come from a continuous distribution; 
	  if \code{FALSE}, indicating that they come from a discrete distribution}
    \item{method}{character; the method used for the fit.}
    \item{fits}{list; of S4-objects of class \code{\link{optimParams-class}}
  }}
}

\section{Methods}{
  \describe{
    \item{sort}{\code{signature(x = "globalfit")}: sorts the results in slot 
                \code{fits} by the information criterium selected.}
    \item{summary}{\code{signature(object = "globalfit")}: 
                        summarizes the object and creates an object of 
                        \code{\link{globalfitSummary-class}}}
    \item{print}{\code{signature(x = "globalfit")}: applies the
                 \code{summary}-method and prints the result.}
%    \item{IC}{\code{signature(x = "globalfit")}: returns the selected 
%                                                 information
%              criteria of the items in \code{fits} slot}
    \item{AIC}{\code{signature(x = "globalfit")}: applies the \code{IC}-method 
               for the Akaike Information Criterium.}
    \item{BIC}{\code{signature(x = "globalfit")}: applies the \code{IC}-method 
               for the Bayes Information Criterium.}
    \item{hist}{\code{signature(x = "globalfit")}: 
                computes a histogram of the given data points and plots it
		together with the density of the estimated best fit}
    }
}
  
\author{
  Moritz Lauff, Kiril Dik, Nadine Tampe, Borui Niklas Zhu, 
  Benedikt Geier, Moritz Kern
}

\seealso{
  \code{\link{globalfitSummary-class}} \cr
  \code{\link{optimParams-class}} \cr
  \code{\link{globalfit}}
}

\examples{
  data <- rnorm(n=100, mean=10, sd= 1)
  r <- globalfit(data, cores= if (interactive()) NULL else 1,
                 packages="stats", append_packages=FALSE)
  
  # r2 <- sort(r, ic='BIC')
    
  #print(r)
  
  summary(r)
  # summary(r2)
  summary(r, ic='AICc', count=7)
      
  hist(r, ic='BIC', which=4)
  
  AIC(r, count=2)
  BIC(r)
}

\keyword{distribution}
