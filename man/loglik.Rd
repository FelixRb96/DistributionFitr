\name{loglik}
\alias{loglik}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Loglikelihood Function 
}
\description{
  Returns the Loglikelihood Function for a given distribution that depends on the distribution's parameters.
}
\usage{
loglik(family, data, fixed, log, lower, upper)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{family}{
     list of two elements. family: distribution family callable in the \code{package}, e.g. 'norm'; package: name of the R package, which contains the distribution family.
  }
  \item{data}{
     numerical vector.
  }
  \item{fixed}{
     named vector of fixed parameters, which are not taken into account while optimization process 
  }
  \item{log}{
     logical; if TRUE (default) optimization is run over log-likelihood, requires log-likelihood as parameter in selected distribution family
  }
  \item{lower}{
     named vector of lower bounds of free parameters, if no lower bound \code{-Inf} 
  }
  \item{upper}{
     named vector of upper bounds of free parameters, if no upper bound \code{Inf}, names must coincide with \code{lower}
  }
}


\value{
  \code{loglik} returns a function \code{likelihood} with arguments:
  \item{params}{An named list of values.}

}
\references{
 ~ put references to the literature/web site here ~
}
\author{
  Niclas Lietzow, Till Freihaut, Leonardo Vela
}



\section{Warning }{
  \item{loglik does not depend on parameters} {The returned log-likelihood function is a constant function.}
  
  \item{Parameter x with value y outside the boundaries} {At least one parameter value passed on to the log-likelihood function exceeds the distribution's parameter boundaries of upper or lower bound.}
  
  \item{Could be numerically instable} {The respective distribution function lacks the argument \code{log} and was hence called without \code{log = TRUE}. Logging the summands afterwards renders the result numerically instable.}
}  
  

\seealso{
 \code{\link{optimParamsContinuous}}
}

\examples{

# Example 1 for loglik
\dontrun{
family <- list(family="norm", package="stats")
data <- rnorm(10, mean = 7, sd=2)
fixed <- list(sd=2)
log = TRUE
lower <- c(mean=2)
upper <- c(mean=10)

likelihood <- loglik(family = family, data = data, fixed = fixed,
              log = log, lower = lower, upper = upper)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{Maximum-Likelihood}
\keyword{Log-Likelihood}
