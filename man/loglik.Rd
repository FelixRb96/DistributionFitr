\name{loglik}
\alias{loglik}

\title{
  Loglikelihood Function 
}
\description{
  Returns the Loglikelihood Function for a given distribution and its parameters.
}
\usage{
loglik(family, data, fixed, log, lower, upper)
}

\arguments{
  \item{family}{
     list of two elements; family: distribution family callable in the 
     \code{package}, e.g. 'norm'; package: name of the R package which 
     contains the distribution family.
  }
  \item{data}{
     numerical vector of data points.
  }
  \item{fixed}{
     named numeric vector; values of fixed parameters, which are not taken
     into account during optimisation.
  }
  \item{log}{
     logical; if TRUE (default) optimisation is run over log-likelihood, 
     requires log-likelihood as parameter in the selected distribution family.
  }
  \item{lower}{
     named numeric vector; lower bounds of free parameters, if no lower bound: 
     \code{-Inf} 
  }
  \item{upper}{
     named numeric vector; upper bounds of free parameters, if no upper bound: 
     \code{Inf}; names must coincide with \code{lower}.
  }
}


\value{
  \code{loglik} returns a function \code{likelihood} with arguments:
  \item{params}{named list; values of parameters.}

}
\references{
 ~ Log-Likelihood function - Introduction to mathematical statistics. ~
}
\author{
  Niclas Lietzow, Till Freihaut, Leonardo Vela
}
  

%\seealso{
% \code{\link{optimParamsContinuous}}
%}

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
\keyword{distribution}
\keyword{models}
