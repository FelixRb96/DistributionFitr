\name{loglik}
\alias{loglik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Loglikelihood Function 
}
\description{
  Returns the Loglikelihood Function for a given distribution that depends on the      distribution's parameters.
}
\usage{
loglik(arg_opt, family, data, fixed, log, lower, upper)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arg_opt}{
     character vector
  }
  \item{family}{
     list of two elements. family: distribution family callable in the \code{package}, e.g. 'norm'; package: name of the R package, which contains the distribution family.
  }
  \item{data}{
     XXX
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

\details{
  ** If necessary, more details than the description above **
}
\value{
  For \code{loglik} a list with components:
  \item{par}{Optimal parameters as named vector, over which it was optimized.}
  \item{value}{(Log-)Likelihood of optimal parameters}
  \item{convergence}{An integer code. 0 indicates sucessful completion. Refer to \code{optim} in \code{stats} for further details}
  \item{AIC}{Akaike Information Criterion of optimal parameters}
  \item{BIC}{Bayes Information Criterion of optimal parameters}
  \item{AIC}{Corrected Akaike Information Criterion of optimal parameters}
}
\references{
 ~ put references to the literature/web site here ~
}
\author{
  Till Freihaut, Leonardo Vela
}
\note{
  **Raum für Notizen**
}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 **objects to See Also as \code{\link{help}}, **~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

# Example 1 for loglik
arg_opt <- c("mean")
family <- "norm"
data <- rnorm(10, mean = 7, sd=2)
fixed <- list(sd=2)
log = TRUE
lower <- 2
upper <- 10

out <- loglik(arg_opt = arg_opt, family = family, data = data, fixed = fixed,
              log = log, lower = lower, upper = upper)

test <- NULL
for(i in 1:9) {
  test[i] <- out(i+1)
}
plot(2:10,test, xlab = "mean", ylab = "likelihood")
out(7)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution }
