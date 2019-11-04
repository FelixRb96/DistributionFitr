\name{optimParam_plot}
\alias{optimParam_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Show Convergence Behaviour of Parameter Optimisation for a Distribution Family
}
\description{
  Given data and a distribution family, this function carries out optimisation w.r.t. parameters like \code{optimParam}, but its purpose is showing a two-dimensional plot of the convergence behaviour.
}
\usage{
optimParam_plot(plot = TRUE, dim1, dim2, fixed, data, family, lower, upper, defaults)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot}{
     logical; if TRUE (default) the plot is produced, if FALSE standard \code{optimParam} is carried out.
  }
  \item{dim1}{
     character; parameter to plot on the x-axis. Has to be set if \code{plot} is set to TRUE.
  }
  \item{dim2}{
     character; parameter to plot on the y-axis. Has to be set if \code{plot} is set to TRUE.
  }
  \item{fixed}{
     named vector of fixed parameters and their values, which are not taken into account while optimization process. \\
     If \code{plot} is set to TRUE, parameters specified as \code{dim1} or \code{dim2} can not be fixed, and all other parameters except those two have to be fixed along with a valid value.
  }
  \item{data}{
     vector of data points
  }
  \item{family}{
     list of two elements. family: distribution family callable in the \code{package}, e.g. 'norm'; package: name of the R package, which contains the distribution family.
  }
  \item{lower}{
     named vector of lower bounds of free parameters, if no lower bound \code{-Inf} 
  }
  \item{upper}{
     named vector of upper bounds of free parameters, if no upper bound \code{Inf}, names must coincide with \code{lower}
  }
  \item{start_parameters}{
     named vector of starting parameters for optimisation, names must coincide with \code{lower} and \code{upper}
  }
  \item{method}{
     so far only Maximum-Likelihood is implemented, thus argument must be \code{"MLE"}.
  }
  \item{log}{
     logical; if TRUE (default) optimization is run over log-likelihood, requires log-likelihood as parameter in selected distribution family
  }
  \item{debug_error}{
     logical; if TRUE (default) shows optimization progress in case an error occured during optimization
  }
  \item{show_optim_progress}{
    logical; if TRUE, always show optimization progress
  }
}

\details{
  A generalisation of the \code{optimParam}-function. Useful to apprehend convergence behaviour when more than two parameters are to be optimised for (making things potentially stable and inaccurate).
}
\value{
  For \code{plot} set to TRUE a matrix:
  First (number of free parameters) columns: grid of parameters, latter half of columns: values after one iteration of optimisation.
  For \code{plot} set to FALSE a list with components:
  \item{par}{Optimal parameters as named vector, over which it was optimized.}
  \item{value}{(Log-)Likelihood of optimal parameters}
  \item{convergence}{An integer code. 0 indicates sucessful completion. Refer to \code{optim} in \code{stats} for further details}
  \item{AIC}{Akaike Information Criterion of optimal parameters}
  \item{BIC}{Bayes Information Criterion of optimal parameters}
  \item{AIC}{Corrected Akaike Information Criterion of optimal parameters}
}
\references{
 ~put references to the literature/web site here ~
}
\author{
  Borui Niklas Zhu
}
\note{
  refer to \code{optimParam} on a detailed treatise on the underlying optimisation.
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
data <- rnorm(n=100, mean=70, sd= 4)
family <- list(family='norm', package="stats")
lower <- c('mean' = - Inf)
upper <- c('mean' = Inf)
fixed <- c('sd'=2)
start_parameters <- c('mean' = 0)
optimParam(data = data, family=family, lower=lower, upper=upper, start_parameters = start_parameters, fixed=fixed, log = T, show_optim_progress = TRUE)

# Example 2 for optimParam
data <- rbeta(n=100, shape=10, shape2=2)
family <- list(family='beta', package="stats")
lower <- c('shape1' = 0, 'shape2' = 0)
upper <- c('shape1' = Inf, 'shape2' = Inf)
start_parameters <- c('shape1' = 1, 'shape2' = 1)
fixed <- list()
optimParam(data =data, family = family, lower = lower, upper = upper, start_parameters = start_parameters, log = T, show_optim_progress = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution }
