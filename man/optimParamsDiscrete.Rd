\name{optimParamsDiscrete}
\alias{optimParamsDiscrete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Fit Optimal Parameters for a Distribution Family
}
\description{
  Given data and a distribution family, this function optimises over the given continuous parameters of the function and returns the best fitted parameters and information criteria.
}
\usage{
optimParamsDiscrete(data, family, family_info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
     vector of data points
  }
  \item{family}{
     list with two elements (character strngs): "family" (name of distribution family) and "package"
  }
  \item{family_info}{
     named list, as returned by get_params. See below for more details.
  }
  % TODO: we could allow the user to fix a parameter manually, e.g. for cases of certainty or higher dimensional optimisation.
  %\item{fixed}{
  %   named vector of fixed parameters, which are not taken into account during the optimization process
  %}
  \item{prior}{
     named vector of user guesses for true parameters. Might improve optimisation speed and accuracy - giving priors for only some of the parameters is allowed.
  }
  \item{method}{
     so far only Maximum-Likelihood is implemented, thus argument must be \code{"MLE"}.
  }
  \item{log}{
     logical; if TRUE (default) optimization is run over log-likelihood, requires log-likelihood as parameter in selected distribution family
  }
  \item{optim_method}{
     which optimisation method R is to use internally. Generally recommended: L-BFGS-B (default)
  }
  \item{n_starting_points}{
     number of different starting values. Choosing more starting values might be helpful to avoid local but not global maxima
  }
  \item{debug_error}{
     logical; if TRUE (default) shows optimization progress in case an error occures during optimization
  }
  \item{show_optim_progress}{
    logical; if TRUE, always show optimization progress
  }
  \item{on_error_use_best_result}{
    logical; if TRUE (default) if an error occurs/convergence is not established after the max. number of iterations, the best intermediate result achieved during optimisation process is returned as optimal aprameter fit.
  }
  \item{max_discrete_steps}{
    integer; if distribution has one integer-only parameter, this argument controls how many steps are allowed in our internal algorithm.
  }
  \item{discrete_fast}{
    integer; if distribution has one integer-only parameter, this argument controls how rigorous convergence is identifies as such by our internal algorithm.
  }
  \item{plot}{
    logical; if distribution has one integer-only parameter and TRUE, a plot of the convergence process will be provided. If FALSE (default) no such output.
  }
  \item{max_zoom_level}{
    integer; if distribution has two or more integer-only parameters, this argument controls how many steps are allowed in our internal algorithm.
  }
}

\details{
  This function is a wrapper for \code{optim_params}: In case of parameters that only allow for integer values, it ensures appropriate handling.
  In the near future direct usage of \code{optim_params} will be deprecated in the DistributionFitR-package internally and will be replaced with always using this wrapper function. \\

  family_info is expected to be a named list as returned by get_params, with the entries:
  \itemize{
    \item{
      \code{family}
      family name of the distribution, e.g. for \code{runif(...)} -> "unif", or \code{dnorm(...)} -> "norm"
    }
    \item{
      \code{lower}
      named vector. names: parameter names. values: lower bounds of validity for each parameter.
    }
    \item{
      \code{upper}
      named vector. names: parameter names. values: upper bounds of validity for each parameter.
    }
    \item{
      \code{defaults}
      named vector. names: parameter names. values: some valid parameter values such that \code{d[...]} produces a valid result with the whole 'defaults' vector.
    }
    \item{
      optional: \\
      the other values returned by \code{get_params}, such as information on the support.
    }
  }
}
\value{
  For \code{optim_params_wrapper} a list with components:
  \item{par}{Optimal parameters as named vector, over which it was optimized.}
  \item{value}{(Log-)Likelihood of optimal parameters}
  \item{convergence}{An integer code. 0 indicates sucessful completion. Refer to \code{optim} in \code{stats} for further details}
  \item{AIC}{Akaike Information Criterion of optimal parameters}
  \item{BIC}{Bayes Information Criterion of optimal parameters}
  \item{AIC}{Akaike Information Criterion of optimal parameters, corrected for small sample sizes}
}
\references{
 ~put references to the literature/web site here ~
}
\author{
  Moritz Kern, Borui N. Zhu
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

\dontrun{
# Example 1 for optimParam
data <- rnorm(n=100, mean=70, sd= 4)
family <- list(family='norm', package="stats")
lower <- c('mean' = - Inf)
upper <- c('mean' = Inf)
fixed <- c('sd'=2)
start_parameters <- c('mean' = 0)
optimParamsDiscrete(data = data, family=family, lower=lower, upper=upper, start_parameters = start_parameters, fixed=fixed, log = T, show_optim_progress = TRUE)

# Example 2 for optimParam
data <- rbeta(n=100, shape=10, shape2=2)
family <- list(family='beta', package="stats")
lower <- c('shape1' = 0, 'shape2' = 0)
upper <- c('shape1' = Inf, 'shape2' = Inf)
start_parameters <- c('shape1' = 1, 'shape2' = 1)
fixed <- list()
optimParamsDiscrete(data =data, family = family, lower = lower, upper = upper, start_parameters = start_parameters, log = T, show_optim_progress = TRUE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
