\name{get_params}
\alias{get_params}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get parameters and their properties for a Distribution Family
}
\description{
  Given a distribution family, this function attempts to retrieve the distribution parameters and various characteristics, such as: valid parameter ranges, whether they accept non-integer values and the support of the distribution.
}
\usage{
get_params(family)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{family}{
     List with two entries: "package" and "family", e.g. list(package="stats", family="beta")  }
}

\details{
	The family name is defined as the part of the function name that follows "d", "p", "q" and "r". Example: uniform distribution. function call: "runif(...)" \rightarrow family name: "unif". \\
	The values returned by get_params are included in the DistributionFitr-package and updated regularly. Users may use it to update the library of parameter characteristics themselves or find the function useful to use the parameter characteristics retrieved for other purposes.
}
\value{
  For \code{get_params} a list with components, each of them a named vector: The name is the arg name as specified in the original "d", "p", ...-function, the value is described below:
  \item{lower}{a named vector. Values: numeric, lower bound of the respective parameter value}
  \item{upper}{a named vector. Values: numeric, upper bound of the respective parameter value. Length and names coincides with entry \code{lower}.}
  \item{accepts_float}{a named vector. Values: boolean, \code{TRUE} if respective parameter value is continuous, \code{FALSE} if only integers are accepted. Length and names must coincide with entry \code{lower}.} 
  \item{defaults}{a named vector. Values: numeric, default values for the respective parameter (needed for optimisation to work). Length and names must coincide with entry \code{lower}.}
  \item{log}{a single boolean. \code{TRUE} if log(probability) is provided by d[family](), \code{FALSE} if they are not. Generating log-probabilities oneself may be numerically unstable.}
  \item{discrete}{a single boolean. \code{TRUE} if only integers are taken as values, \code{FALSE} otherwise.}
  \item{support_min}{a single numeric. Left bound of support of the distributions density, i.e. minimum value where the density is not zero.}
  \item{support_max}{a single numeric. Right bound of support of the distributions density, i.e. maximum value where the density is not zero.}
  \item{supp_max_depends_on}{a named vector. Value: booleans, \code{TRUE} if right support bound depends on the repective parameter, \code{FALSE} if not. Note that dependency is currently implemented as equality between bound and parameter, linear relationships may be implemented in the future. Length and names must coincide with entry \code{lower}.}
  \item{supp_max_depends_on}{a named vector. Value: booleans, \code{TRUE} if right support bound depends on the repective parameter, \code{FALSE} if not. See note in entry \code{supp_max_depends_on}. Length and names must coincide with entry \code{lower}.} 
}
\references{
 ~put references to the literature/web site here ~
}
\author{
  Benedikt Geier, Borui Niklas Zhu
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

# Example 1 for get_params
parameter_characteristics <- get_params("beta")

# Example 2 for optimParam
this_should_also_work <- get_params("unif")

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution, parameters }
