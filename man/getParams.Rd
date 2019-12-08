\name{getParams}
\alias{getParams}
% MS: Bitte Datei von Muell bereinigen
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get parameters and their properties for a Distribution Family
}
\description{
  Given a distribution family, this function attempts to retrieve the 
  distribution parameters and various characteristics, such as: valid parameter
  ranges, whether they accept non-integer values and the support of the 
  distribution.
}
\usage{
getParams(fam, package)
}
%- maybe also 'usage' for other objects documented here.

% MS: KEINE Anfuehrungszeichen ausserhalb von \example{}, \usage{},
% \code{} ! Wenn \code nicht passt, bitte \sQuote und \dQuote{}
% verwenden !
\arguments{
  \item{fam}{
    character or list.
    If \code{fam} is a list or a named vector,
    then it has the two entries "package" and
    "family", e.g. list(package="stats", family="beta")  }
  \item{package}{character. Optional argument if \code{fam} is given.
    In most cases the package can be determined by \command{getParams} itself.
  }
}

\details{
  The family name is defined as the part of the function name that follows "d", 
  "p", "q" and "r". Example: uniform distribution. function call: "runif(...)"
  --> % illegal. Bitte durch Text ersetzen
  family name: "unif". \cr	The values returned by
  getParams % ist ein \command
  are included
  in the DistributionFitr-package and updated regularly. Users may use it to
  update the library of parameter characteristics themselves or find the
  function useful to use the parameter characteristics retrieved for other
  purposes.
}
\value{
  For \code{getParams} % MS: genauer: \command{}
  a list with components, each of them a named vector: The 
  name is the arg name as specified in the original "d", "p", ...-function, the 
  value is described below:
  \item{lower}{named vector; values: numeric, lower bound of the respective
	  parameter value}
  \item{upper}{named vector; values: numeric, upper bound of the respective
	  parameter value. Length and names must coincide with entry \code{lower}.}
  \item{accepts_float}{named vector; values: boolean, \code{TRUE} if 
	  respective parameter value is continuous, \code{FALSE} if only
	  integers are accepted. Length and names must coincide with entry
	  \code{lower}.} 
  \item{defaults}{named vector; values: numeric, default values for the 
	  respective parameter (needed for optimisation to work).
	  Length and names must coincide with entry \code{lower}.}
  \item{log}{single boolean; \code{TRUE} if log(probability) is provided by
	  d[family](), \code{FALSE} if they are not. Generating
	  log-probabilities oneself may be numerically unstable.}
  \item{discrete}{single boolean; \code{TRUE} if only integers are taken as
	  values, \code{FALSE} otherwise.}
  \item{support_min}{single numeric; left bound of support of the 
	  distributions density, i.e. minimum value where the density
	  is not zero.}
  \item{support_max}{single numeric; right bound of support of the 
  distributions density, i.e. maximum value where the density is not zero.}
  \item{supp_max_depends_on}{named vector; value: booleans, \code{TRUE} if 
	  right support bound depends on the repective parameter,
	  \code{FALSE} if not. Note that dependency is currently
	  implemented as equality between bound and parameter, linear
	  relationships may be implemented in the future. Length and 
	  names must coincide with entry \code{lower}.}
  \item{supp_max_depends_on}{named vector; value: booleans, \code{TRUE} if 
	  right support bound depends on the repective parameter,
	  \code{FALSE} if not. See note in entry
	  \code{supp_max_depends_on}. Length and names must coincide 
	  with entry \code{lower}.} 
}
%\references{
% ~put references to the literature/web site here ~
%}
\author{
  Benedikt Geier, Borui Niklas Zhu
}
%\note{
%  **further notes**
%}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See also \code{\link{getFamilies}} for a convenient wrapper available to the 
  user where distributions are extracted from whole packages.
}
\examples{
getParams("beta", package = "stats")
getParams("unif", package = "stats" )
}
\keyword{distribution}
\keyword{parameters}
