\name{optimParam}
\alias{optimParam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Fit Optimal Parameters for a Distribution Family
}
\description{
  Given data and a distribution family, this function optimises over the given continuous parameters of the function and returns the best fitted parameters and information criteria.
}
\usage{
optimParam(data, family, lower, upper, start_parameters, fixed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
     vector of data points
}
  \item{family}{
     distribution family callable in the \code{package}, e.g. 'norm'
}
  \item{lower}{
     named vector of lower bounds of free parameters, if no lower bound \code{-Inf} 
}
  \item{upper}{
     named vector of upper bounds of free parameters, if no upper bound \code{Inf}, names must coincide with \code{lower}
}
  \item{start_parameters}{
     named vector of starting parameters for optimisation, names must coincide with \code{lower} and \code{upper}
}
  \item{method}{
     so far just Maximum-Likelihood implemented
}
  \item{fixed}{
     named vector of fixed parameters, over which not to optimize  
}
####### TODO ab hier #####
\item{lower}{
     named vector of lower bounds of free parameters, if no lower bound \code{Inf} 

  \item{y}{
     **Describe \code{y} here**
}
}
\details{
  ** If necessary, more details than the description above **
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
 ...
}
\references{
 ~put references to the literature/web site here ~
}
\author{
  **who you are**
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

## The function is currently defined as
plus(6,7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
