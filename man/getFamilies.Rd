\name{getFamilies}
\alias{getFamilies}
\title{
   Lists (all) distribution families with their parameters
}
\description{
   This function extracts distribution families from R-packages along with their 
   parameters and characteristics, and scans specific packages if desired.
   
}
\usage{
getFamilies(all.packages, file="R/all_families.R")

}

\arguments{
  \item{all.packages}{
      character vector; package names in which to scan for distribution families 
      or \cr
      \code{TRUE} to scan all packages  or \code{FALSE} to scan base packages 
      only or \code{MISSING} to return families saved in file
  }
  \item{file}{
      character; path to the file, where the information regarding the loaded 
      distribution families shall be stored
  }
}

\details{
  This function retrieves distribution families from R-packages as specified 
  in \sQuote{arguments}. A distribution family is defined as follows: \cr
  Take the functions \code{dnorm}, \code{pnorm}, \code{qnorm} and \code{rnorm} 
  from the package \sQuote{stats}. These functions are related to sampling, 
  density etc. of the normal distribution. The family name is the part of 
  the function name without the prefixes \sQuote{d},  \sQuote{p}, \sQuote{q}, or
  \sQuote{r} , in this case: \sQuote{norm}.
}
\value{
  A list of lists. Each sublist pertains to exactly one distribution 
  family (such as \sQuote{norm} from \sQuote{stats}) and contains:
  \item{package}{
    character; name of the package containing the family
  }
  \item{family}{
    character; name of the family, as defined under \dQuote{Details}
  }
  \item{family_info}{
    \itemize{
      \item{lower}{
	named numeric vector; lower bounds for distribution parameters
      }
      \item{upper}{
	named numeric vector; upper bounds for distribution parameters
      }
      \item{accepts_float}{
	named logical vector; whether each parameter can be any real 
	number (\code{TRUE}) or only integers are valid inputs (\code{FALSE})
      }
      \item{defaults}{
	names numeric vector; set of default parameters that jointly constitute valid 
	input. Used for optimisation.
      }
      \item{log}{
	logical; whether log values are provided by the density function
      }
      \item{discrete}{
	logical; whether the distribution itself (not the parameters) takes on 
	discrete values only
      }
      \item{support_min}{
	numeric; lower bound of the support, i.e. lowest value of \eqn{x} 
	where \eqn{f(x) > 0} for any valid parameter set, \eqn{f} the density function
      }
      \item{support_max}{
	numeric; upper bound of the support
      }
      \item{support_max_depends_on}{
	named logical vector; whether \code{support_max} depends on the value of each 
	parameter
      }
      \item{support_min_depends_on}{
	named logical vector; whether \code{support_min} depends on the value of each 
	parameter
      }
    }
  }
  Additionally, if \code{all.packages} is not missing, it is saved in an R file 
  as \code{FamilyList <- list(list(...))}
}

\author{
   Tim Glockner, Adrian Heppeler, Borui Niklas Zhu
}
% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%  See Also as \code{\link{help}}, **~
%}
\examples{ 
  % library(DistributionFitr)
  str(getFamilies())
}
\keyword{distribution}
