# Function for stopping an expression if it takes too long
# Simplified version of https://github.com/HenrikBengtsson/R.utils/issues/74
eval_with_timeout <- function(expr, envir = parent.frame(), timeout, return_value_on_timeout=NULL) {
  # substitute expression so it is not executed as soon it is used
  expr <- substitute(expr)
  
  # for Borui
  if (.Platform$OS.type != "unix") {
    return(eval(expr, envir = envir))
  }
  
  # execute expr in separate fork
  myfork <- parallel::mcparallel({
    eval(expr, envir = envir)
  }, silent = FALSE)
  
  # wait max n seconds for a result.
  myresult <- parallel::mccollect(myfork, wait = FALSE, timeout = timeout)
  # kill fork after collect has returned
  tools::pskill(myfork$pid, tools::SIGKILL)
  tools::pskill(-1 * myfork$pid, tools::SIGKILL)
  
  # clean up:
  parallel::mccollect(myfork, wait = FALSE)
  
  # timeout?
  if (is.null(myresult)) {
    myresult <- return_value_on_timeout
  }
  
  # move this to distinguish between timeout and NULL returns
  myresult <- myresult[[1]]
  
  if ("try-error" %in% class(myresult)) {
    stop(attr(myresult, "condition"))
  }
  
  # send the buffered response
  return(myresult)
}

# t <- Sys.time()
# res <- eval_with_timeout(dsignrank(1, 2000), timeout=0.01, return_value_on_timeout = "TIMEOUT")
# print(res)
# print(Sys.time() -t)



# given a family name (e.g. "beta"), a package name (e.g. "stats") and the type ("r" for "rbeta", "d" for "dbeta") gets the function
# from the desired package and returns it
                                        # this is needed as we cant use "stats::rbeta" directly in formals or do.call -> error, that it cannot find the function

## jede Funktion ruft get_fun_from_package mit
##   get_fun_from_package(fam = fam$family, package = fam$package)
## auf.  Zumindest als Option waere gut, dass package nicht angegeben
## wird und get_fun_from_package(fam = fam, type="r") aufgerufen werden kann
get_fun_from_package <- function(fam, package, type="r") {
  return( get(paste0(type, fam), envir = asNamespace(package)) )
}

# get_fun_from_package("beta", "stats", "r")

# # show an example where a function exists in two packages and it loads the right one
# get_fun_from_package("filter", "stats", "")
# get_fun_from_package("filter", "dplyr", "")


sample_params <- function(family, family_info, params=NULL,
                          ## max = 100
                          ) {
  pars <- if (is.null(params)) family_info$lower else params
  
  # bound possible values to reasonable range
  upper <- pmin(family_info$upper,
                ## max
                100) ## nie Konstanten vergraben
  lower <- pmax(family_info$lower,
                ## -max
                -100)
  
  # simulate some "true" parameter values
  # cat("Sampling some 'true' parameter values and simulating sample data\n")

  ## bitte ohne for chleife
  ## float <- pars[family_info$accepts_float]
  ## pars[float] <- runif(length(float), lower[float], upper[float])
  ## int <- pars[!family_info$accepts_float]
  ## if (any(...)) stop
  ## part[int] <- ...
  
  for(param in names(pars)) {
    
    if (family_info$accepts_float[param]) {
      pars[param] <- runif(1, lower[param], upper[param])
      
      # check if range contains an integer and if yes sample intergers from this range
    } else if (floor(family_info$upper[param]) - ceiling(family_info$lower[param]) >= 0) {
      pars[param] <- sample(ceiling(lower[param]) : floor(upper[param]), 1)
    } else {
      stop("Param", param, "does not accept floats and its range does not include any integers")
    }
  }

  ## taucht das Problem nur bei unif auf ??
  # make sure that sampled values make sense for uniform distribution
  if (family$family == "unif") {
    h <- pars
    pars["min"] <- min(h)
    pars["max"] <- max(h)
  }
  return(pars)
}

# family <- list(package="stats", family="beta")
# sample_params(family, get_params(family), c(shape1=1, shape2=2))

sample_data <- function(n, family, params) {
  rfun <- get_fun_from_package(fam=family$family, package=family$package, type="r")
  n_or_nn <- if (! "nn" %in% names(formals(rfun))) list(n=n) else list(nn=n)
  args <- c(n_or_nn, params)
  testing_data <- do.call(rfun, args)
  return(testing_data)
}

# information criteria
informationCriteria <- function(ll, k, n) {
  aic = 2*k - 2 *  ll
  bic = log(n) * k - 2 *  ll
  aicc = aic + (2 * k^2 + 2 * k) / (n - k - 1)
  return(list(AIC=aic, BIC=bic, AICc=aicc))
}



is.natural <- function(x, tol = .Machine$double.eps^0.5) {
  (abs(Im(x)) < tol) &&
    (abs(Re(x)) > tol) &&
    isTRUE(all.equal(x, round(x),
                     tolerance=tol,
                     check.attributes=FALSE,
                     check.names=FALSE))
}
