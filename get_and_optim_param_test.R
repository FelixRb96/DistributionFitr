# -------------------------------------------------------------------------------- 
#  Testing function
# -------------------------------------------------------------------------------- 

rm(list=ls())
source("optimParam.R")
source("get_params.R")

test_families <- function(n, families) {
  
  for(fam in families) {
    cat("\n\nCurrent Family:", fam, "\n")
    
    # cat("Getting infos about the distribution\n")
    result <- get_params(fam)
    
    # if we couldn't find infos on the distribution
    if (is.null(result)) next
    
    npar <- length(result$lower)
    pars <- numeric(npar)
    names(pars) <- names(result$lower)
    
    # bound possible values to reasonable range
    upper <- pmin(result$upper, 100)
    lower <- pmax(result$lower, -100)
    
    # simulate some "true" parameter values
    # cat("Sampling some 'true' parameter values and simulating sample data\n")
    for(param in names(pars)) {
      
      if (result$accepts_float[param]) {
        pars[param] <- runif(1, lower[param], upper[param])
        
        # check if range contains an integer and if yes sample intergers from this range
      } else if (floor(result$upper[param]) - ceiling(result$lower[param]) >= 0) {
        pars[param] <- sample(ceiling(lower[param]) : floor(upper[param]), 1)
      } else {
        stop("Param", param, "does not accept floats and its range does not include any integers")
      }
    }
    
    # make sure that sampled values make sense for uniform distribution
    if (fam == "unif") {
      h <- pars
      pars["min"] <- min(h)
      pars["max"] <- max(h)
    }
    cat("Sampled params:", paste(names(pars), pars, sep=": ", collapse=", "), "\n")
    
    n_or_nn <- if (! "nn" %in% names(formals(paste0("r", fam)))) list(n=n) else list(nn=n)
    args <- c(n_or_nn, pars)
    testing_data <- do.call(paste0("r", fam), args)
    
    # we do it this way for now since we want to evaluate optim_param
    optimum <- tryCatch(optimParam(data = testing_data, family = fam, lower = result$lower, upper = result$upper, 
                                   start_parameters = result$default, log = result$log, debug_error = TRUE),
                        error = function(e) {
                          message(e);
                          NULL}
    )
    
    if (! is.null(optimum)) {
      cat("Optimized params:", paste(names(pars), optimum$par, sep=": ", collapse=", "), "\n")
      error_percent <- 100 * (pars - optimum$par) / pars
      cat("Deviation true - estimated params in %\n")
      print(error_percent)
      cat("(Log-)Likelihood for optimal parameters: ", optimum$value, "\n")
    }
  }
}

test_families(1000, families)

# TODOs:
# optimisation with L-BFGS-B still sometimes a bit unstable, to often "L-BFGS-B benötigt endliche Werte von 'fn'", that is loglik seems to 
# return Inf to often

# maybe try different optimisation procedures
# for each procedure compare optimisation results across all families (sth like mean percentage deviation)
