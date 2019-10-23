# -------------------------------------------------------------------------------- 
#  Testing function
# -------------------------------------------------------------------------------- 

# refer to line 280 to see what result looks like
# NONE OF THE FOLLOWING HAS BEEN TESTED
# all due to the premeditated and most malicious exclusion of one of our beloved team members
# by the vicious and cold-hearted BENEDIKT GEIER

rm(list=ls())
source("optimParam.R")
source("get_params.R")

test_families <- function(n, families) {
  
  # TODO: add more info on current family and result of optimization
  
  for(fam in families) {
    result <- get_params(fam)
    npar <- length(result$lower)
    pars <- numeric(npar)
    names(pars) <- names(result$lower)
    
    result$upper <- pmin(result$upper, 100)
    result$lower <- pmin(result$lower, 100)
    
    for(param in names(pars)) {
      
      if (result$accepts_float[param]) {
        pars[param] <- runif(1, result$lower[param], result$upper[param])
      } else if (floor(result$upper[param]) - ceiling(result$lower[param]) >= 0) {
        pars[param] <- sample(ceiling(result$lower[param]) : floor(result$upper[param]), 1)
      } else {
        stop("Param", param, "does not accept floats and its range does not include any integers")
      }
    }
    args <- c(list(n = n), pars)
    testing_data <- do.call(paste0("r", fam), args)
    
    # we do it this way for now since we want to evaluate optim_param
    lower <- result$lower
    upper <- result$upper
    log <- result$log
    optimum <- optimParam(data = testing_data, family = fam, lower = lower, upper = upper, start_parameters = result$default, log = log)
    error_percent <- (optimum$par - pars)/pars
    cat("(Log-)Likelihood for optimal parameters", names(pars), ": ", optimum$value)
    
  }
}

test_families(families, 1000)
