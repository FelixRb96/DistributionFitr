source("utils.R")

# Loglik-Function
# important: family should be list with elements "package" and "family"
loglik <- function(param_values, family, data, fixed=list(), log=T) {
  arguments <- list(x=data)
  # check wheter log-distribution function is directly available for distribution
  if(log==T)
    arguments$log <- T
  # add variable parameter values of distribution to list
  if(length(param_values)>0) {
    arguments <- c(arguments, param_values)
  }
  # add fixed parameter values of distribution to list
  if(length(fixed)>0) {
    arguments <- c(arguments, fixed)
  }
  # calculate vector of (log)-densities
  dfun <- get_fun_from_package(fam = family$family, package = family$package, type="d")
  summands <- do.call(dfun, args=arguments)
  if(any(is.na(summands)))
    stop('In Log-Likelihood-Function NA occured.')
  # log values if not log so far
  if(!log) {
    summands <- log(summands)
    warning('Could be numerically instable.')
  } 
  ll <- sum(summands)
  
  # recursively go through parent frames and check whether there is a variable that tracks the optimisation process
  return_optim_progress <- FALSE
  for (i in 1:length(sys.parents())) {
    
    if (exists("optim_progress", envir = parent.frame(i))) {
      # print(parent.frame(i))
      return_optim_progress <- TRUE
      break
    }
  }
  
  # if we've found one, then we can update the progress
  if (return_optim_progress) {
    # cat("Found optimization progress in parent frame", i, "\n")
    progress <- get("optim_progress", envir = parent.frame(i))
    #print(progress)
    #print(c(param_values, fixed, log_lik = ll))
    progress[nrow(progress)+1, ] <- c(param_values, fixed, log_lik = ll)
    assign("optim_progress", envir = parent.frame(i), progress)
  }
  
  return(ll)
}

## Parameters of optimParam:
# family: list with two elements "family" and "package"
# lower, upper and start_parameters must only contain the continuous parameters that should be optimized
# prior: user-given prior information on parameters, updates default values from get_param
# debug_error: show optimization progress when an error occured
# show_optim_progress: always show optimization progress
# on_error_use_best_result: if TRUE and an error occured during optimization the best result achieved prior to the error will be taken
optimParam <- function(data, family, lower, upper, defaults, method = 'MLE', fixed=list(), prior = NULL, log=TRUE,
                       optim_method = 'L-BFGS-B',
                       debug_error=TRUE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, ...) {
  # Input parameter validation

  # TODO:
	# currently it seems customary to allowing the provision of boundaries for non-fixed parameters only.
	# below I added input validation that checks if the param names given in fixed are present in lower,
	# throwing an error
	# It is probably safer to require provision of boundaries for all parameters, regardless of whether fixed or not

  if(method!='MLE')
    stop('Not implemented.')
  if(length(lower)!=length(upper) || length(defaults)!= length(upper))
    stop('Length of lower and upper bounds vector do not coincide.')
  if(length(lower)==0) {
    stop('No parameters to optimize as no bounds delivered.')
  }
  if(any(names(lower) != names(upper)) || any(names(lower) != names(defaults)) ) {
    stop('Parameter names of lower and upper bounds and start parameters must coincide. ')
  }
  if(any(!(names(fixed) %in% names(lower))) || any(!(names(prior) %in% names(lower)))) {
    stop('Parameter names given as fixed/prior unknown.')
  }
  if(anyDuplicated(names(fixed)) || anyDuplicated(names(prior))) {
    stop('Duplicate entries in fixed/prior.')
  }
  
  # replace default values from get_params with user-given priors
  if(length(prior) > 0) {
    prior_positions <- match(names(prior), names(lower), nomatch = NULL)
    # nomatch should not occur due to the check above: "Parameter names given as prior unknown"
    defaults[prior_positions] <- prior
  }
  
  # create dataframe where to save the optimization progress
  # 1 column for each parameter and a column for the associated log likelihood
  optim_progress <- data.frame(matrix(nrow=0, ncol=length(lower) + length(fixed) + 1))
  colnames(optim_progress) <- c(names(lower), names(fixed), "log_lik")
  optim_successful <- FALSE
  
  on.exit({
    if (exists("optim_progress") && (show_optim_progress || (debug_error && !optim_successful))) {
      cat("Optimization progress:\n")
      print(tail(optim_progress, 2))
    }
  })
  
  optim_result <- tryCatch(
    {
      # Optimize first time
      # TODO: in second optimization set fnscale and parscale accordingly (check if it is set correctly below)
      cat("First Optimisation\n")
      round <- "first"
      optim_result <- optim(defaults, loglik, family = family, data = data, fixed=fixed, lower=lower, upper=upper,
                            log=log, control = list(fnscale=-1, trace=0), method=optim_method)
      if(optim_result$convergence!=0)
        warning('No convergence in first optimization!')

      # to see what happens in first
      print(tail(optim_progress, 2))
      # TODO: 
      # Problems with convergence can occur, if parscale and fscale not well selected
      # therefore 2 steps with right selection need to be implemented
      # optim_result <- optim(optim_result$par, loglik, family = family, data = data, fixed=fixed, lower=lower, upper=upper,
      #                      log=log, control = list(fnscale=-1 / abs(optim_result$value), trace=0, parscale = 1/optim_result$par), method='L-BFGS-B')
      
      args <- list(...)
      fnscale <- if (hasArg("fnscale") && args$fnscale) -1/abs(optim_result$value) else -1
      parscale <- if (hasArg("parscale") && args$parscale) 1/optim_result$par else 1
      
      # if a parameter is optimised as zero, parscale will be Infinity, causing trouble.
      # setting might not be optimal, but never fatal.
      adjust <- which(parscale == Inf | parscale == -Inf)
      parscale[adjust] <- mean(parscale[!(parscale == Inf | parscale == -Inf)], na.rm = TRUE)
      
      cat("\n\nfnscale:", fnscale, "\n")
      cat("parscale:\n")
      print(parscale)
      # linebreak if parscale not set initially
      cat("Second Optimisation\n")
      
      # adjust precision to number of parameters
      # note that with many parameters even though likelihood may have converged, parameters are still changing
      precision <- max(0, length(lower) - 2)
      # floating numbers are not equally spaced, only about 1e-16 is reliable
      precision <- max(1e-8/(10^(precision*2)), 1e-16)

      round <- "second"
      optim_result <- optim(optim_result$par, loglik, family = family, data = data, fixed=fixed, lower=lower, upper=upper,
                            log=log, control = list(fnscale=fnscale, trace=0, parscale = parscale, factr = precision), method=optim_method)
      
      optim_successful <- TRUE
      
      if(optim_result$convergence!=0)
        warning('No convergence in second optimization!')
      
      # so that it is returned and saved to optim_result
      optim_result
    },
    
    error = function(e) {
      # TODO: try to rethrow the original error (with correct stack trace, here context is changed)
      # message("Original error message:")
      # message(e)
      if (!on_error_use_best_result) stop(e)
      
      message(paste("Error occured during", round, "optimization, trying to take best result achieved up to now"))
      if (nrow(optim_progress) == 0 || max(optim_progress$log_lik) == -Inf) 
        stop( e, "occured during first optimization, so no valid result can be used instead")
      
      # getting best result from optimization progress up to now
      best_idx <- which.max(optim_progress$log_lik)
      best_row <- optim_progress[best_idx,]
      optim_result <- list()
      optim_result$value <- best_row$log_lik
      optim_result$par <- unlist(best_row[names(lower)])
      optim_result$convergence <- 51  # corresponds to warning
      return(optim_result)
    }
  )
  
  if (optim_result$value < max(optim_progress$log_lik) - 1e-8) {
    message("Final Optimization result is worse than the best result achieved during optimization")
    cat("Diff to best:", abs(optim_result$value - max(optim_progress$log_lik)), "\n")
  }
  

  # Information criteria calculation
  k = length(upper)
  n = length(data)
  aic = 2*k - 2 *  optim_result$value          # TODO: should k also include the length of the fixed parameters???
  bic = log(n) * k - 2 *  optim_result$value
  aicc = aic + (2*k^2+2*k)/(n-k-1)
  
  return(list(
    par = optim_result$par,
    value = optim_result$value,
    convergence = optim_result$convergence,
    AIC = aic, 
    BIC = bic,
    AICc = aicc
    )
  )
}

if (sys.nframe() == 0) {

  # Example 1 for optimParam
  data <- rnorm(n=100, mean=70, sd= 4)
  family <- list(family='norm', package="stats")
  lower <- c('mean' = - Inf)
  upper <- c('mean' = Inf)
  fixed <- c('sd'=2)
  defaults <- c('mean' = 0)
  optimParam(data = data, family=family, lower=lower, upper=upper, defaults = defaults, fixed=fixed, log = T, 
             parscale=TRUE, fnscale=TRUE, show_optim_progress = TRUE)
  
  
  # Example 2 for optimParam
  data <- rbeta(n=100, shape=10, shape2=2)
  family <- list(family='beta', package="stats")
  lower <- c('shape1' = 0, 'shape2' = 0)
  upper <- c('shape1' = Inf, 'shape2' = Inf)
  defaults <- c('shape1' = 1, 'shape2' = 1)
  fixed <- list()
  optimParam(data = data, family = family, lower = lower, upper = upper, defaults = defaults, log = T, show_optim_progress = TRUE)
  
  
  # Example 3 for optimParam
  # TODO: Dos not work, since optimParam doesnt work for integers (discrete parameterspace)
  if (FALSE) {
  data <- rbinom(n=100, size=10, prob=0.5)
  family <- list(family='binom', package="stats")
  lower <- c('size' = 0, 'prob' = 0)
  upper <- c('size' = Inf, 'prob' = 1)
  defaults <- c('size' = 1, 'prob' = 0.2)
  fixed <- list()
  optimParam(data = data, family = family, lower = lower, upper = upper, defaults = defaults, log = T)
  }
  
}

# TODO: set fnscale and parscale appropriately

# TODO: on error try to return best value from optimization progress up to now -> DONE

# TODO: globalfit needs to remove the fixed parameters from upper, lower and start_parameter
