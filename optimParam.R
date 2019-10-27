# Loglik-Function
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
  summands <- do.call(paste('d', family, sep=''), args=arguments)
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
    if (exists(".optim_progress", envir = parent.frame(i))) {
      return_optim_progress <- TRUE
      break
    }
  }
  
  # if we've found one, then we can update the progress
  if (return_optim_progress) {
    # cat("Found optimization progress in parent frame", i, "\n")
    progress <- get(".optim_progress", envir = parent.frame(i))
    new_progress <- rbind(progress, c(param_values, fixed, log_lik = ll))
    assign(".optim_progress", envir = parent.frame(i), new_progress)
  }
  
  return(ll)
}

# debug_error: show optimization progress when an error occured
# show_optim_progress: always show optimization progress
optimParam <- function(data, family, lower, upper, start_parameters, method = 'MLE', fixed=list(), log=TRUE,
                       debug_error=TRUE, show_optim_progress=FALSE) {
  # Input parameter validation
  if(method!='MLE')
    stop('Not implemented.')
  if(length(lower)!=length(upper) || length(start_parameters)!= length(upper))
    stop('Length of lower and upper bounds vector do not coincide.')
  if(length(lower)==0) {
    warning('No bounds delivered.')
    return(NA)
  }
  if(any(names(lower)!=names(upper)) || any(names(lower)!=names(start_parameters)) ) {
    stop('Parameter names of lower and upper bounds and start parameters must coincide. ')
  }
  
  # create dataframe where to save the optimization progress
  .optim_progress <- data.frame()
  optim_successful <- FALSE
  
  on.exit({
    if (exists(".optim_progress") && (show_optim_progress || (debug_error && !optim_successful))) {
      cat("Optimization progress:\n")
      print(.optim_progress)
    }
  })
  
  # Check whether there are free parameters to optimize
  if(length(lower)>0) {
    # Optimize first time
    # TODO:
    # Optimize works so far just for continuous parameters, not for discrete (integers)
    optim_result <- optim(start_parameters, loglik, family = family, data = data, fixed=fixed, lower=lower, upper=upper,
                          log=log, control = list(fnscale=-1, trace=0), method='L-BFGS-B')
    if(optim_result$convergence!=0)
      warning('Did not converge!')
    # TODO: 
    # Problems with convergence can occur, if parscale and fscale not well selected
    # therefore 2 steps with right selection need to be implemented
    optim_result <- optim(optim_result$par, loglik, family = family, data = data, fixed=fixed, lower=lower, upper=upper,
                          log=log, control = list(fnscale=-1, trace=0), method='L-BFGS-B')
    
    optim_successful <- TRUE
    
    if(optim_result$convergence!=0)
      warning('Did not converge!')
  } else {
    
    # TODO: What's the purpose of that? ////////////////////////////////////////////////////////////////////
    loglik(family = family, data = data, fixed=fixed, log=log)
    
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


# Example 1 for optimParam
data <- rnorm(n=100, mean=70, sd= 4)
family = 'norm'
lower = c('mean' = - Inf)
upper = c('mean' = Inf)
fixed <- c('sd'=2)
start_parameters <- c('mean' = 0)
optimParam(data = data, family=family, lower=lower, upper=upper, start_parameters = start_parameters, fixed=fixed, log = T, show_optim_progress = TRUE)

# Example 2 for optimParam
data <- rbeta(n=100, shape=10, shape2=2)
family = 'beta'
lower = c('shape1' = 0, 'shape2' = 0)
upper = c('shape1' = Inf, 'shape2' = Inf)
start_parameters = c('shape1' = 1, 'shape2' = 1)
fixed <- list()
optimParam(data =data, family = family, lower = lower, upper = upper, start_parameters = start_parameters, log = T, show_optim_progress = TRUE)


# Example 3 for optimParam
# TODO: Dos not work, since optimParam doesnt work for integers (discrete parameterspace)
if (FALSE) {
data <- rbinom(n=100, size=10, prob=0.5)
family = 'binom'
lower = c('size' = 0, 'prob' = 0)
upper = c('size' = Inf, 'prob' = 1)
start_parameters = c('size' = 1, 'prob' = 0.2)
fixed <- list()
optimParam(data =data, family = family, lower = lower, upper = upper, start_parameters = start_parameters, log = T)
}

# TODO: set fnscale and parscale appropriately
