source("utils.R")
source("get_params.R")
source("optimParam.R")

# Loglik-Function
# important: family should be list with elements "package" and "family"
loglik <- function(param_values, family, data, fixed=list(), log=TRUE, show_optim_progress = FALSE) {
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
  
  if(show_optim_progress) {
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
optimParam_plot <- function(plot = TRUE, dim1 = NULL, dim2 = NULL, data, family, lower, upper, defaults, method = 'MLE', fixed = NULL, prior = NULL, log=TRUE,
                       optim_method = 'L-BFGS-B',
                       debug_error=TRUE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, ...) {
  # Input parameter validation
  # if(length(fixed) > 0) {
  #  stop('Fixed parameters not allowed for this testing function.')
  # }
  if(method!='MLE')
    stop('Not implemented.')
  if(length(lower)!=length(upper) || length(defaults)!= length(upper))
    stop('Vector lengths of upper/lower/defauls do not coincide.')
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
  matching <- match(names(fixed), names(lower))
  if(any( fixed < lower[matching] | fixed > upper[matching] )) {
    stop('fixed levels given are out of possible parameter bounds')
  }
  if(length(prior) > 0) {
    prior_positions <- match(names(prior), names(lower), nomatch = NULL)
    # nomatch should not occur anyway due to the check above: "Parameter names given as prior unknown"
    defaults[prior_positions] <- prior
  }
  if(plot == TRUE) {
    if( !hasArg(dim1) || is.null(dim1) || !hasArg(dim2) || is.null(dim2) ) {
      stop('If "plot" is set to TRUE, dim1 and dim2 have to be specified')
    }
    plotted <- c(deparse(substitute(dim1)), deparse(substitute(dim2))) 
    if(any( names(fixed) %in% plotted )) {
      stop('Parameter can not be plotted if given as fixed')
    }
    if(length(fixed) < length(lower) - 2) {
      stop('If "plot" is set to TRUE, a fixed level has to be supplied for each param not being plotted (dim1 or dim2).')
    }
  }
  
  # no fancy stuff if plot == FALSE
  if(plot == FALSE) {
    ret <- optimParam(data = data, family = family, lower = lower, upper = upper, defaults = defaults, method = 'MLE', fixed = NULL, prior = NULL, log = TRUE,
                       optim_method = 'L-BFGS-B',
                       debug_error=TRUE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, ...)
    return(ret)
  }

  # create dataframe where to save the optimization progress
  # 1 column for each parameter and a column for the associated log likelihood
  optim_progress <- data.frame(matrix(nrow=0, ncol=length(lower) + length(fixed) + 1))
  colnames(optim_progress) <- c(names(lower), names(fixed), "log_lik")
  optim_successful <- FALSE
  
  on.exit({
    if (exists("optim_progress") && (show_optim_progress || (debug_error && !optim_successful))) {
      cat("Optimization progress:\n")
      print(tail(optim_progress, 20))
    }
  })
  diagram <- NULL
  # get parameters to maximise over, partition their intervals and form all possible combinations
  matching <- !(names(lower) %in% names(fixed))
  params <- lower[matching]
  print(params)
  sequence <- list()
  for(par in names(params)) {
	  # sequence with infinity obviously does not work
	  leftbound <- max(lower[par], -100)
	  rightbound <- min(upper[par], 100)
	  sequence[[par]] <- seq(leftbound, rightbound, length.out = 21)
  }
  inits <- expand.grid(sequence)

  nexts <- matrix(NA, nrow = nrow(inits), ncol = length(params))
  
  for(i in 1:nrow(inits)) {
    step <- tryCatch(
    {
      step <- numeric(length(lower))
      optim_result <- optim(inits[i,], loglik, family = family, data = data, fixed = fixed, lower = lower, upper = upper, log = log, control = list(fnscale = -1, trace = 0, maxit = 1), method = optim_method)
      step <- optim_result$par
    },
    error = function(e) {
      step <- rep(NA, length.out = length(lower))
      names(step) <- names(lower)
      return(step)
    }
    )
    nexts[i,] <- step
  }
  colnames(nexts) <- paste0(colnames(inits), "_it")
  diagram <- cbind(inits, nexts)

  gg <- ggplot(data = diagram) +
      		geom_segment(aes_string(x = dim1, y = dim2, xend = paste0(dim1, "_it"), yend = paste0(dim2, "_it")), arrow = arrow(length = unit(0.2, "cm"))) +
      		geom_segment(x = max(lower[dim1], -100), y = max(lower[dim2], -100), xend = min(upper[dim1], 100), yend = max(lower[dim2], -100), arrow = arrow(length = unit(0.2, "cm")), col = "green") +
      		geom_segment(x = max(lower[dim1], -100), y = max(lower[dim2], -100), xend = max(lower[dim1], -100), yend = min(upper[dim2], 100), arrow = arrow(length = unit(0.2, "cm")), col = "green") +
      		ggtitle( paste("Konvergenzverhalten; konstante Parameter:", paste(names(fixed), fixed, sep = " = ", collapse = "; ")) ) +
      		xlab(dim1) + ylab(dim2)
  print(gg)
  return(diagram)
}
  
# Example 1 : norm
data <- rnorm(n=100, mean=70, sd= 4)
family <- list(family='norm', package="stats")
lower <- c('sd' = 0, 'mean' = - Inf)
upper <- c('sd' = Inf, 'mean' = Inf)
fixed <- NULL
defaults <- c('sd' = 1, 'mean' = 0)
diagram <- optimParam_plot(dim1 = "mean", dim2 = "sd", data = data, family=family, lower=lower, upper=upper, defaults = defaults, fixed=fixed, log = T, 
           parscale=TRUE, fnscale=TRUE, show_optim_progress = TRUE)


# Example 2: beta (three parameters)
# TODO: Find out where the instability w.r.t. ncp comes from
data <- rbeta(n=100, ncp = 20, shape1 = 10, shape2 = 10)
family <- list(family='beta', package="stats")
lower <- c('ncp' = 0, 'shape1' = 0, 'shape2' = 0)
upper <- c('ncp' = Inf, 'shape1' = Inf, 'shape2' = Inf)
defaults <- c('ncp' = 0, 'shape1' = 1, 'shape2' = 1)

diagram <- optimParam_plot(dim1 = "ncp", dim2 = "shape1", fixed = (shape2 = 10), data = data, family = family, lower = lower, upper = upper, defaults = defaults, log = T, show_optim_progress = F)
diagram <- optimParam_plot(dim1 = "ncp", dim2 = "shape2", fixed = (shape1 = 10), data = data, family = family, lower = lower, upper = upper, defaults = defaults, log = T, show_optim_progress = F)
diagram <- optimParam_plot(dim1 = "shape1", dim2 = "shape2", fixed = (ncp = 20), data = data, family = family, lower = lower, upper = upper, defaults = defaults, log = T, show_optim_progress = F)

# Example 3: f-distribution (also three parameters)
# see if optim fails in three dimensions or whether the problem is specific w.r.t. ncp in beta
  
data <- rf(n=100, df1 = 5, df2 = 10, ncp = 5)
family <- list(family='f', package="stats")
lower <- c('ncp' = 0, 'df1' = 0, 'df2' = 0)
upper <- c('ncp' = Inf, 'df1' = Inf, 'df2' = Inf)
defaults <- c('ncp' = 0.5, 'df1' = 0.5, 'df2' = 0.5)
fixed <- list()
diagram <- optimParam_plot(dim1 = "ncp", dim2 = "df1", fixed = c(df2 = 10), data = data, family = family, lower = lower, upper = upper, defaults = defaults, log = T, show_optim_progress = F)
