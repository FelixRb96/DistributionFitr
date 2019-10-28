

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


HAllo
