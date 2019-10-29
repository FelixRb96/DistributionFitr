
rm(list=ls())

# args: benamte Liste
# fixed: benamte Liste

# Loglik-Function
loglik <- function(param_values, family, data, fixed, log) {
  arguments <- list(x=data)
  # check wheter log-distribution function is directly available for distribution
  if(log==T)
    arguments$log <- T
  # add fixed parameter values of distribution to list
  if(length(fixed)>0) {
    arguments <- c(arguments, fixed)
  }
  # define loglikelihood function 
  likelihood <- function(params) {
    if(length(params)>0) {
      arguments <- c(arguments, params)
    }
    else{warning('loglik does not depend on parameters.')}
    summands <- do.call(paste('d', family, sep=''), args=arguments)
    if(any(is.na(summands)))
      stop('In Log-Likelihood-Function NA occured.')
    # log values if not log so far
    if(!log) {
      summands <- log(summands)
      warning('Could be numerically instable.')
    } 
    loglik_value <- sum(summands)
    return(loglik_value)
  }
  return(likelihood)
}

# To Do:


# Testbeispiele 

likelihood <- loglik(param_values = list(), family = "unif", data = 1:10, fixed=list(min=1, max=10), log=T)
likelihood(params = list())

likelihood <- loglik(param_values = list(), family = "unif", data = 1:10, fixed=list(min=1, max=10), log=F)
likelihood(params = list())

likelihood <- loglik(param_values = list(), family = "exp", data = 1:10, fixed=list(), log=T)
likelihood(params = list(2))

likelihood <- loglik(param_values = list(), family = "norm", data = 1:10, fixed=list(), log=T)
likelihood(params = list(0,1))




