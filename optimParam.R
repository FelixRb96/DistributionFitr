log_likelihood <- function(family, data, param_name, x) {
  arguments <- list(mean=0, q=data, log=T)
  arguments[[param_name]] <- x
  sum(do.call(paste('p', family, sep=''), args=arguments))
}

optimParam <- function(data, family, lower, upper, method = 'MLE', fixed, log)  {
  if(length(lower)!=length(upper))
    stop('Length of lower and upper bounds vector do not coincide.')
  if(length(lower)==0) {
    warning('No bounds deliverd.')
    return(NA)
  }
  lower <- ifelse(lower==-Inf, -1e5, lower)
  upper <- ifelse(upper==Inf, 1e5, upper)
  # TODO:
  #ifelse(lower>upper, rep(stop('Lower bound is larger than upper bound.'), length(lower)), NA)
  do.call(paste('p', family, sep=''), args=list(0, 1))
  if(length(lower)==1) {
    #optimize(do.call(paste('p', family, sep=''), args=list(q=data, mean=0)), lower = lower, upper = 10)
    optimize(log_likelihood, lower = lower, upper=upper, maximum= TRUE, family = family, data = data, param_name = names(lower))
  }
}


data <- c(0, 0.3, 0.1, 3, 7)
family = 'norm'
lower = c('mean' = - Inf, 'sd' = 0)
upper = c('mean' = Inf, 'sd' = Inf)
lower = c('sd' = 0)
upper = c('sd' = Inf)
fixed <- list('lower.tail' = T, 'mean'=0)

optimParam(data, family, lower, upper, fixed, log = F)
