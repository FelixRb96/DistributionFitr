source("utils.R")

# Loglik-Function
# Formate

# family:   Liste,              Name der Familie + Liste mit Paket
# data:     Numerischer Vektor, Datenreihe
# fixed:    banamte Liste,      Namen und fixierte Werte für die übringen Parameter
#                               Arg_opt und fixed sind disjunkt und ergeben zusammen ALLE paramter der Familie
# log:      Boolean (T/F),      Does the log argument exist when calling the distribuition function of the family
# lower:    Benamter Vektor,    Lower Boundaries for paramters in arg_opt   (same order!)
# upper:    Benamter Vektor,    Upper Boundaries for paramters in arg_opt   (same order!)

loglik <- function(family, data, fixed=list(), log, lower, upper) { #NEW: fixed=list() default
  
  arguments <- list(x=data) # NEW: arg_opt wird erst in likelihood in die Liste eingefügt
  
  # check wheter log-distribution function is directly available for distribution
  if(log==T)
    arguments$log <- T
  # add fixed parameter values of distribution to list
  if(length(fixed)>0) {
    arguments <- c(arguments, fixed)
  }
  
  
  # define loglikelihood function 
  likelihood <- function(params) {
    
    # print(params) # to be deleted later, checking input  
  
    if(length(params)==0) warning('loglik does not depend on parameters.')
      
    #NEW: Check boundaries
    for(param_name in names(params)) {
      if(lower[param_name]>params[[param_name]] || upper[param_name] < params[[param_name]])
        stop('Parameter ', param_name, ' with value ', params[[param_name]], ' outside the boundaries.') #Alternativ einen penalty
    }
    
    #Add params with names of parameters to arguments list
    arguments <- c(arguments, params)
    
    summands <- do.call(get_fun_from_package(family$family, family$package, type = "d"), args=arguments)
    
    if(any(is.na(summands))) stop('In Log-Likelihood-Function NA occured.')
    
    # log values if not log so far
    if(!log) {
      summands <- log(summands)
      warning('Could be numerically instable.')
    } 
    loglik_value <- sum(summands)
    
    ## The following is only for tracking the optimisation progress (might be deactivated sometime)
    # recursively go through parent frames and check whether there is a variable that tracks the optimisation process
    return_optim_progress <- FALSE
    for (i in 1:length(sys.parents())) {
      
      if (exists("optim_progress", envir = parent.frame(i))) {
        # cat("Env in loglik:\n")
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
      progress[nrow(progress)+1, ] <- c(params, fixed, log_lik = loglik_value)
      assign("optim_progress", envir = parent.frame(i), progress)
      # print(tail(progress,2))
    }
    
    # message(loglik_value)
    
    return(loglik_value)
  }
  return(likelihood)
}