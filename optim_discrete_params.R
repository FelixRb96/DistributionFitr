source("optimParam.R")


# optim_result <- tryCatch(optimParam(data = testing_data, family = family, lower = family_info$lower, upper = family_info$upper, 
#                                    defaults = family_info$default, log = family_info$log, debug_error = TRUE, show_optim_progress = FALSE),
#                         error = function(e) {
#                           message(e);
#                           NULL}
                         
                         
#optimParam <- function(data, family, lower, upper, defaults, method = 'MLE', fixed=list(), prior = NULL, log=TRUE,
#                       optim_method = 'L-BFGS-B', n_starting_points=1,
#                       debug_error=TRUE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, ...) {
  
  

# Function for doing the optimisation of discrete parameters
optim_discrete_params <- function(data, family, family_info, method = 'MLE', prior = NULL, log=TRUE,
                                  optim_method = 'L-BFGS-B', n_starting_points=1,
                                  debug_error=TRUE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, 
                                  max_discrete_steps = 100, ...){
  
  if (all(family_info$accepts_float)) {
    optimParam(data=data, family=family, lower= family_info$lower, upper=family_info$upper,
               defaults = family_info$defaults, method = method, fixed=c(), log = log, optim_method = optim_method,
               nn_starting_points= n_starting_points, debug_error = debug_error, show_optim_progress = FALSE,
               on_error_use_best_result = TRUE, ...)
  } else if(sum(!family_info$accepts_float)==1) {
    dispar_id <- family_info$accepts_float==FALSE
    dispar_default <- family_info$defaults[dispar_id]
    dispar <- dispar_default
      i <- 1
      stop_discrete <- FALSE
      cont_optim_result <- list()
      cont_optim_value <- rep(NA, max_discrete_steps)
      touched_lower <- touched_upper <- FALSE
      while(!stop_discrete) {
        cont_optim_result[[i]] <- tryCatch(optimParam(
                   data=data, family=family, lower=family_info$lower[-dispar_id], upper=family_info$upper[-dispar_id],
                   defaults = family_info$defaults[-dispar_id], method = method, fixed=dispar, log = log, optim_method = optim_method,
                   nn_starting_points= n_starting_points, debug_error = debug_error,show_optim_progress = on_error_use_best_result,
                   on_error_use_best_result = on_error_use_best_result, ...),
                   error = function(e) {
                                               message(e);
                                                NULL})
        #cont_optim_result[[i]]$dispar <- dispar
        cont_optim_value[i] <-tryCatch(cont_optim_result[[i]]$value, error= function(e) {NA})
        if(!touched_lower && !touched_upper) {
          # avoid sign(0)=0
          dispar <- dispar_default - (abs(dispar-dispar_default)+1) * sign(dispar-dispar_default+0.1)
        } else if(touched_lower && !touched_upper)  {
          dispar <- dispar + 1
        } else if(!touched_lower && !touched_upper) {
          dispar <- dispar - 1
        } else {
          stop_discrete <- TRUE
        }
        if(dispar == family_info$lower[dispar_id])
          touched_lower <- TRUE
        if(dispar == family_info$upper[dispar_id])
          touched_upper <- TRUE
        cat('Discrete Parameter: ', dispar, '\n')
      
      if(i>max_discrete_steps)
        stop_discrete <- TRUE
      if(i>2) {
        print(cont_optim_value[(i-2):i])
        if(!any(is.na(cont_optim_value[(i-2):i]))) {
          if((cont_optim_value[i-2] > cont_optim_value[i-1]) & (cont_optim_value[i-1] > cont_optim_value[i])) 
            stop_discrete <- TRUE
        }
      }
      i <- i+1
      
    }
    plot((ifelse(is.infinite(cont_optim_value), NA, cont_optim_value)))
    return(cont_optim_result[[which.max(cont_optim_value)]])
    #return(cont_optim_result)
  } else
    stop('So far just implemented for up to one discrete parameter')
}




# Test

optim_discrete_params(rbinom(n=1000, size=10, prob=0.5), family_list[[2]][c('package', 'family')], 
                      family_list[[2]]$family_info, debug_error=F, max_discrete_steps = 100)
optim_discrete_params(rnorm(n=1000, mean=2, sd=0.5), family_list[[12]][c('package', 'family')],
                      family_list[[12]]$family_info, debug_error=F)

family_info <- family_list[[12]]
