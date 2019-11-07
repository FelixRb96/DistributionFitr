source("optimParam.R")

### STATEMENT OF PURPOSE ###
############################

# This function is a wrapper for optimParam.
# Implementation is not final, target for now is to get a working solution - no matter the runtime efficiency.
# This wrapper discerns the following cases:
# (a) no integer parameters: call optimParam straight away
# (b) one integer parameter: use greedy solution. Questions to: Moritz Kern
# (c) multiple integer parameters: use naive solution. Question to: Borui N. Zhu



# TODO: in general, we often restrict parameters to be in a reasonable bound: [-100,100]
# this might not hold. Maybe we can let the user specify something like parscale, that lets us space out everything,
# and some kind of accuracy so we optimise rigorously locally after a roguh "global" view

# Function for doing the optimisation of discrete parameters
optim_params_wrapper <- function(data, family_info, method = 'MLE', prior = NULL, log=TRUE,
                                  optim_method = 'L-BFGS-B', n_starting_points=1,
                                  debug_error=TRUE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, ...){
  
  if (all(family_info$accepts_float)) {
    result <- optimParam(data=data, family=family_info$family, lower= family_info$lower, upper=family_info$upper,
               defaults = family_info$defaults, method = method, fixed=c(), log = log, optim_method = optim_method,
               nn_starting_points= n_starting_points, debug_error = debug_error, show_optim_progress = FALSE,
               on_error_use_best_result = TRUE, ...)   # TODO: 
    return(result)
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
  } else {
    non_floats <- which(family_info$accepts_float)
    ## naive implementation
    # get paramter ranges of non-float parameters, make grid
    # as always, chop to reasonable ranges
    lows <- max(family_info$lower[non_floats], -100)
    highs <- min(family_info$upper[non_floats], 100)
    # get_params shall insure that lower and upper are all integers
    # is there a vectorised version of seq()?
    seq_vec <- Vectorize(seq.default, vectorize.arg = c("from", "to"), SIMPLIFY = FALSE)
    grid <- list(seq_vec(from = lows, to = highs, by = 1))
    # output is a list, list entry number = position of param in family_info$lower 
    grid <- expand.grid(grid)
    colnames(grid) <- names(family_info$lower)[non_floats]
    # number of columns of result matrix: number of variable parameters + two (loglikelihood & convergence code)
    num_free_params <- length(family_info$lower) - sum(non_floats)
    grid_results <- matrix(NA, nrow = nrow(grid), ncol = (num_free_params + 2) )
    colnames(grid_results) < c(colnames(grid), "loglik", "convergence")
    for(i in 1:grid) {
      optim_res <- tryCatch(
	{
	  optimParam(data = data, family = family, lower = family_info$lower[!non_floats], upper = family_info$upper[!non_floats], defaults = family_info$defaults[!non_floats], method = method, fixed = grid[i, ], prior = prior, log = log, optim_method = optim_method, n_starting_points = n_starting_points, debug_error = debug_error, show_optim_progress = show_optim_progress, on_error_use_best_result = on_error_use_best_result, ...)
	},
	error <- function(e) {
	  message(e);
	  # generate a NA row of appropriate length to impute into grid_results
	  list(
	    par = rep(NA, times = (num_free_params)),
	    val = NA,
	    convergence = 99
	  )
	} # end error handler
      ) # end tryCatch
      grid_results[i, ] <- c(optim_res$par, optim_res$val, optim_res$convergence)
    } # end for-loop in grid
    # drop all weird cases
    discrete_results <- grid_results[grid_results$convergence == 0, ]
    optimum_index <- which.max(discrete_results$loglik)
    # run optimParam again for the best grid cell to retrieve information criteria, otherwise grid_results would blow up too much
    # difference to the optimParam above: argument fixed is changed!
    optim_res <- tryCatch(
      {
        optimParam(data = data, family = family, lower = family_info$lower[!non_floats], upper = family_info$upper[!non_floats], defaults = family_info$defaults[!non_floats], method = method, fixed = grid[optimum_index, ], prior = prior, log = log, optim_method = optim_method, n_starting_points = n_starting_points, debug_error = debug_error, show_optim_progress = show_optim_progress, on_error_use_best_result = on_error_use_best_result, ...)
      },
      # error should not occur because the combination had passed the first time!
      error <- function(e) {
        message(e);
        list(par = rep(NA, times = (num_free_params)),
	     val = NA,
	     convergence = 99)
        }
      )
    return(optim_res)
  }
}


### Testing Area ###
####################

optim_params_wrapper(rbinom(n=1000, size=10, prob=0.5), family_list[[2]][c('package', 'family')], 
                      family_list[[2]]$family_info, debug_error=F, max_discrete_steps = 100)
optim_params_wrapper(rnorm(n=1000, mean=2, sd=0.5), family_list[[12]][c('package', 'family')],
                      family_list[[12]]$family_info, debug_error=F)
# TODO: What is this? :D
family_info <- family_list[[12]]
