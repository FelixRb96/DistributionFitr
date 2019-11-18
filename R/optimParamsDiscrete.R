source('utils.R')
source('optimParamsContinuous.R')

### STATEMENT OF PURPOSE ###
############################

# This function is a wrapper for optimParamsContinuous
# Implementation is not final, target for now is to get a working solution - no matter the runtime efficiency.
# This wrapper discerns the following cases:
# (a) no integer parameters: call optimParamsContinuous straight away
# (b) one integer parameter: use greedy solution. Questions to: Moritz Kern
# (c) multiple integer parameters: use naive solution. Question to: Borui N. Zhu

# TODO: make sure input validation is implemented on the uppermost level!

# TODO: in general, we often restrict parameters to be in a reasonable bound: [-100,100] (refer to case (b) )
# this might not hold. Maybe we can let the user specify something like parscale, that lets us space out everything,
# and some kind of accuracy so we optimise rigorously locally after a roguh "global" view

# Function for doing the optimisation of discrete parameters
optimParamsDiscrete <- function(data, family, family_info, method = 'MLE', prior = NULL, log=TRUE,
                                optim_method = 'L-BFGS-B', n_starting_points=1,
                                debug_error=FALSE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, 
                                max_discrete_steps=100, plot=FALSE, discrete_fast = TRUE, ...) {
  
  # update defaults with priors
  if(length(prior) > 0) {
    match <- match(names(prior), names(family_info$lower))
    family_info$defaults[match] <- prior
  }

  #: define loop variables
  i <- 1
  stop_discrete <- FALSE
  
  # CASE 1: No discrete params -> we can directly redirect to optimParamsContinuous
  if (all(family_info$accepts_float)) {
    optim_res <- tryCatch({
      optimParamsContinuous(data=data, family=family, lower=family_info$lower, upper=family_info$upper,
                            defaults = family_info$defaults, method = method, fixed=c(), log = log, optim_method = optim_method,
                            nn_starting_points=n_starting_points, debug_error = debug_error, show_optim_progress = show_optim_progress,
                            on_error_use_best_result = on_error_use_best_result, ...) 
      }, error=function(e) NULL
    )
    if (is.null(optim_res)) return(NULL)
    
  } # CASE 2: exactly one discrete parameter 
  else if( sum(!family_info$accepts_float)==1 ) {

    # get the discrete parameter
    dispar_id <- family_info$accepts_float==FALSE
    dispar_default <- family_info$defaults[dispar_id]
    
    cont_optim_results <- vector('list',length=max_discrete_steps)       # vector of the single optimisation results
    history_ <- data.frame(matrix(NA, nrow=max_discrete_steps, ncol=3))  # history dataframe where the optim progress is stored
    colnames(history_) <- c("param_value", "direction", "log_lik")
    
    # as we iterate both left and rightwards staring from the default value we need to save the current values
    # first iteration with the default value is considered to be left
    cur_left_val <- dispar_default
    cur_right_val <- dispar_default + 1
    touched_lower <- touched_upper <- FALSE    # whether our left or right iteration has reached the border
    while(!stop_discrete) {
      if (!touched_lower && ! touched_upper) {
        direction <- c("right", "left")[i%%2 + 1]  # make sure that first one is left!, apart from that always alternate if possible
      } else if (!touched_upper) {
        direction <- "right"
      } else if (!touched_lower) {
        direction <- "left"
      } else break
      
      dispar <- if (direction == "right") cur_right_val else cur_left_val
      if(show_optim_progress) cat("Discrete Parameter:", dispar, "\n")
      
      # optimise with the current fixed value
      curr_res <- tryCatch(optimParamsContinuous(
        data=data, family=family, lower=family_info$lower[-dispar_id], upper=family_info$upper[-dispar_id],
        defaults = family_info$defaults[-dispar_id], method = method, fixed=dispar, log = log, optim_method = optim_method,
        n_starting_points = n_starting_points, debug_error = debug_error, show_optim_progress = show_optim_progress,
        on_error_use_best_result = on_error_use_best_result),
        error = function(e) {
          # message(e);
          NULL})
      
      # if successful add results to dataframe
      if(!is.null(curr_res)) {
        cont_optim_results[[i]] <- curr_res
        cont_optim_results[[i]]$par[names(dispar_default)] <- dispar[1]
        history_[i, ] <- list(dispar[1], direction, cont_optim_results[[i]]$value)
      } else {
        history_[i, ] <- list(dispar[1], direction, NA)
      }
      
      # update current iteration values and check whether bound is reached or score has not improved
      if (direction == "right") {
        cur_right_val <- cur_right_val + 1
        if (cur_right_val > family_info$upper[dispar_id]) touched_upper <- TRUE
        
        # get all the results for "right" achieved up to now
        # we stop when the current result is worse than the best one achieved in this direction
        relevant_hist <- history_[history_$direction == "right", "log_lik"]
        relevant_hist <- relevant_hist[!is.na(relevant_hist)]
        if (discrete_fast && length(relevant_hist) > 2 && relevant_hist[length(relevant_hist)] < max(relevant_hist))
          touched_upper <- TRUE
      }
      
      if (direction == "left") {
        cur_left_val <- cur_left_val - 1
        if (cur_left_val < family_info$lower[dispar_id]) touched_lower <- TRUE
        
        relevant_hist <- history_[history_$direction == "left", "log_lik"]
        relevant_hist <- relevant_hist[!is.na(relevant_hist)]
        if (discrete_fast && length(relevant_hist) > 2 && relevant_hist[length(relevant_hist)] < max(relevant_hist)) 
          touched_lower <- TRUE
      }
      
      i <- i+1
      
      # stop if not converged so far
      if(i>max_discrete_steps) {
        stop_discrete <- TRUE
        warning('Discrete Optimization aborted, did not converge.')
      }
    }
    
    print(history_)
    if(plot) {
      plot(history_$param_value, ifelse(is.finite(history_$log_lik), history_$log_lik, NA), 
           ylab="log_lik", xlab=names(family_info$lower)[dispar_id])
    }
    
    # take the best result
    if (sum(!is.na(history_$log_lik)) > 0) {
      optim_res <- cont_optim_results[[which.max(history_$log_lik)]]
    } else {
      message("No valid discrete optimization result achieved")
      return(NULL)
    }
      
    
  } else { # Case 3: more than one non-integer parameter, but at least one continuous one
    non_floats <- !family_info$accepts_float
    num_discrete <- sum(non_floats)
    final_ll <- numeric(1)
    ## naive implementation
    # get parameter ranges of non-float parameters, make compact grid
    # To deal with a piori arbitrarily large values, let's try something I'll call the Google Earth algorithm
    # start with reasonable ranges
    found <- FALSE
    zoom <- zoom_level <- rep(0, times = num_discrete)
    # at the start, centre over defaults. later: centre over maximum and zoom in/out
    centre <- family_info$defaults[non_floats]
    while_counter <- 0
    while(found == FALSE) {
      while_counter <- while_counter + 1
      zoom_level <- zoom_level + zoom
      # centre is always an integer
      grid_low <- centre-(25*(10^zoom_level))
      grid_high <- centre+(25*(10^zoom_level))
      stepsize <- rep(1, times = num_discrete)*(10^zoom_level)
      lows <- pmax(family_info$lower[non_floats], grid_low)
      # at the start, centre over defaults. later: centre over maximum and zoom in/out
      highs <- pmin(family_info$upper[non_floats], grid_high)
      # get_params shall insure that lower and upper are all integers
      # is there a vectorised version of seq()?
      seq_vec <- Vectorize(seq.default, vectorize.arg = c("from", "to", "by"), SIMPLIFY = FALSE)
      grid <- seq_vec(from = lows, to = highs, by = stepsize)
      grid <- expand.grid(grid)
      # output is a list, list entry number = position of param in family_info$lower 
      colnames(grid) <- names(family_info$lower)[non_floats]
      # number of columns of result matrix: number of variable parameters + two (loglikelihood & convergence code)
      num_free_params <- length(family_info$lower) - sum(non_floats)
      grid_results <- matrix(NA, nrow = nrow(grid), ncol = 2 )
      colnames(grid_results) <- c("loglik", "convergence")
      pb <- txtProgressBar(min = 0, max = nrow(grid))
      print("Please stand by shortly...")
      for(i in 1:nrow(grid)) {
	      optim_res <- tryCatch(
	        {
	          optimParamsContinuous(data = data, family = family, lower = family_info$lower[!non_floats], upper = family_info$upper[!non_floats], 
	               defaults = family_info$defaults[!non_floats], method = method, fixed = grid[i, ], prior = prior, log = log, 
	               optim_method = optim_method, n_starting_points = n_starting_points, debug_error = debug_error, 
	               show_optim_progress = show_optim_progress, on_error_use_best_result = on_error_use_best_result, no_second = TRUE, ...)
	  },
	  error = function(e) {
	    message(e);
	    # generate a NA row of appropriate length to impute into grid_results
	    list(
	      val = NA,
	      convergence = 99
	    )
	  } # end error handler
        ) # end tryCatch
        grid_results[i, ] <- c(optim_res$val, optim_res$convergence)
	setTxtProgressBar(pb, i)
      } # end for-loop in grid
      # drop all weird cases
      discrete_results <- grid_results[grid_results[,'convergence'] == 0, ]
      optimum_index <- which.max(discrete_results[,'loglik'])
      cat('\noptimal index:', optimum_index, '\n')

      final_ll <- grid_results[optimum_index,'loglik']

      # Google Earth: check if optimum is at the bound of our grid. If so, zoom out and center! if not: accept and break.
      boundary_check <- ( (grid[optimum_index, ] == grid_low) | (grid[optimum_index, ] == grid_high) )
      if (sum(boundary_check) > 0) {
	      centre <- grid[optimum_index, ]
        # zoom out only in dimensions where max was at boundaries
        zoom <- as.numeric(boundary_check)
	print("Zooming out!")
      } else if (max(zoom_level) > 1) {
	      centre <- grid[optimum_index, ]
        # since no boundary optima, zoom in wherever zoom is highest
        max_zoom <- max(zoom_level)
	which_max <- (zoom_level == max_zoom) # do not use which.max, as index may not be unique
        zoom <- as.numeric(which_max) * (-1)
	print("Zooming back in!")
      } else if (while_counter > 5) {
        break
      } else {
	break
      }
      # take optimum_index and proceed further    
      if(i>max_discrete_steps) {
        stop_discrete <- TRUE
        warning('Discrete Optimization aborted, did not converge.')
      }  
    }

    # run optimParamsContinuous again for the best grid cell to retrieve information criteria, otherwise grid_results would blow up too much
    # difference to the optimParamsContinuous above: argument fixed is changed!
    optim_res <- tryCatch(
      {
        optimParamsContinuous(data = data, family = family, lower = family_info$lower[!non_floats], upper = family_info$upper[!non_floats], 
                   defaults = family_info$defaults[!non_floats], method = method, fixed = grid[optimum_index, ], prior = prior, 
                   log = log, optim_method = optim_method, n_starting_points = n_starting_points, debug_error = debug_error, 
                   show_optim_progress = show_optim_progress, on_error_use_best_result = on_error_use_best_result, ...)
      },
      # error should not occur because the combination had passed the first time!
      error = function(e) {
        message(e);
        list(par = rep(NA, times = (num_free_params)),
	     val = NA,
	     convergence = 99)
      }
    )
    final_params <- as.vector(c(optim_res$par, grid[optimum_index, ]), mode = "numeric")
    names(final_params) <- c(names(optim_res$par), colnames(grid[optimum_index, ]))
    reorder <- match(names(final_params), names(family_info$lower))
    optim_res$par <- final_params[reorder]
    print(optim_res$par)
    optim_res$value <- final_ll
  }

  # ICs are the same, since discrete parameters are still parameters we optimise over
  ic <- informationCriteria(ll = optim_res$value, n = length(data), k = length(family_info$upper))
  optim_res <- c(optim_res, ic)
  return(optim_res)
}


