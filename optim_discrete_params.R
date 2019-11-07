source("optimParam.R")


# TODO: in general, we often restrict parameters to be in a reasonable bound: [-100,100]
# this might not hold. Maybe we can let the user specify something like parscale, that lets us space out everything,
# and some kind of accuracy so we optimise rigorously locally after a roguh "global" view

# Function for doing the optimisation of discrete parameters
optim_discrete_params <- function(data, family_info, method = 'MLE', prior = NULL, log=TRUE,
                                  optim_method = 'L-BFGS-B', n_starting_points=1,
                                  debug_error=TRUE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, ...){
  
  if (all(family_info$accepts_float)) {
    return(optimParam())   # TODO: 
  }
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
  names(grid) <- names(family_info$lower)[non_floats]
  grid_results <- matrix(NA, nrow = nrow(grid), ncol = ncol(grid)+2)
  names(grid_results) < c(names(grid), "loglik", "convergence")
  for(i in 1:grid) {
    optim_res <- optimParam([ TODO: ERWISCHT! BENNI KOMM KOCHEN ], fixed = grid[i, ], [ HELLO BENEDIKT])
    grid_results[i, ] <- c(optim_res$par, optim_res$val, optim_res$convergence)
  }
  # drop all weird cases
  discrete_results <- grid_results[grid_results$convergence == 0, ]
  optimum_index <- which.max(discrete_results$loglik)
  # run optimParam again for the best grid cell to retrieve information criteria, otherwise grid_results would blow up too much
  optim_res <- optimParam( [TODO: whatever], fixed = grid[optimum_index, ], [blaaargh] )
  return(optim_res)
}
