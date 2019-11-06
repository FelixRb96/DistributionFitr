source("optimParam.R")

# Function for doing the optimisation of discrete parameters
optim_discrete_params <- function(data, family_info, method = 'MLE', prior = NULL, log=TRUE,
                                  optim_method = 'L-BFGS-B', n_starting_points=1,
                                  debug_error=TRUE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, ...){
  
  if (all(family_info$accepts_float)) {
    return(optimParam())   # TODO: 
  }
}