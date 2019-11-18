# -------------------------------------------------------------------------------- 
#  Testing function
# -------------------------------------------------------------------------------- 

rm(list=ls())
source("optimParamsContinuous.R")
source("getParams.R")
source("utils.R")

evaluate_optimization <- function(true_pars, optim_result) {
  error_percent <- 100 * (true_pars - optim_result$par) / true_pars
  
  return(list(
    # optim_result = optim_result,
    error_percent = error_percent,
    log_lik = optim_result$value
  ))
}


test_single_family <- function(n, family) {
    
  # cat("Getting infos about the distribution\n")
  family_info <- getParams(family)
  
  # if we couldn't find infos on the distribution
  if (is.null(family_info)) {
    message("Couldn't get infos about the distribution ", family$family)
    return(NULL)
  }
  
  # sample parameters for the family and generate sample data
  true_pars <- sample_params(family, family_info)
  cat("Sampled params:", paste(names(true_pars), true_pars, sep=": ", collapse=", "), "\n")
  
  testing_data <- sample_data(n, family, true_pars)
  
  
  # we do it this way for now since we want to evaluate optim_param
  optim_result <- tryCatch(optimParamsContinuous(data = testing_data, family = family, lower = family_info$lower, upper = family_info$upper, 
                                 defaults = family_info$default, log = family_info$log, debug_error = TRUE, show_optim_progress = FALSE),
                      error = function(e) {
                        message(e);
                        NULL}
  )
  if (! is.null(optim_result)) {
    eval <- evaluate_optimization(true_pars, optim_result)
    cat("Optimized params:", paste(names(true_pars), optim_result$par, sep=": ", collapse=", "), "\n")
    cat("Deviation true - estimated params in %\n")
    print(eval$error_percent)
    cat("(Log-)Likelihood for optimal parameters: ", eval$log_lik, "\n")
  }
}

# Example:
test_single_family(1000, list(package="stats", family="beta"))

test_families <- function(n, families) {
  for(fam in families) {
    cat("\n\nCurrent Family:", fam$family, "\n")
    test_single_family(1000, fam)
  }
}

test_families(1000, families)

# Function to compare different optimization techniques
compare_optimizers <- function(n, families, repetitions_per_family=5) {
  
  # result dataframe: columns will be the different approaches / methods, rows will be the distribution families
  # change column names to whatever you are comparing
  cols <- c("noscale5", "fnscale5", "parscale", "bothscale")
  rows <- sapply(families, function(fam) fam$family)
  res <- data.frame(matrix(NA, nrow=length(rows), ncol=length(cols)))
  rownames(res) <- rows
  colnames(res) <- cols
  
  for (fam in families) {
    cat("\nCurrent Family", fam$family, "\n")
    family_info <- getParams(fam)
    print(family_info)
    # if we couldn't find infos on the distribution
    if (is.null(family_info)) next
    
    optim_results <- matrix(NA, nrow=repetitions_per_family, ncol=length(cols))
    
    for (i in 1:repetitions_per_family) {
      # sample parameters for the family and generate sample data
      true_pars <- sample_params(fam, family_info)
      cat("Try Nr.", i, "\nSampled params:", paste(names(true_pars), round(true_pars, 4), sep=": ", collapse=", "), "\n")
      
      testing_data <- sample_data(n, fam, true_pars)
      
      ##  CHANGE OPTIMZATION SETTINGS HERE to compare different ones!!!
      
      cat("fnscale = FALSE, parscale = FALSE\n")
      optim_results[i, 1] <- tryCatch(optimParamsContinuous(data = testing_data, family = fam, lower = family_info$lower, upper = family_info$upper,
                                                fnscale=FALSE, parscale=FALSE, optim_method = "L-BFGS-B",
                                                defaults = family_info$default, log = family_info$log, 
                                                debug_error = TRUE, show_optim_progress = FALSE, n_starting_points = 5)$value,
                                     error = function(e) NA
                                     )

      cat("fnscale = TRUE, parscale = FALSE\n")
      optim_results[i, 2] <- tryCatch(optimParamsContinuous(data = testing_data, family = fam, lower = family_info$lower, upper = family_info$upper,
                                                fnscale=TRUE, parscale=FALSE, optim_method = "L-BFGS-B",
                                                defaults = family_info$default, log = family_info$log, 
                                                debug_error = TRUE, show_optim_progress = FALSE, n_starting_points = 5)$value,
                                     error = function(e) NA
      )
      cat("fnscale = FALSE, parscale = TRUE\n")
      optim_results[i, 3] <- tryCatch(optimParamsContinuous(data = testing_data, family = fam, lower = family_info$lower, upper = family_info$upper,
                                                fnscale=FALSE, parscale=TRUE, optim_method = "L-BFGS-B",
                                                defaults = family_info$default, log = family_info$log, 
                                                debug_error = TRUE, show_optim_progress = FALSE)$value,
                                     error = function(e) NA
      )
      cat("fnscale = TRUE, parscale = TRUE\n")
      optim_results[i, 4] <- tryCatch(optimParamsContinuous(data = testing_data, family = fam, lower = family_info$lower, upper = family_info$upper,
                                                fnscale=TRUE, parscale=TRUE, optim_method = "L-BFGS-B",
                                                defaults = family_info$default, log = family_info$log, 
                                                debug_error = TRUE, show_optim_progress = FALSE)$value,
                                     error = function(e) NA
      )
    }
    cat("\nOptimResults:\n")
    print(optim_results)
    # for each method calculate the means and add them to the result matrix
    res[fam$family, ] <- colMeans(optim_results, na.rm = TRUE)
    print(res)
  }
  return(res)
}

compare_optimizers(1000, families, repetitions_per_family = 5)


# TODOs:
# optimisation with L-BFGS-B still sometimes a bit unstable, to often "L-BFGS-B benötigt endliche Werte von 'fn'", that is loglik seems to 
# return Inf to often

# maybe try different optimisation procedures
# for each procedure compare optimisation results across all families (sth like mean percentage deviation)

# maybe try to find the best optimisation procedure for each family independently already in getParams with sampled data

# -------------------------------------------------------------------------------- 
#  Testing function
# -------------------------------------------------------------------------------- 

rm(list=ls())
source("optimParamsContinuous.R")
source("getParams.R")
source("utils.R")

evaluate_optimization <- function(true_pars, optim_result) {
  error_percent <- 100 * (true_pars - optim_result$par) / true_pars
  
  return(list(
    # optim_result = optim_result,
    error_percent = error_percent,
    log_lik = optim_result$value
  ))
}


test_single_family <- function(n, family) {
    
  # cat("Getting infos about the distribution\n")
  family_info <- getParams(family)
  
  # if we couldn't find infos on the distribution
  if (is.null(family_info)) {
    message("Couldn't get infos about the distribution ", family$family)
    return(NULL)
  }
  
  # sample parameters for the family and generate sample data
  true_pars <- sample_params(family, family_info)
  cat("Sampled params:", paste(names(true_pars), true_pars, sep=": ", collapse=", "), "\n")
  
  testing_data <- sample_data(n, family, true_pars)
  
  
  # we do it this way for now since we want to evaluate optim_param
  optim_result <- tryCatch(optimParamsContinuous(data = testing_data, family = family, lower = family_info$lower, upper = family_info$upper, 
                                 defaults = family_info$default, log = family_info$log, debug_error = TRUE, show_optim_progress = FALSE),
                      error = function(e) {
                        message(e);
                        NULL}
  )
  if (! is.null(optim_result)) {
    eval <- evaluate_optimization(true_pars, optim_result)
    cat("Optimized params:", paste(names(true_pars), optim_result$par, sep=": ", collapse=", "), "\n")
    cat("Deviation true - estimated params in %\n")
    print(eval$error_percent)
    cat("(Log-)Likelihood for optimal parameters: ", eval$log_lik, "\n")
  }
}

# Example:
test_single_family(1000, list(package="stats", family="beta"))

test_families <- function(n, families) {
  for(fam in families) {
    cat("\n\nCurrent Family:", fam$family, "\n")
    test_single_family(1000, fam)
  }
}

test_families(1000, families)

# Function to compare different optimization techniques
compare_optimizers <- function(n, families, repetitions_per_family=5) {
  
  # result dataframe: columns will be the different approaches / methods, rows will be the distribution families
  # change column names to whatever you are comparing
  cols <- c("noscale5", "fnscale5", "parscale", "bothscale")
  rows <- sapply(families, function(fam) fam$family)
  res <- data.frame(matrix(NA, nrow=length(rows), ncol=length(cols)))
  rownames(res) <- rows
  colnames(res) <- cols
  
  for (fam in families) {
    cat("\nCurrent Family", fam$family, "\n")
    family_info <- get_params(fam)
    print(family_info)
    # if we couldn't find infos on the distribution
    if (is.null(family_info)) next
    
    optim_results <- matrix(NA, nrow=repetitions_per_family, ncol=length(cols))
    
    for (i in 1:repetitions_per_family) {
      # sample parameters for the family and generate sample data
      true_pars <- sample_params(fam, family_info)
      cat("Try Nr.", i, "\nSampled params:", paste(names(true_pars), round(true_pars, 4), sep=": ", collapse=", "), "\n")
      
      testing_data <- sample_data(n, fam, true_pars)
      
      ##  CHANGE OPTIMZATION SETTINGS HERE to compare different ones!!!
      
      cat("fnscale = FALSE, parscale = FALSE\n")
      optim_results[i, 1] <- tryCatch(optimParamsContinuous(data = testing_data, family = fam, lower = family_info$lower, upper = family_info$upper,
                                                fnscale=FALSE, parscale=FALSE, optim_method = "L-BFGS-B",
                                                defaults = family_info$default, log = family_info$log, 
                                                debug_error = TRUE, show_optim_progress = FALSE, n_starting_points = 5)$value,
                                     error = function(e) NA
                                     )

      cat("fnscale = TRUE, parscale = FALSE\n")
      optim_results[i, 2] <- tryCatch(optimParamsContinuous(data = testing_data, family = fam, lower = family_info$lower, upper = family_info$upper,
                                                fnscale=TRUE, parscale=FALSE, optim_method = "L-BFGS-B",
                                                defaults = family_info$default, log = family_info$log, 
                                                debug_error = TRUE, show_optim_progress = FALSE, n_starting_points = 5)$value,
                                     error = function(e) NA
      )
      cat("fnscale = FALSE, parscale = TRUE\n")
      optim_results[i, 3] <- tryCatch(optimParamsContinuous(data = testing_data, family = fam, lower = family_info$lower, upper = family_info$upper,
                                                fnscale=FALSE, parscale=TRUE, optim_method = "L-BFGS-B",
                                                defaults = family_info$default, log = family_info$log, 
                                                debug_error = TRUE, show_optim_progress = FALSE)$value,
                                     error = function(e) NA
      )
      cat("fnscale = TRUE, parscale = TRUE\n")
      optim_results[i, 4] <- tryCatch(optimParamsContinuous(data = testing_data, family = fam, lower = family_info$lower, upper = family_info$upper,
                                                fnscale=TRUE, parscale=TRUE, optim_method = "L-BFGS-B",
                                                defaults = family_info$default, log = family_info$log, 
                                                debug_error = TRUE, show_optim_progress = FALSE)$value,
                                     error = function(e) NA
      )
    }
    cat("\nOptimResults:\n")
    print(optim_results)
    # for each method calculate the means and add them to the result matrix
    res[fam$family, ] <- colMeans(optim_results, na.rm = TRUE)
    print(res)
  }
  return(res)
}

compare_optimizers(1000, families, repetitions_per_family = 5)


# TODOs:
# optimisation with L-BFGS-B still sometimes a bit unstable, to often "L-BFGS-B benötigt endliche Werte von 'fn'", that is loglik seems to 
# return Inf to often

# maybe try different optimisation procedures
# for each procedure compare optimisation results across all families (sth like mean percentage deviation)

# maybe try to find the best optimisation procedure for each family independently already in get_params with sampled data
