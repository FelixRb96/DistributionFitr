if (! "stringr" %in% installed.packages()) install.packages("stringr")
library(stringr)

### 1) Get all distributions within a package: ---------------------------------------------------------------------

package <- "stats"
possible_dists <- lsf.str(paste0("package:", package), pattern="^[rdpq]")   # all functions starting with r, d, p or q
possible_dists
start_chars <- c("d", "p", "q", "r")   
first_args <- c("x", "q", "p", "n")    # first parameters of the d, p, q, r functions
l <- list()
for (i in 1:length(start_chars)) {
  char <- start_chars[i]
  first_arg <- first_args[i]
  subset <- grep(paste0("^", char), possible_dists, value=TRUE)                     # all functions starting with char
  valid_idx <- sapply(subset, function(x) names(as.list(args(x)))[1] == first_arg)  # check if all functions have the correct first arg
  print(valid_idx)
  l[[char]] <- subset[valid_idx]
}

get_endings <- function(vec) str_sub(vec, start=2)

l_endings <- lapply(l, get_endings)    # remove the d, p, q, r suffixes

# we definitely need a function for the density starting with d, as otherwise we cannot evaluate likelihood function
# so we only take the endings from p, q and r that also appear in d
for (char in start_chars[-1]) {
  l_endings[[char]] <- intersect(l_endings[[char]], l_endings$d)
}

freq <- table(unlist(l_endings))     # get a frequency table of the endings
freq <- freq[freq>=2]                # only take thos distributions that have at least 2 functions implemented
freq

res <- list(package = package, family=names(freq))
res


### 2) Try to get the parameters (+ infos) from a distribution ----------------------------------------------------------------

## Main ideas:
# 1) extract all possible params from r... function as the r function should need all params
# 2) check if all of them have default values set
# 2.1) If yes got to 3)
# 2.2) If not for each param with missing default value guess such a value and test if r..(1, params) returns a value or NA
#      If a valid value is returned take the current set of default values, otherwise try a different combination of default values
# 3) For each of the params test some values (non-integer, negative, not in [0,1]) while keeping the others at their defaults

## optional TODO:
# check whether param name contains prob or sth like that -> range should be [0,1]

families <- res$family
fam <- "beta" # "gamma"

get_all_params <- function(fam) {
  # idea: all params need to be present in the r... method for generating random samples from the distribution
  all_params <- as.list(args(paste0("r", fam)))
  
  # if "nn" is contained then "n" is probably a real param , c.f. rhyper
  to_remove <- if("nn" %in% names(all_params)) c("", "nn") else c("", "n", "nn")
  
  all_params <- all_params[! names(all_params) %in% to_remove]    # remove empty and the n_samples argument
  
  # in cases like gamma distribution we have multiple parameters describing the same (e.g. rate and scale)
  # -> we need to drop those as we can't optimize them independently
  for (i in 1:length(names(all_params))) {
    param <- names(all_params[i])
    if (typeof(all_params[[param]]) == "language") {          # TRUE if default val is function of another param, e.g. scale = 1/rate
     function_parts <- as.character(all_params[[param]])      # divide the function into its components
     
     # check if one function part is the name of one of the earlier parameters, and remove if thats true
     if (i > 1 && any(function_parts %in% names(all_params)[1:(i-1)]))
       cat(param, "depends on another earlier parameter and is thus removed\n")
       all_params[[param]] <- NULL
    }
  }
  return(all_params)
}

all_params <- get_all_params(fam)



## Find default values ----------------------------------------------------------------------------------------------------

# first we need to find one set of values for each of the params that actually works before we can check for valid values for each
# of the single params individually
get_default_values <- function(all_params, fam) {
  missing_defaults <- sapply(all_params, function(x) typeof(x) == "symbol")   # missing values seem to have type "symbol"
  
  if (sum(missing_defaults) == 0) return(all_params)
  
  with_defaults <- all_params[!missing_defaults]
  non_defaults <- all_params[missing_defaults]
  
  # both floats and integers and both positive and negative as well as 0 so that at least one of those hopefully is valid
  default_guesses <- c(1, 0.5, 0, -0.5, -1)
  
  # create a dataframe with all combinations of default guesses
  combs <- expand.grid(lapply(non_defaults, function(x) default_guesses))
  combs_list <- split(combs, seq(nrow(combs)))   # convert to list for iteration
  valid_params <- NULL
  
  # parameter that describes the number of random numbers to take, usually "n", but in cases like hyper "nn"
  n_or_nn <- if (! "nn" %in% names(as.list(args(paste0("r", fam))))) list(n=1) else list(nn=1)
  
  errors <- c()
  for (i in 1:length(combs_list)) {
    
    # combine fixed with guessed default values and try to generate a random number
    curr_params <- c(with_defaults, combs_list[[i]])
    res <- suppressWarnings(tryCatch(do.call(paste0("r", fam), c(n_or_nn, curr_params)), 
                                     error=function(e) {
                                       errors <<- union(errors, strsplit(as.character(e), ":", fixed=TRUE)[[1]][2]);
                                       return(NA)}
                                     ))
    
    # break if we've found a set of valid values
    if (!is.na(res)){
      valid_params <- curr_params
      cat("Found the following set of valid default values for family", fam, ":", 
          paste(names(valid_params), valid_params , sep=": ", collapse=", "), "\n")
      break
    } 
  }
  if (is.null(valid_params)) {
    message("Could not find a set of valid default values for family ", fam)
    message("Errors:", errors)
  }
  return(valid_params)
}

all_params <- get_default_values(all_params, fam)


## Get parameter ranges -----------------------------------------------------------------------------------------------------

## Now we can iterate over the parameters while keeping fixed all others in order to guess some valid ranges
# Note that all_params will now always contain some default values for each of the params (as long as one has been found)

# function that checks if all "values" are valid for "param". "all_params" contains the values of the other params of the family "fam"
check_values_for_param <- function(param, all_params, fam, values) {
  
  # parameter that describes the number of random numbers to take, usually "n", but in cases like hyper "nn"
  n_or_nn <- if (! "nn" %in% names(as.list(args(paste0("r", fam))))) list(n=1) else list(nn=1)
  
  res <- suppressWarnings(
    sapply(values, function(x) {
      # set the value of "param" to each of the "values" that should be tested
      all_params[[param]] <- x;
      # try to generate a random number from the distribution
      tryCatch(do.call(paste0("r", fam), c(n_or_nn, all_params)), 
               error=function(e) return(NA))
    })
  )
  return(is.finite(res))      # TRUE if value was valid, FALSE if not
}

#  example (for beta family)
check_values_for_param("shape1", all_params, fam, values=c(-100, -10, -1, 1))

# function that iterates over descending step sizes to find the minimal and maximal valid value of a parameter
# Explanation:
  # in previous iteration we tested the values [-10, -5, 0, 5, 10], that is step_size was 5
  # valid values: [FALSE, FALSE, TRUE, TRUE, TRUE]
  # now the lower limit can be anywhere between -5 and 0
  # so we test with next step size=1 the values [-5, -4, -3, -2, -1, 0]
  # from those values we again search the minimum valid value and continue in the same way with always smaller steps
  # for the upper bound the procedure is the same, just that we search for valid values in the raneg higher than the current upper limit
iterate_min_max_vals <- function(param, all_params, fam, cur_val, step_sizes, is_min=TRUE) {
  for (i in 1:(length(step_sizes)-1)) {
    
    # first get the interval that should be tested
    if (is_min) {
      border <- cur_val-step_sizes[i]
      vals <- seq(border, cur_val, by=step_sizes[i+1])
    } else{
      border <- cur_val+step_sizes[i]
      vals <- seq(cur_val, border, by=step_sizes[i+1])
    }
    # test values and adjust the current estimate
    # cat(param, "-> Currently checking values in the interval [", min(vals), ",", max(vals), "]\n")
    check_res <- check_values_for_param(param, all_params, fam, vals)
    cur_val <- ifelse(is_min, min(vals[check_res]), max(vals[check_res]))
    
    # if the smallest (or highest) of the tested values was valid we can break
    # however this should not happen when the method is used as below
    if (cur_val == border) {
      # cat("Abbruchbedingung")
      return(cur_val)
    }
  }
  # cat(param, "-> Final value:", cur_val, "\n")
  return(cur_val)
}

# example (for beta family)
iterate_min_max_vals("shape1", all_params, fam, cur_val=0, step_sizes = c(1, 0.5, 0.1, 0.05))


### Main function for generating the info for each of the params ------------------------------------------------------

get_param_ranges <- function(all_params, fam) {
  lower <- upper <- accepts_float <- rep(0, length(all_params))
  names(lower) <- names(upper) <- names(accepts_float) <- names(all_params)
  
  for (param in names(all_params)){
    
    # 1) Try to find upper and lower bounds (if there are some)
    initial_min_val <- -1e6
    initial_max_val <- 1e6
    step_sizes <- c(1e5, 50, 10, 1, 0.1, 0.01)
    
    # add the default value to have at least one valid entry
    vals <- seq(initial_min_val, initial_max_val, by = step_sizes[1]) + all_params[[param]]  
    # cat("current step size:", step_sizes[1], "\n")
    check_res <- check_values_for_param(param, all_params, fam, vals)
    
    # if lowest or highest value was valid in the first check we already have an min_val or max_val
    # otherwise we iterate with the above method
    if (check_res[1]) {
      min_val <- initial_min_val
    } else {
      min_val <- iterate_min_max_vals(param=param, all_params = all_params, fam=fam,
                                      cur_val = min(vals[check_res]), step_sizes = step_sizes, is_min = TRUE)
    }
    
    if (check_res[length(check_res)]) {
      max_val <- initial_max_val
    } else {
      max_val <- iterate_min_max_vals(param=param, all_params = all_params, fam=fam,
                                      cur_val = max(vals[check_res]), step_sizes = step_sizes, is_min = FALSE)
    }
    # set the estimated values in the named vactor that will be returned
    lower[param] <- min_val
    upper[param] <- max_val
    
    ### TODO:
    n <- 10
    testsequence <- runif(n, min_val, max_val)
    testsequence <- unique(c(testsequence, trunc(testsequence)))
    num_tests <- length(testsequence)

    is_integer <- is.integer(testsequence)
    num_integer <- sum(is_integer)

    testoutcome <- check_values_for_param(param, all_params, fam, testsequence)
    accepted_int <- sum(is_integer & testoutcome)
    accepted_int_rate <- accepted_int/num_integer
    accepted_float <- sum(!is_integer & testoutcome)
    accepted_float_rate <- accepted_float/(num_tests - num_integer)

    if(accepted_float_rate > (1/num_tests - num_integer)) accepts_float_param = TRUE
    else if(accepted_int_rate == 0) error("distribution does not seem to accept any values")
    else accepts_float_param = FALSE

    accepts_float[param] <- accepts_float_param
  }
  return(list(lower=lower,
              upper=upper,
	      accepts_float=accepts_float))
}



### Final function -------------------------------------------------------------------------------------------------------
# Input:
  # family: list-> package: package_name, family: name of distribution family inside package
get_params <- function(fam){
  # fam <- family$family
  
  # 1) Get list of all parameters:
  all_params <- get_all_params(fam)
  
  # 2) Add default values to all params that don#t have any
  all_params <- get_default_values(all_params, fam)
  
  # 3) Get valid parameter ranges:
  ranges <- get_param_ranges(all_params, fam)
  lower <- ranges$lower
  upper <- ranges$upper
  return(ranges)
}

for (fam in families) {
  cat("Current Family:", fam, "\n")
  ranges <- get_params(fam)
  cat("Lower bounds:\n")
  print(ranges$lower)
  cat("Upper bounds:\n")
  print(ranges$upper)
}

### TODO:

# 3) Check if log is working


### Open problems:
# 1) Distributions like nbinom where 2 params ("prob" and "mu") describe the same but only one may be set and 
#    none of them has a default value derived from the other
# 2) Distributions like "unif" where the parameters interact -> ranges can be represented as [lower, upper] but rather as min <= max

### Possible solutions -> setting correct values manually


## TODO: 
# definitionsbereich der Verteilung zurück geben 
#1 rechteschranke --> inf, oder vom parameter abh oder fix 
#2 linkeschrank --> inf oder vom parameter abh oder fix (O bei expvert)
# um zu lösen Probl von stetiger gleichvert. 






