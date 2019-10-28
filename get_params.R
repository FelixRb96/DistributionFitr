if (! "stringr" %in% installed.packages()) install.packages("stringr")
library(stringr)
library(parallel)
source("utils.R")

# ----------------------------------------------------------------------  
# 1) Get all distributions within a package:
# ----------------------------------------------------------------------

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
  # print(valid_idx)
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
freq <- freq[freq>=2]                # only take those distributions that have at least 2 functions implemented
freq

res <- list(package = package, family=names(freq))
res

to_drop <- c("multinom")
res$family <- res$family[! (res$family %in% to_drop)]

# ----------------------------------------------------------------------
# 2) Try to get the parameters (+ infos) from a distribution
# ----------------------------------------------------------------------

## Main ideas:
# 1) extract all possible params from r... function as the r function should need all params
# 2) check if all of them have default values set
# 2.1) If yes go to 3)
# 2.2) If not for each param with missing default value guess such a value and test if r..(1, params) returns a value or NA
#      If a valid value is returned take the current set of default values, otherwise try a different combination of default values
# 3) For each of the params test some values (non-integer, negative, not in [0,1]) while keeping the others at their defaults

## optional TODO:
# check whether param name contains prob or sth like that -> range should be [0,1]

families <- res$family
fam <- "beta"   # gamma

# ----------------------------------------------------------------------
# (2.1) Given distribution family, return list of parameters
# ----------------------------------------------------------------------

get_all_params <- function(fam) {
  # idea: all params need to be present in the r... method for generating random samples from the distribution
  all_params <- formals(paste0("r", fam))
  
  # if "nn" is contained then "n" is probably a real param , c.f. rhyper
  to_remove <- if("nn" %in% names(all_params)) c("", "nn") else c("", "n")
  
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

# data structure "all_params"
# list, for given distribution family;
# list elements name: name of parameter
# list elements field: default value, NULL if not set

# ------------------------------------------------------------------------------------------
# (2.2) Given distribution family, parameters, some x-values: test if combination is valid
# ------------------------------------------------------------------------------------------

.validate_values <- function(fam, n_or_nn, params, x_test) {
  # try to generate a random number from the distribution
  r <- do.call(paste0("r", fam), c(n_or_nn, params))
  # additionally check whether values are valid for density function
  # as this sometimes takes a while we use a timeout to stop the execution after a while
  # however we consider a timeout as a valid parameter value as no error occurs (it just takes to lang)
  if (is.finite(r)) {
    r_ <- eval_with_timeout(do.call(paste0("d", fam), c(x_test, params)), timeout = 1, return_value_on_timeout = "TIMEOUT")
    if (r_ == "TIMEOUT") message(fam, " produced timeout for params ", paste(names(params), params, sep=": ", collapse=","), " on ", x_test)
    any(!is.na(r_))
  } else {
    FALSE
  }
}

# return value: boolean
# does parameter combination in input yield a valid value?

# -----------------------------------------------------------------------------------------------------------
# (2.3) Given updated list of parameters, and a specific family, find one parameter combination that works
# -----------------------------------------------------------------------------------------------------------

# first we need to find one set of values for each of the params that actually works before we can check for valid values for each
# of the single params individually

get_default_values <- function(all_params, fam) {
  missing_defaults <- sapply(all_params, function(x) typeof(x) == "symbol")   # missing values seem to have type "symbol"
  
  if (sum(missing_defaults) == 0) return(all_params)
  
  with_defaults <- all_params[!missing_defaults]
  non_defaults <- all_params[missing_defaults]
  
  # both floats and integers and both positive and negative as well as 0 so that at least one of those hopefully is valid
  # start with 0.5 to avoid extreme values in case of probabilities
  default_guesses <- c(0.5, 1, 0, -0.5, -1)
  
  # create a dataframe with all combinations of default guesses
  combs <- expand.grid(lapply(non_defaults, function(x) default_guesses))
  combs_list <- split(combs, seq(nrow(combs)))   # convert to list for iteration
  valid_params <- NULL
  
  # parameter that describes the number of random numbers to take, usually "n", but in cases like hyper "nn"
  n_or_nn <- if (! "nn" %in% names(as.list(args(paste0("r", fam))))) list(n=1) else list(nn=1)
  x_test <- list(x=seq(-10, 10, 1))
  
  errors <- c()
  for (i in 1:length(combs_list)) {
    
    # combine fixed with guessed default values and try to generate a random number
    # in most cases invalid parameter choices will just generate a warning and return NA, but sometimes also an error is thrown
    # so we need to handle both
    curr_params <- c(with_defaults, combs_list[[i]])
    res <- suppressWarnings(tryCatch({
      .validate_values(fam, n_or_nn, curr_params, x_test)
    },
    error = function(e) {
      errors <<- union(errors, strsplit(as.character(e), ":", fixed = TRUE)[[1]][2])
      return(FALSE)
    }))
    
    # break if we've found a set of valid values
    if (res){
      valid_params <- curr_params
      #cat("Found the following set of valid default values for family", fam, ":", 
      #    paste(names(valid_params), valid_params , sep=": ", collapse=", "), "\n")
      break
    } 
  }
  if (is.null(valid_params)) {
    message("Could not find a set of valid default values for family ", fam)
    message("Errors:", errors)
  }
  return(valid_params)
}

all_params_defaulted <- get_default_values(all_params, fam)

# data structure "all_params", now updated by function "get default values"
# list, for given distribution family;
# list elements name: name of parameter
# list elements field: default value, NULL if not set

#------------------------------------------------------------------------------------------------------ 
# (2.4) Get parameter ranges
# -----------------------------------------------------------------------------------------------------

# Now we can iterate over the parameters while keeping fixed all others in order to guess some valid ranges
# Note that all_params will now always contain some default values for each of the params (as long as one has been found)

# -------------------------------------------------------------------------------- 
# function that checks if all "values" are valid for "param". "all_params" contains the values of the other params of the family "fam"
# -------------------------------------------------------------------------------- 

check_values_for_param <- function(param, all_params, fam, values) {
  
  # parameter that describes the number of random numbers to take, usually "n", but in cases like hyper "nn"
  n_or_nn <- if (! "nn" %in% names(formals(paste0("r", fam)))) list(n=1) else list(nn=1)
  x_test <- list(x=seq(-10, 10, 1))
  
  res <- suppressWarnings(
    sapply(values, function(x) {
      # overwrite the value of "param" to each of the "values" that should be tested
      all_params[[param]] <- x;
      tryCatch(
        .validate_values(fam, n_or_nn, all_params, x_test),
      error=function(e) return(FALSE))
    })
  )
  return(res)
}
# return value: TRUE if value was valid, FALSE if not

#  example (for beta family)
check_values_for_param("shape1", all_params, fam, values=c(-100, -10, -1, 1))

# --------------------------------------------------------------------------- 
# function that iterates over descending step sizes to find the minimal and maximal valid value of a parameter
# --------------------------------------------------------------------------- 

# Explanation:
  # in previous iteration we tested the values [-10, -5, 0, 5, 10], that is step_size was 5
  # valid values: [FALSE, FALSE, TRUE, TRUE, TRUE]
  # now the lower limit can be anywhere between -5 and 0
  # so we test with next step size=1 the values [-5, -4, -3, -2, -1, 0]
  # from those values we again search the minimum valid value and continue in the same way with always smaller steps
  # for the upper bound the procedure is the same, just that we search for valid values in the range higher than the current upper limit

iterate_min_max_vals <- function(param, all_params, fam, cur_val, step_sizes, is_min=TRUE) {
  for (i in 1:(length(step_sizes)-1)) {
    
    # first get the interval that should be tested (lower than cur_val when searching lower limit and higher than cur_val when searchin upper limit)
    if (is_min) {
      border <- cur_val - step_sizes[i]
      vals <- seq(border, cur_val, by = step_sizes[i+1])
    } else{
      border <- cur_val + step_sizes[i]
      vals <- seq(cur_val, border, by = step_sizes[i+1])
    }
    # test values and adjust the current estimate
    # cat(param, "-> Currently checking values in the interval [", min(vals), ",", max(vals), "] with step size", step_sizes[i+1], "\n")
    check_res <- check_values_for_param(param, all_params, fam, vals)
    
    # usually at least the first (when is_min = FALSE) or the last (when is_min = TRUE) should be valid
    # but due to overflow it can happen that none is TRUE then we break with the current value
    if (any(check_res)) {
      cur_val <- ifelse(is_min, min(vals[check_res]), max(vals[check_res]))
    } else {
      return(cur_val)
    }
    
    # if the smallest (or highest) of the tested values was valid we can break
    # however this should usually not happen when the method is used as below (but happens due to float imprecisions...)
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

# --------------------------------------------------------------------------- 
# (2.5) Main function for generating the info for each of the params
# ---------------------------------------------------------------------------

get_param_ranges <- function(all_params, fam) {
  
  # SPECIAL CASE: uniform distribution
  if (fam == "unif") {
    lower <- rep(-Inf, length(all_params))
    upper <- rep(Inf, length(all_params))
    accepts_float <- rep(TRUE, length(all_params))
    names(lower) <- names(upper) <- names(accepts_float) <- names(all_params)
    
    return(list(lower=lower,
                upper=upper,
                accepts_float=accepts_float, 
                defaults=unlist(all_params)))
  }
  
  # create empty result vectors
  lower <- upper <- accepts_float <- rep(NA, length(all_params))
  names(lower) <- names(upper) <- names(accepts_float) <- names(all_params)
  
  for (param in names(all_params)){
    
    # 1) Try to find upper and lower bounds (if there are some)
    initial_min_val <- -1e3
    initial_max_val <- 1e3
    step_sizes <- c(1e2, 50, 10, 1, 0.1, 0.01)
    
    # add the default value to have at least one valid entry
    vals <- seq(initial_min_val, initial_max_val, by = step_sizes[1]) + all_params[[param]]  
    # cat("current step size:", step_sizes[1], "\n")
    check_res <- check_values_for_param(param, all_params, fam, vals)
    
    # if lowest or highest value was valid in the first check we already have an min_val or max_val
    # otherwise we iterate with the above method
    if (check_res[1]) {
      min_val <- - Inf
    } else {
      min_val <- iterate_min_max_vals(param=param, all_params = all_params, fam=fam,
                                      cur_val = min(vals[check_res]), step_sizes = step_sizes, is_min = TRUE)
    }
    
    if (check_res[length(check_res)]) {
      max_val <- Inf
    } else {
      max_val <- iterate_min_max_vals(param=param, all_params = all_params, fam=fam,
                                      cur_val = max(vals[check_res]), step_sizes = step_sizes, is_min = FALSE)
    }
    # set the estimated values in the named vactor that will be returned
    lower[param] <- min_val
    upper[param] <- max_val
    
    # 2) Check if the parameter accepts floats or only integers
    n <- 10
    testsequence <- runif(n, max(min_val, -1e3), min(max_val, 1e3))
    testsequence <- unique(c(testsequence, trunc(testsequence)))
    num_tests <- length(testsequence)

    is_integer <- testsequence %% 1 == 0
    num_integer <- sum(is_integer)

    testoutcome <- check_values_for_param(param, all_params, fam, testsequence)
    accepted_int <- sum(is_integer & testoutcome)
    accepted_int_rate <- accepted_int/num_integer
    accepted_float <- sum(!is_integer & testoutcome)
    accepted_float_rate <- accepted_float/(num_tests - num_integer)

    if(accepted_float_rate > 1/(num_tests - num_integer)) accepts_float[param] <- TRUE
    else if(accepted_int_rate == 0) stop("distribution does not seem to accept any values")
    else accepts_float[param] <- FALSE
  }
  return(list(lower=lower,
              upper=upper,
              accepts_float=accepts_float, 
              defaults=unlist(all_params)))
}

# return value: 4 component list
# each list entry: vector with length = number of parameters
#                  each list entry saves either lower, upper, or etc.

# example:
get_param_ranges(all_params_defaulted, fam)


### Function that checks if log is working ---------------------------------------------------------------------------
check_log <- function(fam) {
  if('log' %in% names(formals(paste0('d', fam)))) {
    return(T)
  } else {
    return(F)
  }
}


### Function that checks whether a family is a discrete distribution, that is it only takes integers as values ----------
check_integer <- function(fam, all_params) {
  n_test <- 10
  n_or_nn <- if (! "nn" %in% names(formals(paste0("r", fam)))) list(n=n_test) else list(nn=n_test)
  args_ <- c(all_params, n_or_nn)
  res <- do.call(paste0("r", fam), args = args_)
  return(all(abs(res %% 1) < sqrt(.Machine$double.eps)))
}

# -------------------------------------------------------------------------------- 
# (3) Determining the support of a distribution
# --------------------------------------------------------------------------------

# Function for determining the support of a given distribution family and whether the limits of the support are determined by one
# of the distributions parameters
# params should be a list containing named vectors lower, upper, accepts_float with one entry for each of the distributions parameters
# additionally params$discrete specifies whether fam is a discrete distribution
get_support <- function(fam, params) {
  
  # in case the parameter is unbounded we cap it to avoid extreme values
  low_capped <- pmax(params$lower, -10)
  upp_capped <- pmin(params$upper, 10)
  
  # we additionally ignore the 10% highest and lowest parameter values (e.g. for "binom" we only consider prop in [0.1, 0.9])
  # remark: what happens with values x: lox_capped < x < low_capped + 0.1 * (upp_capped - low_capped), i.e. 0.05?
  low <- ifelse(params$lower == low_capped, low_capped + 0.1 * (upp_capped-low_capped), low_capped)
  upp <- ifelse(params$upper == upp_capped, upp_capped - 0.1 * (upp_capped-low_capped), upp_capped)
  
  # initialize named vector that stores whether each parameter determines the bounds of a distribution
  supp_min_depends_on <- supp_max_depends_on<- rep(FALSE, length(params$lower))
  names(supp_depends_on) <- names(params$lower)
  
  # define the base choices for all parameters that are chosen when only varying one parameter and keeping the others constant
  # base_choices <- (low + upp)/2
  #base_choices <- as.list(ifelse(params$accepts_float, base_choices, round(base_choices)))
  base_choices <- as.list(ifelse(params$accepts_float, params$defaults, round(params$defaults)))
  
  # define the sequence of test points
  precision <- 0.01
  x <- seq(-100, 100, precision)
  if (params$discrete) x <- unique(round(x))
  
  # and add the points to the list of parameter choices
  base_choices$x <- x
  
  # define the number of values to test for each parameter
  n_test <- 11
  
  # initialize limits of support to maximum / minimum possible value each
  support_min <- Inf
  support_max <- -Inf
  
  for (param in names(low)) {
    # define n_test equally distributed values for the current param dependend on its adapted range from above
    param_choices <- seq(low[param], upp[param], length.out = n_test)
    if (!params$accepts_float[param]) param_choices <- trunc(param_choices)
    
    # copy base choices to args_ so that we can change the value for the current param below
    args_ <- base_choices
    
    get_result_mat <- function(param_choices){
      
      # row i of the result matrix will be the density values at x when taking the i-th choice for the current param
      result_mat <- matrix(NA, nrow=length(param_choices), ncol=length(x))
      i <- 1
      for (choice in param_choices) {
        # calulate density value and add to result matrix
        args_[[param]] <- choice
        res <- suppressWarnings(do.call(paste0("d", fam), args = args_))
        result_mat[i, ] <- res
        i <- i+1
      }
      return(result_mat)
    }
    
    result_mat <- get_result_mat(param_choices)
    
    # for each row calculate the minimum and maximum evaluation point with positive density
    row_support_min <- apply(result_mat, 1, function(row) {if (length(which(row>0)) > 0) x[min(which(row>0))] else Inf})
    row_support_max <- apply(result_mat, 1, function(row) {if (length(which(row>0)) > 0) x[max(which(row>0))] else -Inf})
    
    # check if the lower or upper bound is always the same as the current parameter value (up to the chosen precision + some small machine error)
    # then the support depends on the current parameter and the support is at least as big as the possible ranges of this parameter
    # notice that we need to ignore rows where the min or max support value was +/- Inf
    min_criterium <- abs( param_choices-row_support_min )
    max_criterium <- abs( param_choices-row_support_max )
    if(max(min_criterium[is.finite(row_support_min)]) <= precision + 1e-10) {
      supp_min_depends_on[param] <- TRUE
      support_min <- min(params$lower[param], support_min)
      support_max <- max(max(row_support_max), support_max)
    }
    if(max(max_criterium[is.finite(row_support_min)]) <= precision + 1e-10) {
      supp_max_depends_on[param] <- TRUE
      support_max <- max(params$upper[param], support_max)
      support_min <- min(min(row_support_min), support_min)
    }
    
    # else we just adapt the current maximum and minimum support values with the minimum or maximum row support
    if (!supp_depends_on[param]) {
      support_min <- min(min(row_support_min), support_min)
      support_max <- max(max(row_support_max), support_max)
    }
    
    # cat("After param", param, "--> \tsupport_min:", support_min, "\tsupport_max", support_max, "\n")
  }
  # if minimum / maximum support is small / high enough we assume that the support is the whole real line
  if (support_min <= -50) support_min <- -Inf
  if (support_max >= 50) support_max <- Inf
  
  return(list(support_min = support_min, support_max = support_max, 
              supp_max_depends_on=supp_max_depends_on, supp_min_depends_on=supp_min_depends_on))
}

# -------------------------------------------------------------------------------- 
# (5) Final function
# --------------------------------------------------------------------------------

# Input:
  # family: list-> package: package_name, family: name of distribution family inside package
get_params <- function(fam){
  # fam <- family$family
  
  # 1) Get list of all parameters:
  all_params <- get_all_params(fam)
  
  # 2) Add default values to all params that don't have any
  all_params <- get_default_values(all_params, fam)
  
  if (is.null(all_params)) return(NULL)
  
  # 3) Get valid parameter ranges:
  result <- get_param_ranges(all_params, fam)
  
  # 4) Add log argument
  result$log <- check_log(fam)
  
  # 5) Add discrete argument
  result$discrete <- check_integer(fam, all_params)
  
  # 6) Add support informations
  supp <- get_support(fam , result)
  result <- c(result, supp)
  
  return(result)
}

# do not execute when sourced from a different file, like if __name__ == "__main__" in Python
if (sys.nframe()==0) {
  for (fam in families) {
    cat("\nCurrent Family:", fam, "\n")
    result <- get_params(fam)
    print(result)
  }
}


### Open problems:
# 1) Distributions like nbinom where 2 params ("prob" and "mu") describe the same but only one may be set and 
#    none of them has a default value derived from the other
# 2) Distributions like "unif" where the parameters interact -> ranges can be represented as [lower, upper] but rather as min <= max
#  -> SOLVED
# 3) Distribution hyper: here rhyper also works with floats for m,n,k but hyper not, maybe also check d function in check_values_for_param
#    Current errors have to be catched in get_support but it would be better if they didn't occur at all
#  -> SOLVED
# 4) TODO: if support_min/support_max depends on parameter (as given by the respective output), introduce NA.
#          Also: specify on which parameter(s) a certain support bound depends (or the other way 'round? What makes more sense?)
