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

## additional TODO:
# check whether param name contains prob or sth like that -> range should be [0,1]

families <- res$family
fam <- "binom" # "beta

# idea: all params need to be present in the r... method for generating random samples from the distribution
all_params <- as.list(args(paste0("r", fam)))
all_params <- all_params[! names(all_params) %in% c("", "n", "nn")]    # remove empty and the n_samples argument


# TODO: in cases like gamma distribution we have multiple parameters describing the same (e.g. rate and scale) -> drop one of them


## Find default values ----------------------------------------------------------------------------------------------------

# first we need to find one set of values for each of the params that actually works before we can check for valid values for each
# of the single params individually
get_default_values <- function(all_params, fam) {
  missing_defaults <- sapply(all_params, function(x) typeof(x) == "symbol")   # missing values seem to have type "symbol"
  
  if (sum(missing_defaults) > 0) {
    with_defaults <- all_params[!missing_defaults]
    non_defaults <- all_params[missing_defaults]
    
    # both floats and integers and both positive and negative as well as 0 so that at least one of those hopefully is valid
    default_guesses <- c(1, 0.5, 0, -0.5, -1)
    
    # create a dataframe with all combinations of default guesses
    combs <- expand.grid(lapply(non_defaults, function(x) default_guesses))
    combs_list <- split(combs, seq(nrow(combs)))   # convert to list for iteration
    valid_params <- NULL
    for (i in 1:length(combs_list)) {
      
      # combine fixed with guessed default values and try to generate a random number
      curr_params <- c(with_defaults, combs_list[[i]])
      res <- suppressWarnings(do.call(paste0("r", fam), c(list(n=1), curr_params)))
      
      # break if we've found a set of valid values
      if (!is.na(res)){
        valid_params <- curr_params
        cat("Found the following set of valid default values for family", fam, ":", 
            paste(names(valid_params), valid_params , sep=": ", collapse=", "), "\n")
        break
      } 
    }
    if (is.null(valid_params)) {
      cat("Could not find a set of valid default values for family", fam, "\n")
    }
    return(valid_params)
  }
}

all_params <- get_default_values(all_params, fam)


## Get parameter ranges -----------------------------------------------------------------------------------------------------

## Now we can iterate over the parameters while keeping fixed all others in order to guess some valid ranges
# Note that all_params will now always contain some default values for each of the params (as long as one has been found)

# function that checks if all "values" are valid for "param". "all_params" contains the values of the other params of the family "fam"
check_values_for_param <- function(param, all_params, fam, values) {
  res <- suppressWarnings(
    sapply(values, function(x) {
      all_params[[param]] <- x;    # set the value of "param" to each of the "values" that should be tested
      do.call(paste0("r", fam), c(list(n=1), all_params))
    })
  )
  return(!is.na(res))      # TRUE if value was valid, FALSE if not
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
    cat(param, "-> Currently checking values in the interval [", min(vals), ",", max(vals), "]\n")
    check_res <- check_values_for_param(param, all_params, fam, vals)
    cur_val <- ifelse(is_min, min(vals[check_res]), max(vals[check_res]))
    
    # if the smallest (or highest) of the tested values was valid we can break
    # however this should not happen when the method is used as below
    if (cur_val == border) return(cur_val)
  }
  cat(param, "-> Final value:", cur_val, "\n")
  return(cur_val)
}

# example (for beta family)
iterate_min_max_vals("shape1", all_params, fam, cur_val=0, step_sizes = c(1, 0.5, 0.1, 0.05))


### Main loop for generating the info for each of the params ------------------------------------------------------

lower <- upper <- rep(0, length(all_params))
names(lower) <- names(upper) <- names(all_params)

for (param in names(all_params)){
  
  # 1) Try to find upper and lower bounds (if there are some)
  initial_min_val <- -1e6
  initial_max_val <- 1e6
  step_sizes <- c(1e5, 50, 10, 1, 0.1, 0.01)
  vals <- seq(initial_min_val, initial_max_val, by = step_sizes[1])
  cat("current step size:", step_sizes[1], "\n")
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
  
  # 2) Try to check if float or integer
  
}

lower
upper









