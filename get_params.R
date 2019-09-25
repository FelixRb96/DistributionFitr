if (! "stringr" %in% installed.packages()) install.packages("stringr")
library(stringr)

### 1) Get all distributions within a package: ---------------------------------------------------------------------

package <- "stats"
possible_dists <- lsf.str(paste0("package:", package), pattern="^[rdpq]")
possible_dists
start_chars <- c("d", "p", "q", "r")
first_args <- c("x", "q", "p", "n")
l <- list()
for (i in 1:length(start_chars)) {
  char <- start_chars[i]
  first_arg <- first_args[i]
  subset <- grep(paste0("^", char), possible_dists, value=TRUE)
  valid_idx <- sapply(subset, function(x) names(as.list(args(x)))[1] == first_arg)
  print(valid_idx)
  l[[char]] <- subset[valid_idx]
}

get_endings <- function(vec) str_sub(vec, start=2)

l_endings <- lapply(l, get_endings)

# we definitely need a function for the density starting with d, as otherwise we cannot evaluate likelihood function
# so we only take the endings from p, q and r that also appear in d
for (char in start_chars[-1]) {
  l_endings[[char]] <- intersect(l_endings[[char]], l_endings$d)
}

freq <- table(unlist(l_endings))
freq <- freq[freq>=2]
freq

res <- list(package = package, family=names(freq))
res


### 2) Try to get the parameters from a distribution ----------------------------------------------------------------

## Main ideas:
# 1) extract all possible params from r... function as this should have all names
# 2) check if all of them have default values set
# 2.1) If yes got to 3)
# 2.2) If not for each param with missing default value guess such a value and test if r..(1, params) returns a value or NA
#      If a valid value is returned take the current set of default values, otherwise try a different combination of default values
# 3) For each of the params test some values (non-integer, negative, not in [0,1]) while keeping the others at their defaults

## additional TODOs:
# check whether param name contains prob or sth like that -> range should be [0,1]

families <- res$family
fam <- families[1]

# idea: all params need to be present in the r... method for generating random samples from the distribution
all_params <- as.list(args(paste0("r", fam)))
all_params <- all_params[! names(all_params) %in% c("", "n", "nn")]    # remove empty and the n_samples argument

# TODO: in cases like gamma distribution we have multiple parameters describing the same (e.g. rate and scale) -> drop one of them

# first we need to find one set of values for each of the params that actually works before we can check for valid values for each
# of the single params

get_default_values <- function(all_params, fam) {
  missing_defaults <- sapply(all_params, function(x) typeof(x) == "symbol")
  
  if (sum(missing_defaults) > 0) {
    with_defaults <- all_params[!missing_defaults]
    non_defaults <- all_params[missing_defaults]
    # both floats and integers and both positive and negative as well as 0 so that at least one of those hopefully is valid
    default_guesses <- c(1, 0.5, 0, -0.5, -1)
    
    # create a dataframe with all combinations of default guesses
    combs <- expand.grid(lapply(non_defaults, function(x) default_guesses))
    combs_list <- split(combs, seq(nrow(combs)))
    valid_params <- NULL
    for (i in 1:length(combs_list)) {
      curr_params <- c(with_defaults, combs_list[[i]])
      res <- suppressWarnings(do.call(paste0("r", fam), c(list(n=1),curr_params)))
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

## Now we can iterate over the parameters while keeping fixed all others in order to gues some valid ranges
# Note that all_params will now always contain some default values for each of the params (as long as one has been found)
lower <- upper <- rep(0, length(all_params))
names(lower) <- names(upper) <- names(all_params)

check_values_for_param <- function(param, all_params, fam, values) {
  res <- suppressWarnings(
    sapply(values, function(x) {
      all_params[[param]] <- x;
      do.call(paste0("r", fam), c(list(n=1), all_params))
    })
  )
  return(!is.na(res))
}

check_values_for_param("shape1", all_params, fam, values=c(-100, -10, -1))

for (param in names(all_params)){
  # 1) Try to find upper and lower bounds (if there are some)
  min_val <- -1e6
  max_val <- 1e6
  step_sizes <- c(1e5, 50, 10, 1, 0.1, 0.05)
  for (i in 1: length(step_sizes)) {
    cat("current step size:", step_sizes[i], "\n")
    vals <- seq(min_val, max_val, by = step_sizes[i])
    check_res <- check_values_for_param(param, all_params, fam, vals)
    if (all(check_res)) {
      cat("All values valid. Taking range", min_val, "to", max_val, "\n")
      break
    } else if (all(!check_res)) {
      cat("No value valid. Proceeding with next smaller step size\n")
      min_val <- min_val / 2
      max_val <- max_val / 2
    } else {
      min_val <- min(vals[check_res])
      max_val <- max(vals[check_res])
    }
  }
  lower[param] <- min_val
  upper[param] <- max_val
  
  # 2) Try to check if float or integer
  
}










