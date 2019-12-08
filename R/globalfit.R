## Authors 
## Moritz Lauff, mlauff@mail.uni-mannheim.de
## Kiril Dik, kdik@mail.uni-mannheim.de
## Moritz Kern, mkern@mail.uni-mannheim.de
## Nadine Tampe, ntampe@mail.uni-mannheim.de
## Borui Niklas Zhu, bzhu@mail.uni-mannheim.de
## Benedikt Geier, bgeier@mail.uni-mannheim.de
##
## Fit multiple distribution families to a given univariate dataset
##
## Copyright (C) 2019 -- 2020 
## Moritz Lauff, Kiril Dik, Moritz Kern, Nadine Tampe, Borui Niklas Zhu, 
## Benedikt Geier
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 3
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.




### 1)  necessary helper functions --------------------------------------------

# determine decimals
getDecimals <- function(x){
  
  return(round(x %% 1, 10))
  
}


some_percent <- function(decs, numbers, percent){
  # some_percent: TRUE, if for a given number of decimals
  #               at least a proportion of 'percent' out of
  #               all possible decimals occurs 
  
  for (i in min(numbers):max(numbers)){
    if (length(unique(decs[numbers == i])) >= percent * 10){
      return(TRUE) 
    }
  }
  return(FALSE)
}


# test, if data is discrete
is.discrete <- function(data, border = 0.35, percent = 0.8){
  
  # # convert data, if not a vector
  if(is.data.frame(data)){
    data <- as.vector(data[,1])
  }
  
  # remove NA's
  data <- data[!is.na(data)]
  
  obs <- length(data)
  decs <- getDecimals(data)
  numbers <- nchar(as.character(decs)) - 2
  numbers[numbers == -1] <- 1
  n_unique_dec <- length(unique(decs))
  
  if (1 / border > obs){
    border <- 1 / obs
  }
  
  if (n_unique_dec / obs <= border && 
      !any(numbers >= 4) && 
      !some_percent(decs, numbers, percent)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# treatment of discrete non-integral numbers
disc_trafo <- function(data){
  
  # transformation only if data is discrete
  if (is.discrete(data)){ 
    
    data_new <- sort(data) # sort the data
    ## Keine data.frames! Die sind i.W. nur fuer user
    # data with corresponding decimals
    data_new <- data.frame(data_new = data_new,
                           decimals = getDecimals(data_new)) 
    
    # occuring distinct decimals
    unique_decimals <- sort(unique(data_new$decimals))
    m <- length(unique_decimals) # number of occuring distinct decimals
    
    # decimals and transformed decimals
    decimals <- data.frame(original_decimals = unique_decimals,
                           new_decimals = round(seq(0, (m-1)/m, 1/m), 10))
    
    # put together
    data_new <- merge(data_new, decimals,  
                      by.x = "decimals", by.y = "original_decimals") 
    
    data_new <- (data_new$data_new - data_new$decimals 
                 + data_new$new_decimals) * m # new data
    
    return(list(data = data_new,
                trafo_df = decimals,
                discrete = TRUE,
                trafo_decription = 
                  paste0("Divide the simulated data by ",
                         m,
                         " and replace the decimals c(", 
                         paste(decimals$original_decimals, collapse=", "),
                         ") of the simulated data by c(",
                         paste(decimals$new_decimals, collapse=", "),
                         ").")))
    
  } else {
    return(list(data = data,
                trafo_df = NULL,
                discrete = FALSE,
                trafo_decription = NULL))
  }
  
}



### 2) Main Function ----------------------------------------------------------

globalfit <- function(data, continuity = NULL, method = "MLE", verbose = TRUE,
                      packages = "stats", append_packages = FALSE,
                      cores = NULL, max_dim_discrete = Inf,
                      sanity = 1, timeout = 15, ...) {
  ic <- "AIC"

  all_funs <- c('%@%', 'check_integer', 'check_log', 'check_values_for_param', 
                'construct_package_list', 'disc_trafo', 'eval_with_timeout', 
                'fitting_sanity_check', 'get_all_params', 
                'get_best_result_from_progress', 'get_default_values', 
                'get_fun_from_package', 'get_fun_from_package_internal', 
                'get_param_ranges', 'get_support', 'getDecimals', 'getFamilies', 
                'getFamily', 'getParams', 'globalfit', 'IC', 
                'informationCriteria', 'is.discrete', 'is.natural', 
                'iterate_min_max_vals', 'iterate_packages', 'loglik', 
                'optimParamsContinuous', 'optimParamsDiscrete', 'print', 
                'sample_data', 'sample_params', 'some_percent', 'sort', 
                'standardizeFam', 'validate_values', 'write_file')
  
  ## Input Validation

  if ( length(sanity) != 1 || !( (is.numeric(sanity) && sanity >= 0) ||
			 sanity == FALSE) ) {
    stop("Invalid input for argument 'sanity'.")
  }
  if( length(timeout) != 1 || !(is.logical(timeout) || is.numeric(timeout) ) ||
     (is.numeric(timeout) && timeout < 0) ||
     (is.logical(timeout) && timeout == TRUE))
    stop("Invalid input for argument 'timeout'")

  families <- FamilyList
  
  if(length(packages) > 0) {
    
    if(is.vector(packages) && typeof(packages) == "character") {
      
      missing_pkgs <- setdiff(packages, rownames(installed.packages()))
      if (length(missing_pkgs) > 0) {
        message("The following packages were provided to argument 'packages' 
                but are not installed, so they will be ignored. ",
                "Please install manually: ",
                paste(missing_pkgs, collapse = ", "))
      }
      
      packages <- intersect(packages, rownames(installed.packages()))
      
      known_packages <- unique(sapply(families, function(x) x$package))
      
      additionals <- setdiff(packages, known_packages)
      if (length(additionals) > 0) {
        message("The following packages were provided in argument 'packages' 
                but are not part of the default set of packages: ",
                paste(additionals, collapse=", "),
                "\nThus the distribution families in those packages need to be 
                extracted now, which might take some time. ",
                "When executed multiple times, consider extracting those 
                families once with 'getFamilies(packages)' ",
                "and provide the result of that to argument 'packages.'")
        additionals_info <- iterate_packages(additionals)
        if (length(additionals_info) == 0) {
          message("No distribution families found in the additionally 
                  provided packages.")
        }
      } else {
        additionals_info <- list()
      }
      
      # add the manual ones to FamilyList as used in default
      if(append_packages) {
        families <- c(families, additionals_info)
        
        # ignore whatever else is in FamilyList
      } else {
        # these are specified by the user, but params are known
        known <- packages[! packages %in% additionals] 
        known <- families[ which(sapply(families, 
                                        function(x) x$package %in% known)) ]
        families <- c(known, additionals_info)
      }
      
    } else if(is.list(packages)) {
      # in the future this should be deprecated in favour of an S4 object
      # with better validity check 
      if(append_packages) {
        families <- c(families, additionals)
      } else {
        families <- packages
      }
    } else {
      stop("Invalid argument 'packages'.")
    }
  }
  
  if (length(families) == 0) {
    stop("The provided input to argument 'packages' did not 
         contain any distribution family. Can not optimize.")
  }
  
  # filter out those distributions that have too many discrete parameters.
  if(max_dim_discrete < Inf) {
    families <- families[which(sapply(families, 
                              function(x) {
                                sum(x$family_info$discrete) <= max_dim_discrete
                                } )) ]
  }
  
  # indices for the discrete distributions
  discrete_families <- which(sapply(families, 
                                    function(x) x$family_info$discrete))
  
  # if not specified, find out whether data is continuous or not
  if (is.null(continuity)){
    trafo_list <- disc_trafo(data)
    data <- trafo_list$data
    continuity <- !trafo_list$discrete
  } 
  
  # the following simpler version is not working as vector[-integer(0)] = empty
  # relevant_families <-  families[if (continuity) -discrete_families else discrete_families]
  if (continuity) {
    relevant_families <- 
      if (length(discrete_families) > 0) families[-discrete_families] else families
  } else {
    if (length(discrete_families) > 0)
      relevant_families <- families[discrete_families]
    else
      stop("Data is assumed to be discrete but there are no discrete distributions", 
           "contained in the set of distributions to be compared")
  }
  
  # Again check that all of the families that should 
  # be compared are also installed
  all_pkgs <- sapply(relevant_families, function(x) x$package)
  all_pkgs_unique <- unique(all_pkgs)
  missing_pkgs <- setdiff(all_pkgs_unique, rownames(installed.packages()))
  if (length(missing_pkgs) > 0) {
    message("The following packages are not installed, and are thus ignored 
            during optimisation. ",
            "If you want to use them please install manually: ", 
            paste(missing_pkgs, collapse=", "))
    relevant_families <- relevant_families[!(all_pkgs %in% missing_pkgs)]
  }
  
  if(length(relevant_families) < 1)
    stop("No relevant distribution families to fit or compare.")

  if(verbose)
    message("Comparing the following distribution families: ", 
            paste(sapply(relevant_families, function(x) x$family), 
                  collapse = ", "))
  
  if(is.null(cores)) {
    CRAN_check_limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (length(CRAN_check_limit) > 0 && CRAN_check_limit == TRUE) cores <- 2
    ## CRAN_check_limit == TRUE because it might not be a boolean
    else cores <- detectCores()
  }
  if(verbose)
    message('Parallelizing over ', cores, ' cores.\n')
  cl <- makeCluster(cores, outfile = 'log.txt')
  
  # for showing a progressbar we apparently need to use a SNOW cluster
  # registerDoParallel(cl)
  if (verbose) {
    if ( !"doSNOW" %in% rownames(installed.packages()) ) {
      message("(Install the package 'doSNOW' to show a progress bar.)")
      registerDoParallel(cl)
      opts <- c()
    } else {
      doSNOW::registerDoSNOW(cl)
      message("Optimization Progress")
      pb <- txtProgressBar(max = length(relevant_families), style = 3)
      progress_fn <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress_fn)
    }
  } else {
    registerDoParallel(cl)
    opts <- c()
  }
  
  i <- NULL ## BNZ: to prevent an issue, seems to be related to parallel. 
            ##      Do not delete!
  output_liste <- foreach(i=1:length(relevant_families), .packages = c(), 
                          .errorhandling = 'remove', .verbose = FALSE, 
                          .export = all_funs, .inorder = FALSE,
                          .options.snow = opts) %dopar% {
                            
    fam <- relevant_families[[i]]
    t <- Sys.time()
    
    if(verbose)
      message("Current Family: ", fam$family , " from Package: ",
              fam$package)
    
    output_liste <- eval_with_timeout(
      optimParamsDiscrete(data = data,
                          family = fam[c('package', 'family')],
                          family_info = fam$family_info,
                          method = 'MLE', prior = NULL, 
                          log = fam$family_info$log,
                          optim_method = 'L-BFGS-B', 
                          n_starting_points = 1,
                          debug_error = FALSE, 
                          show_optim_progress=FALSE,
                          on_error_use_best_result=TRUE, 
                          max_discrete_steps=100, plot=FALSE,
                          discrete_fast = TRUE, timeout = timeout),
      return_value_on_timeout = "TIMEOUT",
      timeout = 1.5*timeout)
    
    if (length(output_liste) == 1 && output_liste == "TIMEOUT") {
      message("Timeout occured for Family ", fam$family)
      output_liste <- NULL
    }
    
    if(!is.null(output_liste) && !is.na(output_liste$value) && 
       !is.infinite(output_liste$value)) {
      output <- new('optimParams', family = fam$family,
                    package = fam$package,
                    estimatedValues = output_liste$par,
                    log_lik = output_liste$value,
                    AIC = output_liste$AIC,
                    BIC = output_liste$BIC,
                    AICc = output_liste$AICc) 
      # aim: check whether solution has good loglik 
      # but does not fit nonetheless
      if(sanity != FALSE) { # input validation ensures:
	                           # either FALSE or valid numeric
        sanity_check <- fitting_sanity_check(output, data, 
                                             continuity = continuity, 
                                             sensitivity = sanity)
        output@sanity <- sanity_check
      }
    } else {
      if(sanity != FALSE)
        sanity_check <- list(hist_check = NA, int_check = NA, gini_check = NA,
			     good = FALSE)
    }
    if(sanity != FALSE && !sanity_check$good) {
      output <- new('optimParams', 
                    family = fam$family,
                    package = fam$package,
                    log_lik = NA_integer_,
                    AIC = NA_integer_,
                    BIC = NA_integer_,
                    AICc = NA_integer_,
                    sanity = sanity_check)
    }
    
    if (verbose) {
      cat("Elapsed time for family", fam$family, "from package", fam$package,
	  ":", difftime(Sys.time(), t, units="secs"), "secs\n")
    }
    
    return(output)
  } # end %dopar%
  stopCluster(cl)
  
  if(sanity != FALSE) {
    # compare gini-coefficients with each other, drop if negative outlier
      # gini <- sapply(output_liste, function(x) x@sanity$gini_check)
      # names(gini) <- sapply(output_liste, function(x) x@family)
      # print(gini)
    boxplot <- boxplot(gini, plot = FALSE)
    indices <- which(gini %in% boxplot$out)
    if (length(indices > 0)) output_liste <- output_liste[-indices]
  }
  
  r <- new('globalfit', data = data, 
           continuity = continuity,
           method = method,
           fits = output_liste)
  r <- sort(r, ic=ic)
  
  r@fits <- r@fits[!is.na(sapply(r@fits, function(x) x %@% ic))]
  
  return(r)
}
