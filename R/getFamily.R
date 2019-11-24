getFamily <- function(pkg){
  
  # load package pkg (can potentially lead to errors if some requirements are not fulfilled)

  ## seltsame Konstruktion.
  ## Ist
  ## load_successful <- !is(try( suppressPackageStartupMessages(library(pkg, character.only=TRUE)), silent=TRUE), "try-error")
  ## nicht einfacher?
  load_successful <- tryCatch({
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
    TRUE
    }, error = function(e) {
      message(e)
      FALSE
      }
    )
  if (!load_successful) return(list())
  
  ## all functions starting with r, d, p or q
  ## rdpq ist ein   paste(start_chars, collapse="")  
  possible_dists <- lsf.str(paste0("package:", pkg), pattern="^[rdpq]")
  
  if (length(possible_dists) == 0) return(list())
  
  start_chars <- c("d", "p", "q", "r")  ## definitionen nach oben
  first_args <- c("x", "q", "p", "n")    # first parameters of the d, p, q, r functions
  l <- list()
  ## l <- vector("list", length(start_chars))
  ## names(l) <- start_chars
  
  # function for checking whether the first argument of fun in first_arg (used with first_arg = "x", "n",...)
  check_first_param <- function(fun, first_arg) {
    f_arg <- names(formals(fun))[1]
    if(length(f_arg) == 0) FALSE else f_arg == first_arg ## genau dafuer gibt es &&
  }
  
  for (i in 1:length(start_chars)) {
    char <- start_chars[i]
    first_arg <- first_args[i] ## wird nur 1x verwendet
    subset <- grep(paste0("^", char), possible_dists, value=TRUE)                     # all functions starting with char

    ## mit obigen nur noch
    ##   if (length(subset) != 0) ...
    if (length(subset) == 0) {
      l[[char]] <- c()
    } else {
      valid_idx <- sapply(subset, check_first_param, first_arg = first_arg)             # check if all functions have the correct first arg
      # print(valid_idx)
      l[[char]] <- subset[valid_idx]
    }
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
  
  families <- names(freq)
  
  # list of lists, where each sublist has the form list(package=some_pkg, family=some_family)
  families <- lapply(families, function(x) list(package=pkg, family=x))
  
  return(families)
  
  
  # # list all methods in package
  # l <- ls(paste0('package:',pkg))
  # if(length(l) == 0) return(list())
  # 
  # # apply regex to find all method-names matching the criteria
  # m <- str_match_all(l, '^([rqpd])(.+)$')
  # m <- do.call(rbind, m)
  # 
  # nMethods <- dim(m)[1]
  # if (nMethods==0) return(list())
  # 
  # # find all methodnames, which start 
  # # double for-loop is slow, but does not risk running out of RAM
  # occ <- vector(mode='double', length = nMethods)
  # for(i in 1:nMethods){
  #   for(j in 1:nMethods){
  #     if(m[i,3]==m[j,3]){
  #       occ[i] <- occ[i] + 1
  #     }
  #   }
  # }
  # 
  # families <- unique(m[occ>1,3])
  # 
  # ret <- lapply(families, function(fam){list('package'=pkg, 'family'=fam)})
  # return(ret)
}



