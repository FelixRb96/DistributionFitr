
require(stringr)


getFamily <- function(pkg){
  
  # load package pkg (can potentially lead to errors if some requirements are not fulfilled)
  load_successful <- tryCatch({
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
    TRUE
    }, error = function(e) {
      message(e)
      FALSE
      }
    )
  if (!load_successful) return(list())
  
  
  # list all methods in package
  l <- ls(paste0('package:',pkg))
  if(length(l) == 0) return(list())
  
  # apply regex to find all method-names matching the criteria
  m <- str_match_all(l, '^([rqpd])(.+)$')
  m <- do.call(rbind, m)
  
  nMethods <- dim(m)[1]
  if (nMethods==0) return(list())
  
  # find all methodnames, which start 
  # double for-loop is slow, but does not risk running out of RAM
  occ <- vector(mode='double', length = nMethods)
  for(i in 1:nMethods){
    for(j in 1:nMethods){
      if(m[i,3]==m[j,3]){
        occ[i] <- occ[i] + 1
      }
    }
  }
  
  families <- unique(m[occ>1,3])
  
  ret <- lapply(families, function(fam){list('package'=pkg, 'family'=fam)})
  return(ret)
}

# TODO: needs to check whether r and d methods exist, because we definetely need those 2



