
require(stringr)


getFamily <- function(pkg){
  # load package pkg
  library(pkg, character.only=TRUE)
  
  # list all methods in package
  l <- ls(paste0('package:',pkg))
  
  # apply regex to find all method-names matching the criteria
  m <- str_match_all(l, '^([rqpd])(.+)$')
  m <- do.call(rbind, m)
  
  nMethods <- dim(m)[1]
  
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




