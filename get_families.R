source("getParams.R")
source("getFamily.R")


#iterate over packages and extract families and params
iterate_packages <- function(packages) {
  
  res <- list()
  
  
  #iterate over packages
  for (i in 1:length(packages)) {
    package_content <- getFamily(packages[i])
    
    #iterate over all families withing package
    for (j in 1:length(package_content)) {
      #
      stopifnot(package_content[[j]]$package == packages[i])
      
      
      #fetch params for each family and add to df
      params <- tryCatch(get_params(package_content[[j]]),
                         error = function(e) {
                           message("Error occured for family ", package_content[[j]]$family, "\n")
                           message(e)
                           NULL
                         })
      if (!is.null(params))
        res[[length(res) + 1]] <- c(package_content[[j]], list(family_info = params))
    }
  }
  
  return(res) 
  
}


## internal
construct_package_list <- function(all.packages) {
 
  if (all.packages == TRUE)
  {
    all.packages <- as.vector(installed.packages()[,"Package"])
    
  } else if (all.packages == FALSE) 
  {
    #only select "base" and "recommended" packages
    ins_pck <- installed.packages()
    
    ## boolean value of what to keep
    package_filter <- ((ins_pck[,"Priority"] == "base" | ins_pck[,"Priority"] == "recommended") & !is.na(ins_pck[,"Priority"]))
    
    all.packages <- as.vector(ins_pck[package_filter,"Package"])
    
    ##neccessary?
    rm(ins_pck)
  }
  
  return(all.packages)

  ##Tim
  
}

# write_file <- function(family_list, file="all_families.Rda") {   # do we need family list as an argument here?
write_file <- function(file="all_families.Rda") {
  ###save list as txt
  ##Adrian
  family_list <- iterate_packages(c("stats"))   # TODO: extend to all packages instead of just stats
  save(family_list, file=file)
}



get_families <- function(file="all_families.Rda") {
  if (! (file %in% list.files()) ) write_file(file=file)
  #read .txt and return list of lists
  load("all_families.Rda")
  return(family_list)
}