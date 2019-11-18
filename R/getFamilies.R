
#iterate over packages and extract families and params
iterate_packages <- function(packages) {
  
  res <- list()
  
  #iterate over packages
  for (i in 1:length(packages)) {
    cat("Current Package:", packages[i])
    package_content <- getFamily(packages[i])
    cat("\tNumber of families:", length(package_content), "\n")
    
    # no distribution family contained in package
    if (length(package_content) == 0) next
    
    #iterate over all families withing package
    for (j in 1:length(package_content)) {
      #
      stopifnot(package_content[[j]]$package == packages[i])
      
      cat("Current Family:", package_content[[j]]$family, "\n")
      #fetch params for each family and add to 
      params <- tryCatch(getParams(package_content[[j]]),
                         error = function(e) {
                           message("Error occured for family ", package_content[[j]]$family)
                           message(e, "\n")
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
  
  
}


write_file <- function(family_list, file="all_families.R") {
  dput(family_list, file=file)
}


# Case 1: all.packages gegeben: Suche nach Verteilungsfamilien
# Case 1.1 vector of strings (Liste von Packetnamen) -> take families from those packages
# Case 1.2 FALSE -> only take recommended / base packages
# Case 1.3 TRUE -> take all installed packages
# Case 2 all.packages missing: Take families saved in the file

getFamilies <- function(all.packages, file="../R/all_families.R") {
  # CASE 2:
  if (missing(all.packages)) {
    if (! (file %in% list.files()) ) getFamilies(all.packages = FALSE, file=file)
    #read file and return list of lists
    family_list <- dget(file=file)
    return(family_list)
  }
  
  # CASE 1.3
  if (length(all.packages) == 1 && isTRUE(all.packages)) {
    family_list <- iterate_packages(construct_package_list(all.packages = TRUE))
    write_file(family_list=family_list,file="all_families.R")
    return(family_list)
  }
  
  # CASE 1.2
  if (length(all.packages) == 1 && isFALSE(all.packages)) {
    family_list <- iterate_packages(construct_package_list(all.packages = FALSE))
    write_file(family_list=family_list,file="all_families.R")
    return(family_list)
  }
  
  # CASE 1.1
  family_list <- iterate_packages(all.packages)
  write_file(family_list=family_list,file="all_families.R")
  return(family_list)
}